{-# LANGUAGE OverloadedStrings #-}
module Crawler
  ( initCrawler
  , getDoc
  , sendRequest
  , requestDoc
  , printResponse
  , isErrorPage
  , scrapeProducts
  , scrapeStores
  , scrapeInv
  , cityPage
  , allInventory
  ) where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import qualified Data.Text as T
import Scraper
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Simple (getResponseBody, getResponseHeaders)
import Data.Text.Encoding (encodeUtf8)
import Control.Concurrent (threadDelay)
import Control.Monad (forM, liftM)
import Text.XML (Document)
import Text.HTML.DOM (parseLBS)
import Lib

domain :: String
domain = "http://www.oregonliquorsearch.com"

data Crawler = Crawler
  {
    cCookie :: CookieJar,
    cManager :: Manager
  }

-- Form a link to get all the inventory on one page
allInventory :: (T.Text, String) -> String
allInventory link =
  let locationRowNum = head . keepMatches "locationRowNum=([0-9]+)" $ (T.pack (snd link))
  in "/servlet/FrontController?view=locationdetails&agencyNumber="
     ++ (T.unpack $ fst link) ++ "&action=pagechange&locationRowNum=" ++ (T.unpack locationRowNum)
     ++ "&column=Description&pageSize=100000"

scrapeInv :: Crawler -> IO [[T.Text]]
scrapeInv crawler = do
  locDoc <- requestDoc crawler "/servlet/FrontController?view=browselocations&action=display"
  tables <- forM (cities locDoc)
    (\x -> do
        -- For each city
        scrapeInv' crawler x)
  return (foldr (++) [] tables)

scrapeInv' :: Crawler -> T.Text -> IO [[T.Text]]
scrapeInv' crawler city = do
  links <- liftM (stores) $ requestDoc crawler $ cityPage city
  tables <- forM links
    (\x ->
       -- Do something with each link
       scrapeInv'' crawler city x)
  return (foldr (++) [] tables)

scrapeInv'' :: Crawler -> T.Text -> (T.Text, String) -> IO [[T.Text]]
scrapeInv'' crawler city link = do
  newCrawler <- initCrawler
  sendRequest newCrawler $ cityPage city
  sendRequest newCrawler (snd link)
  doc <- requestDoc newCrawler $ allInventory link
  return $ inventory doc (fst link)

scrapeStores :: Crawler -> IO [[T.Text]]
scrapeStores crawler = do
  locDoc <- requestDoc crawler "/servlet/FrontController?view=browselocations&action=display"
  tables <- forM (cities locDoc)
    (\x -> do
        -- Do something for each city page
        scrapeStores' crawler x)

  return (foldr (++) [] tables)

scrapeStores' :: Crawler -> T.Text -> IO [[T.Text]]
scrapeStores' crawler city =
  if city == "CAMAS VALLEY" -- Hack to skip the vacant store
    then
      do return [[]]
    else do
      links <- liftM (stores) $ requestDoc crawler $ cityPage city
      tables <- forM links
        (\x ->
           -- Do something with each link
           scrapeStores'' crawler x)
      let storeLabels = ["Number", "Name","Street","City","Zip","Phone", "Hours"]
      dumpCSV ((T.unpack city) ++ ".csv") $ storeLabels :  tables
      return tables

scrapeStores'' :: Crawler -> (T.Text, String) -> IO [T.Text]
scrapeStores'' crawler link = do
  doc <- requestDoc crawler (snd link)
  return $ storeInfo doc

scrapeProducts :: Crawler -> IO [[T.Text]]
scrapeProducts crawler = do
  cats <- requestDoc crawler "/servlet/FrontController?view=home&action=categoriesdisplay"
  let links = catLinks cats
  scrapeProducts' crawler links

-- Once we have links to all categories, iterate through each link
scrapeProducts' :: Crawler -> [T.Text] -> IO [[T.Text]]
scrapeProducts' crawler links = do
  tables <- forM links
     (\x -> do
         -- Do something for each link to a subcat
         scrapeProducts'' crawler x)
  return $ foldr (++) [] tables


-- On each link, get links to subcat pages and iterate through those
scrapeProducts'' :: Crawler -> T.Text -> IO [[T.Text]]
scrapeProducts'' crawler link = do
  links <- liftM (subCatLinks) $ requestDoc crawler $ catPage link
  tables <- forM links
    (\x -> do scrapeProducts''' crawler link (fst x) $ snd x)
  return (foldr (++) [] tables)

-- Take a link to a subcat and get the table from it
scrapeProducts''' :: Crawler -> T.Text -> T.Text -> String -> IO [[T.Text]]
scrapeProducts''' crawler cat subcat link  =
  liftM (getProductTable cat subcat) $ requestDoc crawler $ link


initCrawler :: IO Crawler
initCrawler = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest $ domain ++ "/servlet/WelcomeController"
  response <- httpLbs request manager
  let cookie = responseCookieJar response
  return Crawler { cCookie = cookie, cManager = manager }

isErrorPage :: L.ByteString -> Bool
isErrorPage p = "<div class=\"error\">" `S8.isInfixOf` L.toStrict p

catPage :: T.Text -> String
catPage cat =
  "/servlet/FrontController?view=homecategories&action=select&category="
  ++
  T.unpack cat

cityPage :: T.Text -> String
cityPage city =
  "/servlet/FrontController?view=browselocations&action=select&city="
  ++
  T.unpack city

requestDoc :: Crawler -> String -> IO Document
requestDoc crawler path = do
  req <- sendRequest crawler path
  return $ getDoc req

getDoc :: Response L.ByteString -> Document
getDoc r = parseLBS . getResponseBody $ r

sendRequest :: Crawler -> String -> IO (Response L.ByteString)
sendRequest crawler path = do
  threadDelay 3000000 -- We will delay to avoid overloading server
  request' <- parseRequest $ domain ++ path
  let request = request' {cookieJar = Just $ cCookie crawler}
  response <- httpLbs request (cManager crawler)
  if isErrorPage (getResponseBody response)
    then print $ "Error accessing " ++ domain ++ path
    else print $ "Success accessing " ++ domain ++ path
  return response

printResponse :: Response L.ByteString -> IO ()
printResponse response = do
  let msg = encodeUtf8 $ T.pack $ concat
       ["#### CODE:"
       , show $ statusCode $ responseStatus response
       , "\n"
       ]
  L.putStrLn $ getResponseBody response
  S8.putStr msg
  putStrLn "### COOKIES:"
  print $ responseCookieJar response
  putStrLn "### HEADERS:"
  print $ getResponseHeaders response
