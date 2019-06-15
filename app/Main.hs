{-# LANGUAGE OverloadedStrings #-}

import Lib
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack, stripPrefix)
import Text.HTML.DOM (parseLBS, readFile)
import Text.XML (Document)
import Text.XML.Cursor
import Crawler (allInventory, cityPage, scrapeProducts, scrapeStores, printResponse, initCrawler, getDoc, sendRequest, scrapeInv, requestDoc)
import Scraper

main :: IO ()
main = do
  crawler <- initCrawler

  Main.products
  Main.stores
  Main.inventory
  return ()

stores :: IO ()
stores = do
  crawler <- initCrawler
  storesTable <- scrapeStores crawler
  dumpCSV "./stores.csv" $ storeLabels : storesTable

products :: IO ()
products = do
  crawler <- initCrawler
  productTable <- scrapeProducts crawler
  dumpCSV "./products.csv" $ productLabels : productTable

inventory :: IO ()
inventory = do
  crawler <- initCrawler
  inventoryTable <- scrapeInv crawler
  dumpCSV "./inventory.csv" $ inventoryTable

productLabels :: [Text]
productLabels = ["Code", "Description", "Size", "Proof", "Age", "Case Price", "Bottle Price", "Category", "Subcategory"]

storeLabels :: [Text]
storeLabels = ["Number", "Name","Street","City","Zip","Phone", "Hours"]
