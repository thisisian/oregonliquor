module Lib
    where

import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack, unpack, null, stripPrefix)
import System.IO (FilePath)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Data.Csv (encode)

-- Strip prefix from strings in a list. If a string does not contain the prefix, it
-- is left untouched.
stripPrefixesL :: Text -> [Text] -> [Text]
stripPrefixesL prefix = map (\x -> fromMaybe x (Data.Text.stripPrefix prefix x))

mapNth :: Int -> (a -> a) -> [a] -> [a]
mapNth n _ [] = []
mapNth n f (x:xs)
  | n == 0 =  f x : xs
  | otherwise = x : mapNth (n - 1) f xs

mapInd :: [Int] -> (a -> a) -> [a] -> [a]
mapInd xs f = mapInd' 0 xs f
  where mapInd' n xs f [] = []
        mapInd' n xs f (a:as) =
          if n `elem` xs
          then f a : mapInd' (n + 1) xs f as
          else a : mapInd' (n + 1) xs f as

-- Like mapNth but will replace a single entry with a list
replaceNth :: Int -> (a -> [a]) -> [a] -> [a]
replaceNth n f xs = take n xs ++ f (xs !! n) ++ drop (n + 1) xs

-- Remove all non-numeral text
onlyNumbers :: Text -> Maybe Text
onlyNumbers t =
  if Data.Text.null match then Nothing else Just match
  where re = "[0-9]+\\.?[0-9]*" :: String
        match = t =~ re :: Text

-- Keep only the matches of a regular expression
keepMatches :: String -> Text -> [Text]
keepMatches re t = _4th (t =~ re :: (Text, Text, Text, [Text]))
  where _4th (_, _, _, x) = x

toLiters :: Text -> Maybe Text
toLiters t
  | t =~ reL :: Bool = Just ((head (t =~ reL :: [[Text]])) !! 1)
  | t =~ reML :: Bool = Just (fromML ((head (t =~ reML :: [[Text]])) !! 1))
  | t =~ "LITER" :: Bool = Just $ pack $ show 1
  | otherwise = Nothing
    where
      reL = "\\`([0-9\\.]+) L\\'" :: String
      reML = "\\`([0-9]+) ML\\'" :: String
      fromML :: Text -> Text
      fromML t = pack $ show ((read (unpack t) :: Float) / 1000)

dumpCSV :: FilePath -> [[Text]] -> IO ()
dumpCSV fp txt =
  LBS.writeFile fp $ encode $ txt
