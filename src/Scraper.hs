{-# LANGUAGE OverloadedStrings #-}

module Scraper
  where

import Lib
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Text.XML (Document)
import Text.XML.Cursor
  ((&/), ($//), (&//), (&|), attributeIs, element, content, fromDocument, attribute)
import Data.Text (Text, strip, null, unpack)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()


-- Scrape store info from a store page
storeInfo :: Document -> [Text]
storeInfo doc =
  let
    cursor = fromDocument doc
    info' =
      cursor
        $// attributeIs "id" "location-display"
        &// content
  in
    replaceNth 6 (keepMatches "Open: (.*)")
    . replaceNth 3 (keepMatches "(.*), OR ([0-9]+)")
    . replaceNth 0 (keepMatches "Store ([0-9]+): (.*)")
    . init
    $ filter
      (\x -> not (Data.Text.null x) && not (x =~ ("inside another business" :: String) ))
      (map (strip) info')

-- Scrape inventory from a store page
inventory :: Document -> Text -> [[Text]]
inventory doc storeNo =
  let
    cursor = fromDocument doc
    inventory' =
      cursor
        $// attributeIs "class" "list"
        &// element "td"
        &// content
  in map (\x -> storeNo : [(head x)]) (chunksOf 8 $ inventory')

-- Get list of cities from the browse locations page
cities :: Document -> [Text]
cities doc =
  let
    cursor = fromDocument doc
    text =
      cursor
      $// attributeIs "id" "browse-content"
      &// element "li"
      &// element "a"
      &/ content
  in map strip text

-- Get store number and link from a city page
stores :: Document -> [(Text, String)]
stores doc =
  let
    cursor = fromDocument doc
    text =
      cursor
        $// attributeIs "class" "store-no"
        &/ element "span"
        &/ content
    link =
      cursor
        $// attributeIs "class" "list"
        &// attribute "onclick"
    re = "window\\.location\\.href='([^']*)"
  in zip text (map (unpack . head . keepMatches re) link)


getProductTable :: Text -> Text -> Document -> [[Text]]
getProductTable cat subcat doc =
  let
    table' =
      map (
        mapNth 2 (\x -> fromMaybe "ERROR" $ toLiters x) .
        mapInd [4,5,6] (\x -> fromMaybe "" $ onlyNumbers x))
      $ (chunksOf 7 $ selectProdTable doc)
  in map (++ [cat, subcat]) table'

selectProdTable :: Document -> [Text]
selectProdTable doc =
  let
    cursor = fromDocument doc
  in
    cursor
        $// attributeIs "class" "list"
        &// element "td"
        &// content

selectProdCat :: Document -> [Text]
selectProdCat doc =
  let
    cursor = fromDocument doc
    mainCat =
      cursor
      $// attributeIs "id" "breadcrumbs"
      &/ element "a"
      &/ content
    subCat =
      cursor
      $// attributeIs "id" "breadcrumbs"
      &/ content
  in
    stripPrefixesL "\n\t>> " $ mainCat ++ [(subCat !! 1)]

-- Get category links from home page
catLinks :: Document -> [Text]
catLinks doc =
  let
    cursor = fromDocument doc
  in
    cursor
      $// attributeIs "id" "browse-content"
      &// element "a"
      &/ content

-- Get sub category link from category page
subCatLinks :: Document -> [(Text, String)]
subCatLinks doc =
  let
    cursor = fromDocument doc
    links =
      cursor
      $// attributeIs "id" "browse-content"
      &/ element "ul"
      &// attribute "href"
    text =
      cursor
      $// attributeIs "id" "browse-content"
      &// element "ul"
      &// element "a"
      &/ content
    re = "- ALL" :: String
  in
    filter
    (\x -> not $ fst x =~ re)
    (zip (map strip text) (map unpack links))
