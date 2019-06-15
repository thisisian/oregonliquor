{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Lib
import Crawler
import Scraper
import Text.HTML.DOM (readFile)
import qualified Data.ByteString.Lazy as L

errorPage = "./test/test_pages/errorPage.html"
catPage = "./test/test_pages/catPage.html"
subCatPage = "./test/test_pages/subCatPage.html"
homePage = "./test/test_pages/homePage.html"
storePage = "./test/test_pages/storePage.html"
locPage = "./test/test_pages/locPage.html"
subLocPage = "./test/test_pages/subLocPage.html"

main :: IO ()
main = do
  -- cralwer <- initCrawler
  hspec $ do
    describe "Crawler" $ do
      it "correctly detects error pages" $ do
        badFile <- L.readFile errorPage
        isErrorPage badFile `shouldBe` True
        goodFile <- L.readFile catPage
        isErrorPage goodFile `shouldBe` False
      it "correctly creates the link for the entire inventory" $ do
        allInventory ("1154", "/servlet/FrontController?view=browsesublocations&action=select&agencyNumber=1154&locationRowNum=1")
        `shouldBe`
        "/servlet/FrontController?view=locationdetails&agencyNumber=1154&action=pagechange&locationRowNum=1&column=Description&pageSize=1000000"




    describe "Scraper" $ do
      it "gets each category from the homepage" $ do
        doc <- Text.HTML.DOM.readFile homePage
        let res = catLinks doc
        res `shouldBe` ["RUM","CORDIALS","TEQUILA","BRANDY / COGNAC","DOMESTIC WHISKEY","COCKTAILS","GIN","VODKA","SCOTCH","CANADIAN","IRISH","MEZCAL","OTHER IMPORTED WHISKY","VERMOUTH","CACHACA","NEUTRAL GRAIN SPIRIT","NOT CATEGORIZED"]
      it "gets links to each subcat page from a category page" $ do
        doc <- Text.HTML.DOM.readFile catPage
        let res = subCatLinks doc
        length res `shouldBe` 24
      it "gets store info from a store page" $ do
        doc <- Text.HTML.DOM.readFile storePage
        let res = storeInfo doc
        res `shouldBe`
          ["1064","Arlington", "200 Arlington Mall", "ARLINGTON", "97812",
           "541-454-2633", "8-6 M-S; 10-2 Sun"]
      it "gets inventory info from a store page" $ do
        doc <- Text.HTML.DOM.readFile storePage
        let res = inventory doc "12"
        res `shouldBe` [["12","3302B"],["12","6227H"],["12","0973B"],["12","6458B"],["12","6062B"],["12","8430B"],["12","2076B"],["12","5168B"],["12","0656B"],["12","0690B"]]
      it "gets links to sublocations from location page" $ do
        doc <- Text.HTML.DOM.readFile locPage
        let res = cities doc
        res `shouldBe` ["AGNESS","ALBANY","ALOHA","AMITY","ARLINGTON","ASHLAND","ASTORIA","ATHENA","BAKER CITY","BANDON","BANKS","BEAVERTON","BEND","BLUE RIVER","BLY","BOARDMAN","BONANZA","BROWNSVILLE","BURNS","BUTTE FALLS","CAMAS VALLEY","CANBY","CANNON BEACH","CANYONVILLE","CASCADE LOCKS","CAVE JUNCTION","CENTRAL POINT","CHARLESTON","CHEMULT","CHILOQUIN","CHRISTMAS VALLEY","CLACKAMAS","CLATSKANIE","CONDON","COOS BAY","COQUILLE","CORBETT","CORVALLIS","COTTAGE GROVE","CRESWELL","DALLAS","DAMASCUS","DAYVILLE","DEPOE BAY","DETROIT","DEXTER","DRAIN","EAGLE POINT","ELGIN","ENTERPRISE","ESTACADA","EUGENE","FIELDS","FLORENCE","FOREST GROVE","FOSSIL","GARIBALDI","GEARHART","GILCHRIST","GLADSTONE","GLENDALE","GOLD BEACH","GOLD HILL","GOVERNMENT CAMP","GRAND RONDE","GRANTS PASS","GRESHAM","HALFWAY","HAPPY VALLEY","HARBOR","HARRISBURG","HEPPNER","HERMISTON","HILLSBORO","HOOD RIVER","HUNTINGTON","IDLEYLD PARK","INDEPENDENCE","IONE","JACKSONVILLE","JOHN DAY","JORDAN VALLEY","JOSEPH","JUNCTION CITY","KEIZER","KENO","KING CITY","KLAMATH FALLS","LA GRANDE","LAKE OSWEGO","LAKESIDE","LAKEVIEW","LANGLOIS","LAPINE","LEBANON","LINCOLN CITY","LONG CREEK","MADRAS","MALIN","MAPLETON","MAUPIN","MCMINNVILLE","MEDFORD","MERRILL","MILL CITY","MILTON FREEWATER","MILWAUKIE","MOLALLA","MONUMENT","MT HOOD PARKDALE","MYRTLE CREEK","MYRTLE POINT","NEWBERG","NEWPORT","NORTH BEND","NORTH PLAINS","NORTH POWDER","NYSSA","OAKRIDGE","ONTARIO","OREGON CITY","OTIS","PACIFIC CITY","PAISLEY","PENDLETON","PHILOMATH","PILOT ROCK","PORT ORFORD","PORTLAND","POWERS","PRAIRIE CITY","PRINEVILLE","PROSPECT","RAINIER","REDMOND","REEDSPORT","RICHLAND","RIDDLE","ROCKAWAY","ROGUE RIVER","ROSEBURG","SALEM","SANDY","SCAPPOOSE","SCIO","SEASIDE","SELMA","SHADY COVE","SHERIDAN","SHERWOOD","SILVER LAKE","SILVERTON","SISTERS","SPRINGFIELD","ST HELENS","STANFIELD","STAYTON","SUMPTER","SUNRIVER","SUTHERLIN","SWEET HOME","TALENT","TERREBONNE","THE DALLES","TIGARD","TILLAMOOK","TOLEDO","TROUTDALE","TUALATIN","UKIAH","UMATILLA","UNION","UNITY","VALE","VENETA","VERNONIA","WALDPORT","WALLOWA","WAMIC","WARRENTON","WASCO","WELCHES","WEST LINN","WHEELER","WILLAMINA","WILSONVILLE","WINSTON","WOODBURN"]
      it "gets links to stores from sublocations page" $ do
        doc <- Text.HTML.DOM.readFile subLocPage
        let res = stores doc
        res `shouldBe`
          [("1181", "/servlet/FrontController?view=browsesublocations&action=select&agencyNumber=1181&locationRowNum=1"),
           ("1191", "/servlet/FrontController?view=browsesublocations&action=select&agencyNumber=1191&locationRowNum=2")]
    describe "Lib.onlyNumbers" $ do
      it "returns numbers surrounded by text" $ do
        onlyNumbers "asdf123qwert" `shouldBe` Just "123"
        onlyNumbers "asdf123" `shouldBe` Just "123"
        onlyNumbers "123qwert" `shouldBe` Just "123"
        onlyNumbers "123" `shouldBe` Just "123"
      it "works with floating point numbers" $ do
        onlyNumbers "asdf12.34qwert" `shouldBe` Just "12.34"
        onlyNumbers "12.34qwert" `shouldBe` Just "12.34"
        onlyNumbers "asdf12.34" `shouldBe` Just "12.34"
        onlyNumbers "12.34" `shouldBe` Just "12.34"
        onlyNumbers "1234." `shouldBe` Just "1234."
      it "returns the first numbers encountered" $ do
        onlyNumbers "asd123.23dfd56.32" `shouldBe` Just "123.23"
        onlyNumbers "asd123.23.32" `shouldBe` Just "123.23"
        onlyNumbers "123a345" `shouldBe` Just "123"
      it "returns Nothing for invalid text" $ do
        onlyNumbers "dfdenc.kef" `shouldBe` Nothing
        onlyNumbers "envkjgtr" `shouldBe` Nothing
    describe "Lib.keepMatches" $ do
      it "should return the submatches" $ do
        keepMatches "asdf([0-9]*)asdf" "asdf3423asdf" `shouldBe` ["3423"]
        keepMatches "a(b)c(d)e(f)" "abcdef" `shouldBe` ["b", "d", "f"]
        keepMatches "([0-9]+).([0-9]+).([0-9]+)" "b12c34d56g" `shouldBe` ["12", "34", "56"]
        keepMatches "window\\.location\\.href='([^']*)" "window.location.href='/servlet/FrontController?view=browsesublocations&action=select&agencyNumber=1181&locationRowNum=1';" `shouldBe` ["/servlet/FrontController?view=browsesublocations&action=select&agencyNumber=1181&locationRowNum=1"]
    describe "Lib.toLiters" $ do
      it "converts ML to liters" $ do
        toLiters "750 ML" `shouldBe` Just "0.75"
        toLiters "250 ML" `shouldBe` Just "0.25"
      it "converts L to liters" $ do
        toLiters "1 L" `shouldBe` Just "1"
        toLiters "1.5 L" `shouldBe` Just "1.5"
      it "converts LITER to liter" $ do
        toLiters "LITER" `shouldBe` Just "1"
