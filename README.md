# oregonliquor

Build with `stack build`

Run with `stack run`

There are no arguments, csv files will appear in the running directory once they are scraped. 

Products will be scraped to `./products.csv`.

Inventory wlil be scraped to `./inventory.csv`.

Store information will be scraped to `./stores.csv`.

This is not really usable. There is no anti-scraper avoidance so you'll probably be IP banned before you're done scraping the entire inventory. There is also no way to resume scraping, you'll need to manually modify the code to select stores you haven't scraped from yet if you wish to resume the scrape.
