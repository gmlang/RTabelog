RTabelog
=========

Overview
--------

A R scraper for [Tabelog](https://tabelog.com). Tabelog doesn't provide an API, so I built this scraper and you can use it to scrape data from Tabelog. It currently supports 2 languages: Japanese and English.

Installation
------------

``` r
install.packages("devtools")
devtools::install_github("gmlang/RTabelog")
```

Usage
-----

Load it with:

``` r
library(RTabelog)
```

Obtain the tabelog URLs of all restaurants listed under Osaka.
``` r
city = "osaka" # must be a Japanese city with all lowercased letters.
shopURLs_ja = get_shopURLs_ja(city, pages=1:2) # in Japanese 
shopURLs_en = get_shopURLs(shopURLs_ja, "en") # in English
shopURLs_cn = get_shopURLs(shopURLs_ja, "cn") # in Simplified Chinese
shopURLs_tw = get_shopURLs(shopURLs_ja, "tw") # in Traditional Chinese
shopURLs_kr = get_shopURLs(shopURLs_ja, "kr") # in Korean
```

Scrape restaurants info
``` r
get_shopinfo_ja(shopURLs_ja[1]) # in Japanese
get_shopinfo_en(shopURLs_en[1]) # in English
``` 
