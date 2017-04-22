rm(list = ls())

# user input
city = "osaka"

# set paths
r_path = "R"
for (fname in list.files(r_path)) source(file.path(r_path, fname))

# get URLs of resturants in Japanese
shopURLs_ja = get_shopURLs_ja(city, pages = 1)

# convert to URLs in English, Simplified Chinese, Traditional Chinese and Korean
languages = c("en", "cn", "tw", "kr")
lst_shopURLs = lapply(languages,
                      function(x) get_shopURLs(shopURLs_ja, lang=x))
names(lst_shopURLs) = languages

# also add Japanese URLs to the list
lst_shopURLs$ja = shopURLs_ja

