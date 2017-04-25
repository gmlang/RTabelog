rm(list = ls())

# user input
kansai = c("osaka", "kyoto", "hyogo", "shiga", "nara", "wakayama")
kanto  = c("tokyo") # , "kanagawa", "saitama", "chiba", "tochigi", "ibaraki", "gunma")
hokkaido_tohoku = c("hokkaido")
chubu = c("aichi")
kyushu = c("fukuoka")
cities = c(kansai, kanto, hokkaido_tohoku, chubu, kyushu)

# run
for (city in cities) {
        source("01-set-up.R")
        source("02-get-shopURLs.R")
}
