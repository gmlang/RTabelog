# library(rvest)
# library(RCurl)
# library(httr)
#
# ## shops in Osaka
# baseurl <- "http://tabelog.com/osaka/"
#
# ## get shops' IDs
# pages <- 1:1 # set page range
# shop_ids <- character()
# for (i in pages) {
#   target1 <- paste0(baseurl, "rstLst/", i, "/")
#   ids <- read_html(GET(target1, user_agent("myagent")))
#   tmp <- ids %>% html_nodes (".list-rst__rst-name-target") %>% html_attr("href")
#   shop_ids <- c(shop_ids, tmp)
# }

##  get urls to user comments
pages_c <- 1:1 # set page range
urls <- character()
for (a in 1:length(shop_ids)) {
  baseurl <- shop_ids[a]
  ## get reviewers' IDs
  user_ids <- character()
  for (i in pages_c) {
    target1 <- paste0(baseurl, "dtlrvwlst/COND-0/smp1/?lc=0&rvw_part=all&PG=", i)
    ids2 <- read_html(GET(target1, user_agent("myagent")))
    tmp <- ids2 %>% html_nodes (".js-show-review-items") %>% html_attr("data-bookmark-id")
    user_ids <- c(user_ids, tmp)
  }
  tmp2 <- paste0(baseurl, "dtlrvwlst/B", user_ids, "/")
  urls <- c(urls, tmp2)
}

contents <-
  data.frame(
    shop_name = NULL,
    cat = NULL,
    texts = NULL,
    score = NULL
  )

## access to the urls and extract reviews
for (url in urls) {
  print (url)
  try(shop2 <- read_html(GET(url, user_agent("myagent"))))
  # shop's name
  shop_name <-
    shop2 %>% html_nodes(".display-name")  %>% html_text ()
  if (length(shop_name) < 1)
    shop_name <- ""
  # age & gender
  cat <-
    shop2 %>% html_nodes (".rvw-item__rvwr-profile")  %>% html_text ()
  if (length(cat) < 1)
    cat <- ""
  # reviews
  texts <-
    shop2 %>% html_nodes (".rvw-item__rvw-comment") %>% html_text ()
  if (length(texts) < 1)
    texts <- ""
  # score
  score <-
    shop2 %>% html_nodes (".rvw-item__ratings") %>% html_text ()
  if (length(score) < 1)
    score <- ""
  contents <-
    rbind(
      contents,
      data.frame(
        shop_name = shop_name,
        cat = cat,
        texts = texts,
        score = score,
        stringsAsFactors = FALSE
      )
    )
}

summary(contents)

## cleaning reviews
contents2 <- contents
contents2$score <- gsub(" ", "", contents2$score, fixed = TRUE)
contents2$score <- gsub("\n", "", contents2$score)
contents2$shop_name <- gsub(" ", "", contents2$shop_name, fixed = TRUE)
contents2$shop_name <- gsub("\n", "", contents2$shop_name)
contents2$texts <- gsub("\n", "", contents2$texts)

contents2 <- mutate(contents2, gender = ifelse(grepl("男性",cat), "Men",
                                               ifelse(grepl("女性", cat), "Women", NA)),
                    age = ifelse(grepl("90", cat), "90's",ifelse(grepl("80", cat), "80's",
                                                                 ifelse(grepl("70", cat), "70's",
                                                                        ifelse(grepl("60", cat), "60's",
                                                                               ifelse(grepl("50", cat), "50's",
                                                                                      ifelse(grepl("60", cat), "60's",
                                                                                             ifelse(grepl("50", cat), "50's",
                                                                                                    ifelse(grepl("40", cat), "40's",
                                                                                                           ifelse(grepl("30", cat), "30's",
                                                                                                                  ifelse(grepl("20", cat), "20's",
                                                                                                                         ifelse(grepl("10", cat), "10's", NA))))))))))),
                    from = ifelse(grepl("北海道", cat), "北海道",
                                  ifelse(grepl("青森県", cat), "青森県",
                                         ifelse(grepl("岩手県", cat), "岩手県",
                                                ifelse(grepl("宮城県", cat), "宮城県",
                                                       ifelse(grepl("秋田県", cat), "秋田県",
                                                              ifelse(grepl("山形県", cat), "山形県",
                                                                     ifelse(grepl("福島県", cat), "福島県",
                                                                            ifelse(grepl("茨城県", cat), "茨城県",
                                                                                   ifelse(grepl("栃木県", cat), "栃木県",
                                                                                          ifelse(grepl("群馬県", cat), "群馬県",
                                                                                                 ifelse(grepl("埼玉県", cat), "埼玉県",
                                                                                                        ifelse(grepl("千葉県", cat), "千葉県",
                                                                                                               ifelse(grepl("東京都", cat), "東京都",
                                                                                                                      ifelse(grepl("神奈川県", cat), "神奈川県",
                                                                                                                             ifelse(grepl("新潟県", cat), "新潟県",
                                                                                                                                    ifelse(grepl("富山県", cat), "富山県",
                                                                                                                                           ifelse(grepl("石川県", cat), "石川県",
                                                                                                                                                  ifelse(grepl("福井県", cat), "福井県",
                                                                                                                                                         ifelse(grepl("山梨県", cat), "山梨県",
                                                                                                                                                                ifelse(grepl("長野県", cat), "長野県",
                                                                                                                                                                       ifelse(grepl("岐阜県", cat), "岐阜県",
                                                                                                                                                                              ifelse(grepl("静岡県", cat), "静岡県",
                                                                                                                                                                                     ifelse(grepl("愛知県", cat), "愛知県",
                                                                                                                                                                                            ifelse(grepl("三重県", cat), "三重県",
                                                                                                                                                                                                   ifelse(grepl("滋賀県", cat), "滋賀県",
                                                                                                                                                                                                          ifelse(grepl("京都府", cat), "京都府",
                                                                                                                                                                                                                 ifelse(grepl("大阪府", cat), "大阪府",
                                                                                                                                                                                                                        ifelse(grepl("兵庫県", cat), "兵庫県",
                                                                                                                                                                                                                               ifelse(grepl("奈良県", cat), "奈良県",
                                                                                                                                                                                                                                      ifelse(grepl("和歌山県", cat), "和歌山県",
                                                                                                                                                                                                                                             ifelse(grepl("鳥取県", cat), "鳥取県",
                                                                                                                                                                                                                                                    ifelse(grepl("島根県", cat), "島根県",
                                                                                                                                                                                                                                                           ifelse(grepl("岡山県", cat), "岡山県",
                                                                                                                                                                                                                                                                  ifelse(grepl("広島県", cat), "広島県",
                                                                                                                                                                                                                                                                         ifelse(grepl("山口県", cat), "山口県",
                                                                                                                                                                                                                                                                                ifelse(grepl("徳島県", cat), "徳島県",
                                                                                                                                                                                                                                                                                       ifelse(grepl("香川県", cat), "香川県",
                                                                                                                                                                                                                                                                                              ifelse(grepl("愛媛県", cat), "愛媛県",
                                                                                                                                                                                                                                                                                                     ifelse(grepl("高知県", cat), "高知県",
                                                                                                                                                                                                                                                                                                            ifelse(grepl("福岡県", cat), "福岡県",
                                                                                                                                                                                                                                                                                                                   ifelse(grepl("佐賀県", cat), "佐賀県",
                                                                                                                                                                                                                                                                                                                          ifelse(grepl("長崎県", cat), "長崎県",
                                                                                                                                                                                                                                                                                                                                 ifelse(grepl("熊本県", cat), "熊本県",
                                                                                                                                                                                                                                                                                                                                        ifelse(grepl("大分県", cat), "大分県",
                                                                                                                                                                                                                                                                                                                                               ifelse(grepl("宮崎県", cat), "宮崎県",
                                                                                                                                                                                                                                                                                                                                                      ifelse(grepl("鹿児島県", cat), "鹿児島県",
                                                                                                                                                                                                                                                                                                                                                             ifelse(grepl("沖縄県", cat), "沖縄県", NA))))))))))))))))))))))))))))))))))))))))))))))),
                    kansai = ifelse(grepl("大阪府", from) |
                                      grepl("京都府", from) |
                                      grepl("兵庫県", from) |
                                      grepl("滋賀県", from) |
                                      grepl("奈良県", from) |
                                      grepl("和歌山県", from), 1, 0),
                    night = ifelse(grepl("夜の点数", score) & !grepl("昼の点数", score), 1, 0)) # 1 = evening experience

contents2$tot_score <- substr(contents2$score, 6, 8)
contents2$tot_score <- gsub(".*-.*", NA, contents2$tot_score)

contents2$tot_score <- as.numeric(contents2$tot_score)
contents2$kansai <- as.numeric(contents2$kansait)
contents2$night <- as.numeric(contents2$night)
contents2$gender <- as.factor(contents2$gender)
contents2$from <- as.factor(contents2$from)
contents2$age <- as.factor(contents2$age)

contents2 <- contents2[ ,c("shop_name", "tot_score", "kansai", "night", "gender", "from", "age", "cat", "texts", "score")]

write.csv(contents2, file = "contents2.csv", row.names = FALSE)


# ## morphological analysis
# library(dplyr); library(stringr)
# library(RMeCab); library(magrittr) # morphological analyzer library
#
# ## save reviews into a text file
# write(contents$texts, file = "tabelog.txt")
#
# frq <- RMeCabFreq("tabelog.txt")
#
# frq %>% filter (Freq > 2, Info1 %in% c("名詞", "形容詞", "動詞")) -> frq2 # noun, adjective, verb
# frq2 %<>% filter(!Term %in% c("それ", "する", "いる" , "の", "なる", "よう", "ある", "円", "/", "_", "０", "、"))
