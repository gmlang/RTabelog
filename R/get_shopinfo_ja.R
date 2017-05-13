#' @title Scrape restaurant information (in Japanese) from a given Tabelog URL.
#'
#' @param shopURL string, URL of restaurant, for example,
#'                "https://tabelog.com/kyoto/A2601/A260301/26002222/".
#' @return a data frame of restaurant info in Japanese.
#'
#' @importFrom dplyr "%>%"
#'
#' @export

get_shopinfo_ja = function(shopURL) {
        # shopURL = "https://tabelog.com/kyoto/A2601/A260301/26002222/"
        request = httr::RETRY("GET", url = shopURL)
        check_request(request)
        ids = xml2::read_html(request)
  
        # pause a few seconds
        Sys.sleep(2)
        
        # mk a function to extract values for a given css node
        extract = extract_functor(ids)
        
        # shop name
        shop_name = extract(".display-name") %>% 
                gsub(pattern = "\\s+", replace = " ")
        if (length(shop_name) == 0) shop_name = NA
        
        # prices
        prices = extract(".rdheader-budget__price")
        price_dinner = ifelse(length(prices)==0, NA, prices[1])
        price_lunch = ifelse(length(prices)==2, prices[2], NA)
        if (length(price_dinner) == 0) price_dinner = NA
        if (length(price_lunch) == 0) price_lunch = NA
  
        # phone number
        tel = extract(".visit-action__tel") %>%
                gsub(pattern="\n.*", replace="") %>% dplyr::first()
        if (length(tel) == 0) tel = NA
        
        # ratings
        rating = extract(".rdheader-rating__score") %>% as.numeric()
        rating_dinner = suppressWarnings(
                extract(".rdheader-rating__time-icon--dinner") %>%
                gsub(pattern="\n|夜の点数：", replace="") %>% as.numeric()
        )
        rating_lunch = suppressWarnings(
                extract(".rdheader-rating__time-icon--lunch") %>%
                gsub(pattern="\n|昼の点数：", replace="") %>% as.numeric()
        )
        if (length(rating_dinner) == 0) rating_dinner = NA
        if (length(rating_lunch) == 0) rating_lunch = NA
        
        # number of reviews
        reviews = suppressWarnings(
                extract(".rdheader-rating__review-target .num") %>%
                        gsub(pattern="\n| |reviews", replace="") %>% 
                        as.integer()
        )
        if (length(reviews) == 0) reviews = NA
        
        # address
        address = extract(".rstinfo-table__address")
        if (length(address) == 0) address = NA
        
        # take reservations
        reservation = ifelse(extract(".rstinfo-table__reserve-status") == "予約可",
                             "Yes", "No")
        if (length(reservation) == 0) reservation = NA
        
        ##----- table of detailed info -- very important ---- ##
        tbl_head = extract(".rstinfo-table__table th") # char vectors
        tbl_content = rvest::html_nodes(ids, ".rstinfo-table__table") %>%
                rvest::html_nodes("td") # list
        
        # cuisine
        pos = which(tbl_head == "ジャンル")
        cuisine = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T))
        
        # nearest station
        pos = which(tbl_head == "交通手段")
        nearby = ifelse(length(pos) == 0, NA_character_,
                        tbl_content %>% magrittr::extract2(pos) %>%
                                rvest::html_text(trim=T) %>%
                                gsub(pattern=".*\n| |\\から.*", replace=""))
        
        # credit cards
        pos = which(tbl_head == "カード")
        cards = ifelse(length(pos) == 0, NA_character_,
                       tbl_content %>% magrittr::extract2(pos) %>%
                               rvest::html_text(trim=T) %>%
                               gsub(pattern=".*\\（|\\）", replace=""))
        
        # good for what occasions
        pos = which(tbl_head == "利用シーン")
        occasion = ifelse(length(pos) == 0, NA_character_,
                          tbl_content %>% magrittr::extract2(pos) %>%
                                  rvest::html_text(trim=T) %>%
                                  gsub(pattern="\n| |こんな.*", replace="") %>%
                                  gsub(pattern="\\|", replace=", "))
        
        # private room
        pos = which(tbl_head == "個室")
        private = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 #stringr::str_extract("[^\n]*\n?[^\n]*") %>%
                                 gsub(pattern = "\n| ", replace=""))
        
        # smoking
        pos = which(tbl_head == "禁煙・喫煙")
        smoking = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern="\n| ",　replace=""))
        
        # parking
        pos = which(tbl_head == "駐車場")
        parking = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern="\n.*", replace=""))
        
        # unique location/setup
        pos = which(tbl_head == "空間・設備")
        uniq_loc = ifelse(length(pos) == 0, NA_character_,
                          tbl_content %>% magrittr::extract2(pos) %>%
                                  rvest::html_text(trim=T))
        
        # restaurant website
        pos = which(tbl_head == "ホームページ")
        website = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T))
        
        # collect into a data frame and return
        out = data.frame(shop_name, cuisine, rating_dinner, rating_lunch,
                         reviews, price_dinner, price_lunch, nearby, occasion,
                         cards, reservation, private, smoking, parking, uniq_loc,
                         address, tel, website, shopURL,
                         stringsAsFactors = F)
        names(out) = c("店名", "ジャンル", "夜の点数", "昼の点数", "口コミ", 
                       "夜の予算", "昼の予算", "最寄り", "利用シーン",
                       "クレジットカード", "予約", "個室", "喫煙・禁煙", 
                       "駐車場", "空間・設備", "住所", "電話番号", 
                       "ホームページ", "食べログURL")
        out
}
