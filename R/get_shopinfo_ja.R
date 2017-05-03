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
        if (httr::http_type(request) != "text/html")
                stop("API did not return HTML", call. = FALSE)
        ids = xml2::read_html(request)

        # pause a few seconds
        Sys.sleep(2)
        
        # extract name
        shop_name = rvest::html_nodes(ids, ".display-name") %>% 
                rvest::html_text() %>% stringr::str_trim()
        
        # extract ratings
        rating = rvest::html_nodes(ids, ".rdheader-rating__score-val") %>% 
                rvest::html_text() %>% stringr::str_trim() %>% as.numeric() %>% 
                suppressWarnings()
        rating_dinner = 
                rvest::html_nodes(ids, ".rdheader-rating__time-icon--dinner") %>% 
                rvest::html_text() 
        rating_dinner = gsub("\n| |夜の点数：", "", rating_dinner) %>% 
                as.numeric() %>% suppressWarnings()
        rating_lunch = 
                rvest::html_nodes(ids, ".rdheader-rating__time-icon--lunch") %>% 
                rvest::html_text()
        rating_lunch = gsub("\n| |昼の点数：", "", rating_lunch) %>% 
                as.numeric() %>% suppressWarnings()
        
        # extract number of reviews
        reviews = rvest::html_nodes(ids, ".rdheader-rating__review-target") %>% 
                rvest::html_text()
        reviews = gsub("\n| |口コミ|件", "", reviews) %>% as.integer() %>% 
                suppressWarnings()
        
        # extract prices
        price_dinner = 
                rvest::html_nodes(ids, ".rdheader-budget__icon--dinner") %>% 
                rvest::html_text()
        price_dinner = 
                gsub("￥", "¥", gsub("～", " - ", 
                                    gsub("\n| |夜の予算", "", price_dinner)))
        price_lunch = rvest::html_nodes(ids, ".rdheader-budget__icon--lunch") %>% 
                rvest::html_text()
        price_lunch = 
                gsub("￥", "¥", gsub("～", " - ", 
                                    gsub("\n| |昼の予算", "", price_lunch)))
        
        # extract phone number
        tel = rvest::html_nodes(ids, ".visit-action__tel") %>% rvest::html_text()
        
        # extract opened date and weekly closed days
        birth_date = rvest::html_nodes(ids, ".rstinfo-opened-date") %>% 
                rvest::html_text()
        when_closed = rvest::html_nodes(ids, ".rdheader-subinfo__closed-text") %>% 
                rvest::html_text() %>% stringr::str_trim()
        
        # extract address 
        address = rvest::html_nodes(ids, ".rstinfo-table__address") %>% 
                rvest::html_text()
        address = stringr::str_trim(address)
        
        # # extract prefecture, city and area
        # loc = rvest::html_nodes(ids, ".listlink") %>% rvest::html_text()
        # prefecture = loc[1]
        # city = loc[2]
        # area = loc[3]
        
        # extract cuisine
        cuisine = rvest::html_nodes(ids, ".linktree__parent") %>% 
                rvest::html_text() %>% dplyr::last() %>% stringr::str_trim()
        
        # extract table of other info
        tbl = rvest::html_nodes(ids, ".rstinfo-table__table") %>% 
                rvest::html_text()
        elt1 = gsub("\n| ", "", tbl %>% dplyr::first())
        elt2 = gsub("\n| ", "", tbl %>% dplyr::nth(2))
        elt4 = gsub("\n| ", "", tbl %>% dplyr::nth(4))
        
        # extract business hours
        hours = gsub("営業時間|定休日", "",
                     stringr::str_extract(elt1, "営業時間.*定休日"))
                
        # extract credit cards
        cards = gsub("カード|サービス料・チャージ", "",
                     stringr::str_extract(elt1, "カード.*サービス料・チャージ"))
        
        # extract private room availability
        private = gsub("個室|禁煙", "", stringr::str_extract(elt2, "個室.禁煙"))
        
        # extract smoking allowed or not
        smoking = gsub("禁煙・喫煙", "", 
                       stringr::str_extract(elt2, "禁煙・喫煙.*煙"))

        # extract parking availability
        parking = gsub("駐車場|空間", "", stringr::str_extract(elt2, "駐車場.空間"))

        # extract appropriate for what occasion
        all_occasions = 
                "家族・子供と|デート|女子会|合コン|大人数の宴会|接待|一人で入りやすい|知人・友人と"
        occasion = stringr::str_extract_all(elt4, all_occasions)[[1]]
        
        # fill in NA if info not available
        if (length(shop_name)==0) shop_name = NA_character_
        if (length(rating)==0) rating = NA_real_
        if (length(rating_dinner)==0) rating_dinner = NA_real_
        if (length(rating_lunch)==0) rating_lunch = NA_real_
        if (length(reviews)==0) reviews = NA_integer_
        if (length(price_dinner)==0) price_dinner = NA_character_
        if (length(price_lunch)==0) price_lunch = NA_character_
        if (length(tel)==0) tel = NA_character_
        if (length(birth_date)==0) birth_date = NA_character_
        if (length(when_closed)==0) when_closed = NA_character_
        if (length(address)==0) address = NA_character_
        if (length(cuisine)==0) cuisine = NA_character_
        if (length(cards)==0) cards = NA_character_
        if (length(private)==0) private = NA_character_
        if (length(smoking)==0) smoking = NA_character_
        if (length(parking)==0) parking = NA_character_
        if (length(occasion)==0) occasion = NA_character_
        
        # collect into a data frame and return
        data.frame(
                店名 = shop_name,
                ジャンル = cuisine,
                利用シーン = occasion,
                点数 = rating,
                夜の点数 = rating_dinner,
                昼の点数 = rating_lunch,
                口コミ = reviews,
                夜の予算 = price_dinner,
                昼の予算 = price_lunch,
                営業時間 = hours,
                定休日 = when_closed, # day of week closed
                カード = cards,
                個室 = private,
                喫煙 = smoking,
                駐車場 = parking,
                住所 = address,
                TEL = tel,
                オープン日 = birth_date,
                stringsAsFactors = F
        )
}
