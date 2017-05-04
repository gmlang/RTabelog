#' @title Scrape restaurant information (in English) from a given Tabelog URL.
#'
#' @param shopURL string, URL of restaurant, for example,
#'                "https://tabelog.com/en/kyoto/A2601/A260301/26002222/".
#' @return a data frame of restaurant info in English.
#'
#' @importFrom dplyr "%>%"
#'
#' @export

get_shopinfo_en = function(shopURL) {
        # shopURL = "https://tabelog.com/en/kyoto/A2601/A260301/26002222/" # "https://tabelog.com/en/osaka/A2701/A270202/27001286/"
        request = httr::RETRY("GET", url = shopURL)
        check_request(request)
        ids = xml2::read_html(request)

        # pause a few seconds
        Sys.sleep(2)

        # shop name
        shop_name = rvest::html_nodes(ids, ".rd-header__rst-name-main") %>%
                rvest::html_text() %>% stringr::str_trim()

        # cuisine
        cuisine = rvest::html_nodes(ids, ".c-display-guide+ section tr:nth-child(2) p") %>%
                rvest::html_text() %>% stringr::str_trim()

        # ratings
        ratings = suppressWarnings(
                rvest::html_nodes(ids, ".rd-header__rst-rate .c-rating__val") %>%
                        rvest::html_text() %>% stringr::str_trim() %>%
                        as.numeric()
                )
        rating = ratings[1]
        rating_dinner = ratings[2]
        rating_lunch = ratings[3]

        # number of reviews
        reviews = suppressWarnings(
                rvest::html_nodes(ids, ".rd-header__rst-reviews-target") %>%
                        rvest::html_text() %>%
                        gsub(pattern="\n| |reviews", replace="") %>%
                        as.integer()
                )

        # prices
        prices = rvest::html_nodes(ids, ".rd-header__info-table .c-rating__val") %>%
                rvest::html_text()
        price_dinner = prices[1]
        price_lunch = ifelse(length(prices)==2, prices[2], NA_character_)

        # phone number
        tel = rvest::html_nodes(ids, "#anchor-rd-detail strong") %>%
                rvest::html_text() %>% stringr::str_trim() %>%
                gsub(pattern="\\s+", replace=" ")

        # address
        address = rvest::html_nodes(ids, ".rd-detail-info__rst-address") %>%
                rvest::html_text()
        address_en = address[1] %>% stringr::str_trim()
        address_ja = gsub("\n| ", "", address[2])
        address = paste(address_en, paste0("(", address_ja, ")"))

        # nearest station
        nearby = rvest::html_nodes(ids, "dl:nth-child(1) dd") %>%
                rvest::html_text() %>%
                stringr::str_extract(pattern="\\s+\\w+\n") %>%
                stringr::str_trim()

        # business hours
        hours = rvest::html_nodes(ids, ".c-display-guide+ section tr:nth-child(6) .translate") %>%
                rvest::html_text() %>% stringr::str_trim()

        # credit cards
        cards = rvest::html_nodes(ids, ".c-display-guide+ section tr:nth-child(9) p") %>%
                rvest::html_text() %>%
                gsub(pattern = ".*\\(|\\).*", replace="") %>%
                stringr::str_trim()

        # private room
        private = rvest::html_nodes(ids, "tr:nth-child(2) b") %>%
                rvest::html_text() %>% stringr::str_trim()

        # # smoking
        # smoking = rvest::html_nodes(ids, "tr:nth-child(4) b") %>%
        #         rvest::html_text() %>% gsub(pattern=" establishment", replace="")
        #
        # # parking
        # parking = rvest::html_nodes(ids, "tr:nth-child(5) b") %>%
        #         rvest::html_text()
        #
        # # good for what occasions
        # occasion = rvest::html_nodes(ids, "section:nth-child(6) tr:nth-child(1) p:nth-child(1)") %>%
        #         rvest::html_text() %>% stringr::str_trim() %>%
        #         gsub(pattern = ",", replace=", ")

        # restaurant website
        website = rvest::html_nodes(ids, ".rd-detail-info__target-blank a") %>%
                rvest::html_text() %>% stringr::str_trim()

        # fill in NA if info not available
        if (length(shop_name)==0) shop_name = NA_character_
        if (length(rating)==0) rating = NA_real_
        if (length(rating_dinner)==0) rating_dinner = NA_real_
        if (length(rating_lunch)==0) rating_lunch = NA_real_
        if (length(reviews)==0) reviews = NA_integer_
        if (length(price_dinner)==0) price_dinner = NA_character_
        if (length(price_lunch)==0) price_lunch = NA_character_
        if (length(tel)==0) tel = NA_character_
        if (length(address)==0) address = NA_character_
        if (length(cuisine)==0) cuisine = NA_character_
        if (length(cards)==0) cards = NA_character_
        if (length(hours)==0) cards = NA_character_
        if (length(private)==0) private = NA_character_
        # if (length(smoking)==0) smoking = NA_character_
        # if (length(parking)==0) parking = NA_character_
        # if (length(occasion)==0) occasion = NA_character_
        if (length(website)==0) website = NA_character_
        if (length(nearby)==0) nearby = NA_character_

        # collect into a data frame and return
        out = data.frame(shop_name, cuisine, rating_dinner, rating_lunch,
                         reviews, price_dinner, price_lunch,
                         # occasion,
                         nearby, hours, cards, private,
                         # smoking, parking,
                         address, tel, website, shopURL,
                         stringsAsFactors = F)
        names(out) = c("Restaurant Name", "Cuisine", "Dinner Rating",
                       "Lunch Rating", "Reviews", "Dinner Price", "Lunch Price",
                       # "Good for",
                       "Nearest Station", "Hours", "Accept Credit Cards",
                       "Private Room",
                       # "Smoking", "Parking",
                       "Address", "Tel", "Restaurant Website", "View on Tabelog")
        out
}
