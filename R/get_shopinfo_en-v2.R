#' @title Scrape restaurant information (in English) from a given Tabelog URL.
#'
#' @param shopURL string, URL of restaurant, for example,
#'                "https://tabelog.com/en/kyoto/A2601/A260301/26002222/".
#' @return a data frame of restaurant info in English.
#'
#' @importFrom dplyr "%>%"
#'
#' @export

get_shopinfo_en2 = function(shopURL) {
        # shopURL = "https://tabelog.com/en/osaka/A2701/A270202/27001286/"
        request = httr::RETRY("GET", url = shopURL)
        check_request(request)
        ids = xml2::read_html(request)

        # pause a few seconds
        Sys.sleep(2)

        # extract ratings
        ratings = suppressWarnings(
                rvest::html_nodes(ids, ".rd-header__rst-rate .c-rating__val") %>%
                        rvest::html_text() %>% stringr::str_trim() %>%
                        as.numeric()
                )
        rating = ratings[1]
        rating_dinner = ratings[2]
        rating_lunch = ratings[3]

        # extract number of reviews
        reviews = suppressWarnings(
                rvest::html_nodes(ids, ".rd-header__rst-reviews-target") %>%
                        rvest::html_text() %>%
                        gsub(pattern="\n| |reviews", replace="") %>%
                        as.integer()
                )

        # extract prices
        prices = rvest::html_nodes(ids, ".rd-header__info-table .c-rating__val") %>%
                rvest::html_text()
        price_dinner = prices[1]
        price_lunch = ifelse(length(prices)==2, prices[2], NA_character_)

        # extract detailed info table
        tbl = rvest::html_nodes(ids, ".rd-detail-info") %>% rvest::html_text()
        basic = tbl[1]
        seats = tbl[2]
        menu  = tbl[3]
        other = tbl[4]

        # extract basic shop info
        shop_name = gsub(".*Restaurant name(.*)Categories.*", "\\1", basic) %>%
                stringr::str_trim() %>% gsub(pattern = "\n *", replacement=" ")
        cuisine = gsub(".*Categories(.*)TEL.*", "\\1", basic) %>%
                stringr::str_trim()
        tel = gsub(".*TEL/reservation(.*)Addresses.*", "\\1", basic) %>%
                stringr::str_trim() %>%
                stringr::str_extract(pattern="[0-9]+-[0-9]+-[0-9]+")
        address = gsub(".*Addresses(.*)Show larger map.*", "\\1", basic) %>%
                stringr::str_trim()
        address_en = gsub("(.*[a-zA-Z]+).*", "\\1", address)
        address_ja = gsub("\n| ", "", gsub(".*[a-zA-Z]+", "", address))
        address = paste(address_en, paste0("(", address_ja, ")"))
        nearby = gsub(".*Transportation(.*)Operating Hours.*", "\\1", basic) %>%
                stringr::str_trim() %>%
                stringr::str_extract(pattern = "[0-9a-zA-Z| ]+") %>%
                stringr::str_trim() %>% gsub(pattern = ".*from ", replace="")
        # hours = gsub(".*Operating Hours(.*)Shop holidays.*", "\\1", basic) %>%
        #         gsub(pattern = "□■.*", replace = "") %>%
        #         stringr::str_trim()
        # hours = paste(substr(hours, 1, 11), substr(hours, 12, nchar(hours)),
        #               sep=" | ")
        cards = gsub(".*Cards(.*)", "\\1", basic) %>%
                gsub(pattern = ".*\\(|\\).*", replace="")

        # # extract seats info
        # private = gsub(".*Private dining rooms(.*)Private use.*", "\\1", seats) %>%
        #         stringr::str_trim()
        # private = gsub(".*Private dining rooms(.*)Private use.*", "\\1", seats) %>%
        #         stringr::str_trim()
        # smoking = gsub(".*Non-smoking/smoking(.*)Parking lot.*", "\\1", seats) %>%
        #         stringr::str_trim() %>% gsub(pattern=" establishment", replace="")
        # parking = gsub(".*Parking lot(.*)Space/facilities.*", "\\1", seats) %>%
        #         gsub(pattern="[^a-zA-Z]", replace="")

        # # extract other info
        # occasion = gsub(".*Occasion(.*)Location.*", "\\1", other) %>%
        #         stringr::str_trim() %>% gsub(pattern=" *\n.*", replace="")
        # website = gsub(".*The homepage(.*)The opening day.*", "\\1", other) %>%
        #         stringr::str_trim()
        # open_date = gsub(".*The opening day(.*)First reviewer.*", "\\1", other) %>%
        #         stringr::str_trim()

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
        # if (length(private)==0) private = NA_character_
        # if (length(smoking)==0) smoking = NA_character_
        # if (length(parking)==0) parking = NA_character_
        # if (length(occasion)==0) occasion = NA_character_
        # if (length(website)==0) website = NA_character_
        # if (length(open_date)==0) open_date = NA_character_
        if (length(nearby)==0) nearby = NA_character_

        # collect into a data frame and return
        out = data.frame(shop_name, cuisine, rating_dinner, rating_lunch,
                         reviews, price_dinner, price_lunch,
                         # occasion,
                         nearby,
                         # hours,
                         cards,
                         # private, smoking, parking,
                         address, tel,
                         # open_date, website,
                         shopURL,
                         stringsAsFactors = F)
        names(out) = c("Restaurant Name", "Cuisine", "Dinner Rating",
                       "Lunch Rating", "Reviews", "Dinner Price", "Lunch Price",
                       # "Good for",
                       "Nearest Station",
                       # "Hours",
                       "Accept Credit Cards",
                       # "Private Room", "Smoking", "Parking",
                       "Address", "Tel",
                       # "Opened Since", "Restaurant Website",
                       "View on Tabelog")
        out
}
