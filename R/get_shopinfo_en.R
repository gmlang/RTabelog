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
        # shopURL = "https://tabelog.com/en/osaka/A2701/A270102/27015488/" # "https://tabelog.com/en/osaka/A2701/A270202/27001286/"
        request = httr::RETRY("GET", url = shopURL)
        check_request(request)
        ids = xml2::read_html(request)

        # pause a few seconds
        Sys.sleep(2)

        # mk a function to extract values for a given css node
        extract = extract_functor(ids)

        # shop name
        shop_name = extract(".rd-detail-info__rst-name") %>%
                gsub(pattern = "\\s+", replace = " ")

        # prices
        prices = extract(".rd-header__info-table .c-rating__val")
        price_dinner = ifelse(length(prices)==0, NA_character_, prices[1])
        price_lunch = ifelse(length(prices)==2, prices[2], NA_character_)

        # phone number
        tel = extract(".rd-detail-info__rst-tel") %>%
                gsub(pattern="\n.*", replace="")

        # ratings
        ratings = suppressWarnings(
                extract(".rd-header__rst-rate .c-rating__val") %>% as.numeric()
                )
        rating = ratings[1]
        rating_dinner = ratings[2]
        rating_lunch = ratings[3]

        # number of reviews
        reviews = suppressWarnings(
                extract(".rd-header__rst-reviews-target") %>%
                        gsub(pattern="\n| |reviews", replace="") %>%
                        as.integer()
                )

        # address
        address = extract(".rd-detail-info__rst-address")
        address_en = address[1]
        address_ja = gsub("\n| ", "", address[2])
        address = paste(address_en, paste0("(", address_ja, ")"))

        # take reservations
        reservation = ifelse(
                extract(".rd-detail-info__rst-booking-status") == "予約可",
                "Yes", "No")


        ##----- table of detailed info -- very important ---- ##
        tbl_head = extract(".rd-detail-info th")
        tbl_content = rvest::html_nodes(ids, ".rd-detail-info") %>%
                rvest::html_nodes("td")

        # cuisine
        pos = which(tbl_head == "Categories")
        cuisine = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T))

        # nearest station
        pos = which(tbl_head == "Transportation")
        nearby = ifelse(length(pos) == 0, NA_character_,
                        tbl_content %>% magrittr::extract2(pos) %>%
                                rvest::html_text(trim=T) %>%
                                gsub(pattern=".*from |\\.", replace=""))

        # credit cards
        pos = which(tbl_head == "Cards")
        cards = ifelse(length(pos) == 0, NA_character_,
                       tbl_content %>% magrittr::extract2(pos) %>%
                               rvest::html_text(trim=T) %>%
                               gsub(pattern=".*\\(|\\)", replace=""))

        # good for what occasions
        pos = which(tbl_head == "Occasion")
        occasion = ifelse(length(pos) == 0, NA_character_,
                          tbl_content %>% magrittr::extract2(pos) %>%
                                  rvest::html_text(trim=T) %>%
                                  gsub(pattern=" *\n.*", replace="") %>%
                                  gsub(pattern=",", replace=", "))

        # private room
        pos = which(tbl_head == "Private dining rooms")
        private = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern="\n *", replace=". "))

        # smoking
        pos = which(tbl_head == "Non-smoking/smoking")
        smoking = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern=" *\n.*| establishment",
                                      replace=""))

        # parking
        pos = which(tbl_head == "Parking lot")
        parking = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern="\n.*", replace=""))

        # unique location/setup
        pos = which(tbl_head == "Location")
        uniq_loc = ifelse(length(pos) == 0, NA_character_,
                         tbl_content %>% magrittr::extract2(pos) %>%
                                 rvest::html_text(trim=T) %>%
                                 gsub(pattern=",", replace=", "))

        # restaurant website
        pos = which(tbl_head == "The homepage")
        website = ifelse(length(pos) == 0, NA_character_,
                          tbl_content %>% magrittr::extract2(pos) %>%
                                  rvest::html_text(trim=T))

        # collect into a data frame and return
        out = data.frame(shop_name, cuisine, rating_dinner, rating_lunch,
                         reviews, price_dinner, price_lunch, nearby, occasion,
                         cards, reservation, private, smoking, parking, uniq_loc,
                         address, tel, website, shopURL,
                         stringsAsFactors = F)
        names(out) = c("Restaurant Name", "Cuisine", "Dinner Rating",
                       "Lunch Rating", "Reviews", "Dinner Price", "Lunch Price",
                       "Nearest Station", "Good for",
                       "Accept Credit Cards", "Take Reservation",
                       "Private Room", "Smoking", "Parking", "Unique Setup",
                       "Address", "Tel", "Restaurant Website", "View on Tabelog")
        out
}
