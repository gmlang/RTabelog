#' @title Extracts URLs of restaurants (in Japanese) listed on tabelog for a given city.
#' 
#' @description 
#' These URLs are sorted by the ratings of the restaurants in descending order.
#' 
#' @param city string, lower cased city name in Japan.
#' @param pages integer vector, for example, 1:60.
#' @return a character vector of URLs.
#' @export

get_shopURLs_ja = function(city = "osaka", pages = 1:60) {
        # Returns a character vector of URLs of sites in Japanese.
        #
        # city: string, city name
        # pages: integer vector, page range. In each city, Tabelog lists up to
        #       1200 shops and ends at page 60.

        # print city
        cat("City/Prefecture:", city, "\n")

        # construct base URL
        baseURL = paste("https://tabelog.com", city, sep = "/")

        # scrape each page
        unlist(lapply(pages, function(i) {
                cat("Page", i, "\n")

                # construct API call and GET data
                endURL = paste0(paste(baseURL,
                                      "rstLst/?SrtT=rt&Srt=D&sort_mode=1&PG=",
                                      sep="/"), i)
                request = httr::RETRY("GET", url = endURL)
                check_request(request)
                if (httr::http_type(request) != "text/html")
                        stop("API did not return HTML", call. = FALSE)

                # pause 3 seconds
                Sys.sleep(3)

                # extract shops' URLs
                ids = xml2::read_html(request)
                leaves = rvest::html_nodes(ids, ".list-rst__rst-name-target")
                rvest::html_attr(leaves, "href")
        }))
}

