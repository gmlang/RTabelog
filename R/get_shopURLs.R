get_shopURLs_ja = function(city = "osaka", pages = 1:60) {
        # GETs and extracts URLs of restaurants listed on x pages for
        #       the given city.
        # Returns a character vector of URLs of sites in Japanese.
        #
        # city: string, city name
        # pages: integer vector, page range. In each city, Tabelog lists up to
        #       1200 shops and ends at page 60.

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

get_shopURLs = function(shopURLs_ja, lang = "en") {
        # Converts the URLs to Japanese sites to URLs to sites in user specified
        #       language
        # Returns a character vector of URLs of sites in user specified language
        #
        # shopURLs_ja: character vector of URLs to sites in Japanese
        # lang: string, language of values: "en", "cn", "tw", "kr"

        tryCatch({ lang %in% c("en", "cn", "tw", "kr")
        }, error = function(e) "Language is not supported. Please enter one of
                        these languages: 'en', 'cn', 'tw', 'kr'"
        )
        gsub("\\.com", paste("\\.com", lang, sep="/"), shopURLs_ja)
}

