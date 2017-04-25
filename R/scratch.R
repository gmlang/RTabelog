library(rvest)
library(httr)


function(shopURL) {
        shopURL = "https://tabelog.com/osaka/A2701/A270101/27000188/"
        request = httr::RETRY("GET", url = shopURL)
        check_request(request)
        if (httr::http_type(request) != "text/html")
                stop("API did not return HTML", call. = FALSE)

        ids = xml2::read_html(request)
        leaves = rvest::html_nodes(ids, ".list-rst__rst-name-target")
        rvest::html_attr(leaves, "href")

}



# target1 = paste0(url, "dtlrvwlst/COND-0/smp1/?lc=0&rvw_part=all&PG=", 1)
# target1 = paste0(url, "dtlrvwlst/?&PG=1")



user_ids <- ids2 %>% html_nodes (".js-show-review-items") %>% html_attr("data-bookmark-id")


paste0(url, "dtlrvwlst/B", user_ids, "/")
