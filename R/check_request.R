#' @title Check if API request returns an error or success.
#' 
#' @description
#' Stop and print an error message if API call doesn't return 200 status code.
#' 
#' @param request Object returned by GET()
#' @keywords internal
check_request = function(request) {
        # request: object returned by GET()

        # if the request wasn't successful, stop
        if (request$status_code != 200)
                stop("Tabelog crawling returned an error.\n")
        if (httr::http_type(request) != "text/html")
                stop("API did not return HTML", call. = FALSE)
}
