#' @title Make a function that can be used to extract values from XML object.
#'
#' @param x Either a document, a node set or a single node.
#'
#' @return
#' \code{function(css)}
#' \itemize{
#'      \item css     :  Nodes to select.
#' }
#'
#' @seealso Used by \code{\link{get_shopinfo_en}} and \code{\link{get_shopinfo_ja}}.
#' @importFrom dplyr "%>%"
#'
#' @keywords internal

extract_functor = function(x)
        function(css) rvest::html_nodes(x, css) %>% rvest::html_text(trim=T)
