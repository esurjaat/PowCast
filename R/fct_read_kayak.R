#' read_kayak
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_kayak <- function(from, to, depart, return){
  # Build URL ====
  url_base <- "https://www.kayak.com/flights"
  url <- paste0(url_base, "/", from, "-", to, "/", depart, "/", return)

  # Get Prices ====
  webpage <-
    url %>%
    read_html()

  prices <-
    webpage %>%
    html_nodes(xpath = "//a/span/span") %>%
    html_text() %>%
    str_replace_all("(\\$)|(\n)", "") %>%
    as.numeric()

  return(prices)
}
