#' read_wiki_airport
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_wiki_airport <- function(){
  # Read and clean tables
  temp <-
    "https://en.wikipedia.org/wiki/List_of_airports_by_IATA_airport_code:_" %>%
    paste0(LETTERS) %>%
    map_df(.f = ~read_html(.x) %>% html_table() %>% .[[1]]) %>%
    janitor::clean_names() %>%
    filter(!(iata %>% str_detect("^-")))

  # Output
  temp
}
