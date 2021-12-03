#' read_codes
#'
#' @description Reads in Airline Codes from Wikipedia
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_codes <- function(type = "airline"){
  if(type == "airline"){
    # Set parameters ====
    urls <-
      paste0("https://www.iata.org/en/publications/directories/code-search/?airline.page=",
             1:224,
             "&airline.search=")

    codes <-
      purrr::map_df(.x = urls,
                    .f = ~rvest::read_html(.x) %>%
                      rvest::html_table() %>%
                      .[[1]]) %>%
      as_tibble() %>%
      set_names(c("airline", "country", "code", "accounting_code", "airline_prefix")) %>%
      filter(airline != "Company name")

    closeAllConnections()
  } else if(type == "airport") {
    urls <-
      paste0("https://www.iata.org/en/publications/directories/code-search/?airport.page=",
             1:10,
             "&airport.search=")
    codes <-
      map_df(.x = urls,
             .f = ~read_html(.x) %>%
               html_table() %>%
               .[[2]]) %>%
      as_tibble() %>%
      set_names(c("city", "airport_name", "airport_code")) %>%
      filter(city != "City Name")

    closeAllConnections()
  }

  # Output ====
  codes

}
