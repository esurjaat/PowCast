#' read_resort_info
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_resort_info <- function(url){

  # Setup ====
  html_object <-
    url %>%
    read_html()

  # Coordinates
  temp <-
    tibble(
      latitude =
        html_object %>%
        html_nodes(xpath = "//a/span[1]/span/span[1]") %>%
        html_text(),
      longitude =
        html_object %>%
        html_nodes(xpath = "//a/span[1]/span/span[2]") %>%
        html_text()
    ) %>%
    unique()

  if(nrow(temp) > 0){
    temp
  } else {
    tibble(latitude = as.character(NA),
           longitude = as.character(NA))
  }


}

