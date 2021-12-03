#' read_wiki_airlines
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_wiki_airlines <- function(){
  "https://en.wikipedia.org/wiki/List_of_airline_codes" %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    janitor::clean_names() %>%
    mutate(across(.fns = function(x){na_if(x = x, y = "")}))
}
