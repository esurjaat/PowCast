#' read_forecast_opensnow
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_forecast_opensnow <- function(url){
  url %>%
    read_html() %>%
    html_table() %>%
    .[[3]] %>%
    janitor::clean_names() %>%
    rename(day = x,
           snowfall_12hr = x12_hr_snowfall) %>%
    select(-starts_with("x")) %>%
    mutate(day = day %>% str_extract("([0-9]{1,2}/[0-9]{1,2})$")) %>%
    filter(!is.na(day)) %>%
    mutate(
      date =
        paste(year(Sys.Date()),
              day %>% str_extract("^([0-9]{1,2})"),
              day %>% str_extract("([0-9]{1,2})$"),
              sep = "/") %>%
        ymd(),
      measurement_type = case_when(
        snowfall_12hr %>% str_detect("[0-9]+-[0-9]+") ~ "range",
        TRUE ~ "exact"
      ),
      snowfall_min = case_when(
        measurement_type == "exact" ~ str_extract(snowfall_12hr, "[0-9]+") %>% as.numeric(),
        measurement_type == "range" ~ str_extract(snowfall_12hr, "[0-9]+-") %>% str_extract("^([0-9]+)") %>% as.numeric(),
        TRUE ~ as.numeric(NA)
      ),
      snowfall_max = case_when(
        measurement_type == "exact" ~ str_extract(snowfall_12hr, "[0-9]+") %>% as.numeric(),
        measurement_type == "range" ~ str_extract(snowfall_12hr, "-[0-9]+") %>% str_extract("([0-9]+)$") %>% as.numeric(),
        TRUE ~ as.numeric(NA)
      )
    ) %>%
    rowwise() %>%
    mutate(snowfall_estimate = mean(c(snowfall_min, snowfall_max))) %>%
    ungroup() %>%
    select(date, snowfall_min, snowfall_max, snowfall_estimate) %>%
    group_by(date) %>%
    summarise(min = sum(snowfall_min),
              max = sum(snowfall_max),
              estimate = sum(estimate))

}
