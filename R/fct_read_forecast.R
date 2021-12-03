#' read_forecast
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
read_forecast <- function(area_selection = "US"){
  # URL
  url_base <- "https://snowforecast.com"
  page_base <- read_html(url_base)

  ## Main Menu ====
  node_base <-
    page_base %>%
    html_nodes(xpath = "//html/body/div[8]/div/div/div[1]/ul/li/a")

  links_menu <-
    tibble(
      link = file.path(
        url_base,
        node_base %>%
          html_attrs() %>%
          unlist() %>%
          unname() %>%
          str_replace("/", "")
      ),
      text = node_base %>%
        html_text()
    ) %>%
    filter(str_detect(text, "Forecasts")) %>%
    mutate(html = link %>% map(.f = ~read_html(.x)),
           text = text %>% str_replace("( Forecasts)$", ""))

  ## Region ====
  nodes_region <-
    links_menu %>%
    pull(html) %>%
    map(.f = ~html_nodes(.x, xpath = "/html/body/div[5]/div/div[3]/div[1]/div[2]/div/ul/li/a"))

  links_region <-
    pmap(
      .l = list(a = nodes_region, b = links_menu$link, c = links_menu$text),
      .f = function(a, b, c) {
        tibble(
          link = file.path(
            b %>% str_replace("/$", ""),
            a %>%
              html_attrs() %>%
              unlist() %>%
              unname() %>%
              str_replace("/", "")
          ),
          region = a %>%
            html_text() %>%
            str_replace_all("( \\(.*\\) +)$", ""),
          area = c
        )
      }
    ) %>%
    map_df(.f = ~ mutate(.x, html = link %>% map(.f = ~ read_html(.x))))

  # Subregion
  nodes_subregion <-
    links_region %>%
    pull(html) %>%
    map(.f = ~html_nodes(.x, xpath = "/html/body/div[5]/div/div[3]/div[1]/div[2]/div/ul/li/a"))


  links_subregion <-
    pmap(
      .l = list(
        a = nodes_subregion,
        b = links_region$link,
        c = links_region$region,
        d = links_region$area
      ),
      .f = function(a, b, c, d) {
        tibble(
          link = file.path(
            b %>% str_replace("/$", ""),
            a %>%
              html_attrs() %>%
              unlist() %>%
              unname() %>%
              str_replace("/", "")
          ),
          subregion = a %>%
            html_text() %>%
            str_replace("( \\(.*\\) +)$", ""),
          region = c,
          area = d
        )
      }
    ) %>%
    map_df(.f = ~mutate(.x, html = link %>% map(.f = ~read_html(.x))))

  # Selection
  links <- links_subregion %>% filter(area == area_selection)

  pmap_df(
      .l = list(
        zLinks = links$html,
        zArea = links$area,
        zRegion = links$region,
        zSubregion = links$subregion),
      .f = function(zLinks, zArea, zRegion, zSubregion){
        html_table(zLinks) %>%
          .[[1]] %>%
          set_names(
            c(
              "x1",
              "x2",
              "location",
              "x3",
              "forecast_24",
              "forecast_72",
              "forecast_120",
              "new",
              "base",
              "season",
              "base_el",
              "top_el",
              "acres",
              "lifts",
              "trails"
            )
          ) %>%
          filter(location != "Location") %>%
          select(-starts_with("x")) %>%
          mutate_at(
            .vars = vars(forecast_24:new),
            .funs = list(. %>% str_replace_all('(\\")|( )', "") %>% as.numeric())
          ) %>%
          mutate_at(.vars = vars(base_el:lifts),
                    .funs = list(. %>% str_replace("^()$", "0") %>% as.numeric())
          ) %>%
          mutate_at(.vars = vars(base:season),
                    .funs = list(. %>% str_replace("^()$", "0"))) %>%
          mutate(base_beg = base %>% str_extract("^([0-9]+)"),
                 base_end = base %>% str_extract("([0-9]+)$"),
                 season_beg = season %>% str_extract("^([0-9]+)"),
                 season_end = season %>% str_extract("([0-9]+)$")) %>%
          select(location, forecast_24:new, base_beg:season_end, base_el:trails) %>%
          as_tibble() %>%
          mutate(area = zArea,
                 region = zRegion,
                 subregion = zSubregion)
        }
    ) %>%
    select(area, region, subregion, resort = location, everything()) %>%
    arrange(area, region, subregion, resort)

}
