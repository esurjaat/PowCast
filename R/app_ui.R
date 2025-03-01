#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("PowCast"),
      br(),
      hr(),
      shiny::tabsetPanel(
        tabPanel(title = "About",
                 br()
                 ),
        tabPanel(title = "PowFlight Ratios",
                 br(),
                 sidebarLayout(
                   sidebarPanel(
                     width = 3,
                     h2("Flight Info"),
                     mod_flight_prices_inputs_ui("flight_prices_inputs_ui_1")
                   ),
                   mainPanel(
                     width = 9
                   )
                   )
                 )
        )
      )
    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'PowCast'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

