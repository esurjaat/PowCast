#' flight_prices_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_flight_prices_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("airport"),
                label = "Flying Out Of:",
                choices = c("SFO", "OAK"),
                selected = "SFO",
                multiple = TRUE),
    selectInput(inputId = ns("price_calculation"),
                label = "Flight Price Calculation",
                choices = c("Median", "Mean", "Min", "Max"),
                selected = "Median"),
    dateInput(inputId = ns("depart"),
              label = "Depart Date",
              value = Sys.Date()),
    dateInput(inputId = ns("return"),
              label = "Return Date",
              value = Sys.Date() + 3)
    )
}

#' flight_prices_inputs Server Functions
#'
#' @noRd
mod_flight_prices_inputs_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    return(
      list(
        airport = reactive({ input$airport }),
        price_calc = reactive({ input$price_calculation }),
        date_depart = reactive({ input$depart }),
        date_return = reactive({ input$return })
      )
    )

  })
}

## To be copied in the UI
# mod_flight_prices_inputs_ui("flight_prices_inputs_ui_1")

## To be copied in the server
# mod_flight_prices_inputs_server("flight_prices_inputs_ui_1")
