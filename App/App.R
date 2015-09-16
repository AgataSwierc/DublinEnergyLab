#' ---
#' title: "Battery simulation"
#' author: "Agata Swierc"
#' date: "2015-08-19"
#' output:
#'   html_document:
#'     highlight: haddock
#'     toc: yes
#'     fig.align: "center"
#'     fig_width: 9.5
#'     fig_height: 5
#' ---
#+ echo=FALSE

source("Common.R")

#save(demand_profiles, generation_normalized, file = "AppData.RData")
load("AppData.RData")

server <- function(input, output) {
  myxts <- reactive({
    pv_array_size <- input[["pv_array_size"]]
    demand_profile_index <- input[["demand_profile_index"]]
    inverter_efficiency <- input[["inverter_efficiency"]]
    run_simulation(pv_array_size, demand_profile_index, inverter_efficiency)
  })
  
  myxts_selected <- reactive({
    date_range_selected <- input[["date_range_selected"]]
    # If second date is smaller than the first one then make
    # them equal. That way data from a single day will be used.
    date_range_selected[2] <- max(date_range_selected)
    index <- paste(date_range_selected[1], date_range_selected[2], sep = "/")
    return(myxts()[index])
  })
  
  output[["demand_profile_plot"]] <- renderDygraph({
    dygraph(myxts_selected()[, c("demand_profile", "pv_array_output")], main = "Energy demand and generation", group = "may") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
      dyRangeSelector() %>%
      dyLegend(width = 500, show = "always")
  })
  
  output[["battery_percentage"]] <- renderDygraph({
    dygraph(myxts_selected()[, "battery_percentage"], main = "Battery charge (%)", group = "may") %>% 
      dyRangeSelector() %>%
      dyAxis("y", valueRange = c(0, 100.1)) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1, colors = RColorBrewer::brewer.pal(3, "Set1")) %>%
      dyLegend(show = "always")
  })
  
  output[["energy_imported"]] <- renderDygraph({
    dygraph(myxts_selected()[, "energy_imported"], main = "Energy imported (+) / exported (-)", group = "may") %>% 
      dyRangeSelector() %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
      dyLegend(show = "always")
  })
}


date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
date_selected_start <- "2009-05-01"
date_selected_end <- "2009-05-31"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
      numericInput("demand_profile_index",
        label = "Household index:",
        min = 1,
        max = length(demand_profiles),
        value = 1),
      sliderInput("pv_array_size",
        label = "Number of solar modules:",
        min = 1,
        max = 25,
        value = 8),
      sliderInput("battery_array_size",
        label = "Number of batteries:",
        step = 1,
        min = 1,
        max = 2,
        value = 1),
      sliderInput("inverter_efficiency",
        label = "Inverter efficiency:",
        step = 0.005,
        min = 0.9,
        max = 1,
        value = 0.975),
      dateRangeInput("date_range_selected",
        label = "Date range:",
        start = date_selected_start,
        end = date_selected_end,
        min = min(date),
        max = max(date))
    ),
    mainPanel(width = 9,
      dygraphOutput("demand_profile_plot",
        height = "300px"),
      dygraphOutput("battery_percentage",
        height = "300px"),
      dygraphOutput("energy_imported",
        height = "300px"))
  )
)

shinyApp(ui = ui, server = server)