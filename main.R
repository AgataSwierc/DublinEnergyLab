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
#rmarkdown::render("main.R")


#' ## Initialize script

#' Load required libraries.
library(xts)
library(dplyr)
library(dygraphs)


#' Define parameters of the powerwall battery.
powerwall_spec = list(
  #model = "10 kWh $3,500 For backup applications",
  model = "7 kWh $3,000 For daily cycle applications",
  capacity = 7, # kWh
  efficiency = 0.92, #round-trip DC efficiency
  power_nominal = 2.0, # kW
  power_peak = 3.3, # kW
  voltage_nominal = 350, # Volts
  voltage_range = c(350, 450), # Volts
  current_nominal = 5.8, # Amp
  current_peak = 8.6 # Amp
)

battery_spec <- powerwall_spec

#' Load data from available files
#+ datasets, cache=TRUE
demand_profiles <- read.table("Data/data1.csv", header = FALSE, sep = ";")
generation_normalized <- read.table("Data/pv30minsgen.csv", header = FALSE)[[1]]


date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
date_selected_start <- "2009-05-01"
date_selected_end <- "2009-05-31"

run_simulation <- function(pv_array_size = 8, demand_profile_index = 1) {
  pv_module <- list(
    capacity = 0.215, # kWp
    cost = 5700 # euro
  )
  
  pv_array <- list(
    capacity = pv_module$capacity * pv_array_size,
    size = pv_array_size
  )  

  #' Simulate charge/discharge cycles of the battery for the demand profile
  #+ simulation, cache=TRUE
  n <- nrow(demand_profiles)
  interval <- 0.5 # 30 minutes
  demand_profile <- demand_profiles[[demand_profile_index]]
  generation_total <- generation_normalized * pv_array$capacity
  battery_energy <- rep(0, n)
  energy_imported <- rep(0, n)
  

  for (i in 1:(n - 1)) {
    energy_diff <- demand_profile[i] - generation_total[i]
    if (energy_diff > 0) {
      # Demand was higher than what was generated. We need to get more energy.
      
      if (battery_energy[i] > 0) {
        # Batter is not empty. We can use this energy.
        
        battery_diff <- min(energy_diff, battery_energy[i], battery_spec$power_nominal * interval)
        
        # Take energy from battery
        battery_energy[i + 1] <- battery_energy[i] - battery_diff
        
        # Decrease energy_diff by the amount taken from the battery
        energy_diff <- energy_diff - battery_diff
        
        # Cover remaining demand from the grid
        energy_imported[i] <- energy_diff
      } else {
        # Battery is empty. We need to import energy from the grid.
        
        energy_imported[i] <- energy_diff
        battery_energy[i + 1] <- battery_energy[i]
      }
    } else {
      # Demand was lower than what was generated. We can save the extra energy.
      
      if (battery_energy[i] < battery_spec$capacity) {
        # Battery is not full. We can save some energy in the battery.
        
        battery_diff <- max((battery_energy[i] - battery_spec$capacity), energy_diff)
        
        # Save energy in the battery
        battery_energy[i + 1] <- battery_energy[i] - battery_diff
        
        # Decrease energy_diff by the amount taken from the battery
        energy_diff <- energy_diff - battery_diff
        
        # Leak extra energy to the grid
        energy_imported[i] <- energy_diff
      } else {
        # Battery is full. We need to sell extra energy to the grid.
        
        energy_imported[i] <- energy_diff
        battery_energy[i + 1] <- battery_energy[i]
      }
    }
  }
  
  # Show result in a form of a graph
  df <- data.frame(
    demand_profile,
    generation_total,
    energy_imported,
    battery_percentage = battery_energy / battery_spec$capacity)
  return(xts(df, date))
}


#+ echo=FALSE
dygraph(myxts_selected[, c("demand_profile", "generation_total")], group = "may") %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
  dyRangeSelector()
#+ echo=FALSE
dygraph(myxts_selected[, "battery_percentage"], group = "may") %>% 
  dyRangeSelector() %>%
  dyAxis("y", valueRange = c(0, 1.001)) %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.1)

dygraph(myxts_selected[, "energy_imported"], group = "may") %>% 
  dyRangeSelector() %>%
  dyOptions(fillGraph = TRUE, fillAlpha = 0.1)


#%>% dyOptions(fillGraph = TRUE, fillAlpha = 0.2)

#write.csv(date, file = "dates.csv", row.names = FALSE)


server <- function(input, output) {
  myxts <- reactive({
    pv_array_size <- input[["pv_array_size"]]
    demand_profile_index <- input[["demand_profile_index"]]
    run_simulation(pv_array_size, demand_profile_index)
  })
  
  myxts_selected <- reactive({
    date_range_selected <- sort(input[["date_range_selected"]])
    myxts()[paste(date_range_selected[1], date_range_selected[2], sep = "/")]
  })
  
  output[["demand_profile_plot"]] <- renderDygraph({
    dygraph(myxts_selected()[, c("demand_profile", "generation_total")], main = "Energy demand and generation", group = "may") %>%
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 2)) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1) %>%
      dyRangeSelector()
  })
  output[["battery_percentage"]] <- renderDygraph({
    dygraph(myxts_selected()[, "battery_percentage"], main = "Battery charge (%)", group = "may") %>% 
      dyRangeSelector() %>%
      dyAxis("y", valueRange = c(0, 1.001)) %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1)
  })
  output[["energy_imported"]] <- renderDygraph({
    dygraph(myxts_selected()[, "energy_imported"], main = "Energy imported (+) / exported (-)", group = "may") %>% 
      dyRangeSelector() %>%
      dyOptions(fillGraph = TRUE, fillAlpha = 0.1)
  })
}

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

app <- shinyApp(ui = ui, server = server)

runApp(app, launch.browser = FALSE)


