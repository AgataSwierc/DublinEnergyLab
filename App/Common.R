#' Load required libraries.
library(xts)
library(dplyr)
library(dygraphs)
library(shiny)
library(RColorBrewer)

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
  current_peak = 8.6, # Amp
  cost = 2654 # EUR
)

battery_spec <- powerwall_spec

#' Load data from available files
#+ datasets, cache=TRUE
#demand_profiles <- read.table("Data/data1.csv", header = FALSE, sep = ";")
#generation_normalized <- read.table("Data/pv30minsgen.csv", header = FALSE)[[1]]

run_simulation <- function(pv_array_size = 8, demand_profile_index = 1, inverter_efficiency = 0.975) {
  inverter_spec <- list(
    efficiency = inverter_efficiency
  )
  
  pv_module <- list(
    capacity = 0.215 # kWp
  )
  
  pv_array <- list(
    capacity = pv_module$capacity * pv_array_size,
    size = pv_array_size
  )  
  
  #' Simulate charge/discharge cycles of the battery for the demand profile
  #+ simulation, cache=TRUE
  n <- nrow(demand_profiles)
  period <- 0.5 # 30 minutes
  demand_profile <- demand_profiles[[demand_profile_index]]
  pv_array_output <- pv_array$capacity * generation_normalized
  energy_imported <- rep(0, n)
  battery_energy <- rep(0, n)
  battery_energy_next <- rep(0, n)
  battery_diff <- rep(0, n)
  battery_roundtrip_loss <- rep(0, n)
  inverter_input <- rep(0, n)
  inverter_output <- rep(0, n)
  inverter_loss <- rep(0, n)
  
  for (i in 1:n) {
    # Battery_energy starts as zero and flows from battery_energy_next variable
    battery_energy[i] <- if (i == 1) 0 else battery_energy_next[i - 1]
    
    # Start with demand matching the demand profile 
    ac_demand <- demand_profile[i]
    dc_demand <- ac_demand / inverter_spec$efficiency
    dc_diff <- dc_demand - pv_array_output[i]
    
    if (dc_diff > 0) {
      # Demand was higher than what was generated. We need to get more energy.
      
      if (battery_energy[i] > 0) {
        # Batter is not empty. We can use this energy.
        
        battery_diff[i] <- min(dc_diff, battery_energy[i], battery_spec$power_nominal * period)
        
        # Take energy from battery
        battery_energy_next[i] <- battery_energy[i] - battery_diff[i]
        
        inverter_input[i] <- pv_array_output[i] + battery_diff[i]
      } else {
        # Battery is empty. Inverter input matches PV array output.
        
        inverter_input[i] <- pv_array_output[i]
        battery_energy_next[i] <- battery_energy[i]
      }
    } else {
      # Demand was lower than what was generated. We can save the extra energy.
      
      if (battery_energy[i] < battery_spec$capacity) {
        # Battery is not full. We can save some energy in the battery.
        
        battery_diff[i] <- max(
          - (battery_spec$capacity - battery_energy[i]) / battery_spec$efficiency,
          dc_diff)
        
        # Save energy in the battery
        battery_energy_next[i] <- battery_energy[i] - battery_diff[i] * battery_spec$efficiency
        battery_roundtrip_loss[i] <- - battery_diff[i] * (1 - battery_spec$efficiency)
        
        inverter_input[i] <- pv_array_output[i] + battery_diff[i]
      } else {
        # Battery is full. We need to sell extra energy to the grid.
        
        inverter_input[i] <- pv_array_output[i]
        battery_energy_next[i] <- battery_energy[i]
      }
    }
    
    inverter_output[i] <- inverter_input[i] * inverter_spec$efficiency
    inverter_loss[i] <- inverter_input[i] * (1 - inverter_spec$efficiency)
    
    energy_imported[i] <- ac_demand - inverter_output[i]
  }
  
  # Show result in a form of a graph
  df <- data.frame(
    pv_array_output,
    demand_profile,
    inverter_loss,
    battery_roundtrip_loss,
    battery_energy_next,
    battery_energy,
    energy_imported,
    battery_diff,
    battery_percentage = battery_energy / battery_spec$capacity * 100)
  df
  return(xts(df, date))
}