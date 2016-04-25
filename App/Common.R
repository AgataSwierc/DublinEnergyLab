#' Load required libraries.
library(xts)
library(dplyr)
library(dygraphs)
library(shiny)
library(RColorBrewer)
library("testthat")

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
  cost = 3000 # EUR
)

# Use Powerwall battery
battery_spec <- powerwall_spec

#' Load data from available files
#+ datasets, cache=TRUE
#demand_profiles <- read.table("Data/data1.csv", header = FALSE, sep = ";")
#generation_normalized <- read.table("Data/pv30minsgen.csv", header = FALSE)[[1]]

run_simulation <- function(
  pv_array_size = 8,
  demand_profile_index = 1,
  inverter_efficiency = 0.975,
  battery_spec = powerwall_spec) {
  
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
    battery_percentage = if (battery_spec$capacity > 0) battery_energy / battery_spec$capacity * 100 else 0)
  df
  return(xts(df, date))
}

create_npv_table <- function(simulation_result, pv_array_size) {
  year_result <- simulation_result
  
  energy_prices_residential <- data.frame(
    band = c("DA", "DB", "DC", "DD", "DE"),
    band_range = c("< 1e+03", "(1e+03,2.5e+03]", "(2.5e+03,5e+03]", "(5e+03,1.5e+04]", "1.5e+04 <"),
    price = c(0.665, 0.321, 0.254, 0.218, 0.182),
    price_change = c(0.037, 0.075, 0.055, 0.036, 0.01))
  
  energy_consumption_band <- as.numeric(cut(sum(year_result$demand_profile), c(0, 1000, 2500, 5000, 15000, 10e6)))
  
  installation_lifespan <- 25 # year
  tariff_export <- 0 # EUR
  tariff_import <- energy_prices_residential$price[energy_consumption_band] # EUR
  # Assume constant price of the imported energy
  #tariff_import_change <- energy_prices_residential$price_change[energy_consumption_band] # change / year
  tariff_import_change <- 0 # change / year
  
  inverter_spec <- list(
    efficiency = 0.975, # -
    lifespan = 10 # year
  )
  
  pv_module <- list(
    capacity = 0.215, # kWp
    cost = 236 # EUR
  )
  
  pv_array <- list(
    capacity = pv_module$capacity * pv_array_size, # kWp
    size = pv_array_size, # -
    cost = pv_module$cost * pv_array_size # EUR
  )
  
  inverter_cost_std <- 619 # EUR/kWp
  instrumentation_and_control_cost <- 177 # EUR
  installation_electrical_cost <- 543 # EUR
  installation_civil_cost <- 904 # EUR
  installation_mechanical_cost <- 191 # EUR

  other_costs <- 88 # EUR
  
  initial_cost_option <- "new"
  if (initial_cost_option == "old") {
    initial_cost <-
      pv_array$cost +
      battery_spec$cost +
      pv_array$capacity * inverter_cost_std +
      instrumentation_and_control_cost +
      installation_electrical_cost +
      installation_civil_cost +
      installation_mechanical_cost +
      other_costs
  } else {
    estimated_system_unit_cost <- 3 * pv_array$capacity ^ (-0.537) # EUR / Wp
    estimated_system_cost <- estimated_system_unit_cost * pv_array$capacity * 1000 # EUR
    initial_cost <- estimated_system_cost + battery_spec$cost
  }
 
  # Maintainance cost takes into account inverter replacement
  maintenance_ratio = 0.01 # 1% of CAPEX without the battery
  maintenance_cost <- maintenance_ratio * (initial_cost - battery_spec$cost) # EUR/year

  inverter_cost <- pv_array$capacity * inverter_cost_std
  
  npv_table <- data.frame(year_index = 0:installation_lifespan)
  investment_year <- npv_table$year_index == 0
  operating_year <- npv_table$year_index > 0
  npv_table$energy_demand <- operating_year * sum(year_result$demand_profile)
  
  npv_table$energy_exported <- operating_year * sum(ifelse(year_result$energy_imported < 0, -year_result$energy_imported, 0))
  npv_table$energy_imported <- operating_year * sum(ifelse(year_result$energy_imported > 0,  year_result$energy_imported, 0))
  
  npv_table$tariff_export <- operating_year * tariff_export
  npv_table$tariff_import <- operating_year * tariff_import * (1 + tariff_import_change) ^ npv_table$year_index
  
  npv_table$inflow_export <- operating_year * npv_table$tariff_export * npv_table$energy_exported
  npv_table$inflow_saving <- operating_year * npv_table$tariff_import * (npv_table$energy_demand - npv_table$energy_imported)
  
  npv_table$outflow_import <- operating_year * npv_table$tariff_import * npv_table$energy_imported
  
  npv_table$outflow_maintainence <- operating_year * maintenance_cost
  
  npv_table$inflow <- operating_year * (npv_table$inflow_export + npv_table$inflow_saving)
  npv_table$outflow <- operating_year * (npv_table$outflow_import + npv_table$outflow_maintainence)
  npv_table$cashflow_investment <- investment_year * initial_cost
  
  npv_table$net_cashflow <- npv_table$inflow - npv_table$outflow - npv_table$cashflow_investment
  
  npv_table$cumulative_balance <- cumsum(npv_table$inflow) - 
    cumsum(npv_table$cashflow_investment) - 
    cumsum(npv_table$outflow_maintainence)
  
  return(npv_table)
}

discount <- function(value, rate) {
  value / (1 + rate) ^ (seq_along(value) - 1)
}

calculate_npv <- function(simulation_result, pv_array_size) {
  # Real discount rate
  discount_rate <- 0.08 # -
  
  npv_table <- create_npv_table(simulation_result, pv_array_size)
  
  npv <- ceiling(sum(npv_table$net_cashflow / (1 + discount_rate) ^ npv_table$year_index))
  
  positive_year <- npv_table$year_index[npv_table$cumulative_balance > 0]
  spp <- if (length(positive_year) > 0) min(positive_year) else NA
  
  lcoe <- 
    sum(discount(
      npv_table$cashflow_investment + npv_table$outflow_maintainence, 
      discount_rate)) / 
    sum(discount(
      (npv_table$energy_demand - npv_table$energy_imported) + npv_table$energy_exported,
      discount_rate))
  
  sir <- 
    sum(discount(
      npv_table$inflow_saving,
      discount_rate))  /
    sum(discount(
      npv_table$cashflow_investment + npv_table$outflow_maintainence, 
      discount_rate))
    
  return(list(npv = npv, spp = spp, lcoe = lcoe, sir = sir))
}