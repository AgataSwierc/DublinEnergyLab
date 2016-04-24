
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
  period <- 0.5 # 30 minutes
  demand_profile <- demand_profiles[[demand_profile_index]]
  pv_array_output <- pv_array$capacity * generation_normalized
  energy_imported <- rep(0, n)
  battery_energy <- rep(0, n)
  battery_energy_next <- rep(0, n)
  battery_input <- rep(0, n)
  battery_output <- rep(0, n)
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
        
        battery_output[i] <- min(dc_diff, battery_energy[i], battery_spec$power_nominal * period)
        
        # Take energy from battery
        battery_energy_next[i] <- battery_energy[i] - battery_output[i]
        
        inverter_input[i] <- pv_array_output[i] + battery_output[i]
      } else {
        # Battery is empty. Inverter input matches PV array output.
        
        inverter_input[i] <- pv_array_output[i]
        battery_energy_next[i] <- battery_energy[i]
      }
    } else {
      # Demand was lower than what was generated. We can save the extra energy.
      
      if (battery_energy[i] < battery_spec$capacity) {
        # Battery is not full. We can save some energy in the battery.
        
        battery_input[i] <- min(
          (battery_spec$capacity - battery_energy[i]) / battery_spec$efficiency,
          - dc_diff)
        
        # Save energy in the battery
        battery_energy_next[i] <- battery_energy[i] + battery_input[i] * battery_spec$efficiency
        battery_roundtrip_loss[i] <- battery_input[i] * (1 - battery_spec$efficiency)
        
        inverter_input[i] <- pv_array_output[i] - battery_input[i]
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
    battery_input,
    battery_output,
    battery_percentage = battery_energy / battery_spec$capacity)
  df
  return(xts(df, date))
}


