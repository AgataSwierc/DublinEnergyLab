
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
  battery_energy <- rep(0, n)
  energy_imported <- rep(0, n)
  battery_roundtrip_loss <- rep(0, n)
  battery_energy_next <- rep(0, n)
  inverter_loss <- rep(0, n)
  inverter_loss_pv <- rep(0, n)
  inverter_loss_battery <- rep(0, n)
  battery_input <- rep(0, n)
  battery_output <- rep(0, n)
  
  for (i in 1:n) {
    if (i == 2855) browser()
    
    # Battery_energy starts as zero and flows from battery_energy_next variable
    battery_energy[i] <- if (i == 1) 0 else battery_energy_next[i - 1]
    
    # Start with demand matching the demand profile 
    ac_demand <- demand_profile[i]
    demand <- demand / inverter_spec$efficiency
    
    # Energy used from PV array output to immediately to cover current demand
    pv_array_output_immediate <- min(demand_dc, pv_array_output[i])
    pv_array_output_available <- pv_array_output[i] - pv_array_output_immediate
    
    # Decrease demand by energy taken from the PV array output
    demand_dc <- demand_dc - pv_array_output_immediate
    demand <- demand_dc * inverter_spec$efficiency
    
    # Energy lost in the inverter while converting pv output
    inverter_loss_pv[i] <- pv_array_output_immediate * (1 - inverter_spec$efficiency)
    
    if (demand > 0) {
      # Demand was higher than what was generated. We need to get more energy.
      
      if (battery_energy[i] > 0) {
        # Batter is not empty. We can use this energy.
        
        battery_output[i] <- min(
          demand / inverter_spec$efficiency,
          battery_energy[i],
          battery_spec$power_nominal * period)
        
        # Take energy from battery
        battery_energy_next[i] <- battery_energy[i] - battery_output[i]
        
        # Decrease demand by the amount taken from the battery
        demand <- demand - battery_output[i] * inverter_spec$efficiency
        
        # Keep track of energy loss in the inverter
        inverter_loss_battery[i] <- battery_output[i] * (1 - inverter_spec$efficiency)
        
        # Cover remaining demand from the grid
        energy_imported[i] <- demand
      } else {
        # Battery is empty. We need to import energy from the grid.
        
        energy_imported[i] <- demand
        battery_energy_next[i] <- battery_energy[i]
      }
    } else {
      # Demand was lower than what was generated. We can save the extra energy.
      
      if (battery_energy[i] < battery_spec$capacity) {
        # Battery is not full. We can save some energy in the battery.
        
        battery_input[i] <- min(
          (battery_spec$capacity - battery_energy[i]) / battery_spec$efficiency,
          pv_array_output_available)
        
        # Save energy in the battery
        battery_energy_next[i] <- battery_energy[i] + battery_input[i] * battery_spec$efficiency
        battery_roundtrip_loss[i] <- battery_input[i] * (1 - battery_spec$efficiency)
        
        # Decrease pv_array_output_available by the amount saved in the battery
        pv_array_output_available <- pv_array_output_available - battery_input[i]
        
        # Leak extra energy to the grid
        energy_imported[i] <- -pv_array_output_available * inverter_spec$efficiency
        inverter_loss_pv[i] <- inverter_loss_pv[i] + pv_array_output_available * (1 - inverter_spec$efficiency)
      } else {
        # Battery is full. We need to sell extra energy to the grid.
        
        energy_imported[i] <- -pv_array_output_available * inverter_spec$efficiency
        inverter_loss_pv[i] <- inverter_loss_pv[i] + pv_array_output_available * (1 - inverter_spec$efficiency)
        
        battery_energy_next[i] <- battery_energy[i]
      }
    }
  }
  
  # Show result in a form of a graph
  df <- data.frame(
    pv_array_output,
    demand_profile,
    inverter_loss_pv,
    inverter_loss_battery,
    battery_roundtrip_loss,
    energy_imported,
    battery_input,
    battery_output,
    battery_percentage = battery_energy / battery_spec$capacity)
  df
  #return(xts(df, date))
}

s <- run_simulation()


which(-0.05 > with(s, pv_array_output - demand_profile - inverter_loss_pv - inverter_loss_battery - battery_roundtrip_loss + energy_imported - (battery_energy_next - battery_energy)))


summary(with(s, pv_array_output - demand_profile - inverter_loss_pv - inverter_loss_battery - battery_roundtrip_loss + energy_imported - (battery_energy_next - battery_energy) + battery_output))
with(s, pv_array_output - demand_profile - inverter_loss_pv - inverter_loss_battery - battery_roundtrip_loss + energy_imported - battery_input + battery_output)[2855]
View(t(s[2855,]))

