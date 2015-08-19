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

demand_profiles <- read.table("Data/data1.csv", header = FALSE, sep = ";")
generation_normalized <- read.table("Data/pv30minsgen.csv", header = FALSE)[[1]]

pv_module <- list(
  capacity = 0.215, # kWp
  cost = 5700 # euro
)

pv_module_number <- 8

pv_array <- list(
  capacity = pv_module$capacity * pv_module_number
)

n <- nrow(demand_profiles)
interval <- 0.5 # 30 minutes
demand_profile <- demand_profiles[[1]]
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
    }
  }
}


date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30* 60)
df <- data.frame(
  demand_profile = demand_profile,
  battery_energy = battery_energy,
  generation_total = generation_total)
myxts <- xts(df, date, 17520) 
myxts <- myxts['2009-05']
dygraph(myxts) 
#%>% dyOptions(fillGraph = TRUE, fillAlpha = 0.2)



