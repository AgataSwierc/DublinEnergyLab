#' ## Initialize script
source("App/Common.R")


#' Initialize common parameters
lifetime_length <- 25


#' Initialize inverter specification.
pv_inverters <- read.csv2("pv_inverters.csv")

#' Select inverter specification for the simulation.
inverter_spec <- as.list(pv_inverters[pv_inverters$model == "SE3000 (208V) w/ -ER-US or A-US", ])


#' Initialize battery specification
battery_spec <- powerwall_spec


#' Initialize pv specification.
pv_module_spec <- list(
  model = "Panasonic VBHN245SJ25",
  capacity = 0.245, # kWp
  area = 1.26, # m^2
  efficiency = 0.194, # %
  deterioration_curve = list(
    year = c(1, 11, 26),
    guarantee = c(1.0, 0.90, 0.80))
)

#' Decrease module efficiency according to Lacour's analysis.
pv_module_spec$efficiency <-  0.94 * pv_module_spec$efficiency


#' Load energy demand profiles.
load("Cache/energy_demand_profiles.RData")


#' Define bands for demand profiles
energy_demand_profiles_sums <- sapply(energy_demand_profiles, sum)
energy_demand_profiles_bands_levels <- as.numeric(cut(energy_demand_profiles_sums, 
  c(0, 1000, 2500, 5000, 15000, 10e6)))
energy_demand_profiles_bands <- list()
energy_demand_profiles_bands[[1]] <- which(energy_demand_profiles_bands_levels == 1)
energy_demand_profiles_bands[[2]] <- which(energy_demand_profiles_bands_levels == 2)
energy_demand_profiles_bands[[3]] <- which(energy_demand_profiles_bands_levels == 3)
energy_demand_profiles_bands[[4]] <- which(energy_demand_profiles_bands_levels == 4)
energy_demand_profiles_bands[[5]] <- which(energy_demand_profiles_bands_levels == 5)




#' Load roofs.
roofs <- read.csv("Data/roofs.csv", sep=";")
roofs$AzimuthRounded <- round(roofs$Azimuth / (360 / 16)) * (360 / 16)
roofs$AngleRounded <- round(roofs$Angle / 5) * 5

#ggplot(roofs, aes(Area)) + geom_histogram()
#ggplot(roofs, aes(Azimuth)) + geom_histogram()
#ggplot(roofs, aes(Angle)) + geom_histogram()


#' Define bands for roofs.
roofs$AreaBand <- as.numeric(cut(roofs$Area, 
  c(0, quantile(roofs$Area, c(0.02303071, 0.19793057, 0.62082777, 0.95894526, 1.00000000)))))

roofs_bands <- list()
roofs_bands[[1]] <- which(roofs$AreaBand == 1)
roofs_bands[[2]] <- which(roofs$AreaBand == 2)
roofs_bands[[3]] <- which(roofs$AreaBand == 3)
roofs_bands[[3]] <- which(roofs$AreaBand == 4)
roofs_bands[[3]] <- which(roofs$AreaBand == 5)


#' Load solar radiation.
load("Data/solar_radiations.RData")

sample_period <- 0.5 # [h]

results <- data.frame()
for (i in 1:200) {
  # Pick demand, roof, azimuth and output at random
  random_band <- ceiling(runif(1, max = 5))
  random_energy_demand_index <- energy_demand_profiles_bands[[random_band]][ceiling(runif(1, max = length(energy_demand_profiles_bands[[random_band]])))]
  random_roof_index <- roofs_bands[[random_band]][ceiling(runif(1, max = length(roofs_bands[[random_band]])))]
  random_azimuth <- round(runif(1, min = 90, max = 270) / 22.5) * 22.5
  
  random_roof <- roofs[random_roof_index, ]
  random_energy_demand <- energy_demand_profiles[, random_energy_demand_index] # [kWh]
  random_roof_radiation <- solar_radiations[[as.character(random_azimuth)]][[as.character(random_roof$AngleRounded)]] # [kW / m^2]
  
  # Stop if the pv module area would be greater than what would fit on the roof.
  pv_array_size_max <- floor((0.8 * random_roof$Area) / pv_module_spec$area)
  progress <- progress_estimated(pv_array_size_max)
  
  for (pv_array_size in 1:pv_array_size_max) {
    pv_array_spec <- list(
      capacity = pv_module_spec$capacity * pv_array_size, # [kWp]
      area = pv_module_spec$area * pv_array_size, # [m^2]
      size = pv_array_size,
      efficiency = pv_module_spec$efficiency # [%]
    )
    
    random_pv_array_power_output <- pv_array_spec$area * pv_array_spec$efficiency * random_roof_radiation # [kW]
    random_pv_array_energy_output <- random_pv_array_power_output * sample_period # [kWh]
    
    energy_balance <- calculate_lifetime_energy_balance(
      random_energy_demand,
      random_pv_array_energy_output,
      inverter_spec,
      powerwall_spec,
      pv_module_spec,
      lifetime_length)
    
    cashflow_summary <- calculate_cashflow_summary(
      energy_balance,
      pv_array_spec,
      battery_spec)
    
    economical_indicators <- calculate_economical_indicators(cashflow_summary)
    
    results_partial <- data.frame(
      index = i,
      band = random_band,
      demand_index = random_energy_demand_index,
      roof_index = random_roof_index,
      azimuth = random_azimuth,
      pv_array_size = pv_array_size,
      solar_energy_demand_ratio = sum(cashflow_summary$energy_demand_covered) / sum(cashflow_summary$energy_demand),
      npv = economical_indicators$npv,
      spp = economical_indicators$spp,
      lcoe = economical_indicators$lcoe,
      sir = economical_indicators$sir,
      dpp = economical_indicators$dpp)
    
    results <- rbind(results, results_partial)
    
    progress$tick()
    progress$print()
  }
  progress$stop()
  progress$print()
}