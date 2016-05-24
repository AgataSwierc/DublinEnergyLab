#' ## Initialize script
source("App/Common.R")


#' Initialize common parameters
lifetime_length <- 25


#' Initialize inverter specification.
pv_inverters <- read.csv2("pv_inverters.csv")
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
# Decrease efficiency according to Lacour's analysis.
pv_module_spec$efficiency <-  0.94 * pv_module_spec$efficiency


#' Load demand profiles.
load("Cache/demand_profiles.RData")


#' Define bands for demand profiles
demand_profiles_sums <- sapply(demand_profiles, sum)
demand_profiles_bands_levels <- as.numeric(cut(demand_profiles_sums,
                                               quantile(demand_profiles_sums, c(0, 0.05, 0.35, 0.65 ,0.95, 1))))
demand_profiles_bands <- list()
demand_profiles_bands[[1]] <- which(demand_profiles_bands_levels == 2)
demand_profiles_bands[[2]] <- which(demand_profiles_bands_levels == 3)
demand_profiles_bands[[3]] <- which(demand_profiles_bands_levels == 4)


#' Load roofs.
roofs <- read.csv("Data/roofs.csv", sep=";")
roofs$AzimuthRounded <- round(roofs$Azimuth / (360 / 16)) * (360 / 16)
roofs$AngleRounded <- round(roofs$Angle / 5) * 5

#ggplot(roofs, aes(Area)) + geom_histogram()
#ggplot(roofs, aes(Azimuth)) + geom_histogram()
#ggplot(roofs, aes(Angle)) + geom_histogram()


#' Define bands for roofs.
roofs$AreaBand <- as.numeric(cut(roofs$Area, c(0, quantile(roofs$Area, c(0.33, 0.66, 1)))))

roofs_bands <- list()
roofs_bands[[1]] <- which(roofs$AreaBand == 1)
roofs_bands[[2]] <- which(roofs$AreaBand == 2)
roofs_bands[[3]] <- which(roofs$AreaBand == 3)


#' Load solar radiation.
load("Data/solar_radiations.RData")


results <- data.frame()
for(i in 1:200){
  # Pick demand, roof, azimuth and output at random
  random_band <- ceiling(runif(1, max = 3))
  random_demand_index <- demand_profiles_bands[[random_band]][ceiling(runif(1, max = length(demand_profiles_bands[[random_band]])))]
  random_roof_index <- roofs_bands[[random_band]][ceiling(runif(1, max = length(roofs_bands[[random_band]])))]
  random_azimuth <- round(runif(1, min = 90, max = 270) / 22.5) * 22.5
  
  random_roof <- roofs[random_roof_index, ]
  random_demand <- demand_profiles[, random_demand_index] # [kW]
  random_roof_radiation <- solar_radiations[[as.character(random_azimuth)]][[as.character(random_roof$AngleRounded)]] # [kW / m^2]
  
  # Stop if the pv module area would be greater than what would fit on the roof.
  pv_array_size_max <- floor((0.8 * random_roof$Area) / pv_module_spec$area)
  progress <- progress_estimated(pv_array_size_max)
  
  for (pv_array_size in 1:pv_array_size_max) {
    pv_array_spec <- list(
      capacity = pv_module_spec$capacity * pv_array_size, # kWp
      area = pv_module_spec$area * pv_array_size, # m^2
      size = pv_array_size,
      efficiency = pv_module_spec$efficiency # %
    )
    
    random_pv_array_output <- pv_array_spec$area * pv_array_spec$efficiency * random_roof_radiation # kW
    
    energy_balance <- calculate_lifetime_energy_balance(
      random_demand,
      random_pv_array_output,
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
      demand_index = random_demand_index,
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