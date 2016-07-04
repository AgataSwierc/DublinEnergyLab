#' Load power profiles ([kW]).
power_demand_profiles <- dir("Data", pattern = "data.\\.csv", full.names = TRUE) %>%
  lapply(function(path) read.table(path, header = FALSE, sep = ";")) %>%
  do.call(what = cbind) # kW
power_demand_profiles_period <- 0.5 # h
  
#' Convert power profiles to energy profiles.
energy_demand_profiles <- power_demand_profiles * power_demand_profiles_period # kWh 

#' Save energy profiles data frame for faster loading.
save(energy_demand_profiles, file = "Cache/energy_demand_profiles.RData")


solar_radiations <- list(
  "90"    = read_radiation("E"),
  "112.5" = read_radiation("SEE"),
  "135"   = read_radiation("SE"),
  "157.5" = read_radiation("SSE"),
  "180"   = read_radiation("S"),
  "202.5" = read_radiation("SSW"),
  "225"   = read_radiation("SW"),
  "247.5" = read_radiation("SWW"),
  "270"   = read_radiation("W"))
save(solar_radiations, file = "Data/solar_radiations.RData")