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
source("App/Common.R")


# Donload the data about PV inverters from California Energy Commission website GoSolar
tables <- readHTMLTable("http://www.gosolarcalifornia.ca.gov/equipment/inverters.php")
pv_inverters <- tables[[1]]
names(pv_inverters) <- c("manufacturer", "model", "description", "power_rating", "efficiency", "approved_builtin_meter", "notes")
pv_inverters$power_rating <- as.numeric(as.character(pv_inverters$power_rating))
pv_inverters$efficiency <- as.numeric(as.character(pv_inverters$efficiency)) / 100
write.csv2(pv_inverters, file = "pv_inverters.csv", row.names = FALSE)

pv_inverters <- read.csv2("pv_inverters.csv")
inverter_spec <- as.list(pv_inverters[pv_inverters$model == "SE3000 (208V) w/ -ER-US or A-US", ])



battery_spec <- powerwall_spec

#' Load data from available files
#+ datasets, cache=TRUE
#demand_profiles <- read.table("Data/data1.csv", header = FALSE, sep = ";")
#generation_normalized <- read.table("Data/pv30minsgen.csv", header = FALSE)[[1]]
#save(demand_profiles, generation_normalized, file = "AppData.RData")
load("AppData.RData")

date <- seq(as.POSIXct("2009-01-01 00:00"), as.POSIXct("2009-12-31 23:30"), by = 30 * 60)
date_selected_start <- "2009-05-01"
date_selected_end <- "2009-05-31"




#runApp(app)
#shinyAppDir("App")
#runApp(app, launch.browser = FALSE)



simulation_result <- run_simulation()
npv_table <- create_npv_table(simulation_result, 8)
write.csv(npv_table, "npv_table.csv", row.names = FALSE)

calculate_npv(simulation_result, 8)



ggplot(roofs, aes(x = Area)) + geom_density()



demand_profiles <- dir("Data", pattern = "data.\\.csv", full.names = TRUE) %>%
  lapply(function(path) read.table(path, header = FALSE, sep = ";")) %>%
  do.call(what = cbind) # kW
save(demand_profiles, file = "Cache/demand_profiles.RData")
load("Cache/demand_profiles.RData")

demand_profiles_sums <- sapply(demand_profiles, sum)
demand_profiles_bands_levels <- as.numeric(cut(demand_profiles_sums,
  quantile(demand_profiles_sums, c(0, 0.05, 0.35, 0.65 ,0.95, 1))))
demand_profiles_bands <- list()
demand_profiles_bands[[1]] <- which(demand_profiles_bands_levels == 2)
demand_profiles_bands[[2]] <- which(demand_profiles_bands_levels == 3)
demand_profiles_bands[[3]] <- which(demand_profiles_bands_levels == 4)


roofs <- read.csv("Data/roofs.csv", sep=";")
roofs$AzimuthRounded <- round(roofs$Azimuth / (360 / 16)) * (360 / 16)
roofs$AngleRounded <- round(roofs$Angle / 5) * 5
roofs$AreaBand <- as.numeric(cut(roofs$Area, c(0, quantile(roofs$Area, c(0.33, 0.66, 1)))))

roofs_bands <- list()
roofs_bands[[1]] <- which(roofs$AreaBand == 1)
roofs_bands[[2]] <- which(roofs$AreaBand == 2)
roofs_bands[[3]] <- which(roofs$AreaBand == 3)




ggplot(roofs, aes(Area)) + geom_histogram()
ggplot(roofs, aes(Azimuth)) + geom_histogram()
ggplot(roofs, aes(Angle)) + geom_histogram()




b <- data.frame(x = a)
c <- b %>% filter(x < 5e4)
ggplot(c, aes(x)) + geom_density()
       

read_radiation <- function(direction) {
  radiation_part <- read.delim(sprintf("Data/Radiation_0.5h_Article/Radiation_Power_%s.out", direction), header=FALSE, skip = 2)
  # Ignore hour column
  radiation_part[[1]] <- NULL
  # Ignore last column
  radiation_part[[8]] <- NULL
  # Select only one year
  radiation_part <- radiation_part[1:17520, ]
  # Set the right names
  names(radiation_part) <- c("20", "25", "30", "35", "40", "45", "50")
  # Convert from [kJ/ (h * m^2)] to [kW / m^2]
  radiation_part <- radiation_part / 3600
  return(radiation_part)
}

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
  
summary(solar_radiations[["180"]])

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


pv_array_size <- 8
pv_array <- list(
  capacity = pv_module_spec$capacity * pv_array_size, # kWp
  area = pv_module_spec$area * pv_array_size, # m^2
  size = pv_array_size,
  efficiency = pv_module_spec$efficiency # %
)
 

# Pick demand, roof, azimuth and output at random
random_band <- ceiling(runif(1, max = 3))
random_demand_index <- demand_profiles_bands[[random_band]][ceiling(runif(1, max = length(demand_profiles_bands[[random_band]])))]
random_roof_index <- roofs_bands[[random_band]][ceiling(runif(1, max = length(roofs_bands[[random_band]])))]
random_azimuth <- round(runif(1, min = 90, max = 270) / 22.5) * 22.5

random_roof <- roofs[random_roof_index, ]
random_demand <- demand_profiles[, random_demand_index]
random_pv_array_output <- pv_array$area * pv_array$efficiency *
  solar_radiations[[as.character(random_azimuth)]][[as.character(random_roof$AngleRounded)]] # kW

#' Calculate energy balance for a single year.
#' @param demand_profile Energy demand profile for a single year.
#' @param pv_array_output Energy generated by the PV system for a single year (before the inverter).
#' @param inverter_spec Inverter specification.
#' @param battery_spec Battery specification.
calculate_yearly_energy_balance <- function(
  demand_profile,
  pv_array_output,
  inverter_spec,
  battery_spec) {
  
  #' Simulate charge/discharge cycles of the battery for the demand profile
  #+ simulation, cache=TRUE
  n <- length(demand_profile)
  period <- 1 / (n / 365 / 24) # h
  energy_imported <- rep(0, n) # kWh
  battery_energy <- rep(0, n) # kWh
  battery_energy_next <- rep(0, n) # kWh
  battery_diff <- rep(0, n) # kWh
  battery_roundtrip_loss <- rep(0, n) # kWh
  inverter_input <- rep(0, n) # kWh
  inverter_output <- rep(0, n) # kWh
  inverter_loss <- rep(0, n) # kWh
  energy_diff <- rep(0, n) # kWh
  
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
    
    # Energy difference between demand and generated (after inverter).
    energy_diff[i] <- ac_demand - inverter_output[i]
  }
  
  # Check the sign to tell if the ennergy difference is for import or export.
  energy_imported <- ifelse(energy_diff > 0, energy_diff, 0)
  energy_exported <- ifelse(energy_diff < 0, -energy_diff, 0)
  
  # Gather all values in a signel data frame
  return(data.frame(
    pv_array_output,
    demand_profile,
    inverter_loss,
    battery_roundtrip_loss,
    battery_energy_next,
    battery_energy,
    energy_diff,
    energy_imported,
    energy_exported,
    battery_diff,
    battery_percentage = if (battery_spec$capacity > 0) battery_energy / battery_spec$capacity * 100 else 0))
}

#' Calculate energy balance for a complete lifetime of the system.
#' @param demand_profile Energy demand profile for a single year.
#' @param pv_array_output Energy generated by the PV system for a single year (before the inverter).
#' @param inverter_spec Inverter specification.
#' @param battery_spec Battery specification.
#' @param pv_module_spec PV module specification.
#' @param lifetime_length lifetime length in years.
calculate_lifetime_energy_balance <- function(
  demand_profile,
  pv_array_output,
  inverter_spec,
  battery_spec,
  pv_module_spec,
  lifetime_length) {
  
  balance <- data.frame()
  for (year in 1:lifetime_length) {
    # Adjust pv output according to the deterioration curve from the specification. Specify rule = 2
    # in the approx function to use the lowest guaranteed value for years outside of the period
    # mentioned in the specification.
    pv_array_output_deteriorated <- pv_array_output *
      approx(pv_module_spec$deterioration_curve$year, pv_module_spec$deterioration_curve$guarantee, year, rule = 2)$y
    
    balance_yearly <- calculate_yearly_energy_balance(
      demand_profile,
      pv_array_output_deteriorated,
      inverter_spec,
      battery_spec)
    balance_yearly$year <- year
    
    balance <- rbind(balance, balance_yearly)
  }
  
  return(balance)
}


balance <- calculate_lifetime_energy_balance(
  random_demand,
  random_pv_array_output,
  inverter_spec,
  powerwall_spec,
  pv_module_spec,
  25)

balance %>%
  group_by(year) %>%
  summarize_each(funs="sum")



