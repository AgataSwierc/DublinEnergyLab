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
  do.call(what = cbind)
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

# Pick demand profile, roof and azimuth at random
band <- ceiling(runif(100, max = 3))
demand_index <- demand_profiles_bands[[band]][ceiling(runif(1, max = length(demand_profiles_bands[[band]])))]
roof_index <- roofs_bands[[band]][ceiling(runif(1, max = length(roofs_bands[[band]])))]
azimuth <- round(runif(1, min = 90, max = 270) / 22.5) * 22.5


ggplot(roofs, aes(Area)) + geom_histogram()
ggplot(roofs, aes(Azimuth)) + geom_histogram()
ggplot(roofs, aes(Angle)) + geom_histogram()




b <- data.frame(x = a)
c <- b %>% filter(x < 5e4)
ggplot(c, aes(x)) + geom_density()
       