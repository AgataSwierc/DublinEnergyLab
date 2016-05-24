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


# Donload the data about PV inverters from California Energy Commission website GoSolar
tables <- readHTMLTable("http://www.gosolarcalifornia.ca.gov/equipment/inverters.php")
pv_inverters <- tables[[1]]
names(pv_inverters) <- c("manufacturer", "model", "description", "power_rating", "efficiency", "approved_builtin_meter", "notes")
pv_inverters$power_rating <- as.numeric(as.character(pv_inverters$power_rating))
pv_inverters$efficiency <- as.numeric(as.character(pv_inverters$efficiency)) / 100
write.csv2(pv_inverters, file = "pv_inverters.csv", row.names = FALSE)






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














summary(solar_radiations[["180"]])






pv_array_size <- 8
pv_array <- list(
  capacity = pv_module_spec$capacity * pv_array_size, # kWp
  area = pv_module_spec$area * pv_array_size, # m^2
  size = pv_array_size,
  efficiency = pv_module_spec$efficiency # %
)

pv_array_spec <- pv_array






ggplot(results, aes(x = pv_array_size, y=npv, col=as.factor(demand_index))) + geom_line()

