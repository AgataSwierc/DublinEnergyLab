source("App/Common.R")

pv_array_size = 8

installation_lifespan <- 25 # year
tariff_export <- 0.09 # EUR
tariff_import <- 0.254 # EUR
tariff_import_change <- 0.054 # change / year

discount_rate <- 0.08 # -

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
instrumentation_and_control_cost_std <- 177 # EUR/kWp
installation_electrical_cost_std <- 543 # EUR/kWp
installation_civil_cost_std <- 904 # EUR/kWp
installation_mechanical_cost_std <- 191 # EUR/kWp
maintenance_cost_std <- 109 # EUR/kWp
other_costs_std <- 88 # EUR/kWp
  
initial_cost <-
  pv_array$cost +
  battery_spec$cost +
  pv_array$capacity * (
    inverter_cost_std +
    instrumentation_and_control_cost_std +
    installation_electrical_cost_std +
    installation_civil_cost_std +
    installation_mechanical_cost_std +
    maintenance_cost_std +
    other_costs_std)
inverter_cost <- pv_array$capacity * inverter_cost_std

year_result <- run_simulation()


npv_table <- data.frame(year_index = 0:installation_lifespan)
npv_table$energy_demand <- sum(year_result$demand_profile)

npv_table$energy_exported <- sum(ifelse(year_result$energy_imported < 0, -year_result$energy_imported, 0))
npv_table$energy_imported <- sum(ifelse(year_result$energy_imported > 0,  year_result$energy_imported, 0))

npv_table$tariff_export <- tariff_export
npv_table$tariff_import <- tariff_import * (1 + tariff_import_change) ^ npv_table$year_index

npv_table$inflow_export <- npv_table$tariff_export * npv_table$energy_exported
npv_table$inflow_saving <- npv_table$tariff_import * (npv_table$energy_demand - npv_table$energy_imported)

npv_table$outflow_import <- npv_table$tariff_import * npv_table$energy_imported

# Include inverter replacement cost. Add it for years which match inverter lifespan.
npv_table$outflow_inverter <- ifelse(
  npv_table$year_index > 0 & npv_table$year_index %% inverter_spec$lifespan == 0,
  inverter_cost, 0)

npv_table$inflow <- npv_table$inflow_export + npv_table$inflow_saving
npv_table$outflow <-  npv_table$outflow_import + npv_table$outflow_inverter
npv_table$cashflow_investment <- c(initial_cost, rep(0, nrow(npv_table) - 1))

npv_table$net_cashflow <- npv_table$inflow - npv_table$outflow - npv_table$cashflow_investment

npv <- sum(npv_table$net_cashflow / (1 + discount_rate) ^ npv_table$year_index)
  
  
  
npv




