context("calculate_yearly_energy_balance")

test_that("System with no PV system imports all the energy", {
  balance <- calculate_yearly_energy_balance(
    energy_demand_profile = energy_demand_profiles[, 1],
    pv_array_energy_output = rep(0, nrow(energy_demand_profiles)),
    inverter_spec = list(
      efficiency = 0),
    battery_spec = list(
      capacity  = 0,
      power_nominal = 0,
      efficiency = 0))
  
  expect_equal(sum(balance$pv_array_energy_output), 0)
  expect_equal(sum(balance$inverter_loss), 0)
  expect_equal(sum(balance$battery_roundtrip_loss), 0)
  expect_equal(sum(balance$battery_energy), 0)
  
  expect_equal(balance$energy_imported, balance$energy_imported)
})

test_that("Energy balance is preserved", {
  balance <- calculate_yearly_energy_balance(
    energy_demand_profile = energy_demand_profiles[, 1],
    pv_array_energy_output =  0.5 * 1.83 * solar_radiations[["157.5"]][["30"]],
    inverter_spec = pv_inverters[pv_inverters$model == "SE3000 (208V) w/ -ER-US or A-US", ],
    battery_spec = powerwall_spec)
  
  max_discrepancy <- max(with(balance,
    + pv_array_energy_output
    - energy_demand_profile
    - inverter_loss
    - battery_roundtrip_loss
    + energy_imported
    - energy_exported
    - (battery_energy_next - battery_energy)))
  
  expect_less_than(max_discrepancy, 1e-10)
})

context("calculate_lifetime_energy_balance")

test_that("Lifetime balance rowsfor each year and energy_imported is increasing", {
  lifetime_length <- 2
  balance <- calculate_lifetime_energy_balance(
    energy_demand_profile = energy_demand_profiles[, 1],
    pv_array_energy_output = 0.5 * 1.83 * solar_radiations[["157.5"]][["30"]],
    inverter_spec = pv_inverters[pv_inverters$model == "SE3000 (208V) w/ -ER-US or A-US", ],
    battery_spec = powerwall_spec,
    pv_module_spec = pv_module_spec,
    lifetime_length = lifetime_length)
  
  # Check the number of rows in the balance data frame.
  expect_equal(nrow(balance), lifetime_length * nrow(energy_demand_profiles))
  
  # Check if energy_imported is decreasing.
  df <- balance %>%
    group_by(year) %>%
    summarize(sum(energy_imported)) %>%
    arrange(year)
  energy_imported_1 <- df[[2]][1]
  energy_imported_2 <- df[[2]][2]
  expect_less_than(energy_imported_1, energy_imported_2)
})