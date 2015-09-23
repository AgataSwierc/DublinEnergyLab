context("run_simulation")

test_that("System with no PV and battery imports all the energy", {
  result <- run_simulation(
    demand_profile_index = 1,
    pv_array_size = 0,
    battery_spec = list(
      capacity  = 0,
      power_nominal = 0,
      efficiency = 0,
      cost = 0))
  
  expect_equal(sum(result$pv_array_output), 0)
  expect_equal(sum(result$inverter_loss), 0)
  expect_equal(sum(result$battery_roundtrip_loss), 0)
  expect_equal(sum(result$battery_energy), 0)
  
  expect_equal(result$energy_imported, result$energy_imported)
})

test_that("Energy balance is preserved", {
  result <- run_simulation()
  
  max_discrepancy <- max(with(result,
    + pv_array_output
    - demand_profile
    - inverter_loss
    - battery_roundtrip_loss
    + energy_imported
    - (battery_energy_next - battery_energy)))
  
  expect_less_than(max_discrepancy, 1e-10)
})