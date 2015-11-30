library(testthat)
library(riskscorer)

context("STS Case Checking - Check against the database Version 2.81")

test_that("Case 1 gets correctly calculated (see PDF)", {
  sts_res <- calc_sts(proc_cabg = TRUE,
           proc_valve = "avr",
           gender = "male",
           age = 60,
           lvef = 35,
           weight_kg = 65,
           height_cm = 185,
           chf_2w = "yes")

  expect_true(is.list(sts_res))
  expect_equal(length(sts_res), 10)
  expect_equal(sts_res$Procedure, "AV Replacement + CAB")
  expect_equal(sts_res$Mortality, 0.01415)
  expect_equal(sts_res$Renal_failure, 0.01895)
})

test_that("Iterative test case", {
  sts_res <- calc_sts(proc_cabg = TRUE,
           proc_valve = "avr",
           gender = "f",
           age = 75,
           lvef = 50,
           chf_2w = FALSE,
           dialysis = FALSE,
           crea = 1.2)

  expect_equal(sts_res$Mortality, 0.02363)

  sts_res <- calc_sts(proc_cabg = TRUE,
           proc_valve = "avr",
           gender = "f",
           age = 75,
           lvef = 50,
           chf_2w = FALSE,
           dialysis = FALSE,
           acs_type = "NSTEMI",
           crea = 1.2)

  expect_equal(sts_res$Mortality, 0.02363)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      crea = 1.2)

  expect_equal(sts_res$Mortality, 0.02623)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      crea = 1.2)

  expect_equal(sts_res$Mortality, 0.03138)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = "yes-severe",
                      crea = 1.2)

  expect_equal(sts_res$Mortality, 0.05223)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE)

  expect_equal(sts_res$Mortality, 0.03797)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE)

  expect_equal(sts_res$Mortality, 0.04831)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral")

  expect_equal(sts_res$Mortality, 0.05366)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y")

  expect_equal(sts_res$Mortality, 0.07087)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      endocarditis_type = "active")

  expect_equal(sts_res$Mortality, 0.13469)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure"
                      )

  expect_equal(sts_res$Mortality, 0.40422)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre"
                      )

  expect_equal(sts_res$Mortality, 0.49308)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre",
                      inotropes = TRUE
                      )

  expect_equal(sts_res$Mortality, 0.49308)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre",
                      inotropes = TRUE,
                      pre_pci_interval = "<=6h"
                      )

  expect_equal(sts_res$Mortality, 0.49308)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre",
                      inotropes = TRUE,
                      pre_pci_interval = "<=6h",
                      vd_mitral_stenosis = TRUE
                      )

  expect_equal(sts_res$Mortality, 0.51777)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre",
                      inotropes = TRUE,
                      pre_pci_interval = "<=6h",
                      vd_mitral_stenosis = TRUE,
                      vd_aortic = TRUE,
                      vd_aortic_stenosis = FALSE
                      )

  expect_equal(sts_res$Mortality, 0.51777)

  sts_res <- calc_sts(proc_cabg = TRUE,
                      proc_valve = "avr",
                      gender = "f",
                      age = 75,
                      lvef = 50,
                      chf_2w = FALSE,
                      dialysis = FALSE,
                      acs_type = "Unstable Angina",
                      prior_mi = TRUE,
                      afib = "Paroxysmal",
                      chrlungd = FALSE,
                      crea = 1.2,
                      cvd = TRUE,
                      stroke = TRUE,
                      pvd = TRUE,
                      diabetes_ctrl = "oral",
                      immsupp = "Y",
                      chd = "KHK3",
                      urgency = "Notfall",
                      resuscitation = "yes-1h",
                      cardiogenic_shock = "yes-at_procedure",
                      iabp_when = "pre",
                      inotropes = TRUE,
                      pre_pci_interval = "<=6h",
                      vd_mitral_stenosis = TRUE,
                      vd_aortic = TRUE,
                      vd_aortic_stenosis = FALSE,
                      vd_mitral_regurg = "undocumented",
                      vd_tricuspid_regurg = "severe",
                      vd_aortic_regurg = "moderate",
                      no_cardiovascular_surgeries = 2
                      )

  expect_equal(sts_res$Mortality, 0.73026)
})
