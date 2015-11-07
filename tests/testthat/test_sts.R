library(testthat)
library(riskscorer)

context("Case Checking")

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
