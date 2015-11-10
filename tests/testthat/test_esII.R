library(testthat)
library(riskscorer)

context("ES II calculation")

test_that("ESII gets calculated correctly according to the published model", {
  esII_test1 <- calc_esII(NYHA = "II",
                          CCS4 = 1,
                          IDDM = 1,
                          age = 66,
                          female = 0,
                          ECA = 1,
                          CPD = 0,
                          nm_mob = 0,
                          redo = 0,
                          renal_dysfunc = NULL,
                          active_endo = 0,
                          critical = 0,
                          lv_func = NULL,
                          recent_mi = 1,
                          sPAP = NULL,
                          urgency = "Urgent",
                          proc_weight = NULL,
                          thoracic_aorta = 0
                          )

  esII_test2 <- calc_esII(NYHA = "III",
                          CCS4 = 1,
                          IDDM = 0,
                          age = 78,
                          female = 1,
                          ECA = 0,
                          CPD = 1,
                          nm_mob = 0,
                          redo = 0,
                          renal_dysfunc = "50_85",
                          active_endo = 0,
                          critical = 0,
                          lv_func = NULL,
                          recent_mi = 0,
                          sPAP = NULL,
                          urgency = NULL,
                          proc_weight = "1-non-CABG",
                          thoracic_aorta = 0
                          )

  esII_test3 <- calc_esII(NYHA = "I",
                          CCS4 = 0,
                          IDDM = 0,
                          age = 55,
                          female = 0,
                          ECA = 0,
                          CPD = 0,
                          nm_mob = 0,
                          redo = 1,
                          renal_dysfunc = NULL,
                          active_endo = 1,
                          critical = 1,
                          lv_func = "31_50",
                          recent_mi = 0,
                          sPAP = "lt_31",
                          urgency = "Urgent",
                          proc_weight = "1-non-CABG",
                          thoracic_aorta = 0
                          )

  esII_test4 <- calc_esII(NYHA = NULL,
                          CCS4 = 0,
                          IDDM = 0,
                          age = 45,
                          female = 0,
                          ECA = 0,
                          CPD = 0,
                          nm_mob = 0,
                          redo = 0,
                          renal_dysfunc = NULL,
                          active_endo = 0,
                          critical = 1,
                          lv_func = "gt_50",
                          recent_mi = 0,
                          sPAP = NULL,
                          urgency = "Salvage",
                          proc_weight = "1-non-CABG",
                          thoracic_aorta = 1
                          )

   # NSTEMI Patient
  expect_true(is.numeric(esII_test1))
  expect_equal(esII_test1, 0.0312)

  # older AKE Patient
  expect_true(is.numeric(esII_test2))
  expect_equal(esII_test2, 0.0280)

  # Prothesis-Endocarditis with LVEF-Dysfunction due to AR
  expect_true(is.numeric(esII_test3))
  expect_equal(esII_test3, 0.1379)

  # Acute Type-A patient, intubated with catecholamines, CPR before anesthesia
  # induction
  expect_true(is.numeric(esII_test4))
  expect_equal(esII_test4, 0.1009)
})
