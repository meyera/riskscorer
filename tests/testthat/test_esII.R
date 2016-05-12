library(testthat)
library(riskscorer)

context("ES II calculation interface")

test_that("Test programming interface", {
  expect_equal(0.0141, calc_esII(89, 1))
  expect_equal(0.0141, calc_esII(89, "F"))
  expect_equal(0.0113, calc_esII(89, "Male"))
  expect_equal(0.0242, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE))
  expect_equal(0.0242, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE))
  expect_equal(0.0553, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = 45))
  expect_equal(0.0553, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = "schwerstgradige NI"))
  expect_equal(0.0553, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = "severe"))
  expect_equal(0.045, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = "Dialyse"))
  expect_equal(0.0325, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = "moderat"))
  expect_equal(0.0325, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, renal_impairment_or_eGFR = "moderat"))
  expect_equal(0.0407, calc_esII(89, 1, CABG = TRUE, valve_surgery = TRUE, extracardiac_arteriopathy = 1))
  expect_equal(0.0113, calc_esII(89, "m"))
  expect_equal(0.0126, calc_esII(89, "m", nyha = "NYHA2"))
  expect_equal(0.0197, calc_esII(89, "m", nyha = "4"))
  expect_equal(0.0197, calc_esII(89, "m", nyha = "NYHA class 4"))
  expect_equal(0.0267, calc_esII(89, "m", nyha = "NYHA class 4", lv = "moderat"))
  expect_equal(0.0375, calc_esII(89, "m", nyha = "NYHA class 4", lv = "moderat", sPAP = 70))
  expect_equal(0.0730, calc_esII(89, "m", nyha = "NYHA class 4", lv = "moderat", sPAP = 70, urgency = "Notfall"))
})

context("ES II internal calculation")

test_that("ESII piecewise - Age and NYHA", {
  expect_equal(0.0050, es_II(NYHA = "I", female = 0, age = 50))
  expect_equal(0.0050, es_II(NYHA = NULL, female = 0, age = 50))
  expect_equal(0.0055, es_II(NYHA = "II", female = 0, age = 50))
  expect_equal(0.0098, es_II(NYHA = "II", female = 0, age = 80))
  expect_equal(0.0118, es_II(NYHA = "III", female = 0, age = 80))
  expect_equal(0.0153, es_II(NYHA = "IV", female = 0, age = 80))
})

test_that("ESII piecewise - Gender", {
  expect_equal(0.0062, es_II(age = 50, female = 1))
  expect_equal(0.005, es_II(age = 50, female = 0))
  expect_equal(0.005, es_II(age = 50, female = NULL))
})

test_that("ESII piecewise - Renal impairment", {
  expect_equal(0.005, es_II(age = 50, female = 0, renal_dysfunc = NULL))
  expect_equal(0.005, es_II(age = 50, female = 0, renal_dysfunc = "gt_85"))
  expect_equal(0.0067, es_II(age = 50, female = 0, renal_dysfunc = "50_85"))
  expect_equal(0.0117, es_II(age = 50, female = 0, renal_dysfunc = "lt_50"))
  expect_equal(0.0094, es_II(age = 50, female = 0, renal_dysfunc = "dialysis"))
})

test_that("ESII piecewise - Poor mobility", {
  expect_equal(0.0063, es_II(age = 50, female = 0, nm_mob = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, nm_mob = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, nm_mob = NULL))
})

test_that("ESII piecewise - Extracardiac Arteriopathy", {
  expect_equal(0.0085, es_II(age = 50, female = 0, ECA = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, ECA = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, ECA = NULL))
})

test_that("ESII piecewise - Redo", {
  expect_equal(0.0151, es_II(age = 50, female = 0, redo = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, redo = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, redo = NULL))
})

test_that("ESII piecewise - Chronic lung disease", {
  expect_equal(0.006, es_II(age = 50, female = 0, CPD = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, CPD = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, CPD = NULL))
})

test_that("ESII piecewise - Active endocarditis", {
  expect_equal(0.0092, es_II(age = 50, female = 0, active_endo = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, active_endo = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, active_endo = NULL))
})

test_that("ESII piecewise - Critical pre-op state", {
  expect_equal(0.0146, es_II(age = 50, female = 0, critical = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, critical = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, critical = NULL))
})

test_that("ESII piecewise - IDDM", {
  expect_equal(0.0071, es_II(age = 50, female = 0, IDDM = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, IDDM = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, IDDM = NULL))
})

test_that("ESII piecewise - CCS4", {
  expect_equal(0.0062, es_II(age = 50, female = 0, CCS4 = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, CCS4 = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, CCS4 = NULL))
})

test_that("ESII piecewise - LV Function", {
  expect_equal(0.005, es_II(age = 50, female = 0, lv_func = NULL))
  expect_equal(0.005, es_II(age = 50, female = 0, lv_func = "gt_50"))
  expect_equal(0.0068, es_II(age = 50, female = 0, lv_func = "31_50"))
  expect_equal(0.0111, es_II(age = 50, female = 0, lv_func = "21_30"))
  expect_equal(0.0126, es_II(age = 50, female = 0, lv_func = "lt_20"))
})

test_that("ESII piecewise - Recent MI", {
  expect_equal(0.0058, es_II(age = 50, female = 0, recent_mi = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, recent_mi = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, recent_mi = NULL))
})

test_that("ESII piecewise - Pulmonary hypertension", {
  expect_equal(0.005, es_II(age = 50, female = 0, sPAP = NULL))
  expect_equal(0.005, es_II(age = 50, female = 0, sPAP = "lt_31"))
  expect_equal(0.0060, es_II(age = 50, female = 0, sPAP = "31_55"))
  expect_equal(0.0071, es_II(age = 50, female = 0, sPAP = "gt_55"))
})

test_that("ESII piecewise - Urgency", {
  expect_equal(0.005, es_II(age = 50, female = 0, urgency = NULL))
  expect_equal(0.005, es_II(age = 50, female = 0, urgency = "elective"))
  expect_equal(0.0068, es_II(age = 50, female = 0, urgency = "urgent"))
  expect_equal(0.0100, es_II(age = 50, female = 0, urgency = "emergency"))
  expect_equal(0.0192, es_II(age = 50, female = 0, urgency = "salvage"))
})

test_that("ESII piecewise - Procedure Weight", {
  expect_equal(0.005, es_II(age = 50, female = 0, proc_weight = NULL))
  expect_equal(0.005, es_II(age = 50, female = 0, proc_weight = "isolated-CABG"))
  expect_equal(0.005, es_II(age = 50, female = 0, proc_weight = "single-non-CABG"))
  expect_equal(0.0086, es_II(age = 50, female = 0, proc_weight = "2-procedures"))
  expect_equal(0.0131, es_II(age = 50, female = 0, proc_weight = "3-procedures"))
})

test_that("ESII piecewise - Surgery on thoracic aorta", {
  expect_equal(0.0095, es_II(age = 50, female = 0, thoracic_aorta = 1))
  expect_equal(0.005, es_II(age = 50, female = 0, thoracic_aorta = 0))
  expect_equal(0.005, es_II(age = 50, female = 0, thoracic_aorta = NULL))
})


test_that("Combinatoric works",{
  expect_equal(0.0145, es_II(age = 50, female = 1, renal_dysfunc = "lt_50"))
  expect_equal(0.0246, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1))
  expect_equal(0.0311, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1))
  expect_equal(0.1059, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1))
  expect_equal(0.1804, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1))
  expect_equal(0.3948, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1))
  expect_equal(0.5555, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III"))
  expect_equal(0.6096, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1))
  expect_equal(0.6815, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50"))
  expect_equal(0.7137, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50", recent_mi = 1))
  expect_equal(0.7488, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50", recent_mi = 1, sPAP = "31_55"))
  expect_equal(0.8577, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50", recent_mi = 1, sPAP = "31_55",
                             urgency = "emergency"))
  expect_equal(0.9128, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50", recent_mi = 1, sPAP = "31_55",
                             urgency = "emergency", proc_weight = "2-procedures"))
  expect_equal(0.9526, es_II(age = 50, female = 1, renal_dysfunc = "lt_50", ECA = 1,
                             nm_mob = 1, redo = 1, CPD = 1, active_endo = 1,
                             critical = 1, IDDM = 1, NYHA = "III", CCS4 = 1,
                             lv_func = "31_50", recent_mi = 1, sPAP = "31_55",
                             urgency = "emergency", proc_weight = "2-procedures",
                             thoracic_aorta = 1))
})



test_that("ESII gets calculated correctly according to the published model", {
  esII_test1 <- es_II(age = 75,
                      female = 1,
                      ECA = 1,
                      IDDM = 1,
                      NYHA = "II",
                      CCS4 = 1,
                      recent_mi = 1,
                      urgency = "urgent")

  esII_test2 <- es_II(age = 85,
                      female = 1,
                      renal_dysfunc = "50_85",
                      NYHA = "III",
                      proc_weight = "single-non-CABG")

  esII_test3 <- es_II(age = 60,
                      female = 0,
                      redo = 1,
                      active_endo = 1,
                      NYHA = "III",
                      lv_func = "21_30",
                      urgency = "urgent",
                      proc_weight = "single-non-CABG"
                      )

  esII_test4 <- es_II(age = 45,
                      female = 0,
                      critical = 1,
                      urgency = "salvage",
                      proc_weight = "single-non-CABG",
                      thoracic_aorta = 1
                      )

  # NSTEMI Patient
  expect_equal(esII_test1, 0.0493)

  # older AKE Patient
  expect_equal(esII_test2, 0.0228)

  # Prothesis-Endocarditis with LVEF-Dysfunction due to AR
  expect_equal(esII_test3, 0.1062)

  # Acute Type-A patient, intubated with catecholamines, CPR before anesthesia
  # induction
  expect_equal(esII_test4, 0.1009)
})
