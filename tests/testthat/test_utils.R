library(testthat)
library(riskscorer)

context("Checking boolean parsing")

test_that("Boolean strings return TRUE", {
  expect_true(parse_bool("y"))
  expect_true(parse_bool("Y"))
  expect_true(parse_bool("Yes"))
  expect_true(parse_bool("yEs"))
  expect_true(parse_bool("YES"))
  expect_true(parse_bool("t"))
  expect_true(parse_bool("T"))
  expect_true(parse_bool("True"))
  expect_true(parse_bool("TRUE"))
  expect_true(parse_bool("TRUE"))
  expect_true(parse_bool("1"))
})

test_that("Boolean strings return FALSE", {
  expect_false(parse_bool("n"))
  expect_false(parse_bool("N"))
  expect_false(parse_bool("No"))
  expect_false(parse_bool("no"))
  expect_false(parse_bool("nO"))
  expect_false(parse_bool("NO"))
  expect_false(parse_bool("f"))
  expect_false(parse_bool("F"))
  expect_false(parse_bool("FALSE"))
  expect_false(parse_bool("false"))
  expect_false(parse_bool("faLSe"))
  expect_false(parse_bool("0"))
})

test_that("parse_bool_and_add works", {
  expect_equal("No", parse_bool_and_add("FaLSE"))
  expect_equal("No", parse_bool_and_add("F"))
  expect_false(parse_bool_and_add("0", return_val_false = FALSE))
  expect_true(parse_bool_and_add("T", return_val_false = FALSE, return_val_true = TRUE))
  expect_equal("Unknown", parse_bool_and_add("Unknown",
                                             additionals = "Unknown",
                                             return_val_false = FALSE,
                                             return_val_true = TRUE))
  expect_true(parse_bool_and_add("1",
                                 additionals = "Unknown",
                                 return_val_false = FALSE,
                                 return_val_true = TRUE))

  expect_false(parse_bool_and_add("0",
                                  additionals = c("Unknown", "Undisclosed"),
                                  return_val_false = FALSE,
                                  return_val_true = TRUE))

  expect_equal("Undisclosed", parse_bool_and_add("Undisclosed",
                                                 additionals = c("Unknown", "Undisclosed")))
})

context("Checking Sex parsing")
test_that("parse_sex works without arguments", {
  expect_equal("Male", parse_sex("male"))
  expect_equal("Male", parse_sex("m"))
  expect_equal("Male", parse_sex("mALE"))
  expect_equal("Male", parse_sex("M"))
  expect_equal("Female", parse_sex("Female"))
  expect_equal("Female", parse_sex("FEMalE"))
  expect_equal("Female", parse_sex("F"))
  expect_equal("Female", parse_sex("f"))
  expect_true(is.null(parse_sex(NULL)))
  expect_true(is.null(parse_sex(NA)))
})

test_that("parse_sex works with special arguments", {
  expect_equal("Male", parse_sex("male", male_numeric_code = 1, female_numeric_code = 0))
  expect_equal("Male", parse_sex("1", male_numeric_code = 1, female_numeric_code = 9))
  expect_equal("Male", parse_sex("1", male_numeric_code = 1, female_numeric_code = 0))
  expect_equal("Male", parse_sex("0", male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Female", parse_sex("1", male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Female", parse_sex("F"))
  expect_equal("Female", parse_sex("f"))
  expect_equal("Female", parse_sex("FAlse"))
  expect_equal("Male", parse_sex(TRUE))
  expect_equal("Female", parse_sex("No"))
  expect_equal("Male", parse_sex("Y"))
  expect_equal("Female", parse_sex("N"))
  expect_equal("Female", parse_sex("n"))
  expect_equal("Female", parse_sex("n", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Female", parse_sex("FALSE", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Female", parse_sex(F, male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Female", parse_sex("NO", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Male", parse_sex("TRUE", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_error(parse_sex("T", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Male", parse_sex("Y", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
  expect_equal("Male", parse_sex("Yes", male_bool_code = TRUE, male_numeric_code = 0, female_numeric_code = 1))
})
