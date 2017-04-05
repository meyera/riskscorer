source("Functions3.R")
library(testthat)

# toDo:
#   testthat error messages einheitlich. "or", "," einheitlich mit Alex


# Test if error messages occur in case of invalid values
validValues <- list(age = 50, sex = "f", crea = 1.5, access = "femoral", nyha = 0,
                    scld = 0, procStatus = "elective", pcArrest = 0, pcShock = 0,
                    ppInotr = 0, ppMechan = 0, race = "white", dialysis = 0,
                    unit = "US")
# e.g. do.call(calcRisk, validValues)

context("ACC Internal Calculation - Error messages")

test_that("Error message, if age is no numeric between 18 and 100", {
  a <- validValues
  a$age <- 17
  expect_error(do.call(calcRisk, a), "age is no numeric between 18 and 100")
  a$age <- 101
  expect_error(do.call(calcRisk, a), "age is no numeric between 18 and 100")
  a$age <- "19"
  expect_error(do.call(calcRisk, a), "age is no numeric between 18 and 100")
})
test_that("Error message, if sex is invalid", {
  b <- validValues
  b$sex <- "fe"
  expect_error(do.call(calcRisk, b), 'Invalid sex. Valid values are: f, female, F, Female, w or m, male, M, Male')
  b$sex <- "ma"
  expect_error(do.call(calcRisk, b), 'Invalid sex. Valid values are: f, female, F, Female, w or m, male, M, Male')
  b$sex <- 0
  expect_error(do.call(calcRisk, b), 'Invalid sex. Valid values are: f, female, F, Female, w or m, male, M, Male')
})
test_that("Error message, if access is invalid", {
  c <- validValues
  c$access <- "fem"
  expect_error(do.call(calcRisk, c), "Invalid access. Valid values are: femoral, Femoral, NA or non-femoral, Non-femoral")
  c$access <- 2
  expect_error(do.call(calcRisk, c), "Invalid access. Valid values are: femoral, Femoral, NA or non-femoral, Non-femoral")
  c$access <- "normal"
  expect_error(do.call(calcRisk, c), "Invalid access. Valid values are: femoral, Femoral, NA or non-femoral, Non-femoral")
})
test_that("Error message, if nyha is invalid", {
  e <- validValues
  e$nyha <- "a"
  expect_error(do.call(calcRisk, e), "Invalid nyha. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  e$nyha <- 2
  expect_error(do.call(calcRisk, e), "Invalid nyha. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  e$nyha <- 10
  expect_error(do.call(calcRisk, e), "Invalid nyha. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if scld is invalid", {
  f <- validValues
  f$scld <- "a"
  expect_error(do.call(calcRisk, f), "Invalid scld. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$scld <- 2
  expect_error(do.call(calcRisk, f), "Invalid scld. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$scld <- 10
  expect_error(do.call(calcRisk, f), "Invalid scld. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if pcArrest is invalid", {
  f <- validValues
  f$pcArrest <- "a"
  expect_error(do.call(calcRisk, f), "Invalid pcArrest. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$pcArrest <- 2
  expect_error(do.call(calcRisk, f), "Invalid pcArrest. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$pcArrest <- 10
  expect_error(do.call(calcRisk, f), "Invalid pcArrest. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if pcShock is invalid", {
  f <- validValues
  f$pcShock <- "a"
  expect_error(do.call(calcRisk, f), "Invalid pcShock. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$pcShock <- 2
  expect_error(do.call(calcRisk, f), "Invalid pcShock. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$pcShock <- 10
  expect_error(do.call(calcRisk, f), "Invalid pcShock. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if ppInotr is invalid", {
  f <- validValues
  f$ppInotr <- "a"
  expect_error(do.call(calcRisk, f), "Invalid ppInotr. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$ppInotr <- 2
  expect_error(do.call(calcRisk, f), "Invalid ppInotr. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$ppInotr <- 10
  expect_error(do.call(calcRisk, f), "Invalid ppInotr. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if ppMechan is invalid", {
  f <- validValues
  f$ppMechan <- "a"
  expect_error(do.call(calcRisk, f), "Invalid ppMechan. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$ppMechan <- 2
  expect_error(do.call(calcRisk, f), "Invalid ppMechan. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$ppMechan <- 10
  expect_error(do.call(calcRisk, f), "Invalid ppMechan. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if dialysis is invalid", {
  f <- validValues
  f$dialysis <- "a"
  expect_error(do.call(calcRisk, f), "Invalid dialysis. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$dialysis <- 2
  expect_error(do.call(calcRisk, f), "Invalid dialysis. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
  f$dialysis <- 10
  expect_error(do.call(calcRisk, f), "Invalid dialysis. Valid values are: no, n, No, N, 0, NA or yes, y, Yes, Y, 1" )
})
test_that("Error message, if unit is invalid", {
  f <- validValues
  f$unit <- 0
  expect_error(do.call(calcRisk, f), 'Invalid unit. Valid values are "US" or "SI"')
  f$unit <- 1
  expect_error(do.call(calcRisk, f), 'Invalid unit. Valid values are "US" or "SI"')
  f$unit <- "f"
  expect_error(do.call(calcRisk, f), 'Invalid unit. Valid values are "US" or "SI"')
})

# to be continued...


context("ACC Internal Calculation - Internal Calculation")

test_that("ACC predRisk piecewise - age", {
  expect_equal(0.0155, calcRisk(age = 50, sex = "f", crea = 1.3)$predRisk)
  expect_equal(0.0335, calcRisk(age = 80, sex = "f", crea = 1.3)$predRisk)
})

test_that("ACC predRisk piecewise - gender", {
  expect_equal(0.0155, calcRisk(age = 50, sex = "f", crea = 1.3)$predRisk)
  expect_equal(0.0125, calcRisk(age = 50, sex = "m", crea = 1.3)$predRisk)
})

test_that("ACC predRisk piecewise - crea", {
  expect_equal(0.0097, calcRisk(age = 50, sex = "f", crea = 0.8)$predRisk)
  expect_equal(0.0155, calcRisk(age = 50, sex = "f", crea = 1.3)$predRisk)
  expect_equal(0.0193, calcRisk(age = 50, sex = "f", crea = 2.6)$predRisk)
  expect_equal(0.0193, calcRisk(age = 50, sex = "f", crea = 3.0)$predRisk)
})

test_that("ACC predRisk piecewise - access", {
  expect_equal(0.0155, calcRisk(age = 50, sex = "f", crea = 1.3, access = "femoral")$predRisk)
  expect_equal(0.0300, calcRisk(age = 50, sex = "f", crea = 1.3, access = "non-femoral")$predRisk)
  expect_equal(0.0155, calcRisk(age = 50, sex = "f", crea = 1.3, access = NA)$predRisk)
  
})

# to be continued...









# Create dataframe with n combinations of possible valid values for Risk Calculations, to be prooved manually on the ACC website
# Problem: zu viele Test mit großen Crea Werten, theoretisch erlaubt, aber nicht sinnvoll, daher numTestCases1(0.1 < Crea < 2.5) und numTestCases2(0.1 < Crea < 150)
# ähnliches Problem bei Dialyse, daher Wahrscheinlichkeiten für 0 und 1 verschieden
numTestCases1 <- 50
numTestCases2 <- 10
combiRisk1 <- data.frame(TestID = seq(1, numTestCases1))
combiRisk2 <- data.frame(TestID = seq((numTestCases1 + 1) , (numTestCases1 + numTestCases2)))
                        
for (i in seq(numTestCases1)){
  combiRisk1$Age[i] <- round(runif(1, min = 18, max = 100), 0)
  combiRisk1$Sex[i] <- sample(c("f", "m"), 1)
  combiRisk1$Unit[i] <- sample(c("US", "SI"), 1)
  
  if (combiRisk1$Unit[i] == "US"){
    combiRisk1$Crea[i] <- round(runif(1, min = 0.1, max = 2.4), 1)
  } else {
    combiRisk1$Crea[i] <- round(runif(1, min = 8.8, max = 221), 1)
  }
  
  combiRisk1$Dialysis[i] <- sample(c(0, 1), 1, prob = c(0.85, 0.15))
  combiRisk1$Access[i] <- sample(c("femoral", "non-femoral"), 1)
  combiRisk1$Nyha[i] <- sample(c(0, 1), 1)
  combiRisk1$Scld[i] <- sample(c(0, 1), 1)
  combiRisk1$ProcStatus[i] <- sample(c("elective", "urgent", "emergency", "salvage"), 1)
  combiRisk1$PcArrest[i] <- sample(c(0, 1), 1)
  combiRisk1$PcShock[i] <- sample(c(0, 1), 1)
  combiRisk1$PpInotr[i] <- sample(c(0, 1), 1)
  combiRisk1$PpMechan[i] <- sample(c(0, 1), 1)
  combiRisk1$Race[i] <- sample(c("white", "black"), 1)
}
for (i in seq(numTestCases2)){
  combiRisk2$Age[i] <- round(runif(1, min = 18, max = 100), 0)
  combiRisk2$Sex[i] <- sample(c("f", "m"), 1)
  combiRisk2$Unit[i] <- sample(c("US", "SI"), 1)
  
  if (combiRisk2$Unit[i] == "US"){
    combiRisk2$Crea[i] <- round(runif(1, min = 0.1, max = 150), 1)
  } else {
    combiRisk2$Crea[i] <- round(runif(1, min = 8.8, max = 13260), 1)
  }
    
  combiRisk2$Dialysis[i] <- sample(c(0, 1), 1, prob = c(0.85, 0.15))
  combiRisk2$Access[i] <- sample(c("femoral", "non-femoral"), 1)
  combiRisk2$Nyha[i] <- sample(c(0, 1), 1)
  combiRisk2$Scld[i] <- sample(c(0, 1), 1)
  combiRisk2$ProcStatus[i] <- sample(c("elective", "urgent", "emergency", "salvage"), 1)
  combiRisk2$PcArrest[i] <- sample(c(0, 1), 1)
  combiRisk2$PcShock[i] <- sample(c(0, 1), 1)
  combiRisk2$PpInotr[i] <- sample(c(0, 1), 1)
  combiRisk2$PpMechan[i] <- sample(c(0, 1), 1)
  combiRisk2$Race[i] <- sample(c("white", "black"), 1, prob = c(0.85, 0.15))
}

combiRisk <- rbind(combiRisk1, combiRisk2)
# View(combiRisk)

testResultsDF <- combiRisk

for (j in seq(nrow(testResultsDF))){
  testResultsList <- calcRisk(age = testResultsDF$Age[j],
                              sex = testResultsDF$Sex[j],
                              crea = testResultsDF$Crea[j],
                              access = testResultsDF$Access[j],
                              nyha = testResultsDF$Nyha[j],
                              scld = testResultsDF$Scld[j],
                              procStatus = testResultsDF$ProcStatus[j],
                              pcArrest = testResultsDF$PcArrest[j],
                              pcShock = testResultsDF$PcShock[j],
                              ppInotr = testResultsDF$PpInotr[j],
                              ppMechan = testResultsDF$PpMechan[j],
                              race = testResultsDF$Race[j],
                              dialysis = testResultsDF$Dialysis[j],
                              unit = testResultsDF$Unit[j])

  
  testResultsDF$GFR[j] <- testResultsList$GFR
  testResultsDF$category[j] <- testResultsList$category
  testResultsDF$predRisk[j] <- testResultsList$predRisk
}
# View(testResultsDF)


# Create dataframe with all possible combinations of Acuity Calculations, to be prooved manually on the ACC website
combinAcuity <- expand.grid(ProcStatus = c("elective", "urgent", "emergency", "salvage"), 
                            PcArrest = c(0,1), PpInotr = c(0,1),
                            PcShock = c(0,1), PpMechan = c(0,1))

for (i in seq(nrow(combinAcuity))){
  acuList <- calcAcuity(procStatus = combinAcuity$ProcStatus[i], 
                        pcArrest = combinAcuity$PcArrest[i],
                        ppInotr = combinAcuity$PpInotr[i],
                        pcShock= combinAcuity$PcShock[i],
                        ppMechan = combinAcuity$PpMechan[i])
  
  combinAcuity$category[i] <- acuList$category
}
# View(combinAcuity)





# Testung1 <- testResultsDF
# 
# View(Testung1)

# AcuityDF <- data.frame(ProcStatus = factor(levels = c("Elective", "Urgent", "Emergency", "Salvage")), 
#                       PcArrest = factor(levels = c(0,1)), PpInotr = factor(levels = c(0,1)),
#                       PcShock = factor(levels = c(0,1)), PpMechan = factor(levels = c(0,1)),
#                       Categ1 = factor(levels = c(0,1)), Categ2 = factor(levels = c(0,1)),
#                       Categ3 = factor(levels = c(0,1)), Categ4 = factor(levels = c(0,1)))
