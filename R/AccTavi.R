# to do:
# - default values in für alle Funktionen festlegen. 
# NA or NULL? bei eigentlicher predRisk-Formel NULL sinnvoll. bei calcAcuity NA sinnvoller, da Argumente keine "optional arguments", sondern logicals
# - Testen
# - testthat error messages einheitlich. "or", "," einheitlich mit Alex
# - zulässige Argumente einheitlich festlegen mit Alex und einheitliche Beschreibung bei @param X


#' @param age numeric between 18 and 100
#' @param sex Valid values: "f", "female", "F", "Female", "w" or "m", "male", "M", "Male"
#' @param race "w", "white", "other", "Other", NA -> white race or other than black race
#'             "b", "black" -> black or African American
#' @param access Access site. Valid values: NA, "femoral", "Femoral" or "Non-femoral", "non-femoral"
#' @param crea Valid values: 0.1 - 150 (mg/dL = default unit) or 8.8 - 13260 (µmol/L Unit must be changed in param unit)
#' @param unit Default value: mg/dL, if needed, change to "SI" (µmol/L)
#' @param dialysis      Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param nyha      Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param scld      Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param procStatus    Valid values: NA or "elective", "urgent", "emergency", "salvage"
#' @param pcArrest      Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param ppInotr   Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param pcShock       Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA
#' @param ppMechan  Valid values: "yes", "y", "Yes", "Y", 1, "no", "n", "No", "N", 0, NA



# parseIfValid:
#   argument x: vector (e.g dialysis, access...)
#   argument vector1: vector of valid values for x (e.g c("yes", "y", "Yes", "Y", 1))
#   argument vector0: vector of valid values for x (e.g c("no", "n", "No", "N", 0)))
#   returns parsedValue: 1 if x in vector1, 0 if x in vector0. If x is neither in vector1 
#   nor in vector0, an error message occurs.
# e.g.:
# parseIfValid("y", c("Y", "y", 1), c("N", 0))

parseIfValid <- function(x, vector1, vector0){
  if (x %in% vector1){
    parsedValue <- 1
  } else if (x %in% vector0){
    parsedValue <- 0
  } else {
    stop ("Invalid ", substitute(x), ". Valid values are: ", toString(vector0), " or ", toString(vector1))
  }
  return(parsedValue)
}


# calcGFR: 
#   returns a list with GFR, ageValue and dialysisValue. All three are needed for risk calculation in calcRisk.
# e.g.:
# calcGFR(age = 33, "f", 150, unit = "SI")

calcGFR <- function(age, sex, crea, race = "white", unit = "US", dialysis = NA){
  boolYes <- c("yes", "y", "Yes", "Y", 1)
  boolNo <- c("no", "n", "No", "N", 0, NA)
  
  if ( age < 18 | age > 100 | !is.numeric(age)) {
    stop ('age is no numeric between 18 and 100')
  } else {ageValue <- floor(age)}
  
  sexValue <- parseIfValid(sex, c("m", "male", "M", "Male"), c("f", "female", "F", "Female", "w"))
  if (sexValue == 0){sexValue <- 0.742}
  
  raceValue <- parseIfValid(race, c("w", "white", "other", "Other", NA), c("b", "black"))
  if (raceValue == 0){raceValue <- 1.21}
  
  if (unit == "US") {
    if (crea < 0.1 | crea > 150 | !is.numeric(crea)){
      stop ('crea must be a numeric between 0.1 and 150 (mg/dL)')
    }
    creaValue <- round(crea, 1)
  } else if (unit == "SI") {
    if (crea < 8.8 | crea > 132600 | !is.numeric(crea)){
      stop ('crea must be a numeric between 8.8 and 13260 (µmol/L)')
    }
    creaValue <- crea/88.4
  } else {
    stop('Invalid unit. Valid values are "US" or "SI"')
  }
  
  dialysisValue <- parseIfValid(dialysis, boolYes, boolNo)
  
  exactGFR <- (186 * creaValue^-1.154 * ageValue^-0.203 * sexValue * raceValue)
  if (exactGFR < 30) {endGFR <- 30}
  else if (exactGFR > 90 | dialysisValue == 1) {endGFR <- 90}
  else {endGFR <- round(exactGFR, 1)}
  gfrList <- list()
  gfrList$GFR <- endGFR
  gfrList$dialysisValue <- dialysisValue
  gfrList$ageValue <- ageValue
  return(gfrList)
} 


# calcAcuity:
#   returns a list with category and catValue. catValue is needed for risk calculation in calcRisk.
# e.g.:
# calcAcuity("urgent", 0, 0, 0, NA)

calcAcuity <- function(procStatus = NA, 
                       pcArrest = NA, 
                       ppInotr = NA, 
                       pcShock = NA, 
                       ppMechan = NA){
  
  if (!(procStatus %in% c("elective", "urgent", "emergency", "salvage", NA))) {
    stop ('Invalid procedure status. Valid values are: NA or "elective", "urgent", "emergency", "salvage"')
  }
  
  boolYes <- c("yes", "y", "Yes", "Y", 1)
  boolNo <- c("no", "n", "No", "N", 0, NA)
  
  pcArrestValue <- parseIfValid(pcArrest, boolYes, boolNo)
  ppInotrValue <- parseIfValid(ppInotr, boolYes, boolNo)
  pcShockValue <- parseIfValid(pcShock, boolYes, boolNo)
  ppMechanValue <- parseIfValid(ppMechan, boolYes, boolNo)
  
  
  if (is.na(procStatus)){procStatus <- "elective"}
  if (procStatus == "elective" & pcArrestValue == 0 & ppInotrValue == 0 & pcShockValue == 0 & ppMechanValue == 0){
    category <- 1
    catValue <- 0
  } else if (procStatus == "urgent" & pcArrestValue == 0 & ppInotrValue == 0 & pcShockValue == 0 & ppMechanValue == 0){
    category <- 2
    catValue <- 0.4506953
  } else if ((procStatus == "elective" | procStatus == "urgent") & pcArrestValue == 0){
    category <- 3
    catValue <- 0.9926857
  } else if (procStatus == "emergency" | procStatus == "salvage" | pcArrestValue == 1){
    category <- 4
    catValue <- 1.2073734
  } else {stop("There is a problem in the Acuity Calculation")}
  
  categList <- list()
  categList$category <- category
  categList$catValue <- catValue
  
  return(categList)
}

# calcAcuity()

# calcRisk:
#   returns a list with GFR, category and predRisk
# e.g.:
# calcRisk(34, "f", 15, unit="SI", procStatus = "urgent", pcShock = "Y", access = "femoral" )

calcRisk <- function(age,
                     sex,
                     crea,
                     access = "femoral",
                     nyha = NA,
                     scld = NA,
                     procStatus = "elective",
                     pcArrest = NA,
                     pcShock = NA,
                     ppInotr = NA,
                     ppMechan = NA,
                     race = "white",
                     dialysis = NA,
                     unit = "US"){
  
  boolYes <- c("yes", "y", "Yes", "Y", 1)
  boolNo <- c("no", "n", "No", "N", 0, NA)
  
  nyhaValue <- parseIfValid(nyha, boolYes, boolNo)
  scldValue <- parseIfValid(scld, boolYes, boolNo)
  accessValue <- parseIfValid(access, c("non-femoral", "Non-femoral"), c("femoral", "Femoral", NA))
  
  gfrList <- calcGFR(age = age, sex = sex, crea = crea, race = race, dialysis = dialysis, unit = unit)
  
  acuityList <- calcAcuity(procStatus = procStatus, pcArrest = pcArrest, ppInotr = ppInotr, pcShock = pcShock, 
                           ppMechan = ppMechan)
  
  
  pre <- (-4.72976
          + (0.0243695 * min(max(gfrList$ageValue, 50), 100))
          + (-0.0138667 * gfrList$GFR)
          + (1.1793185 * gfrList$dialysisValue)
          + (0.2230387 * nyhaValue)
          + (0.5108357 * scldValue)
          + (0.6734738 * accessValue)
          + (acuityList$catValue));
  
  predRisk <- round(((2.718281828^pre)/(1 + 2.718281828^pre)), 4)
  # print(predRisk)
  resultList <- list()
  resultList$GFR <- gfrList$GFR 
  resultList$category <- acuityList$category
  resultList$predRisk <- predRisk

  return(resultList)
}

