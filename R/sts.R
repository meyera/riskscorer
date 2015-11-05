
#' Using the webservice from STS to calculate the Scores
#'
#' Service can also be used as a JSON webservice using plumber. Arguments that are
#' not given are automatically null, thus missing for the STS web service
#'
#' @param proc_cabg Was CABG performed? Valid values: 'TRUE', 'FALSE', '1', '0',
#'                  'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param proc_valve Specifies the type of valve procedure performed: 'AVR',
#'                   'MVReplacement', 'MVRepair'
#' @param age Age in years, must be within 20 and 100
#' @param gender Gender of the patient, should be "Male" or "Female"
#' @param height_cm Patient's height in cm
#' @param weight_kg Patient's height in kg
#' @param lvef Patient's left ventricular ejection fraction (1-99%) measured before
#'             induction of anesthesia
#' @param chf_2w Indicate if there is physician documentation or report that the
#'               patient has been in a state of heart failure within the past 2 weeks.
#'               Valid values: "Yes", "No", "Unknown"; TRUE or FALSE
#' @param race Following values are allowed: 'Undocumented', 'undisclosed',
#'             'black', 'asian', 'asianblack'
#' @param hispanic_ethnicity Following values are allowed: 'No', 'Yes',
#'             "Not Documented"
#' @param dialysis Indicate whether the patient is currently (prior to surgery)
#'                 undergoing dialysis: boolean string or "Unknown"
#' @param crea Indicate the creatinine level closest to the date and time prior
#'             surgery but prior to anesthetic management (induction area or operating room)
#' @param acs_type Type of Acute Coronary Syndrom at admission:
#'                 'No Symptoms', 'Other', 'Angina equivalent', 'Stable Angina',
#'                 'Unstable Angina', 'STEMI', 'NSTEMI'
#'                 This field is called in the STS web app "Cardiac Symptoms - At Time Of Surgery"
#'                 I do not think this is appropiate - it mixes symptoms with disease entities
#' @param prior_mi Indicate if the patient has had at least one documented previous
#'                 myocardial infarction at any time prior to this surgery.
#'                 Values: '6h', '6-24h', '1-7d', '8-21d', 'longer', 'No', 'Unknown'
#'
#' @return a list of the predicted risks of the at the time of request current
#'         STS risk model
#' @export
#'
#' @examples
#'  service <- plumber::plumb("R/sts.R")
#'  service$run(port = 8080)
#
# Plumber decoration
#* @get /calc_sts
#* @post /calc_sts
calc_sts <- function(proc_cabg = NULL,
                     proc_valve = NULL,
                     age = NULL,
                     gender = NULL,
                     height_cm = NULL,
                     weight_kg = NULL,
                     lvef = NULL,
                     chf_2w = NULL,
                     race = NULL,
                     hispanic_ethnicity = NULL,
                     dialysis = NULL,
                     crea = NULL,
                     acs_type = NULL,
                     prior_mi = NULL
                     ) {

  queryList <- list()

  queryList$age <- ensurer::ensure(age, is.numeric(.), . %in% 20:100)
  queryList$gender <- ensurer::ensure(stringr::str_to_title(gender),
                                     . %in% c("Male", "Female"))

  if (!is.null(proc_cabg)) {
    if (parse_bool(proc_cabg)) {
      queryList$opcab <- "Yes, planned"
    }
  }

  if (!is.null(proc_valve)) {
    proc_valve <- stringr::str_to_lower(proc_valve)

    if (proc_valve == "avr") {
      queryList$opvalve <- "Yes"
      queryList$vsav <- "Yes, planned"
      queryList$vsavpr <- "Replacement"
    } else if (proc_valve == "mvreplacement") {
      queryList$opvalve <- "Yes"
      queryList$vsmv <- "Yes, planned"
      queryList$vsmvpr <- "Replacement"
    } else if (proc_valve == "mvrepair") {
      queryList$opvalve <- "Yes"
      queryList$vsmv <- "Yes, planned"
      queryList$vsmvpr <- "Repair"
    } else {
      stop("'proc_valve' must be one of the following values: 'AVR',
           'MVReplacement', 'MVRepair'")
    }
  }

  if (!is.null(height_cm)) {
    queryList$heightcm <- ensurer::ensure(height_cm, is.numeric(.), . %in% 50:250)
  }

  if (!is.null(weight_kg)) {
    queryList$weightkg <- ensurer::ensure(weight_kg, is.numeric(.), . %in% 30:250)
  }

  if (!is.null(lvef)) {
    queryList$hdef <- ensurer::ensure(lvef, is.numeric(.), . %in% 1:99)
    queryList$hdefd <- "Yes"
  }

  if (!is.null(chf_2w)) {
      queryList$chf <- parse_bool_and_add(stringr::str_to_title(chf_2w),
                                          "Unknown")
  }

  if (!is.null(race)) {
    race <- ensurer::ensure(stringr::str_to_title(race),
                                     . %in% c("Undocumented", "Not Documented", "Undisclosed",
                                              "Black", "Asian", "Asianblack"))

    if (race %in% c("Undocumented", "Not Documented")) {
      queryList$racedocumented = "No"
    } else if (race == "Undisclosed") {
      queryList$racedocumented = "Patient declined to disclose"
    } else {
      queryList$racedocumented = "Yes"
      if (race %in% c("Black", "Asianblack")) {
        queryList$raceblack = "Yes"
      }

      if (race %in% c("Asian", "Asianblack")) {
        queryList$raceasian = "Yes"
      }
    }
  }

  if (!is.null(hispanic_ethnicity)) {
      queryList$ethnicity <- parse_bool_and_add(stringr::str_to_title(hispanic_ethnicity),
                                                additionals = "Not Documented")
  }

  if (!is.null(dialysis)) {
      queryList$dialysis <- parse_bool_and_add(stringr::str_to_title(dialysis),
                                                additionals = "Unknown")
  }

  if (!is.null(crea)) {
    queryList$creatlst <- ensurer::ensure(crea, is.numeric(.), . > 0.1, . < 30)
  }

  if (!is.null(crea)) {
    queryList$creatlst <- ensurer::ensure(crea, is.numeric(.), . > 0.1, . < 30)
  }

  if (!is.null(acs_type)) {
    acs_type <- stringr::str_to_upper(acs_type)
    if (acs_type == "OTHER") {
      queryList$cardsymptimeofsurg <- "Other"
    } else if (acs_type == "NO SYMPTOMS") {
      queryList$cardsymptimeofsurg <- "No Symptoms"
    } else if (acs_type == "ANGINA EQUIVALENT") {
      queryList$cardsymptimeofsurg <- "Angina equivalent"
    } else if (acs_type == "STABLE ANGINA") {
      queryList$cardsymptimeofsurg <- "Stable Angina "
    } else if (acs_type == "UNSTABLE ANGINA") {
      queryList$cardsymptimeofsurg <- "Unstable Angina"
    } else if (acs_type == "NSTEMI") {
      queryList$cardsymptimeofsurg <- "Non-ST Elevation MI (Non-STEMI)"
    } else if (acs_type == "STEMI") {
      queryList$cardsymptimeofsurg <- "ST Elevation MI (STEMI)"
    } else {
      stop("ACS type or cardiac symptoms at time of surgery are not recognized.")
    }
  }

  if (!is.null(prior_mi)) {
    choices = c("6h"      = " <=6 Hrs",
                '6-24h'   = " >6 Hrs but <24 Hrs",
                '1-7d'    = "1 to 7 Days",
                '8-21d'   = "8 to 21 Days",
                'longer'  = " >21 Days",
                'No'      = "No",
                'Unknown' = "Unknown")
    names(choices) <- stringr::str_to_upper(names(choices))

    prior_mi <- stringr::str_to_upper(prior_mi)

    if (prior_mi %in% names(choices[1:5])) {
      queryList$prevmi <- "Yes"
      queryList$miwhen <- choices[prior_mi]
    } else if (prior_mi %in% names(choices[6:7])) {
      queryList$prevmi <- choices[prior_mi]
    } else if (prior_mi %in% c("F", "FALSE", "0")) {
      queryList$prevmi <- "No"
    } else {
      stop("Coding if  of 'prior_mi' not recognized.")
    }
  }

  #queryList <- as.list(match.call())[-1]
  #queryList <- purrr::compact(queryList)
  do_sts_request(queryList, verbose = TRUE)
}

do_sts_request <- function(queryList, verbose = FALSE) {

  webservice_url <- "http://riskcalc.sts.org/stswebriskcalc/v1/calculate/stsall"

  if (verbose) {
    res <- httr::POST(webservice_url,
                      body = queryList,
                      encode = "json",
                      httr::verbose())
  } else {
    res <- httr::POST(webservice_url,
                      body = queryList,
                      encode = "json",
                      httr::verbose())
  }

  if (verbose) {
    httr::http_status(res)
    res$status_code
    httr::stop_for_status(res)
  }

  httr::content(res)
}
