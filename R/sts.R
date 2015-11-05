
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
#'
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
                     hispanic_ethnicity = NULL
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


  #queryList <- as.list(match.call())[-1]
  #queryList <- purrr::compact(queryList)
  res <- httr::POST("http://riskcalc.sts.org/stswebriskcalc/v1/calculate/stsall",
                    body = queryList,
                    encode = "json",
                    httr::verbose())

  httr::http_status(res)
  res$status_code
  httr::stop_for_status(res)

  httr::content(res)
}
