webservice_url <- "http://riskcalc.sts.org/stswebriskcalc/v1/calculate/stsall"

do_sts_request <- function(queryList, verbose = FALSE) {

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

#' Using the webservice from STS to calculate the Scores
#'
#' Service can also be used as a JSON webservice using plumber. Arguments that are
#' not given are automatically null, thus missing for the STS web service
#'
#' @param proc_cabg Was CABG performed? Valid values: 'TRUE', 'FALSE', '1', '0',
#'                  'T', 'F', 'Y', 'N', 'Yes', 'No'
#'
#' @param proc_valve Specifies the type of valve procedure performed: 'AVR',
#'                   'MVReplacement', 'MVRepair'
#'
#' @param age Age in years, must be within 20 and 100
#'
#' @param gender Gender of the patient, should be "Male" or "Female", 'F' or 'M',
#'        plus in addition
#'          all boolean FALSE strings --> Male
#'          all boolean TRUE strings --> Female
#'
#' @param height_cm Patient's height in cm
#'
#' @param weight_kg Patient's height in kg
#'
#' @param lvef Patient's left ventricular ejection fraction (1-99%) measured before
#'             induction of anesthesia
#'
#' @param chf_2w Indicate if there is physician documentation or report that the
#'               patient has been in a state of heart failure within the past 2 weeks.
#'               Valid values: "Yes", "No", "Unknown"; TRUE or FALSE
#'
#' @param race Following values are allowed: 'Undocumented', 'undisclosed',
#'             'black', 'asian', 'asianblack'
#'
#' @param hispanic_ethnicity Following values are allowed: 'No', 'Yes',
#'             "Not Documented"
#'
#' @param dialysis Indicate whether the patient is currently (prior to surgery)
#'                 undergoing dialysis: boolean string or "Unknown"
#'
#' @param crea Indicate the creatinine level closest to the date and time prior
#'             surgery but prior to anesthetic management (induction area or operating room)
#'
#' @param acs_type Type of Acute Coronary Syndrom at admission:
#'                 'No Symptoms', 'Other', 'Angina equivalent', 'Stable Angina',
#'                 'Unstable Angina', 'STEMI', 'NSTEMI'
#'                 This field is called in the STS web app "Cardiac Symptoms - At Time Of Surgery"
#'                 I do not think this is appropiate - it mixes symptoms with disease entities
#'
#' @param prior_mi Indicate if the patient has had at least one documented previous
#'                 myocardial infarction at any time prior to this surgery.
#'                 Values: '6h', '6-24h', '1-7d', '8-21d', 'longer', 'No', 'Unknown'
#'
#' @param arrhythmia Indicate whether the patient has a history of a cardiac rhythm
#'                   disturbance before the start of the operative procedure:
#'                   Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param afib Afib before surgery? "None", "Paroxysmal", "Persistent"
#'
#' @param chrlungd Indicate whether the patient has chronic lung disease,
#'                 and the severity level according to the following classification:
#'                 No; Mild: FEV1 60% to 75% of predicted, and/or on chronic inhaled
#'                 or oral bronchodilator therapy. Moderate: FEV1 50% to 59% of predicted,
#'                 and/or on chronic steroid therapy aimed at lung disease.
#'                 Severe: FEV1 < 60 or Room Air pCO2 > 50.
#'                 For more details see the STS definition.
#'                 Values: 'no', 'unknown', 'yes-unknown', 'yes-mild', 'yes-moderate', 'yes-severe'
#'
#' @param cvd Cerebrovascular Disease; indicate whether the patient has a current
#'            or previous history of any of the following:
#'              a) Stroke - brain, spinal cord, or retinal vascular injury as a
#'                 result of hemorrhage or infarction, where the neurological
#'                 dysfunction lasts for greater than 24 hours
#'              b) TIA - brain, spinal cord, or retinal vascular injury as a
#'                 result of hemorrhage or infarction, where the neurological
#'                 dysfunction resolves within 24 hours
#'              c) Noninvasive or invasive arterial imaging test
#'                 demonstrating >=50% stenosis of any of the major
#'                 extracranial or intracranial vessels to the brain
#'              d)  Previous cervical or cerebral artery revascularization
#'                  surgery or percutaneous intervention This does not include
#'                  chronic (nonvascular) neurological diseases or other acute
#'                  neurological insults such as metabolic and anoxic ischemic
#'                  encephalopathy
#'              Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param stroke Explicit item for stroke (brain, spinal cord, or retinal vascular injury as a
#'                 result of hemorrhage or infarction, where the neurological
#'                 dysfunction lasts for greater than 24 hours)
#'                 Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param pvd Peripheral Arterial Disease (history of peripheral arterial disease,
#'            includes upper and lower extremity, renal, mesenteric, and
#'            abdominal aortic systems)
#'              1. Claudication , either with exertion or at rest
#'              2. Amputation for arterial vascular insufficiency
#'              3. Vascular reconstruction, bypass surgery, or percutaneous intervention
#'                 to the extremities (excluding dialysis fistulas and vein stripping)
#'              4. Documented abdominal aortic aneurysm with or without repair
#'              5. Positive noninvasive test (e.g., ankle brachial index =< 0.9,
#'                 ultrasound, magnetic resonance or computed tomography
#'                 imaging of > 50% diameter stenosis in any peripheral artery,
#'                 i.e., renal, subclavian, femoral, iliac) or angiographic imaging
#'                 Peripheral arterial disease excludes disease in the carotid,
#'                 cerebrovascular arteries or thoracic aorta
#'            PVD does not include DVT
#'            Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param diabetes History of diabetes diagnosed and/or treated by a healthcare
#'                 provider. See Def. STS website or 2013 ACCF/AHA Data Standards
#'                 Cannon et al. JACC Vol. 61, No. 9, 2013
#'                 Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param diabetes_ctrl Indicate the patient’s diabetes control method as presented
#'                      on admission
#'                      Values: "diet", "oral", "insulin", "other", "other_subcutaneous",
#'                              "none", "unknown"
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
                     prior_mi = NULL,
                     arrhythmia = NULL,
                     afib = NULL,
                     chrlungd = NULL,
                     cvd = NULL,
                     stroke = NULL,
                     pvd = NULL,
                     diabetes = NULL,
                     diabetes_ctrl = NULL
                     ) {

  queryList <- list()

  queryList$age <- ensurer::ensure(age, is.numeric(.), . > 20 , . < 100)
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

  if (!is.null(arrhythmia)) {
    queryList$arrhythmia <- parse_bool_and_add(stringr::str_to_title(arrhythmia),
                                             additionals = "Unknown")
  }

  if (!is.null(afib)) {
    afib <- stringr::str_to_upper(afib)

    if (afib %in% names("NONE","NO", "FALSE", "F", "0")) {
      queryList$arrhythafib <- "None"
    } else if (afib %in% c("PAROXYSMAL", "PAROX")) {
      queryList$arrhythafib <- "Paroxysmal"
    } else if (afib %in% c("CONT", "CONTINOUS", "PERSISTENT", "PERSI")) {
      queryList$arrhythafib <- "Continuous / persistent"
    } else {
      stop("Coding if of 'afib' not recognized.")
    }
  }

  # 'no', 'unknown', 'yes-unknown', 'yes-mild', 'yes-moderate', 'yes-severe'
  if (!is.null(chrlungd)) {
    chrlungd <- stringr::str_to_upper(chrlungd)

    choices <- c("unknown" = "Unknown",
                 "yes-unknown" = "Lung disease documented, severity unknown",
                 "yes-mild" = "Mild",
                 "yes-moderate" = "Moderate",
                 "yes-severe" = "Severe")
    names(choices) <- stringr::str_to_upper(names(choices))

    if (chrlungd %in% c("NONE","NO", "N", "FALSE", "F", "0")) {
      queryList$chrlungd <- "No"
    } else if (chrlungd %in% names(choices)) {
      queryList$chrlungd <- choices[chrlungd]
    } else {
      stop("Coding of 'chrlungd' not recognized.")
    }
  }

  if (!is.null(cvd)) {
    queryList$cvd <- parse_bool_and_add(stringr::str_to_title(cvd),
                                               additionals = "Unknown")
  }

  if (!is.null(stroke)) {
    queryList$cva <- parse_bool_and_add(stringr::str_to_title(stroke),
                                        additionals = "Unknown")

    if (queryList$cva == "Yes") {
      queryList$cvd = "Yes"
    }
  }

  if (!is.null(pvd)) {
    queryList$pvd <- parse_bool_and_add(stringr::str_to_title(pvd),
                                        additionals = "Unknown")
  }

  if (!is.null(diabetes)) {
    queryList$diabetes <- parse_bool_and_add(stringr::str_to_title(diabetes),
                                        additionals = "Unknown")
  }

  if (!is.null(diabetes_ctrl)) {
    #diet", "oral", "insulin", "other", "other_subcutaneous", "none", "unknown"

    queryList$diabctrl <- xxx

    chrlungd <- stringr::str_to_upper(chrlungd)

    choices <- c("unknown" = "Unknown",
                 "yes-unknown" = "Lung disease documented, severity unknown",
                 "yes-mild" = "Mild",
                 "yes-moderate" = "Moderate",
                 "yes-severe" = "Severe")
    names(choices) <- stringr::str_to_upper(names(choices))

    if (chrlungd %in% c("NONE","NO", "N", "FALSE", "F", "0")) {
      queryList$chrlungd <- "No"
    } else if (chrlungd %in% names(choices)) {
      queryList$chrlungd <- choices[chrlungd]
    } else {
      stop("Coding of 'chrlungd' not recognized.")
    }
  }

  #queryList <- as.list(match.call())[-1]
  #queryList <- purrr::compact(queryList)
  riskdf <- dplyr::as_data_frame(do_sts_request(queryList, verbose = TRUE))

  dplyr::rename(riskdf,
                procedure = proceduretype,
                mort = predmort,
                morb_mort = predmm,
                dsw_infect = preddeep,
                stroke = predstro,
                long_vent = predvent,
                re_op = predreop,
                renal_failure = predrenf,
                short_los = pred6d,
                long_los = pred14d)
}


