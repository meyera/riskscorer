#' Calculates the ES II score
#'
#' Citation:
#'
#' FUnction for internal use only
#'
#' @param NYHA character vector with the following values: NULL or 'I', 'II', 'III', 'IV'
#' @param IDDM numeric 0 or 1
#' @param age numeric age
#' @param female numeric 0 or 1
#' @param ECA numeric 0 or 1
#' @param CPD numeric 0 or 1
#' @param nm_mob numeric 0 or 1
#' @param redo numeric 0 or 1
#' @param renal_dysfunc character vector with the following values: NULL or 'gt_85', '50_85', 'lt_50', 'dialysis'
#' @param active_endo numeric 0 or 1
#' @param critical numeric 0 or 1
#' @param lv_func character vector with the following values: NULL or 'gt_50', '31_50', '21_30', 'lt_20'
#' @param recent_mi numeric 0 or 1
#' @param sPAP character vector with the following values: NULL or 'lt_31', '31_55', 'gt_55'
#' @param urgency character vector with the following values: NULL or 'elective' or 'urgent', 'emergency', 'salvage'
#' @param proc_weight character vector with the following values: NULL or 'isolated-CABG', 'single-non-CABG', '2-procedures', '3-procedures'
#' @param thoracic_aorta numeric 0 or 1
#'
#' @return predicted probability of mortality
#' @export
#'
#' @examples
es_II <- function(age,
                  female,
                  NYHA = NULL,
                  CCS4 = NULL,
                  IDDM = NULL,
                  ECA = NULL,
                  CPD = NULL,
                  nm_mob = NULL,
                  redo = NULL,
                  renal_dysfunc = NULL,
                  active_endo = NULL,
                  critical = NULL,
                  lv_func = NULL,
                  recent_mi = NULL,
                  sPAP = NULL,
                  urgency = NULL,
                  proc_weight = NULL,
                  thoracic_aorta = NULL
                  ) {

  const <- -5.324537
  logOdds <- const

  age_coef <- 0.0285181
  if (age <=60) {
    logOdds <- logOdds + 1*age_coef
  } else if (age >60) {
    logOdds <- logOdds + ((age - 59) * age_coef)
  }

  if (!is.null(NYHA)) {
    if (NYHA == 'II') {
      logOdds <- logOdds + 0.1070545
    } else if (NYHA == 'III') {
      logOdds <- logOdds + 0.2958358
    } else if (NYHA == 'IV') {
      logOdds <- logOdds + 0.5597929
    }
  }

  if (!is.null(female)) {
    logOdds <- logOdds + (female * 0.2196434)
  }

  if (!is.null(renal_dysfunc)) {
    if (renal_dysfunc == 'gt_85') {
      logOdds <- logOdds + 0
    } else if (renal_dysfunc == '50_85') {
      logOdds <- logOdds + 0.303553
    } else if (renal_dysfunc == 'lt_50') {
      logOdds <- logOdds + 0.8592256
    } else if (renal_dysfunc == 'dialysis') {
      logOdds <- logOdds + 0.6421508
    }
  }

  if (!is.null(ECA)) {
    logOdds <- logOdds + (ECA * 0.5360268)
  }

  if (!is.null(nm_mob)) {
    logOdds <- logOdds + (nm_mob * 0.2407818)
  }

  if (!is.null(redo)) {
    logOdds <- logOdds + (redo * 1.118599)
  }

  if (!is.null(CPD)) {
    logOdds <- logOdds + (CPD * 0.1886564)
  }

  if (!is.null(active_endo)) {
    logOdds <- logOdds + (active_endo * 0.6194522)
  }

  if (!is.null(critical)) {
    logOdds <- logOdds + (critical * 1.086517)
  }

  if (!is.null(IDDM)) {
    logOdds <- logOdds + (IDDM * 0.3542749)
  }

  if (!is.null(CCS4)) {
    logOdds <- logOdds + (CCS4 * 0.2226147)
  }

  if (!is.null(lv_func)) {
    if (lv_func == 'gt_50') {
      logOdds <- logOdds + 0
    } else if (lv_func == '31_50') {
      logOdds <- logOdds + 0.3150652
    } else if (lv_func == '21_30') {
      logOdds <- logOdds + 0.8084096
    } else if (lv_func == 'lt_20') {
      logOdds <- logOdds + 0.9346919
    }
  }

  if (!is.null(recent_mi)) {
    logOdds <- logOdds + (recent_mi * 0.1528943)
  }

  if (!is.null(sPAP)) {
    if (sPAP == 'lt_31') {
      logOdds <- logOdds + 0
    } else if (sPAP == '31_55') {
      logOdds <- logOdds + 0.1788899
    } else if (sPAP == 'gt_55') {
      logOdds <- logOdds + 0.3491475
    }
  }

  if (!is.null(urgency)) {
    if (urgency == 'urgent') {
      logOdds <- logOdds + 0.3174673
    } else if (urgency == 'emergency') {
      logOdds <- logOdds + 0.7039121
    } else if (urgency == 'salvage') {
      logOdds <- logOdds + 1.362947
    }
  }

  if (!is.null(proc_weight)) {
    if (proc_weight == 'single-non-CABG') {
      logOdds <- logOdds + 0.0062118
    } else if (proc_weight == '2-procedures') {
      logOdds <- logOdds + 0.5521478
    } else if (proc_weight == '3-procedures') {
      logOdds <- logOdds + 0.9724533
    }
  }

  if (!is.null(thoracic_aorta)) {
    logOdds <- logOdds + (thoracic_aorta * 0.6527205)
  }

  pred_mort <- exp(logOdds) / (1 + exp(logOdds))
  return(round(pred_mort, digits = 4))
}

#' Calculates the EuroScore II
#'
#' Citation:
#'
#' Fuzzy matching
#'
#' @param age Age in years, must be within 20 and 100
#' @param gender Gender of the patient, should be "Male" or "Female", 'F' or 'M',
#'        plus in addition
#'          all boolean FALSE strings --> Male
#'          all boolean TRUE strings --> Female
#' @param proc_weight accepts the coding as defined in the EuroScore2
#'                     * "isolated-CABG"
#'                     * "single-non-CABG"
#'                     * "2-procedures"
#'                     * "3-procedures"
#'                    This is the leading argument! If it is not NULL, then the arguments
#'                    CABG and valve_surgery are ignored
#' @param CABG Binary value, whether CABG was conducted
#'             Valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#'             !! Will be ignored, if proc_weight is specified !!
#' @param valve_surgery Binary value, whether Valve surgery  was conducted
#'                      Valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#'                      !! Will be ignored, if proc_weight is specified !!
#' @param iddm Valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param ccs4 angina at rest, valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param extracardiac_arteriopathy one or more of the following
#'                                   * claudication
#'                                   * carotid occlusion  or >50% stenosis
#'                                   * amputation for arterial disease
#'                                   * previous or planned intervention on the abdominal aorta, limb arteries or carotids
#'                                  Valid values: 'TRUE', 'FALSE', '1', '0','T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param chronic_pulmonary_disease long term use of bronchodilators or steroids for lung disease.
#'                                  Valid values: 'TRUE', 'FALSE', '1', '0',
#'                                                'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param poor_mobiltiy severe impairment of mobility secondary to musculoskeletal or neurological dysfunction
#'                      Valid values: 'TRUE', 'FALSE', '1', '0', T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param redo_surgery Valid values: 'TRUE', 'FALSE', '1', '0',
#'                  'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param active_endocarditis patient still on antibiotic treatment for endocarditis at time of surgery
#'                            Valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param critical_state_preop One or more of the following:
#'                              * ventricular tachycardia or ventricular fibrillation
#'                              * aborted sudden death
#'                              * preoperative cardiac massage
#'                              * preoperative ventilation before anaesthetic room
#'                              * preoperative inotropes or IABP
#'                              * preoperative acute renal failure (anuria or oliguria <10ml/hr)
#'                             Valid values: 'TRUE', 'FALSE', '1', '0', 'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param recent_mi myocardial infarction within 90 days.
#'                  Valid values: 'TRUE', 'FALSE', '1', '0',
#'                                'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param thoracic_aorta Surgery on thoracic aorta.
#'                       Valid values: 'TRUE', 'FALSE', '1', '0',
#'                                     'T', 'F', 'Y', 'N', 'Yes', 'No'
#' @param urgency Indicate the clinical status of the patient prior to entering the operating room.
#'                Indication urgency, accepts the following values:
#'                NULL or "elective|elektiv", "urgent|dringlich",
#'                "emergent|emergency|notfall|notfallindikation",
#'                "salvage|emergent salvage"
#' @param renal_impairment_or_eGFR Three categories based on creatinine clearance.
#'                                 The 3 categories are:
#'                                  * 'dialysis' ==> on dialysis (regardless of serum creatinine level)
#'                                  * 'moderate' ==> moderately impaired renal function (50-85 ml/min)
#'                                  * 'severe' ==> severely impaired renal function (<50 ml/min) off dialysis
#'                                 In case of a numeric argument, it will be interpreted as the eGFR in ml/min
#'
#' @param lv_impairment_or_ef Four categories based on left ventricular ejection fraction.
#'                                 The 3 categories are:
#'                                  * 'normal' ==> EF > 50%
#'                                  * 'moderate' ==> EF 31%-50%
#'                                  * 'poor' ==> EF 21%-30%
#'                                  * 'very poor' ==> EF < 20%
#'                                 In case of a numeric argument, it will be interpreted as the LV-EF in %
#'
#' @param sPAP_or_classification Three categories based on systolic pulmonary artery pressure(sPAP) in mmHg.
#'                                 The 3 categories are:
#'                                  * 'normal' ==> sPAP < 30mmHg
#'                                  * 'moderate' ==> sPAP 30mmHg-55mmHg
#'                                  * 'severe' ==> sPAP > 55mmHg
#'                                 In case of a numeric argument, it will be interpreted as the sPAP in mmHg
#'
#' @return the calculated risk for operative mortality. Range: 0.000-1.000
#' @export
#'
#' @examples
#
# Plumber decoration
#* @get /calc_esII
#* @post /calc_esII
calc_esII <- function(age,
                      gender,
                      proc_weight= NULL,
                      CABG = TRUE, valve_surgery = FALSE, # if proc_weight is defined, those will be ignored
                      nyha = NULL,
                      ccs4 = NULL,
                      iddm = NULL,
                      extracardiac_arteriopathy = NULL,
                      chronic_pulmonary_disease = NULL,
                      poor_mobiltiy = NULL,
                      redo_surgery = NULL,
                      active_endocarditis = NULL,
                      critical_state_preop = NULL,
                      recent_mi = NULL,
                      thoracic_aorta = NULL,
                      urgency = NULL,
                      renal_impairment_or_eGFR = NULL,
                      sPAP_or_classification = NULL,
                      lv_impairment_or_ef = NULL
                      ) {
  queryList <- list()

  age <- readr::parse_number(age)
  queryList$age <- ensurer::ensure(age, is.numeric(.), . >= 1 , . <= 110)
  queryList$female <- as.numeric(parse_sex(gender, male_numeric_code = 0,
                                           female_numeric_code = 1,
                                           male_bool_code = FALSE) == "Female")
  if (is.null(proc_weight)) {
    cabg_p <- parse_bool(CABG)
    valve_p <- parse_bool(valve_surgery)

    if (cabg_p && valve_p) {
      queryList$proc_weight <- "2-procedures"
    } else if (!cabg_p && valve_p) {
      queryList$proc_weight <- "single-non-CABG"
    } else if (cabg_p && !valve_p) {
      queryList$proc_weight <- "isolated-CABG"
    } else {
      queryList$proc_weight <- "isolated-CABG"
    }
  } else {
    proc_weight <- stringr::str_to_lower(stringr::str_trim(proc_weight))
    if (!is.na(stringr::str_match(proc_weight, "isolated[- ]?cabg")[1,1])) {
      queryList$proc_weight <- "isolated-CABG"
    } else if (!is.na(stringr::str_match(proc_weight, "single[- ]?non[- ]?cabg")[1,1])) {
      queryList$proc_weight <- "single-non-CABG"
    } else if (!is.na(stringr::str_match(proc_weight, "(two|2)[- ]?pro[cz]")[1,1])) {
      queryList$proc_weight <- "2-procedures"
    } else if (!is.na(stringr::str_match(proc_weight, "(three|3)[- ]?pro[cz]")[1,1])) {
      queryList$proc_weight <- "3-procedures"
    } else {
      stop("Coding of 'proc_weight' not recognized.")
    }
  }

  if (!is.null(nyha)) {
    queryList$NYHA <- c("I", "II", "III", "IV")[parse_nyha(nyha)]
  }

  queryList$IDDM            <- logical_to_numeric_or_null(parse_bool(iddm))
  queryList$CCS4            <- logical_to_numeric_or_null(parse_bool(ccs4))
  queryList$ECA             <- logical_to_numeric_or_null(parse_bool(extracardiac_arteriopathy))
  queryList$CPD             <- logical_to_numeric_or_null(parse_bool(chronic_pulmonary_disease))
  queryList$nm_mob          <- logical_to_numeric_or_null(parse_bool(poor_mobiltiy))
  queryList$redo            <- logical_to_numeric_or_null(parse_bool(redo_surgery))
  queryList$active_endo     <- logical_to_numeric_or_null(parse_bool(active_endocarditis))
  queryList$critical        <- logical_to_numeric_or_null(parse_bool(critical_state_preop))
  queryList$recent_mi       <- logical_to_numeric_or_null(parse_bool(recent_mi))
  queryList$thoracic_aorta  <- logical_to_numeric_or_null(parse_bool(thoracic_aorta))

  if (!is.null(urgency)) {
    urgency <- stringr::str_trim(stringr::str_to_lower(urgency))

    if (urgency %in% c("elective", "elektiv")) {
      queryList$urgency <- "elective"
    } else if (urgency %in% c("urgent", "dringlich")) {
      queryList$urgency <- "urgent"
    } else if (urgency %in% c("emergent", "emergency", "notfall", "notfallindikation")) {
      queryList$urgency <- "emergency"
    } else if (urgency %in% c("emergent salvage", "salvage")) {
      queryList$urgency <- "salvage"
    } else if (is.na(urgency)) {
      # ignore
    } else {
      stop("Coding of 'urgency' not recognized.")
    }
  }

  if (!is.null(renal_impairment_or_eGFR)) {
    if (is.character(renal_impairment_or_eGFR)) {
      # check for factor levels
      renal_impairment_or_eGFR <- stringr::str_to_lower(renal_impairment_or_eGFR)

      norm <- stringr::str_match(renal_impairment_or_eGFR, pattern = "normal")
      mild <- stringr::str_match(renal_impairment_or_eGFR, pattern = "mild|leichte?(grad(ig)?)?")
      mod <- stringr::str_match(renal_impairment_or_eGFR, pattern = "moderat|mittel(grad(ig)?)?")
      severe <- stringr::str_match(renal_impairment_or_eGFR, pattern = "severe|schwer(grad(ig)?)?")
      dialysis <- stringr::str_match(renal_impairment_or_eGFR, pattern = "end( ?stage)?|dialys(e|is)")

      if (!is.na(dialysis[1, 1])) {
        queryList$renal_dysfunc <- "dialysis"
      } else if (!is.na(severe[1, 1])) {
        queryList$renal_dysfunc <- "lt_50"
      } else if (!is.na(mod[1, 1])) {
        queryList$renal_dysfunc <- "50_85"
      } else if (!is.na(mild[1, 1])) {
        queryList$renal_dysfunc <- "50_85"
      } else if (!is.na(norm[1, 1])) {
        queryList$renal_dysfunc <- "gt_85"
      } else {
        stop("Coding of 'renal_impairment_or_eGFR' not recognized.")
      }
    } else if (is.numeric(renal_impairment_or_eGFR)) {
      # check for numeric eGFR
      stopifnot(renal_impairment_or_eGFR < 300)
      stopifnot(renal_impairment_or_eGFR > 1)

      if (renal_impairment_or_eGFR > 85) {
        queryList$renal_dysfunc <- "gt_85"
      } else if (renal_impairment_or_eGFR >= 50) {
        queryList$renal_dysfunc <- "50_85"
      } else if (renal_impairment_or_eGFR < 50) {
        queryList$renal_dysfunc <- "lt_50"
      }
    }
  }

  if (!is.null(lv_impairment_or_ef)) {
    if (is.character(lv_impairment_or_ef)) {
      # check for factor levels
      lv_impairment_or_ef <- stringr::str_to_lower(lv_impairment_or_ef)

      norm <- stringr::str_match(lv_impairment_or_ef, pattern = "normal")
      mod <- stringr::str_match(lv_impairment_or_ef, pattern = "moderat|mittel(grad(ig)?)?")
      poor <- stringr::str_match(lv_impairment_or_ef, pattern = "severe|poor|schwer(grad(ig)?)?")
      verypoor <- stringr::str_match(lv_impairment_or_ef, pattern = "very ?(severe|poor)|schwerst(grad(ig)?)?")

      if (!is.na(verypoor[1, 1])) {
        queryList$lv_func <- "lt_20"
      } else if (!is.na(poor[1, 1])) {
        queryList$lv_func <- "21_30"
      } else if (!is.na(mod[1, 1])) {
        queryList$lv_func <- "31_50"
      } else if (!is.na(norm[1, 1])) {
        queryList$lv_func <- "gt_50"
      } else {
        stop("Coding of 'lv_impairment_or_ef' not recognized.")
      }
    } else if (is.numeric(lv_impairment_or_ef)) {
      # check for numeric EF
      stopifnot(lv_impairment_or_ef < 99)
      stopifnot(lv_impairment_or_ef > 1)

      if (lv_impairment_or_ef > 50) {
        queryList$lv_func <- "gt_50"
      } else if (lv_impairment_or_ef > 30) {
        queryList$lv_func <- "31_50"
      } else if (lv_impairment_or_ef > 20) {
        queryList$lv_func <- "21_30"
      } else {
        queryList$lv_func <- "lt_20"
      }
    }
  }

  if (!is.null(sPAP_or_classification)) {
    if (is.character(sPAP_or_classification)) {
      # check for factor levels
      sPAP_or_classification <- stringr::str_to_lower(sPAP_or_classification)

      norm <- stringr::str_match(sPAP_or_classification, pattern = "normal|no|nein|false|0")
      mod <- stringr::str_match(sPAP_or_classification, pattern = "moderat|mittel(grad(ig)?)?")
      severe <- stringr::str_match(sPAP_or_classification, pattern = "severe|poor|schwer(grad(ig)?)?")

      if (!is.na(severe[1, 1])) {
        queryList$sPAP <- "gt_55"
      } else if (!is.na(mod[1, 1])) {
        queryList$sPAP <- "31_55"
      } else if (!is.na(norm[1, 1])) {
        queryList$sPAP <- "lt_31"
      } else {
        stop("Coding of 'sPAP_or_classification' not recognized.")
      }
    } else if (is.numeric(sPAP_or_classification)) {
      # check for numeric EF
      stopifnot(sPAP_or_classification < 120)
      stopifnot(sPAP_or_classification > 1)

      if (sPAP_or_classification > 55) {
        queryList$sPAP <- "gt_55"
      } else if (sPAP_or_classification > 30) {
        queryList$sPAP <- "31_55"
      } else {
        queryList$sPAP <- "lt_31"
      }
    }
  }

  #print(queryList)
  do.call(es_II, queryList)
}


