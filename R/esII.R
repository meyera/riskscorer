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
es_II <- function(NYHA = NULL,
                  CCS4 = NULL,
                  IDDM = NULL,
                  age = NULL,
                  female = NULL,
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

calc_esII <- function(age,
                      female = NULL,
                      renal_impairment = NULL,
                      crea = NULL,
                      extracardiac_arteriopathy = NULL,
                      poor_mobiltiy = NULL
                      ) {

}


