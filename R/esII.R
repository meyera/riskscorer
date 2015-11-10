#' Calculates the ES II score
#'
#' Citation:
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
#' @param sPAP character vector with the following values: NULL or 'no' or 'lt_31', '31_55', 'gt_55'
#' @param urgency character vector with the following values: NULL or 'elective' or 'urgent', 'emergency', 'salvage'
#' @param proc_weight character vector with the following values: NULL or 'isolated-CABG', 'single-non-CABG', '2-procedures', '3-procedures'
#' @param thoracic_aorta numeric 0 or 1
#'
#' @return predicted probability of mortality
#' @export
#'
#' @examples
es_II <- function(NYHA,
                  CCS4,
                  IDDM,
                  age,
                  female,
                  ECA,
                  CPD,
                  nm_mob,
                  redo,
                  renal_dysfunc,
                  active_endo,
                  critical,
                  lv_func,
                  recent_mi,
                  sPAP,
                  urgency,
                  proc_weight,
                  thoracic_aorta
                  ) {

  const = 1


}


calc_esII <- function() {

}
