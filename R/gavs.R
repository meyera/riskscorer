# German German Aortic Valve Score
# Kötting, J., Schiller, W., ..., Welz, A. (2013).
# German Aortic Valve Score: a new scoring system for prediction of mortality
# related to aortic valve procedures in adults.
# European Journal of Cardio-Thoracic Surgery 43(5), 971–7.
# http://doi.org/10.1093/ejcts/ezt114

gavs <- function(age_grp = NULL,
                 female = NULL,
                 bmi_grp = NULL,
                 nyha4 = NULL,
                 mi_within_3w = NULL,
                 critical = NULL,
                 pht = NULL,
                 no_sinus_rhythm = NULL,
                 lvef_grp = NULL,
                 endocarditis = NULL,
                 redo = NULL,
                 aterial_vessel_diseae = NULL,
                 copd = NULL,
                 preop_dialysis_or_rf = NULL,
                 emergency = NULL
                 ) {
  const <- -5.504
  logOdds <- const

  pred_mort <- exp(logOdds) / (1 + exp(logOdds))
  return(round(pred_mort, digits = 4))
}


calc_gavs <- function(age = NULL,
                      sex = NULL,
                      weight_kg = NULL,
                      height_cm = NULL,
                      bmi = weight_kg/((height_cm/100)^2),
                      nyha = NULL,
                      mi_3_weeks,

                      ) {

}
