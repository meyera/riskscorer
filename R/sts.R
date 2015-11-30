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
                      encode = "json")
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
#' @param htn Hypertension, see STS definition
#'            Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param immsupp Immunosuppression: Indicate whether immunocompromise is present
#'                due to immunosuppressive medication therapy within 30 days
#'                preceding the operative procedure or existing medical condition
#'                (see training manual). This includes, but is not limited to
#'                systemic steroid therapy, anti-rejection medications and chemotherapy.
#'                This does not include topical steroid applications, one time systemic
#'                therapy, inhaled steroid therapy or preprocedure protocol
#'                Values: "Yes", "No", or booleans string, "Unknown"
#'
#' @param endocarditis History of endocarditis: must meet at least 1 of the
#'                     following criteria:
#'                      1. organisms cultured from valve or vegetation.
#'                      2. 2 or more of the following signs or symptoms:
#'                          * fever (>38°C)
#'                          * new or changing murmur*
#'                          * embolic phenomena*
#'                          * skin manifestations* (i.e., petechiae, splinter
#'                            hemorrhages, painful subcutaneous nodules)
#'                          * congestive heart failure*
#'                          * cardiac conduction abnormality*
#'                      With no other recognized cause and at least 1 of the following:
#'                        a. organisms cultured from 2 or more blood cultures
#'                        b. organisms seen on Gram’s stain of valve when culture
#'                           is negative or not done
#'                        c. valvular vegetation seen during an invasive procedure
#'                           or autopsy
#'                        d. positive laboratory test on blood or urine
#'                           (e.g., antigen tests for H influenzae, S pneumoniae,
#'                           N meningitidis, or Group B Streptococcus)
#'                        e. evidence of new vegetation seen on echocardiogram
#'                           and if diagnosis is made antemortem, physician institutes
#'                           appropriate antimicrobial therapy.
#'                     CDC, January 2013 Choose 'Yes' for patients with pre-operative
#'                     endocarditis who begin antibiotics post-op.
#'                     Code yes for patients who are diagnosed intraoperatively.
#'                     Values: "Yes", "No", or booleans string
#'
#' @param endocarditis_type Type of endocarditis the patient has. If the patient is
#'                          currently being treated for endocarditis, the disease
#'                          is considered active.
#'                          If no antibiotic medication (other than prophylactic
#'                          medication) is being given at the time of surgery,
#'                          then the infection is considered treated.
#'
#'                          If a <i>endocarditis</i> type is set, endocarditis is automatically
#'                          set to "Yes"
#'                          Values: 'Active' or 'Treated'
#'
#' @param chd_known Indicate whether coronary artery anatomy and/or disease is documented
#'            and available prior to surgery.
#'            Values: "Yes", "No", or booleans string
#'
#' @param chd Indicate whether coronary artery anatomy and/or disease is documented
#'            and available prior to
#'
#'            If a valid <i>chd</i> type is set, chd_known is automatically
#'            set to "Yes"
#'            Values: "Yes", "No", or booleans string
#'
#' @param left_main_stenosis Any numeric string between 0 and 100, like "60%" or "45 %"
#'
#' @param urgency Indicate the clinical status of the patient prior to entering the operating room.
#'                Indication urgency, accepts the following values:
#'                "elective|elektiv", "urgent|dringlich",
#'                "emergent|emergency|notfall|notfallindikation",
#'                "salvage|emergent salvage"
#'
#' @param resuscitation Indicate whether the patient required cardiopulmonary
#'                      resuscitation before the start of the operative procedure
#'                      which includes the institution of anesthetic management.
#'                      Capture resuscitation timeframe: within 1 hour or 1-24 hours pre-op.
#'                      Allowed values:
#'                        - "yes-1h"
#'                        - "yes-1_24h"
#'                        - "no"
#'
#' @param cardiogenic_shock Indicate if the patient developed cardiogenic shock.
#'                          Cardiogenic shock is defined as a sustained (>30 min)
#'                          episode of hypoperfusion evidenced by systolic blood
#'                          pressure <90 mm Hg and/or, if available,
#'                          cardiac index <2.2 L/min per square meter determined to
#'                          be secondary to cardiac dysfunction and/or the requirement
#'                          for parenteral inotropic or vasopressor agents or
#'                          mechanical support (e.g., IABP, extracorporeal circulation, VADs)
#'                          to maintain blood pressure and cardiac index above those
#'                          specified levels. Note: Transient episodes of hypotension
#'                          reversed with IV fluid or atropine do not constitute
#'                          cardiogenic shock. The hemodynamic compromise
#'                          (with or without extraordinary supportive therapy) must persist
#'                          for at least 30 min. ACCF/AHA 2013
#'                          Accepted values:
#'                            - Boolean NO strings
#'                            - "yes-at_procedure"
#'                            - "yes-within_24h"
#' @param iabp Indicate whether the patient was placed on an Intra-Aortic Balloon Pump (IABP).
#'             Values: "Yes", "No", or booleans string
#'
#' @param iabp_when Indicate when the IABP was inserted.
#'                  Accepted values:
#'                    - Boolean NO strings --> will set <i>iabp</i> to "No"
#'                    - "Preop" --> will set <i>iabp</i> to "Yes"
#'                    - "Intraop" --> will set <i>iabp</i> to "Yes"
#'                    - "Postop" --> will set <i>iabp</i> to "Yes"
#'
#' @param inotropes Indicate whether the patient received IV inotropic agents within
#'                  48 hours preceding surgery.
#'                  Values: "Yes", "No", or booleans string
#'
#' @param pre_cv_intervention Indicate whether the patient has undergone any previous
#'                            cardiovascular intervention, either <b>surgical or non-surgical</b>,
#'                            which may include those done during the current admission.
#'
#' @param pre_pci Indicate whether a previous Percutaneous Coronary Intervention (PCI)
#'                was performed any time prior to this surgical procedure. Percutaneous
#'                coronary intervention (PCI) is the placement of an angioplasty guide
#'                wire, balloon, or other device (e.g. stent, atherectomy, brachytherapy,
#'                or thrombectomy catheter) into a native coronary artery or coronary
#'                artery bypass graft for the purpose of mechanical coronary revascularization.
#'                Values: "Yes", "No", or booleans string
#'                --> will set the value of <i>pre_cv_intervention</i> accordingly
#'
#' @param pre_pci_interval Indicate the interval of time between the previous PCI
#'                and the current surgical procedure.
#'                Values: ">6h" or "<=6h"
#'                --> will set the value of <i>pre_cv_intervention</i> and
#'                    <i>pre_pci</i> accordingly.
#'
#' @param vd_mitral Indicate whether Mitral valve disease is present.
#'                  Values: "Yes", "No", or booleans string
#'
#' @param vd_mitral_stenosis Indicate whether Mitral Stenosis is present.
#'                  Values: "Yes", "No", or booleans string
#'                  --> A boolean "Yes" value will also set <i>vd_mitral</i> to "Yes"
#'
#' @param vd_aortic Indicate whether Aortic valve disease is present.
#'                  Values: "Yes", "No", or booleans string
#'
#' @param vd_aortic_stenosis Indicate whether Aortic Stenosis is present.
#'                  Values: "Yes", "No", or booleans string
#'                  --> A boolean "Yes" value will also set <i>vd_aortic</i> to "Yes"
#'
#' @param vd_aortic_regurg Indicate whether there is evidence of Aortic valve
#'                         insufficiency/regurgitation. Enter level of valve function
#'                         associated with highest risk (i.e., worst performance).
#'                         Enter the highest level recorded in the chart.
#'                         'Moderately severe' should be coded as 'Severe'.
#'                         Values:
#'                           - "None" or Boolean NO strings
#'                           - "undocumented" | "not documented"
#'                           - "trivial/trace"
#'                           - "mild" | "1+" | "1"
#'                           - "moderate" | "2+" | "2" | "mittel" | "mittelgradig" | "mittelschwer"
#'                           - "severe" | "3+" | "3" | "schwer" | "schwergradig"
#'                            --> Any value >= 1+ will also set <i>vd_aortic</i> to "Yes"
#'
#' @param vd_mitral_regurg Indicate whether there is evidence of Mitral valve
#'                         insufficiency/regurgitation. Enter level of valve function
#'                         associated with highest risk (i.e., worst performance).
#'                         Enter the highest level recorded in the chart.
#'                         'Moderately severe' should be coded as 'Severe'.
#'                         Values:
#'                           - "None" or Boolean NO strings
#'                           - "undocumented" | "not documented"
#'                           - "trivial/trace"
#'                           - "mild" | "1+" | "1"
#'                           - "moderate" | "2+" | "2" | "mittel" | "mittelgradig" | "mittelschwer"
#'                           - "severe" | "3+" | "3" | "schwer" | "schwergradig"
#'                            --> Any value >= 1+ will also set <i>vd_aortic</i> to "Yes"
#'
#' @param vd_tricuspid_regurg Indicate whether there is evidence of Tricuspid valve
#'                         insufficiency/regurgitation. Enter level of valve function
#'                         associated with highest risk (i.e., worst performance).
#'                         Enter the highest level recorded in the chart.
#'                         'Moderately severe' should be coded as 'Severe'.
#'                         Values:
#'                           - "None" or Boolean NO strings
#'                           - "undocumented" | "not documented"
#'                           - "trivial/trace"
#'                           - "mild" | "1+" | "1"
#'                           - "moderate" | "2+" | "2" | "mittel" | "mittelgradig" | "mittelschwer"
#'                           - "severe" | "3+" | "3" | "schwer" | "schwergradig"
#'                            --> Any value >= 1+ will also set <i>vd_aortic</i> to "Yes"
#'
#' @param no_cardiovascular_surgeries Incidence - Indicate if this is the patient's:
#'                                      - 0 ==> first surgery
#'                                      - 1 ==> first re-op surgery
#'                                      - 2 ==> second re-op surgery
#'                                      - 3 ==> third re-op surgery
#'                                      - 4 ==> fourth or more re-op surgery.
#'                                    Surgery is defined as cardiothoracic
#'                                    operations (heart or great vessels) surgical
#'                                    procedures performed with or without cardiopulmonary
#'                                    bypass (CPB). Also include lung procedures
#'                                    utilizing CPB or tracheal procedures utilizing CPB.
#'                                    Reoperation increases risk due to the presence of
#'                                    scar tissue and adhesions.
#'
#' @param return_query_list setting this on TRUE will return only the query list, but not
#'                          do the request to the STS web service. Useful for debugging.
#'
#' @param verbose prints detailed info about parameters and the http web service query
#'
#' @return a list of the predicted risks of the at the time of request current
#'         STS risk model
#' @export
#' @examples
#'  service <- plumber::plumb("R/sts.R")
#'  service$run(port = 8080)
#
# Plumber decoration
#* @get /calc_sts
#* @post /calc_sts
calc_sts <- function(age,
                     gender,
                     proc_cabg,
                     proc_valve = NULL,
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
                     diabetes_ctrl = NULL,
                     htn = NULL,
                     immsupp = NULL,
                     endocarditis = NULL,
                     endocarditis_type = NULL,
                     chd_known = NULL,
                     chd = NULL,
                     left_main_stenosis = NULL,
                     urgency = NULL,
                     resuscitation = NULL,
                     cardiogenic_shock = NULL,
                     iabp = NULL,
                     iabp_when = NULL,
                     inotropes = NULL,
                     pre_cv_intervention = NULL,
                     pre_pci = NULL,
                     pre_pci_interval = NULL,
                     vd_mitral = NULL,
                     vd_mitral_stenosis = NULL,
                     vd_aortic = NULL,
                     vd_aortic_stenosis = NULL,
                     vd_aortic_regurg = NULL,
                     vd_mitral_regurg = NULL,
                     vd_tricuspid_regurg = NULL,
                     no_cardiovascular_surgeries = NULL,
                     return_query_list = FALSE,
                     verbose = FALSE
                     ) {

  queryList <- list()

  queryList$age <- ensurer::ensure(age, is.numeric(.), . >= 1 , . <= 110)
  queryList$gender <- parse_sex(gender)

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
    } else if (proc_valve %in% c("none","no", "n", "false", "f", "0")) {
      # do nothing, coding indicates no VALVE surgery
    } else {
      stop("'proc_valve' must be one of the following values: 'AVR',
           'MVReplacement', 'MVRepair' or a boolean NO/FALSE string")
    }
  }

  if (!is.null(height_cm)) {
    queryList$heightcm <- ensurer::ensure(height_cm, is.numeric(.), . >= 20, . <= 251)
  }

  if (!is.null(weight_kg)) {
    queryList$weightkg <- ensurer::ensure(weight_kg, is.numeric(.), . >= 10, . <= 250)
  }

  if (!is.null(lvef)) {
    queryList$hdef <- ensurer::ensure(lvef, is.numeric(.), . >= 1, . <= 99)
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
    prior_mi_bool <- parse_bool_and_add(prior_mi)

    if (prior_mi %in% names(choices[1:5])) {
      queryList$prevmi <- "Yes"
      queryList$miwhen <- choices[prior_mi]
    } else if (prior_mi %in% names(choices[6:7])) {
      queryList$prevmi <- choices[prior_mi]
    } else if (prior_mi_bool %in% c("Yes", "No")) {
      queryList$prevmi <- prior_mi_bool
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

    if (afib %in% c("NONE","NO", "FALSE", "F", "0")) {
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
    diabetes_ctrl <- stringr::str_to_upper(diabetes_ctrl)

    choices <- c("unknown" = "Unknown",
                 "other_subcutaneous" = "Other subcutaneous medication",
                 "other" = "Other",
                 "insulin" = "Insulin",
                 "diet" = "Diet only",
                 "oral" = "Oral")
    names(choices) <- stringr::str_to_upper(names(choices))

    if (diabetes_ctrl %in% c("NONE","NO", "N", "FALSE", "F", "0")) {
      queryList$diabctrl <- "None"
    } else if (diabetes_ctrl %in% names(choices)) {
      queryList$diabctrl <- choices[diabetes_ctrl]
      queryList$diabetes <- "Yes"
    } else {
      stop("Coding of 'diabetes_ctrl' not recognized.")
    }
  }

  if (!is.null(htn)) {
    queryList$hypertn <- parse_bool_and_add(stringr::str_to_title(htn),
                                             additionals = "Unknown")
  }

  if (!is.null(immsupp)) {
    queryList$immsupp <- parse_bool_and_add(stringr::str_to_title(immsupp),
                                             additionals = "Unknown")
  }

  if (!is.null(endocarditis)) {
    queryList$infendo <- parse_bool_and_add(stringr::str_to_title(endocarditis))
  }

  if (!is.null(endocarditis_type)) {
    endocarditis_type <- stringr::str_to_upper(endocarditis_type)

    if (endocarditis_type %in% c("ACTIVE", "ACT")) {
      queryList$infendty <- "Active"
      queryList$infendo <- "Yes"
    } else if (endocarditis_type %in% c("TREATED", "TRT", "TREAT")) {
      queryList$infendty <- "Treated"
      queryList$infendo <- "Yes"
    } else {
      stop("Coding of 'endocarditis_type' not recognized.")
    }
  }

  if (!is.null(chd_known)) {
    queryList$coranatdisknown <- parse_bool_and_add(stringr::str_to_title(chd_known))
  }

  if (!is.null(chd)) {
    chd <- parseVesselsDisease(chd)

    if (chd %in% c("None", "Three", "Two", "One")) {
      queryList$numdisv <- chd
      queryList$coranatdisknown <- "Yes"
    } else {
      stop("Coding of 'chd' not recognized.")
    }
  }

  if (!is.null(left_main_stenosis) && chd %in% c("Three", "Two", "One")) {
    stenosis <- readr::parse_number(left_main_stenosis)

    queryList$pctstenlmain <- ensurer::ensure(stenosis, is.numeric(.), . >= 0, . <= 100)
    queryList$pctstenknown <- "Yes"
  }

  if (!is.null(urgency)) {
    urgency <- stringr::str_trim(stringr::str_to_lower(urgency))

    if (urgency %in% c("elective", "elektiv")) {
      queryList$status <- "Elective"
    } else if (urgency %in% c("urgent", "dringlich")) {
      queryList$status <- "Urgent"
    } else if (urgency %in% c("emergent", "emergency", "notfall", "notfallindikation")) {
      queryList$status <- "Emergent"
    } else if (urgency %in% c("emergent salvage", "salvage")) {
      queryList$status <- "Emergent Salvage"
    } else {
      stop("Coding of 'urgency' not recognized.")
    }
  }

  if (!is.null(resuscitation)) {
    resusc <- stringr::str_trim(stringr::str_to_lower(resuscitation))

    if (resusc %in% c("none","no", "n", "false", "f", "0")) {
      queryList$resusc <- "No"
    } else if (resusc == "yes-1h") {
      queryList$resusc <- "Yes - Within 1 hour of the start of the procedure"
    } else if (resusc == "yes-1_24h") {
    queryList$resusc <- "Yes - More than 1 hour but less than 24 hours of the start of the procedure"
    } else {
      stop("Coding of 'resuscitation' not recognized.")
    }
  }

  if (!is.null(cardiogenic_shock)) {
    carshock <- stringr::str_trim(stringr::str_to_lower(cardiogenic_shock))

    if (carshock %in% c("none","no", "n", "false", "f", "0")) {
      queryList$carshock <- "No"
    } else if (carshock == "yes-at_procedure") {
      queryList$carshock <- "Yes - At the time of the procedure"
    } else if (carshock == "yes-within_24h") {
      queryList$carshock <- "Yes, not at the time of the procedure but within prior 24 hours"
    } else {
      stop("Coding of 'cardiogenic_shock' not recognized.")
    }
  }

  if (!is.null(iabp)) {
    queryList$iabp <- parse_bool_and_add(stringr::str_to_title(iabp))
  }

  if (!is.null(iabp_when)) {
    iabp_when <- stringr::str_trim(stringr::str_to_lower(iabp_when))

    if (iabp_when %in% c("none","no", "n", "false", "f", "0")) {
      queryList$iabp <- "No"
    } else if (str_starts_with(iabp_when, "pre") ||
               str_starts_with(iabp_when, "prae") ||
               str_starts_with(iabp_when, "prä")) {
      queryList$iabp <- "Yes"
      queryList$iabpwhen <- "Preop"
    } else if (str_starts_with(iabp_when, "intra")) {
      queryList$iabp <- "Yes"
      queryList$iabpwhen <- "Intraop"
    } else if (str_starts_with(iabp_when, "post")) {
      queryList$iabp <- "Yes"
      queryList$iabpwhen <- "Postop"
    } else {
      stop("Coding of 'iabp_when' not recognized.")
    }
  }

  if (!is.null(inotropes)) {
    queryList$medinotr <- parse_bool_and_add(stringr::str_to_title(inotropes))
  }

  if (!is.null(pre_cv_intervention)) {
    queryList$prcvint <- parse_bool_and_add(stringr::str_to_title(pre_cv_intervention),
                                            additionals = "Unknown")
  }

  if (!is.null(pre_pci)) {
    queryList$pocpci <- parse_bool_and_add(stringr::str_to_title(inotropes))
    if (queryList$pocpci == "Yes") {
      queryList$prcvint <- "Yes"
    }
  }

  if (!is.null(pre_pci_interval)) { #">6h" or "<=6h"
    pre_pci_interval <- stringr::str_trim(stringr::str_to_lower(pre_pci_interval))

     if (pre_pci_interval == ">6h") {
      queryList$pocpciin <- " > 6 Hours"
      queryList$prcvint <- "Yes"
      queryList$pocpci <- "Yes"
    } else if (pre_pci_interval == "<=6h") {
      queryList$pocpciin <- " <= 6 Hours"
      queryList$prcvint <- "Yes"
      queryList$pocpci <- "Yes"
    } else {
      stop("Coding of 'pre_pci_interval' not recognized.")
    }
  }

  if (!is.null(vd_mitral)) {
    queryList$vdmit <- parse_bool_and_add(stringr::str_to_title(vd_mitral))
  }

  if (!is.null(vd_mitral_stenosis)) {
    queryList$vdstenm <- parse_bool_and_add(stringr::str_to_title(vd_mitral_stenosis))
    if (queryList$vdstenm == "Yes") {
      queryList$vdmit <- "Yes"
    }
  }

  if (!is.null(vd_aortic)) {
    queryList$vdaort <- parse_bool_and_add(stringr::str_to_title(vd_aortic))
  }

  if (!is.null(vd_aortic_stenosis)) {
    queryList$vdstena <- parse_bool_and_add(stringr::str_to_title(vd_aortic_stenosis))
    if (queryList$vdstena == "Yes") {
      queryList$vdaort <- "Yes"
    }
  }

  if (!is.null(vd_aortic_regurg)) {
    regurg <- parse_vd_regurg(vd_aortic_regurg)

    if (!(is.null(regurg) || is.na(regurg))) {
      queryList$vdinsufa <- regurg
    }
  }

  if (!is.null(vd_mitral_regurg)) {
    regurg <- parse_vd_regurg(vd_mitral_regurg)

    if (!(is.null(regurg) || is.na(regurg))) {
      queryList$vdinsufm <- regurg
    }
  }

  if (!is.null(vd_tricuspid_regurg)) {
    regurg <- parse_vd_regurg(vd_tricuspid_regurg)

    if (!(is.null(regurg) || is.na(regurg))) {
      queryList$vdinsuft <- regurg
    }
  }

  if (!is.null(no_cardiovascular_surgeries)) {
    no_cardiovascular_surgeries <- readr::parse_number(no_cardiovascular_surgeries)

    incidence <- ensurer::ensure(no_cardiovascular_surgeries,
                                          is.numeric(.), . %in% c(0, 1, 2, 3, 4))

    trans <- c("0"="First cardiovascular surgery",
               "1"="First re-op cardiovascular surgery",
               "2"="Second re-op cardiovascular surgery",
               "3"="Third re-op cardiovascular surgery",
               "4"="Fourth or more re-op cardiovascular surgery")

    queryList$incidenc <- trans[as.character(incidence)]
  }

  #queryList <- as.list(match.call())[-1]
  #queryList <- purrr::compact(queryList)

  if (return_query_list) {
    return(queryList)
  }

  res <- do_sts_request(queryList, verbose = verbose)
  riskdf <- dplyr::as_data_frame(res)

  dplyr::rename(riskdf,
                Procedure = proceduretype,
                Mortality = predmort,
                Morbidity_Mortality = predmm,
                DSW_Infection = preddeep,
                Perm_Stroke = predstro,
                Prolong_Vent = predvent,
                Reoperation = predreop,
                Renal_failure = predrenf,
                Long_LOS = pred14d,
                Short_LOS = pred6d
                )
}


