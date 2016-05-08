
## riskscorer [![Package-License](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://opensource.org/licenses/MIT) [![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.51161.svg)](https://zenodo.org/record/51161#)

`riskscorer` provides an interface for calculation of surgical risk scores

At the moment the following score are implemented

* [STS Score](http://riskcalc.sts.org/)
* [EuroScore II](http://www.euroscore.org)

The following score are under development

* [EuroScore I](http://www.euroscore.org)
* [German AV Score](http://doi.org/10.1093/ejcts/ezt114)

### Example

In this simple example known data of a potential patient is entered as arguments.

``` {.r}
calc_sts(proc_cabg = TRUE,
           proc_valve = "avr",
           gender = "male",
           age = 60,
           lvef = 35,
           weight_kg = 65,
           height_cm = 185,
           chf_2w = "yes")

# Source: local data frame [1 x 10]
# 
# Procedure Mortality Morbidity_Mortality DSW_Infection Long_LOS Perm_Stroke Prolong_Vent Renal_failure Reoperation Short_LOS
# (chr)     (dbl)               (dbl)         (dbl)    (dbl)       (dbl)        (dbl)         (dbl)       (dbl)     (dbl)
# 1 AV Replacement + CAB   0.01415             0.11393       0.00168  0.04453     0.00878      0.06237       0.01895     0.06435   0.51145
```

Coding can differ.

``` {.r}
calc_sts(proc_cabg = TRUE,
           proc_valve = "avr",
           gender = "m",
           age = 60,
           lvef = 35,
           weight_kg = 65,
           height_cm = 185,
           chf_2w = 1)
Source: local data frame [1 x 10]

# Procedure Mortality Morbidity_Mortality DSW_Infection Long_LOS Perm_Stroke Prolong_Vent Renal_failure Reoperation
#      (chr)     (dbl)               (dbl)         (dbl)    (dbl)       (dbl)        (dbl)         (dbl)       (dbl)
# 1 AV Replacement + CAB   0.01415             0.11393       0.00168  0.04453     0.00878      0.06237       0.01895     0.06435
# Variables not shown: Short_LOS (dbl)
```

### Installation

Pre-release versions can be installed from this repository via

``` {.r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("meyera/riskscorer")
```

### Authors

Alexander Meyer

### License

The MIT License (MIT)
