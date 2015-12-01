
## riskscorer [![Package-License](https://img.shields.io/github/license/mashape/apistatus.svg)](https://opensource.org/licenses/MIT) 

`riskscorer` provides an interface for calculation of surgical risk scores

At the moment the following score are implemented

* [STS Score](http://riskcalc.sts.org/)
* [EuroScore II](http://www.euroscore.org)

The following score are under development

* [EuroScore I](http://www.euroscore.org)
* [German AV Score](http://doi.org/10.1093/ejcts/ezt114)

### Example

In this simple example, trends for keywords `nhl`, `nba` and `nfl` are
retrieved and then plotted from R.

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
