
## riskscorer [![Package-License](https://img.shields.io/packagist/l/doctrine/orm.svg)](https://opensource.org/licenses/MIT) [![DOI](https://zenodo.org/badge/21914/meyera/riskscorer.svg)](https://zenodo.org/badge/latestdoi/21914/meyera/riskscorer)


`riskscorer` provides an interface for calculation of clinical risk scores

At the moment the following score are implemented

* [STS Score](http://riskcalc.sts.org/)
* [EuroScore II](http://www.euroscore.org)

The following will be added in a future release:

* [EuroScore I](http://www.euroscore.org)
* [German AV Score](http://doi.org/10.1093/ejcts/ezt114)

Eventually 

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

Coding can differ for factor variables. Simple heuristics translate common clinical factor codings. For example "Female", "female" or "f" will all be detected as female gender. Boolean variables such as '0', '1', 'True', 'T', "Y", "Yes" will be detected. Documentation about coding and interpretation is available for every score component.

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

### Web service ready thanks to plumber 

Thanks to the fantastic [plumber](https://github.com/trestletech/plumber) package, every score calculation function can be easily used as a web service. Each risk score has its dedicated self contained R source file, such as `R/sts.R`. 

Setting up a web service is as easy as running the following lines of code:
```
service <- plumber::plumb("R/sts.R")
service$run(port = 8080)
```

The hosting of such a service is well documented at the [plumber](http://plumber.trestletech.com/docs/hosting/) documentation.

### Installation

Pre-release versions can be installed from this repository via

``` {.r}
if (!require("devtools")) install.packages("devtools")
devtools::install_github("meyera/riskscorer")
```

A CRAN release will follow.

### Authors

Alexander Meyer

### License

The MIT License (MIT)
