# Automated risk calculation clinical practice and research - the riskscorer package

## Authors:

> Meyer, Alexander<br>
> Kempfert, Jörg<br>
> Vogel, Stefan<br>
> Falk, Volkmar

_German Heart Institute Berlin, Department of Cardiothoracic and Vascular Surgery, Berlin, Germany_
<br>https://www.dhzb.de/en/homepage/

Clinical risk scores are important tools for stratification prior treatment decision as well as for analysis and adjustments in clinical research. Often risk scores are published without an easily accessible interface for calculation. And if tools exist, mostly these are web based user interfaces and therefor not suitable for either batch processing in research or integration into the clinical information systems infrastructure. 

We developed the _riskscorer_ package for easy and automatic clinical risk score calculation with the following features in mind:

* extensibility
* a simple programming interface with flexible handling of differing data codings
* individual patient risk calculation as well as the possibility of batch processing
* an HTTP web-service interface based on the plumber package for easy integration into the clinical information system infrastructure

Currently three surgical risk scores are implemented: STS score (http://riskcalc.sts.org/), EuroScore I, and EuroScore II (http://www.euroscore.org/). The riskscorer package is under continues development and we have released the source code (https://github.com/meyera/riskscorer) under the MIT license.

The integration of automated risk score calculation in the clinical workflow and in reproducible and efficient data analysis pipelines in clinical research has the potential to improve patient outcomes. 
