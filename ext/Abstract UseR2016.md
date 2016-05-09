# Automated risk calculation in clinical practice and research - the riskscorer package

## Authors:

> Meyer, Alexander<br>
> Vogel, Stefan<br>
> Sündermann, Simon<br>
> Kempfert, Jörg<br>
> Falk, Volkmar

_German Heart Institute Berlin, Department of Cardiothoracic and Vascular Surgery, Berlin, Germany_
<br>https://www.dhzb.de/en/homepage/

Clinical risk scores are important tools in therapeutic decision making as well as for analysis and adjustments in clinical research. Often risk scores are published without an easily accessible interface for calculation. And if tools exist, mostly these are web based user interfaces and therefor not suitable for either batch processing in research or integration into the hospital's clinical information system infrastructure. 

We developed the _riskscorer_ package for easy and automatic clinical risk score calculation with the following features in mind:

* simple programming interface
* extensibility
* flexible handling of differing data codings
* individual patient risk calculation as well as the possibility of batch processing
* an HTTP web-service interface based on the plumber (https://github.com/trestletech/plumber) package for easy integration into an existing clinical information system infrastructure

Currently three surgical risk scores are implemented: STS score (http://riskcalc.sts.org/), EuroScore I and EuroScore II (http://www.euroscore.org/). It is already used in our research and integration into our clinical information system is planned. The riskscorer package is under continues development and we have released the source code under the MIT license on the GitHub platform (https://github.com/meyera/riskscorer).

The integration of automated risk score calculation into the clinical workflow and into reproducible and efficient data analysis pipelines in research has the potential to improve patient outcomes. 
