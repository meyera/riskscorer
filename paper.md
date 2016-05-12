---
title: 'riskscorer - automating risk calculation in clinical practice and research'
tags:
  - clinical risk scores
  - medicine
  - web service
authors:
 - name: Alexander Meyer
   orcid: 0000-0002-6944-2478
   affiliation: German Heart Institute Berlin, Department of Cardiothoracic and Vascular Surgery, Berlin, Germany 
 - name: Jörg Kempfert
   affiliation: German Heart Institute Berlin, Department of Cardiothoracic and Vascular Surgery, Berlin, Germany 
 - name: Volkmar Falk
   affiliation: German Heart Institute Berlin, Department of Cardiothoracic and Vascular Surgery, Berlin, Germany 
date: 12 May 2016
bibliography: paper.bib
---

# Summary

Clinical risk scores are important tools in therapeutic decision making as well as for analysis and adjustments in clinical research. Often risk scores are published without an easily accessible interface for calculation. And if tools exist, these are mostly web based user interfaces and therefor not suitable for either batch processing in research or integration into the hospital's clinical information system infrastructure. 

We developed the _riskscorer_ package for easy and automatic clinical risk score calculation with the following features in mind:

* simple programming interface
* extensibility
* flexible handling of differing data codings
* individual patient risk calculation as well as the possibility of batch processing
* an HTTP web-service interface based on the plumber [@plumber] package for easy integration into an existing clinical information system infrastructure

Currently the two most used risk scores in cardiac surgery are implemented: the STS [@sts] score ([http://riskcalc.sts.org/](http://riskcalc.sts.org/)) and the EuroSCORE II [@euroscore]. It can be readily used in research and easily integrated into clinical information systems. The integration of automated risk score calculation into the clinical workflow and into reproducible and efficient data analysis pipelines in research has the potential to improve patient outcomes. 

# References
