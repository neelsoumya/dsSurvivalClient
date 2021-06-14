# dsSurvivalClient

# Introduction

Survival functions (client side) for DataSHIELD (a platform for federated analysis of private data). These are client side functions for survival models and Cox models.

This is a standalone bolt on package for survival analysis in DataSHIELD. DataSHIELD is a platform for federated analysis of private data.

* The server side package is called dsSurvival:

    * https://github.com/neelsoumya/dsSurvival


# Installation

```

install.packages('devtools')

library(devtools)

devtools::install_github('neelsoumya/dsBaseClient')
	
devtools::install_github('neelsoumya/dsBase')

devtools::install_github('neelsoumya/dsSurvivalClient')

```

If you want to use a certain release then you can do the following

```

library(devtools)

devtools::install_github('neelsoumya/dsSurvivalClient@v1.0.0')

```

If you want to try privacy preserving survival curves (work in progress and to be available in v1.1.0), then you can do the following

```

library(devtools)

devtools::install_github('neelsoumya/dsSurvivalClient', ref = 'privacy_survival_curves')

```


* Install R Studio and the development environment as described below:

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/12943461/Getting+started


* Then install the virtual machines as described below:

    * https://data2knowledge.atlassian.net/wiki/spaces/DSDEV/pages/931069953/Installation+Training+Hub-+DataSHIELD+v6


# Usage:

* see vignettes   

* https://github.com/neelsoumya/dsSurvivalClient/blob/main/vignettes/development_plan.rmd

* https://github.com/neelsoumya/dsSurvivalClient/blob/main/vignettes/development_plan.pdf


A screenshot of meta-analyzed hazard ratios from the survival model is shown below.

![Meta-analyzed hazard ratios from survival models](screenshot_survival_models.png)


# Release notes

v1.0.0: A basic release of survival models in DataSHIELD.  DataSHIELD is a federated analysis platform for secure data. This release has Cox proportional hazards models, summaries of models, diagnostics and the ability to meta-analyze hazard ratios. There is also capability to generate forest plots of meta-analyzed hazard ratios. This release supports study-level meta-analysis.

A shiny graphical user interface for survival models has also been created by Xavier Escriba Montagut and Juan Gonzalez


* https://github.com/isglobal-brge/ShinyDataSHIELD


v1.1.0: Forthcoming. This will have privacy preserving survival curves.


# Acknowledgements

We acknowledge the help and support of the DataSHIELD technical team.
We are especially grateful to Yannick Marcon, Paul Burton, Demetris Avraam, Stuart Wheater, Patricia Ryser-Welch and Wolfgang Vichtbauer for fruitful discussions and feedback.


# Contact

* Soumya Banerjee, Demetris Avraam, Paul Burton, Xavier Escriba Montagut, Juan Gonzalez, Tom R P Bishop and DataSHIELD technical team

* sb2333@cam.ac.uk

* DataSHIELD 

    * https://www.datashield.ac.uk
    
    
# Citation

If you use this code, please cite the following DOI

[![DOI](https://zenodo.org/badge/362161720.svg)](https://zenodo.org/badge/latestdoi/362161720)
