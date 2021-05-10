# dsSurvivalClient

# Introduction

Survival functions (client side) for DataSHIELD. These are client side functions for survival models and Cox models.

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


# Acknowledgements

We acknowledge the help and support of the DataSHIELD technical team.
We are especially grateful to Paul Burton, Demetris Avraam, Stuart Wheater, Patricia Ryser-Welch and Wolfgang Vichtbauer for fruitful discussions and feedback.


# Contact

* Soumya Banerjee, Tom Bishop, Demetris Avraam, Paul Burton and DataSHIELD technical team

* sb2333@cam.ac.uk
    
