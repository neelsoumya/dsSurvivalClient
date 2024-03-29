###########################################
# Script to build manuals and test code
#
# Usage:
#   R --no-save < build_man_test.R
#
###########################################

###################
# load libraries
###################
library(devtools)
library(testthat)
library(dsBase)
library(dsBaseClient)
require('DSI')
require('DSOpal')
library(dsSurvivalClient)

##################
# build manuals
##################
devtools::build_manual()

########################
# update documentation
########################
devtools::check_man()

##################
# build vignettes
##################
devtools::build_vignettes()

##################
# Testing
##################
devtools::test()

##################
# clean up
##################
gc()

