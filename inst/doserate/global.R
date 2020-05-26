## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Dose Rate Estimation App
## Authors: Nicolas Frerebeau, Université Bordeaux Montaigne (France)
##          Brice Lebrun, Université Bordeaux Montaigne (France)
## Contact: nicolas.frerebeau@u-bordeaux-montainge.fr
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Clean current environment ===================================================
rm(list = ls())

## Load packages ===============================================================
library(gammaShiny)

## Set Shiny settings ==========================================================
options(shiny.maxRequestSize = 30*1024^2)
enableBookmarking(store = "server")
# onStop()

## Load datasets ===============================================================
env_calibration <- new.env()
data("BDX_LaBr_1", package = "gamma", envir = env_calibration)
data("AIX_NaI_1", package = "gamma", envir = env_calibration)
