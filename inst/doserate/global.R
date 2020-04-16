## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Dose Rate Estimation
## Authors: Nicolas Frerebeau, Université Bordeaux Montaigne (France)
##          Brice Lebrun, Université Bordeaux Montaigne (France)
## Contact: nicolas.frerebeau@u-bordeaux-montainge.fr
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Load packages ===============================================================
rm(list = ls())
library(gamma)

## Set Shiny settings ==========================================================
options(shiny.maxRequestSize = 30*1024^2)
enableBookmarking(store = "server")
env_current <- environment()

## Load datasets ===============================================================
env_calibration <- new.env()
data("BDX_LaBr_1_curve", package = "gamma", envir = env_calibration)
# data("BDX200_curve", package = "gamma", envir = env_calibration)
# data("BDX300_curve", package = "gamma", envir = env_calibration)
data("AIX_NaI_curve", package = "gamma", envir = env_calibration)