## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Energy Scale Calibration
## Authors: Nicolas Frerebeau, Université Bordeaux Montaigne (France)
##          Brice Lebrun, Université Bordeaux Montaigne (France)
## Contact: nicolas.frerebeau@u-bordeaux-montainge.fr
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Load packages ===============================================================
rm(list = ls())
library(gamma)
library(gammaShiny)

## Set Shiny settings ==========================================================
options(shiny.maxRequestSize = 30*1024^2)
enableBookmarking(store = "server")
env_current <- environment()
