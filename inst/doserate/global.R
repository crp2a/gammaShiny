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
options(shiny.maxRequestSize = 30 * 1024^2)
enableBookmarking(store = "server")
