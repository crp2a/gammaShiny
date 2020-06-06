
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gammaShiny <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R build
status](https://github.com/crp2a/gammaShiny/workflows/R-CMD-check/badge.svg)](https://github.com/crp2a/gammaShiny/actions)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<!-- [![DOI](https://zenodo.org/badge/DOI/xxx)](xxx) -->

<!-- badges: end -->

## Overview

A collection of [**shiny**](https://shiny.rstudio.com) application that
provides exhanced graphical user interfaces for the
[**gamma**](https://github.com/crp2a/gamma) package.

## Installation

Install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("crp2a/gammaShiny")
```

## Usage

``` r
# Load the package
library(gammaShiny)

# Run the app for gamma dose rate estimation
run_app("doserate")
```

![](man/figures/README-shiny-1.png)

## Contributing

Please note that the **gammaShiny** project is released with a
[Contributor Code of
Conduct](https://github.com/crp2a/gammaShiny/blob/master/.github/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Acknowledgements

This work received a state financial support managed by the Agence
Nationale de la Recherche (France) throught the program *Investissements
d’avenir* (ref. [10-LABX-0052](https://lascarbx.labex.u-bordeaux.fr) and
[11-IDEX-0001](https://amidex.univ-amu.fr)).
