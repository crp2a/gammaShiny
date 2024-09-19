
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gammaShiny <img width=120px src="man/figures/logo.png" align="right" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/crp2a/gammaShiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crp2a/gammaShiny/actions/workflows/R-CMD-check.yaml)

[![r-universe](https://crp2a.r-universe.dev/badges/gammaShiny)](https://crp2a.r-universe.dev)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4139005.svg)](https://doi.org/10.5281/zenodo.4139005)
<!-- badges: end -->

## Overview

A collection of [**shiny**](https://shiny.rstudio.com) application that
provides exhanced graphical user interfaces for the
[**gamma**](https://github.com/crp2a/gamma) package.

    To cite gammaShiny in publications use:

      Lebrun B, Frerebeau N, Paradol G, Guérin G, Mercier N, Tribolo C,
      Lahaye C, Rizza M (2020). "gamma: An R Package for Dose Rate
      Estimation from In-Situ Gamma-Ray Spectrometry Measurements."
      _Ancient TL_, *38*(2), 1-5.

    Une entrée BibTeX pour les utilisateurs LaTeX est

      @Article{,
        author = {Brice Lebrun and Nicolas Frerebeau and Guilhem Paradol and Guillaume Guérin and Norbert Mercier and Chantal Tribolo and Christelle Lahaye and Magali Rizza},
        title = {{gamma: An R Package for Dose Rate Estimation from In-Situ Gamma-Ray Spectrometry Measurements}},
        year = {2020},
        journal = {Ancient TL},
        volume = {38},
        number = {2},
        pages = {1-5},
      }

## Installation

You can install **gammaShiny** from [our
repository](https://crp2a.r-universe.dev) with:

``` r
options(repos = c(CRAN = "https://cloud.r-project.org",
                  crp2a = "https://crp2a.r-universe.dev"))

install.packages("gammaShiny")
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
