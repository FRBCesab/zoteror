
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoteror <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/FRBCesab/zoteror/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FRBCesab/zoteror/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/FRBCesab/zoteror/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/FRBCesab/zoteror/actions/workflows/pkgdown.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/zoteror)](https://CRAN.R-project.org/package=zoteror)
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![Dependencies](https://img.shields.io/badge/dependencies-3/27-green?style=flat)](#)
<!-- badges: end -->

The goal of the R package `zoteror` is to retrieve references metadata
stored in the Zotero local database (SQLite file).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("FRBCesab/zoteror")
```

Then you can attach the package `zoteror`:

``` r
library("zoteror")
```

## Overview

The package `zoteror` only contains the function
[`get_zotero_data()`](https://frbcesab.github.io/zoteror/reference/get_zotero_data.html)
to retrieve references metadata from the Zotero local database.

## Citation

Please cite this package as:

> Casajus Nicolas (2023) zoteror: An R package to handle Zotero local
> database. R package version 0.0.1.

## Code of Conduct

Please note that the `zoteror` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
