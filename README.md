
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zoteror <img src="man/figures/package-sticker.png" align="right" style="float:right; height:120px;"/>

<!-- badges: start -->

[![R CMD
Check](https://github.com/FRBCesab/zoteror/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FRBCesab/zoteror/actions/workflows/R-CMD-check.yaml)
[![Website](https://github.com/FRBCesab/zoteror/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/FRBCesab/zoteror/actions/workflows/pkgdown.yaml)
[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

## Table of contents

<p align="left">
• <a href="#overview">Overview</a><br> •
<a href="#installation">Installation</a><br> •
<a href="#get-started">Get started</a><br> •
<a href="#citation">Citation</a><br> •
<a href="#contributing">Contributing</a>
</p>

## Overview

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

## Get started

The package `zoteror` only contains the function
[`get_zotero_data()`](https://frbcesab.github.io/zoteror/reference/get_zotero_data.html)
to retrieve references metadata from the Zotero local database.

## Citation

Please cite this package as:

> Casajus Nicolas (2024) zoteror: An R package to handle Zotero local
> database. R package version 0.0.1.
> <https://github.com/FRBCesab/zoteror>

## Contributing

All types of contributions are encouraged and valued. For more
information, check out our [Contributor
Guidelines](https://github.com/FRBCesab/zoteror/blob/main/CONTRIBUTING.md).

Please note that the `zoteror` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
