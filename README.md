# svFlow - Workflow management and alternate pipe operator

[![Linux & OSX Build Status](https://travis-ci.com/SciViews/svFlow.svg)](https://travis-ci.com/SciViews/svFlow) [![Win Build Status](https://ci.appveyor.com/api/projects/status/github/SciViews/svFlow?branch=master&svg=true)](https://ci.appveyor.com/project/phgrosjean/svFlow) [![Coverage Status](https://img.shields.io/codecov/c/github/SciViews/svFlow/master.svg)](https://codecov.io/github/SciViews/svFlow?branch=master) [![CRAN Status](https://www.r-pkg.org/badges/version/svFlow)](https://cran.r-project.org/package=svFlow) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Installation

### Latest stable version

The latest stable version of {svFlow} can simply be installed from [CRAN](http://cran.r-project.org):

``` r
install.packages('svFlow')
```

### Development version

Make sure you have the **remotes** R package installed:

``` r
install.packages('remotes')
```

Use `install_github()` to install the {svFlow} package from Github (source from **master** branch will be recompiled on your machine):

``` r
remotes::install_github("SciViews/svFlow", build_vignettes = TRUE)
```

R should install all required dependencies automatically, and then it should compile and install {svFlow}.

Latest devel version of {svFlow} (source + Windows binaries for the latest stable version of R at the time of compilation) is also available from [appveyor](https://ci.appveyor.com/project/phgrosjean/svFlow/build/artifacts).

## Usage

Make the {svFlow} package available in your R session:

``` r
library('svFlow')
```

Get help about this package:

``` r
library(help = 'svFlow')
help("flow-package")
```

For further instructions, please, refer to these help pages.

## Note to developers

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
