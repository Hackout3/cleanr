# cleanr

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Linux Build Status](https://travis-ci.org/Hackout3/cleanr.svg?branch=master)](https://travis-ci.org/Hackout3/cleanr)

**WARNING: This is an experimental package tied to some internal projects.  The interface is still being refined and may change at any point.  It may also split or merge into a number of other projects.  You have been warned!**

## Installation

From our drat repository, either:

```r
drat:::add("richfitz")
install.packages("cleanr")
```

or, if you don't have drat installed:

```
install.packages("cleanr", repos=c(CRAN="https://cran.rstudio.com",
                                    drat="https://richfitz.github.io/drat"))
```

Alternatively, you install Directly from GitHub via `devtools`:

```r
devtools::install_github("Hackout3/cleanr")
```
