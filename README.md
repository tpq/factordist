
<!-- README.md is generated from README.Rmd. Please edit that file -->
Quick start
-----------

Welcome to the `factordist` GitHub page!

This package measures the distance between factors. Most functions contained here work by tallying how often the elements of the factors equal one another. Some functions will treat the factor label in an absolute sense, while others will treat the factor label in a relative sense. The latter are useful for measuring the distance between clusterings, where the cluster label may be arbitrary.

``` r
library(devtools)
devtools::install_github("tpq/factordist")
library(factordist)
?factordist
```
