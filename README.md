
<!-- README.md is generated from README.Rmd. Please edit that file -->
Quick start
-----------

Welcome to the `factordist` GitHub page!

This package measures the difference between factors. Most functions contained here work by tallying how often the levels of a factor equal one another.

``` r
library(devtools)
devtools::install_github("tpq/factordist")
library(factordist)
?factordist
```

The `factordist` function
-------------------------

The `factordist` function computes the difference between columns of a `data.frame`.

``` r
x <- simulate_factor_data(30, 6)
dis <- factordist(x, metric = "jaccard", as_dist = TRUE)
#> |------------(25%)----------(50%)----------(75%)----------|
```

If the columns do not contain factors already, the input is coerced to a factor.

``` r
x <- as.data.frame(matrix(1:25, 5, 5))
dis <- factordist(x, metric = "jaccard", as_dist = TRUE)
#> Alert: Coercing all non-factor input into factors.
#> |------------(25%)----------(50%)----------(75%)----------|
```

See `?factordist` for Details about available metrics. I have only implemented a few distance metrics so far, but welcome Feature Requests via <http://github.com/tpq/factordist/issues>.

The `cluster_model` function
----------------------------

The package also contains the `cluster_model` class and methods which are intended to make it easier to generalize clustering results to new data. Its design mirrors methods used for supervised learning. First, an initial function builds a model that relates the input data `x` to the labels `y`. Second, another function deploys the model to `newdata`, for which labels are unknown or hidden. When using the `predict` method, the labels for the new data get assigned procedurally using a nearest neighbor algorithm.

``` r
library(factordist)
x <- simulate_factor_data(30, 6)
y <- sample(c("A", "B"), replace = TRUE, size = 30)
newdata <- simulate_factor_data(10, 6)
m <- cluster_model(x, y, FUN = d_jaccard)
#> Alert: Coercing input y to string.
predict(m, newdata)
#> |------------(25%)----------(50%)----------(75%)----------|
#> Some samples have multiple nearest neighbors. Choosing randomly.
#>   1   2   3   4   5   6   7   8   9  10 
#> "A" "A" "B" "B" "B" "A" "B" "A" "A" "A"
```

By default, `cluster_model` will assume the metric is symmetric. Set the argument `asym = TRUE` if the metric is not symmetric, i.e., `dist(a, b) != dist(b, a)`. When `asym = TRUE`, the `predict` method will choose each neighbor based on the smaller of the two distances.
