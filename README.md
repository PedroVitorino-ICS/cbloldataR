
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbloldataR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of `cbloldataR` is to provide a set of functions aimed for
harvesting data of CBLOL from
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends),
helping analysts, journalists, or enthusiasts to work with and visualize
the tournament stats within R.

## Installation

`cbloldataR` was created as an exercise for practising web scrapping and
package devolopement and is not likely to be submitted to CRAN. You can
install the latest development version from
[GitHub](https://github.com/) with:

``` undefined
# install.packages("devtools")
devtools::install_github("pedrodrocha/cbloldataR")
```

## Load

``` r
library(cbloldataR)
```

## Use

The package offer access to eight tables of tidy data that you can
harvest from
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

1)  `getData_editions()` creates a tibble containing Leaguepedia data on
    each CBLOL edition.

<!-- end list -->

``` r
editions <- getData_editions()
head(editions)
#> # A tibble: 6 x 6
#>    year tournament       prize_pool first            runner_up        league
#>   <dbl> <chr>                 <dbl> <chr>            <chr>            <chr> 
#> 1  2020 Split 2 Playoffs     160000 <NA>             <NA>             CBLOL 
#> 2  2020 Split 2               40000 <NA>             <NA>             CBLOL 
#> 3  2020 Split 1 Playoffs     160000 KaBuM! e-Sports  Flamengo eSports CBLOL 
#> 4  2020 Split 1               40000 Vivo Keyd        Flamengo eSports CBLOL 
#> 5  2019 Split 2 Playoffs     160000 Flamengo eSports INTZ             CBLOL 
#> 6  2019 Split 2               40000 Flamengo eSports KaBuM! e-Sports  CBLOL
```

### Notes

If you use the data always remember to attribute the source to
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/pedrodrocha/cbloldataR/issues).
