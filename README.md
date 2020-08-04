
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

1)  `getData_editions()`: creates a tibble containing Leaguepedia data
    on each CBLOL edition.

<!-- end list -->

``` r
editions <- getData_editions()
glimpse(editions)
#> Rows: 24
#> Columns: 6
#> $ year       <dbl> 2020, 2020, 2020, 2020, 2019, 2019, 2019, 2019, 2018, 20...
#> $ tournament <chr> "Split 2 Playoffs", "Split 2", "Split 1 Playoffs", "Spli...
#> $ prize_pool <dbl> 160000, 40000, 160000, 40000, 160000, 40000, 160000, 400...
#> $ first      <chr> NA, NA, "KaBuM! e-Sports", "Vivo Keyd", "Flamengo eSport...
#> $ runner_up  <chr> NA, NA, "Flamengo eSports", "Flamengo eSports", "INTZ", ...
#> $ league     <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "C...
```

2)  `getData_titlesOrg()`: Creates a tibble containing Leaguepedia data
    on CBLOL tournament wins per Organization.

<!-- end list -->

``` r
titlesOrg <- getData_titlesOrg()
glimpse(titlesOrg)
#> Rows: 19
#> Columns: 5
#> $ team           <chr> "INTZ", "KaBuM! e-Sports", "paiN Gaming", "Vivo Keyd...
#> $ x1st_place     <chr> "4", "4", "2", "1", "1", "1", "1", "1", "0", "0", "0...
#> $ x2nd_place     <chr> "2", "0", "2", "4", "3", "0", "0", "0", "3", "1", "0...
#> $ x3rd_4th_place <chr> "2", "3", "5", "4", "0", "2", "0", "0", "4", "0", "1...
#> $ league         <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL"...
```

### Notes

If you use the data always remember to attribute the source to
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/pedrodrocha/cbloldataR/issues).
