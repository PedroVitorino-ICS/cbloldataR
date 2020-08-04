
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

1)  `getData_editions()`: creates a tibble containing
    [Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
    data on each CBLOL edition.

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

2)  `getData_titlesOrg()`: Creates a tibble containing
    [Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
    data on CBLOL tournament wins per Organization.

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

3)  `getData_titlesPlayer()`: Creates a tibble containing
    [Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
    data on CBLOL tournament wins per player

<!-- end list -->

``` r
titlesPlayer <- getData_titlesPlayer()
glimpse(titlesPlayer)
#> Rows: 62
#> Columns: 5
#> $ player           <chr> "brTT", "Tockers", "Mylon", "micaO", "Revolta", "J...
#> $ titles           <int> 5, 4, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,...
#> $ titles_as_player <chr> "5", "4", "3", "3", "3", "3", "3", "2", "2", "2", ...
#> $ titles_as_coach  <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", ...
#> $ league           <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLO...
```

4)  `getData_rosters()`: Creates a tibble containing
    [Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
    data on CBLOL rosters on each edition

<!-- end list -->

``` r
rosters <- getData_rosters(Team = "paiN Gaming", Role = "Support")
#> It may take a while...

rosters %>% 
  slice(1:10)
#> # A tibble: 10 x 8
#>    country id         name                team         year split  role   league
#>    <chr>   <chr>      <chr>               <chr>       <dbl> <chr>  <chr>  <chr> 
#>  1 KR      Key        Kim Han-gi (<U+AE40><U+D55C><U+AE30>) paiN Gaming  2020 Split~ Suppo~ CBLOL 
#>  2 BR      esA        Eidi Yanagimachi    paiN Gaming  2020 Split~ Suppo~ CBLOL 
#>  3 BR      esA        Eidi Yanagimachi    paiN Gaming  2019 Split~ Suppo~ CBLOL 
#>  4 BR      Loop       Caio Almeida        paiN Gaming  2018 Split~ Suppo~ CBLOL 
#>  5 BR      Loop       Caio Almeida        paiN Gaming  2017 Split~ Suppo~ CBLOL 
#>  6 BR      Loop       Caio Almeida        paiN Gaming  2017 Split~ Suppo~ CBLOL 
#>  7 BR      Picoca     Matheus Tavares     paiN Gaming  2016 Split~ Suppo~ CBLOL 
#>  8 BR      Ziriguidun Pedro Vilarinho     paiN Gaming  2016 Split~ Suppo~ CBLOL 
#>  9 EU      Dioud      Hugo Padioleau      paiN Gaming  2015 Split~ Suppo~ CBLOL 
#> 10 EU      Dioud      Hugo Padioleau      paiN Gaming  2015 Split~ Suppo~ CBLOL
```

5)  `getData_games()`: Creates a tibble containing
    [Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
    data on CBLOL official games.

<!-- end list -->

``` r
# You can specify the Year and Split...
games1 <- getData_games(Year = 2020, Split = c("Split_1","Split_1_Playoffs"))
#> It may take a while...
glimpse(games1)
#> Rows: 93
#> Columns: 13
#> $ date        <chr> "2020-04-26", "2020-04-26", "2020-04-26", "2020-04-26",...
#> $ split       <chr> "Split_1", "Split_1", "Split_1", "Split_1", "Split_1", ...
#> $ patch       <chr> "10.8", "10.8", "10.8", "10.8", "10.8", "10.8", "10.8",...
#> $ blue        <chr> "INTZ", "Vivo Keyd", "FURIA Uppercut", "Prodigy Esports...
#> $ red         <chr> "Flamengo eSports", "KaBuM! e-Sports", "PaiN Gaming", "...
#> $ winner      <chr> "Flamengo eSports", "KaBuM! e-Sports", "FURIA Uppercut"...
#> $ ban_blue    <chr> "Renekton,Varus,Kalista", "Senna,Elise,Akali", "Yuumi,E...
#> $ ban_red     <chr> "Jayce,Bard,Braum", "Kassadin,Yuumi,LeBlanc", "Varus,Le...
#> $ pick_blue   <chr> "Yuumi,Ekko,Cassiopeia,Syndra,Jayce", "Renekton,Taric,B...
#> $ pick_red    <chr> "Olaf,Varus,Trundle,Ornn,LeBlanc", "Miss Fortune,Ezreal...
#> $ blue_roster <chr> "Tay, Shini, Hauz, micaO, RedBert", "Robo, Caos, NOsFer...
#> $ red_roster  <chr> "WooFe, Goku, Absolut, Ranger, Jojo", "Parang, Wiz, Tut...
#> $ league      <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "...
```

``` r
# ...and also the Org.
games2 <- getData_games(Team = "INTZ", Year = 2020, Split = c("Split_2"))
#> It may take a while...
glimpse(games2)
#> Rows: 18
#> Columns: 13
#> $ date        <chr> "2020-08-02", "2020-08-01", "2020-07-26", "2020-07-25",...
#> $ split       <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", ...
#> $ patch       <chr> "10.15", "10.15", "10.14", "10.14", "10.14", "10.14", "...
#> $ blue        <chr> "INTZ", "INTZ", "Flamengo eSports", "INTZ", "KaBuM! e-S...
#> $ red         <chr> "Santos e-Sports", "PaiN Gaming", "INTZ", "FURIA Esport...
#> $ winner      <chr> "INTZ", "PaiN Gaming", "INTZ", "INTZ", "INTZ", "Vivo Ke...
#> $ ban_blue    <chr> "Volibear,Bard,Malphite", "Trundle,Sejuani,Bard", "Irel...
#> $ ban_red     <chr> "Thresh,Ezreal,Kalista", "Renekton,Lee Sin,Graves", "Le...
#> $ pick_blue   <chr> "Karthus,Trundle,Galio,Renekton,Twisted Fate", "Sett,Ol...
#> $ pick_red    <chr> "Ashe,Jax,Olaf,Sett,Lee Sin", "Akali,Ziggs,Senna,Wukong...
#> $ blue_roster <chr> "Tay, Shini, Envy, micaO, RedBert", "Tay, Shini, Envy, ...
#> $ red_roster  <chr> "JackPoT, Mewkyo, Rainbow, Juzo, Hawk", "Robo, CarioK, ...
#> $ league      <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "...
```

\[…\]

### Notes

If you use the data always remember to attribute the source to
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/pedrodrocha/cbloldataR/issues).
