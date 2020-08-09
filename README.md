
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

The package offer access to tidy tables of data that you can harvest
from
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

**1)** `getData_editions()`: creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on each CBLOL edition.

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

**2)** `getData_titlesOrg()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on CBLOL tournament wins per Organization.

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

**3)** `getData_titlesPlayer()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on CBLOL tournament wins per player.

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

**4)** `getData_rosters()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on CBLOL rosters on each edition.

``` r
rosters <- getData_rosters(Team = "paiN Gaming", Role = "Support")
#> It may take a while...

glimpse(rosters)
#> Rows: 10
#> Columns: 8
#> $ country <chr> "KR", "BR", "BR", "BR", "BR", "BR", "BR", "BR", "EU", "EU"
#> $ id      <chr> "Key", "esA", "esA", "Loop", "Loop", "Loop", "Picoca", "Zir...
#> $ name    <chr> "Kim Han-gi (<U+AE40><U+D55C><U+AE30>)", "Eidi Yanagimachi", "Eidi Yanagimachi",...
#> $ team    <chr> "paiN Gaming", "paiN Gaming", "paiN Gaming", "paiN Gaming",...
#> $ year    <dbl> 2020, 2020, 2019, 2018, 2017, 2017, 2016, 2016, 2015, 2015
#> $ split   <chr> "Split_1", "Split_2", "Split_2", "Split_1", "Split_1", "Spl...
#> $ role    <chr> "Support", "Support", "Support", "Support", "Support", "Sup...
#> $ league  <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLO...
```

**5)** `getData_games()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on CBLOL official games.

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
#> Rows: 20
#> Columns: 13
#> $ date        <chr> "2020-08-08", "2020-08-07", "2020-08-02", "2020-08-01",...
#> $ split       <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", ...
#> $ patch       <chr> "10.15", "10.15", "10.15", "10.15", "10.14", "10.14", "...
#> $ blue        <chr> "INTZ", "Prodigy Esports", "INTZ", "INTZ", "Flamengo eS...
#> $ red         <chr> "Vivo Keyd", "INTZ", "Santos e-Sports", "PaiN Gaming", ...
#> $ winner      <chr> "INTZ", "Prodigy Esports", "INTZ", "PaiN Gaming", "INTZ...
#> $ ban_blue    <chr> "Camille,Ekko,Kassadin", "Ashe,Olaf,Jarvan IV", "Irelia...
#> $ ban_red     <chr> "Senna,Sona,Renekton", "Senna,Syndra,Kalista", "Kalista...
#> $ pick_blue   <chr> "Jax,Volibear,Lee Sin,Renekton,Kalista", "Caitlyn,Ezrea...
#> $ pick_red    <chr> "None,None,Senna,Camille,Sett", "Gangplank,Jax,Senna,Re...
#> $ blue_roster <chr> "Tay, Shini, Envy, micaO, RedBert", "fNb, Yampi, dyNque...
#> $ red_roster  <chr> "Hidan, Minerva, NOsFerus, Drop, Professor", "Tay, Shin...
#> $ league      <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "...
```

**6)** `getData_champion()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data on champions played in CBLOL official games.

``` r
champion <- getData_champion(Role = "Mid",
                              Year = 2020,
                              Split = "Split_2")
#> It may take a while...
glimpse(champion)
#> Rows: 28
#> Columns: 21
#> $ champion <chr> "Syndra", "Orianna", "Azir", "Twisted Fate", "Zoe", "LeBla...
#> $ g        <dbl> 25, 22, 22, 14, 12, 10, 8, 6, 5, 5, 5, 4, 3, 3, 2, 2, 1, 1...
#> $ by       <dbl> 6, 7, 6, 5, 7, 5, 5, 4, 3, 4, 3, 2, 1, 2, 2, 2, 1, 1, 1, 1...
#> $ w        <dbl> 9, 14, 11, 9, 6, 5, 4, 3, 4, 2, 2, 3, 2, 2, 1, 0, 1, 1, 1,...
#> $ l        <dbl> 16, 8, 11, 5, 6, 5, 4, 3, 1, 3, 3, 1, 1, 1, 1, 2, 0, 0, 0,...
#> $ wr       <chr> "36%", "63.6%", "50%", "64.3%", "50%", "50%", "50%", "50%"...
#> $ k        <dbl> 2.84, 3.95, 3.64, 3.07, 3.92, 4.00, 2.75, 4.50, 2.40, 5.80...
#> $ d        <dbl> 2.88, 2.41, 2.91, 2.14, 2.83, 1.70, 2.75, 3.17, 3.00, 3.00...
#> $ a        <dbl> 5.36, 6.86, 5.00, 8.29, 5.33, 5.40, 9.75, 4.00, 11.00, 3.6...
#> $ kda      <dbl> 2.85, 4.49, 2.97, 5.30, 3.26, 5.53, 4.55, 2.68, 4.47, 3.13...
#> $ cs       <dbl> 302.60, 300.64, 280.41, 257.93, 231.33, 240.10, 244.63, 24...
#> $ cs_m     <dbl> 8.94, 8.94, 8.86, 7.72, 7.26, 7.45, 7.59, 8.00, 6.56, 8.33...
#> $ g_2      <dbl> 13.5, 13.9, 13.0, 13.7, 11.7, 12.1, 11.6, 11.8, 12.1, 12.5...
#> $ g_m      <dbl> 398, 414, 410, 411, 366, 376, 360, 392, 354, 410, 423, 340...
#> $ kpar     <chr> "61%", "75.3%", "65.3%", "71.9%", "66.1%", "63.9%", "80.6%...
#> $ ks       <chr> "21.1%", "27.5%", "27.5%", "19.5%", "28%", "27.2%", "17.7%...
#> $ gs       <chr> "23%", "23%", "23.4%", "22.2%", "21.1%", "21.3%", "20.5%",...
#> $ role     <chr> "Mid", "Mid", "Mid", "Mid", "Mid", "Mid", "Mid", "Mid", "M...
#> $ year     <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020...
#> $ split    <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", "Sp...
#> $ league   <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBL...
```

**7)** `getData_players()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data of players in CBLOL official games.

``` r
players <- getData_players(Role = "Jungle",
                           Year = 2020,
                           Split = "Split_2")
#> Be patient, it may take a while...

glimpse(players)
#> Rows: 11
#> Columns: 22
#> $ player <chr> "CarioK", "Shini", "Revolta", "Yampi", "St1ng", "Minerva", "...
#> $ team   <chr> "PaiN Gaming", "INTZ", "KaBuM! e-Sports", "Prodigy Esports",...
#> $ role   <chr> "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", ...
#> $ g      <dbl> 20, 20, 20, 20, 20, 20, 19, 15, 4, 1, 1
#> $ w      <dbl> 14, 12, 11, 10, 7, 6, 10, 8, 2, 0, 0
#> $ l      <dbl> 6, 8, 9, 10, 13, 14, 9, 7, 2, 1, 1
#> $ wr     <chr> "70%", "60%", "55%", "50%", "35%", "30%", "52.6%", "53.3%", ...
#> $ k      <dbl> 4.00, 2.15, 3.60, 3.30, 1.90, 1.70, 2.53, 2.20, 3.25, 1.00, ...
#> $ d      <dbl> 1.85, 2.50, 2.25, 2.60, 3.00, 2.30, 3.84, 3.40, 2.00, 4.00, ...
#> $ a      <dbl> 7.45, 6.25, 7.75, 7.95, 6.35, 5.95, 8.84, 7.87, 6.25, 8.00, ...
#> $ kda    <dbl> 6.19, 3.36, 5.04, 4.33, 2.75, 3.33, 2.96, 2.96, 4.75, 2.25, ...
#> $ cs     <dbl> 187.30, 182.60, 190.15, 185.30, 170.20, 199.20, 199.95, 168....
#> $ cs_m   <dbl> 5.99, 5.66, 5.46, 5.39, 5.35, 6.20, 5.84, 5.28, 5.30, 4.57, ...
#> $ g_2    <dbl> 11.2, 10.7, 11.9, 11.6, 10.0, 10.8, 11.6, 10.5, 10.1, 10.8, 9.5
#> $ g_m    <dbl> 357, 333, 341, 337, 315, 335, 340, 329, 341, 287, 311
#> $ kpar   <chr> "70.5%", "64.1%", "71.2%", "72.8%", "65.2%", "67.1%", "70.1%...
#> $ ks     <chr> "24.6%", "16.4%", "22.6%", "21.4%", "15%", "14.9%", "15.6%",...
#> $ gs     <chr> "19.2%", "18.8%", "19.2%", "19.2%", "18.5%", "19.6%", "19.2%...
#> $ cp     <dbl> 8, 8, 10, 11, 11, 10, 11, 7, 3, 1, 1
#> $ year   <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, ...
#> $ split  <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", "Spli...
#> $ league <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL...
```

**8)** `getData_playersChampion()`: Creates a tibble containing
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends)
data of players per Champion played in official games of CBLOL.

``` r
players_champion <- getData_playersChampion(Role = "Jungle",
                                            Split = "Split_2",
                                            Year = 2020)
#> Be patient, it may take a while...
glimpse(players_champion)
#> Rows: 81
#> Columns: 21
#> $ player   <chr> "Cariok", "Cariok", "Cariok", "Cariok", "Cariok", "Cariok"...
#> $ role     <chr> "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", "Jungle"...
#> $ year     <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020...
#> $ split    <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", "Sp...
#> $ champion <chr> "Graves", "Lee Sin", "Olaf", "Nidalee", "Volibear", "Trund...
#> $ g        <dbl> 5, 4, 3, 2, 2, 2, 1, 1, 5, 4, 3, 3, 2, 1, 1, 1, 4, 4, 3, 2...
#> $ w        <dbl> 5, 2, 3, 1, 1, 0, 1, 1, 4, 3, 2, 1, 0, 1, 1, 0, 4, 2, 3, 1...
#> $ l        <dbl> 0, 2, 0, 1, 1, 2, 0, 0, 1, 1, 1, 2, 2, 0, 0, 1, 0, 2, 0, 1...
#> $ wr       <chr> "100%", "50%", "100%", "50%", "50%", "0%", "100%", "100%",...
#> $ k        <dbl> 5.00, 3.75, 5.33, 1.50, 1.50, 2.50, 11.00, 2.00, 1.20, 2.2...
#> $ d        <dbl> 1.00, 3.75, 0.67, 2.00, 2.00, 2.00, 0.00, 3.00, 2.00, 2.00...
#> $ a        <dbl> 8.80, 7.25, 6.67, 6.50, 5.50, 7.00, 7.00, 11.00, 5.00, 9.7...
#> $ kda      <dbl> 13.80, 2.93, 18.00, 4.00, 3.50, 4.75, 18.00, 4.33, 3.10, 6...
#> $ cs       <dbl> 225.80, 164.50, 186.33, 213.00, 148.50, 161.00, 128.00, 22...
#> $ cs_m     <dbl> 7.12, 5.22, 6.20, 6.51, 6.13, 4.53, 5.81, 5.18, 4.96, 5.52...
#> $ g_2      <dbl> 12.8, 10.5, 11.4, 11.1, 7.9, 10.4, 10.5, 14.3, 10.4, 11.2,...
#> $ g_m      <dbl> 402, 333, 380, 338, 326, 292, 475, 326, 308, 328, 387, 343...
#> $ kpar     <chr> "69%", "74.6%", "73.5%", "80%", "56%", "70.4%", "75%", "61...
#> $ ks       <chr> "25%", "25.4%", "32.7%", "15%", "12%", "18.5%", "45.8%", "...
#> $ gs       <chr> "20.4%", "18.8%", "19.6%", "19.3%", "18.1%", "17%", "21.9%...
#> $ league   <chr> "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBLOL", "CBL...
```

### Notes

If you use the data always remember to attribute the source to
[Leaguepedia](https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends).

If you encounter a clear bug, please file a minimal reproducible example
on [GitHub](https://github.com/pedrodrocha/cbloldataR/issues).

### Version Control

  - Version: 0.0.0.9001 - 08.08.2020 - fixing issues with AD Carry
    entries in both `getData_players()` and `getData_playersChampion()`.
