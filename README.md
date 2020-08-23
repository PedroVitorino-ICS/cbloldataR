
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cbloldataR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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
#> $ first      <chr> NA, "paiN Gaming", "KaBuM! e-Sports", "Vivo Keyd", "Flam...
#> $ runner_up  <chr> NA, "INTZ", "Flamengo eSports", "Flamengo eSports", "INT...
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
#> Rows: 21
#> Columns: 13
#> $ date        <chr> "2020-08-09", "2020-08-08", "2020-08-07", "2020-08-02",...
#> $ split       <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", ...
#> $ patch       <chr> "10.15", "10.15", "10.15", "10.15", "10.15", "10.14", "...
#> $ blue        <chr> "INTZ", "INTZ", "Prodigy Esports", "INTZ", "INTZ", "Fla...
#> $ red         <chr> "KaBuM! e-Sports", "Vivo Keyd", "INTZ", "Santos e-Sport...
#> $ winner      <chr> "KaBuM! e-Sports", "INTZ", "Prodigy Esports", "INTZ", "...
#> $ ban_blue    <chr> "Sett,Renekton,Orianna", "Syndra,Orianna,Galio", "Ezrea...
#> $ ban_red     <chr> "Trundle,Lux,Renekton", "Twisted Fate,LeBlanc,Graves", ...
#> $ pick_blue   <chr> "Twisted Fate,Akali,Sylas,Graves,Kalista", "Draven,Trun...
#> $ pick_red    <chr> "Caitlyn,Nidalee,LeBlanc,Sett,Lee Sin", "Varus,Azir,Mor...
#> $ blue_roster <chr> "Yupps, accez, Hanor, Mills, Bounty", "Tay, Shini, Envy...
#> $ red_roster  <chr> "Yang, Revolta, Tutsz, DudsTheBoy, Ceos", "Hidan, Miner...
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
#> $ champion <chr> "Syndra", "Azir", "Orianna", "Twisted Fate", "Zoe", "LeBla...
#> $ g        <dbl> 25, 24, 22, 14, 13, 10, 8, 6, 6, 5, 5, 5, 4, 3, 2, 2, 2, 2...
#> $ by       <dbl> 6, 8, 7, 5, 7, 5, 5, 4, 4, 3, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1...
#> $ w        <dbl> 9, 11, 14, 9, 7, 5, 4, 5, 3, 4, 2, 2, 2, 2, 1, 1, 0, 0, 1,...
#> $ l        <dbl> 16, 13, 8, 5, 6, 5, 4, 1, 3, 1, 3, 3, 2, 1, 1, 1, 2, 2, 0,...
#> $ wr       <chr> "36%", "45.8%", "63.6%", "64.3%", "53.8%", "50%", "50%", "...
#> $ k        <dbl> 2.84, 3.54, 3.95, 3.07, 3.77, 4.00, 2.75, 2.67, 4.50, 2.40...
#> $ d        <dbl> 2.88, 2.83, 2.41, 2.14, 2.62, 1.70, 2.75, 3.00, 3.17, 2.00...
#> $ a        <dbl> 5.36, 4.63, 6.86, 8.29, 5.69, 5.40, 9.75, 11.33, 4.00, 8.6...
#> $ kda      <dbl> 2.85, 2.88, 4.49, 5.30, 3.62, 5.53, 4.55, 4.67, 2.68, 5.50...
#> $ cs       <dbl> 302.60, 279.88, 300.64, 257.93, 235.69, 240.10, 244.63, 22...
#> $ cs_m     <dbl> 8.94, 8.89, 8.94, 7.72, 7.41, 7.45, 7.59, 6.69, 8.00, 7.10...
#> $ g_2      <dbl> 13.5, 12.8, 13.9, 13.7, 11.8, 12.1, 11.6, 12.2, 11.8, 12.8...
#> $ g_m      <dbl> 398, 408, 414, 411, 371, 376, 360, 365, 392, 352, 410, 423...
#> $ kpar     <chr> "61%", "64.7%", "75.3%", "71.9%", "65.4%", "63.9%", "80.6%...
#> $ ks       <chr> "21.1%", "28.1%", "27.5%", "19.5%", "26.1%", "27.2%", "17....
#> $ gs       <chr> "23%", "23.5%", "23%", "22.2%", "21.2%", "21.3%", "20.5%",...
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
#> Rows: 12
#> Columns: 22
#> $ player <chr> "CarioK", "Revolta", "Yampi", "st1ng", "Minerva", "Shini", "...
#> $ team   <chr> "PaiN Gaming", "KaBuM! e-Sports", "Prodigy Esports", "FURIA ...
#> $ role   <chr> "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", ...
#> $ g      <dbl> 21, 21, 21, 21, 21, 20, 20, 16, 4, 1, 1, 1
#> $ w      <dbl> 14, 12, 11, 8, 6, 12, 10, 9, 2, 0, 0, 0
#> $ l      <dbl> 7, 9, 10, 13, 15, 8, 10, 7, 2, 1, 1, 1
#> $ wr     <chr> "66.7%", "57.1%", "52.4%", "38.1%", "28.6%", "60%", "50%", "...
#> $ k      <dbl> 3.86, 3.67, 3.33, 2.05, 1.86, 2.15, 2.60, 2.50, 3.25, 1.00, ...
#> $ d      <dbl> 1.95, 2.14, 2.57, 2.90, 2.29, 2.50, 4.00, 3.38, 2.00, 5.00, ...
#> $ a      <dbl> 7.29, 7.76, 7.95, 6.43, 5.86, 6.25, 8.60, 8.00, 6.25, 5.00, ...
#> $ kda    <dbl> 5.71, 5.33, 4.39, 2.92, 3.38, 3.36, 2.80, 3.11, 4.75, 1.20, ...
#> $ cs     <dbl> 185.38, 190.43, 183.29, 172.00, 202.00, 182.60, 196.35, 168....
#> $ cs_m   <dbl> 5.92, 5.53, 5.35, 5.40, 6.29, 5.66, 5.78, 5.31, 5.30, 4.35, ...
#> $ g_2    <dbl> 11.0, 11.9, 11.5, 10.1, 10.8, 10.7, 11.5, 10.6, 10.1, 7.2, 1...
#> $ g_m    <dbl> 353, 345, 337, 319, 338, 333, 340, 334, 341, 267, 287, 311
#> $ kpar   <chr> "70.9%", "71.6%", "72%", "64.3%", "67.5%", "64.1%", "70%", "...
#> $ ks     <chr> "24.5%", "23%", "21.3%", "15.5%", "16.3%", "16.4%", "16.3%",...
#> $ gs     <chr> "19.1%", "19.3%", "19.1%", "18.6%", "19.8%", "18.8%", "19.2%...
#> $ cp     <dbl> 9, 10, 11, 11, 10, 8, 11, 8, 3, 1, 1, 1
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
#> Rows: 84
#> Columns: 21
#> $ player   <chr> "Cariok", "Cariok", "Cariok", "Cariok", "Cariok", "Cariok"...
#> $ role     <chr> "Jungle", "Jungle", "Jungle", "Jungle", "Jungle", "Jungle"...
#> $ year     <dbl> 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020...
#> $ split    <chr> "Split_2", "Split_2", "Split_2", "Split_2", "Split_2", "Sp...
#> $ champion <chr> "Graves", "Lee Sin", "Olaf", "Nidalee", "Volibear", "Trund...
#> $ g        <dbl> 5, 4, 3, 2, 2, 2, 1, 1, 1, 4, 4, 3, 3, 2, 1, 1, 1, 1, 1, 5...
#> $ w        <dbl> 5, 2, 3, 1, 1, 0, 1, 1, 0, 4, 2, 3, 1, 1, 1, 0, 0, 0, 0, 4...
#> $ l        <dbl> 0, 2, 0, 1, 1, 2, 0, 0, 1, 0, 2, 0, 2, 1, 0, 1, 1, 1, 1, 1...
#> $ wr       <chr> "100%", "50%", "100%", "50%", "50%", "0%", "100%", "100%",...
#> $ k        <dbl> 5.00, 3.75, 5.33, 1.50, 1.50, 2.50, 11.00, 2.00, 1.00, 5.2...
#> $ d        <dbl> 1.00, 3.75, 0.67, 2.00, 2.00, 2.00, 0.00, 3.00, 4.00, 0.75...
#> $ a        <dbl> 8.80, 7.25, 6.67, 6.50, 5.50, 7.00, 7.00, 11.00, 4.00, 6.5...
#> $ kda      <dbl> 13.80, 2.93, 18.00, 4.00, 3.50, 4.75, 18.00, 4.33, 1.25, 1...
#> $ cs       <dbl> 225.80, 164.50, 186.33, 213.00, 148.50, 161.00, 128.00, 22...
#> $ cs_m     <dbl> 7.12, 5.22, 6.20, 6.51, 6.13, 4.53, 5.81, 5.18, 4.65, 7.03...
#> $ g_2      <dbl> 12.8, 10.5, 11.4, 11.1, 7.9, 10.4, 10.5, 14.3, 8.6, 12.0, ...
#> $ g_m      <dbl> 402, 333, 380, 338, 326, 292, 475, 326, 271, 410, 294, 375...
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
  - Version: 0.0.0.9002 - 23.08.2020 - adding 2020/2 PlayOffs
