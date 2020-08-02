% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getData_games.R
\name{getData_games}
\alias{getData_games}
\title{Harvest data of CBLOL games from Leaguepedia}
\usage{
getData_games(
  Team = NULL,
  Year = 2015:2020,
  Split = c("Split_1", "Split_1_Playoffs", "Split_2", "Split_2_Playoffs")
)
}
\arguments{
\item{Team}{character. Its very case sensitive and the name(s) have to be passed exactly as it is written in Leaguepedia}

\item{Year}{numeric. The year of the edition you want to access data.}

\item{Split}{character. Use if you want to specify a Split: "Split_1", "Split_2", "Split_1_Playoffs","Split_2_Playoffs"}
}
\value{
A tibble containing: date, split, patch, blue team, red team, winner, bans blue, bans red, picks blue, picks red, blue roster, red roster and league.
}
\description{
Creates a tibble containing Leaguepedia data on CBLOL games.
}
\examples{
a <- getData_games()
b <- getData_games(Year = 2013)
c <- getData_games(Team = "INTZ",Year = c(2019,2020), Split = "Split_1")
}