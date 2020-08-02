#' Harvest data of CBLOL wins per Organization from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL wins per Organization
#'
#'
#'
#' @param Team character. Its very case sensitive and the name(s) have to be passed exactly as it is written in https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends
#'
#' @return A tibble containing: team, x1st_place, x2nd_place, x3rd_4th_place, league
#' @export
#'
#' @examples
#' a <- getData_titlesOrg()
#' b <- getData_titlesOrg(Team = "INTZ")
#' c <- getData_titlesOrg(Team = c("INTZ","paiN Gaming"))
getData_titlesOrg <- function(Team = NULL) {
  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  titles <- xml2::read_html(url) %>%
    rvest::html_nodes(".wikitable") %>%
    rvest::html_table() %>%
    .[[3]] %>%
    dplyr::mutate(
      `1st Place` = stringr::str_extract(`1st Place`, "[0-9]{1}"),
      `2nd Place` = stringr::str_extract(`2nd Place`, "[0-9]{1}"),
      `3rd/4th Place` = stringr::str_extract(`3rd/4th Place`, "[0-9]{1}"),
      Team = stringr::str_sub(Team, start = 3)
    ) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  if (!is.null(Team)) {
    titles <- titles %>%
      dplyr::filter(team %in% Team)
  } else {
    titles <- titles
  }

  titles$league <- "CBLOL"
  return(titles)
}



