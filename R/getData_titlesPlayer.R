#' Harvest data of CBLOL wins per player from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL wins per player
#'
#' @param playerid character. Its very sensitive and the playerid(s) have to be passed exactly as it is written at https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends
#'
#' @return A tibble containing: player, titles, titles_as_player, titles_as_coach and league.
#' @export
#'
#' @examples
#' a <- getData_titlesPlayer()
#' b <- getData_titlesPlayer(playerid = "brTT")
#' c <- getData_titlesPlayer(playerid = c("Kami","brTT"))
getData_titlesPlayer <- function(playerid = NULL) {
  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  player <- xml2::read_html(url) %>%
    rvest::html_nodes(".wikitable") %>%
    rvest::html_table() %>%
    .[[4]] %>%
    dplyr::mutate(
      `Titles as Player` = stringr::str_extract(`Titles as Player`, "[0-9]{1}"),
      `Titles as Coach` = stringr::str_extract(`Titles as Coach`, "[0-9]{1}")
    ) %>%
    dplyr::select(-Team) %>%
    dplyr::filter(Titles > 0) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  player$league <- "CBLOL"

  if (!is.null(playerid)) {
    player <- player %>%
      dplyr::filter(player %in% playerid)
  } else {
    player <- player
  }

  return(player)
}

