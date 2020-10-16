#' Harvest data of CBLOL wins per player from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL wins per player
#'
#' @param playerid (character) Its very case sensitive and the playerid(s) have to be passed exactly as it is written in https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends
#'
#' @return A tibble containing: player, titles, titles_as_player, titles_as_coach and league.
#' @export
#'
#' @examples
#' titlesPlayer <- getData_titlesPlayer()
getData_titlesPlayer <- function(Playerid = NULL) {

  if(!is.null(Playerid)){
    if(typeof(Playerid) != "character"){
        type <- typeof(Playerid)

        rlang::abort(message = paste0("Playerid should be character, not ", type),
                     class = "class error")
    }
  }

  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  player <- xml2::read_html(url) %>%
    rvest::html_nodes(".wikitable") %>%
    rvest::html_table() %>%
    .[[5]] %>%
    dplyr::mutate(
      `Titles as Player` = stringr::str_extract(`Titles as Player`, "[0-9]{1}"),
      `Titles as Coach` = stringr::str_extract(`Titles as Coach`, "[0-9]{1}")
    ) %>%
    dplyr::select(-Team) %>%
    dplyr::filter(Titles > 0) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()

  player$league <- "CBLOL"

  if (!is.null(Playerid)) {
    player <- player %>%
      dplyr::filter(player %in% Playerid)

    if(nrow(player) == 0){
      rlang::abort(message = paste0("There is no player called ", Playerid),
                          class = "Player not found")
    } else {
      return(player)
    }

  } else {
    return(player)
  }


}

