#' Harvest data of CBLOL wins per Organization from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL wins per Organization
#'
#'
#' @param Team (character) Its very case sensitive and the name(s) have to be passed exactly as it is written in https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends
#'
#' @return A tibble containing: team, x1st_place, x2nd_place, x3rd_4th_place, league
#' @export
#'
#' @examples
#' titlesOrg <- getData_titlesOrg()
getData_titlesOrg <- function(Team = NULL) {

  if(!is.null(Team)){
    if(typeof(Team) != "character"){
      type <- typeof(Team)

      rlang::abort(message = paste0("Team should be character, not ", type),
                   class = "class error")
    }
  }


  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  titles <- xml2::read_html(url) %>%
    rvest::html_nodes(".wikitable") %>%
    rvest::html_table() %>%
    .[[4]] %>%
    dplyr::mutate(
      `1st Place` = stringr::str_extract(`1st Place`, "[0-9]{1}"),
      `2nd Place` = stringr::str_extract(`2nd Place`, "[0-9]{1}"),
      `3rd/4th Place` = stringr::str_extract(`3rd/4th Place`, "[0-9]{1}"),
      Team = stringr::str_sub(Team, start = 3)
    ) %>%
    janitor::clean_names() %>%
    tibble::as_tibble()


  titles$league <- "CBLOL"

  if (!is.null(Team)) {
    titles <- titles %>%
      dplyr::filter(team %in% Team)

    if(nrow(titles) == 0){
      rlang::abort(message = paste0("There is no team called ", Team),
                   class = "Team not found")
    } else {
      return(titles)
    }
  } else {
    titles <- titles
  }






  return(titles)
}



