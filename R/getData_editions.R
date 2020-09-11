#' Harvest CBLOL editions data from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on each CBLOL edition.
#'
#' @param Split (character) The split you want to access data: "Split 1", "Split 2", "Split 1 Playoffs" or "Split 2 Playoffs".
#' @param Year (numeric) The year you want to access data (2015:2020).
#'
#' @return A tibble containing: year, tournament (split/playoff), prize pool (R$), winner, runner up and league.
#' @export
#'
#' @examples
#' editions <- getData_editions(Split = c("Split 1", "Split 2", "Split 1 Playoffs","Split 2 Playoffs"), Year = c(2015:2020))
getData_editions <- function(Split, Year){



  url = "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  edicoes <- xml2::read_html(url) %>%
    rvest::html_nodes(".wikitable") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::mutate(
      `Runner-Up` = stringr::str_sub(`Runner-Up`, start = 3),
      First = stringr::str_sub(First, start = 3),
      `Prize Pool` =  stringr::str_remove(`Prize Pool` , "R+\\$"),
      `Prize Pool` = stringr::str_remove(`Prize Pool`, "BRL \\$ "),
      `Prize Pool` = stringr::str_replace(`Prize Pool`,",","" ),
      `Prize Pool` = stringr::str_trim(`Prize Pool`),
      Tournament = stringr::str_remove(Tournament, "CBLOL [0-9]{4} "),
      Tournament = stringr::str_remove(Tournament, "[0-9]{4}"),
      Tournament = stringr::str_remove(Tournament, "Brazilian "),
      Tournament = stringr::str_trim(Tournament),
      Year = stringr::str_extract(Start,"[0-9]{4}")
    ) %>%
    dplyr::select(7,3:6) %>%
    tibble::as_tibble() %>%
    janitor::clean_names() %>%
    dplyr::na_if("") %>%
    dplyr::na_if("TBD")



  edicoes <- edicoes %>%
    dplyr::mutate(year = as.numeric(year),
                  prize_pool = as.numeric(prize_pool),
                  league = "CBLOL")


    edicoes <- edicoes %>%
      dplyr::filter(year %in% Year) %>%
      dplyr::filter(tournament %in% Split)





  return(edicoes)
}



