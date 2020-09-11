#' Harvest data of CBLOL games from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL games.
#'
#' @param Team (character) Its very case sensitive and the name(s) have to be passed exactly as it is written in Leaguepedia
#' @param Year (numeric) The year of the edition you want to access data (2015:2020).
#' @param Split (character) Use if you want to specify a Split: "Split 1", "Split 2", "Split 1 Playoffs","Split 2 Playoffs"
#'
#' @return A tibble containing: date, split, patch, blue team, red team, winner, bans blue, bans red, picks blue, picks red, blue roster, red roster and league.
#' @export
#'
#' @examples
#' games <- getData_games(Year = 2020, Split = c("Split 2","Split 2 Playoffs"))
getData_games <- function(Team = NULL, Year, Split) {
  message("It may take a while...")

  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  old <- options(warn = 0)
  options(warn = -1)

  Split <- stringr::str_replace_all(Split," ","_")

  xml2::read_html(url) %>%
    rvest::html_nodes("td") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    tibble::as_tibble() %>%
    dplyr::filter(stringr::str_detect(value, "/CBLOL")) %>%
    dplyr::filter(stringr::str_detect(value, "Split")) %>%
    dplyr::filter(!stringr::str_detect(value, "Promotion")) %>%
    dplyr::filter(!stringr::str_detect(value, "Qualifiers")) %>%
    as.list() -> base_edicoes


  montagem_url_edicoes <- function(base) {
    link <- paste0("https://lol.gamepedia.com", base)
    return(link)
  }


  links_edicoes <- purrr::map(base_edicoes, montagem_url_edicoes)
  links_edicoes <- purrr::flatten_chr(links_edicoes)

  links_edicoes %>%
    tibble::as_tibble() %>%
    dplyr::distinct() %>%
    purrr::flatten_chr() -> links_edicoes
  links_historico <- paste0(links_edicoes, "/Match_History")



  links_historico %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      ano = as.integer(stringr::str_extract(value, "[0-9]{4}")),
      split = stringr::str_extract(value, "Split(.*)"),
      split = stringr::str_remove(split, "/(.*)")
    ) %>%
    dplyr::filter(ano %in% Year) %>%
    dplyr::filter(split %in% Split) %>%
    dplyr::select(-ano, -split) %>%
    purrr::flatten_chr() -> links_historico

  ############ <<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> ######################

  get_historico <- function(url) {


    xml2::read_html(url) %>%
      rvest::html_nodes("table") %>%
      rvest::html_nodes("th") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href") %>%
      tibble::as_tibble() %>%
      dplyr::filter(stringr::str_detect(value, "RunQuery")) %>%
      purrr::flatten_chr() -> query_link


    xml2::read_html(query_link) %>%
      rvest::html_nodes("div.wide-content-scroll") %>%
      rvest::html_nodes("table") %>%
      rvest::html_table(fill = TRUE, header = FALSE) -> tables


    tables[[1]] %>%
      janitor::row_to_names(remove_row = TRUE, row_number = 2) %>%
      janitor::clean_names() %>%
      dplyr::slice(-1) -> tab1

    tab1 %>%
      dplyr::select(-c(3:9)) -> tab1


    xml2::read_html(query_link) %>%
      rvest::html_node("table") %>%
      rvest::html_nodes("tr") %>%
      rvest::html_text() -> nrows

    nrows <- length(nrows[-c(1:3)])



    xml2::read_html(query_link) %>%
      rvest::html_node("table") %>%
      rvest::html_nodes("tbody") %>%
      rvest::html_nodes(xpath = "//td[@class='mhgame-result']") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("title") %>%
      tibble::as_tibble() %>%
      dplyr::filter(!stringr::str_detect(value, "Patch")) %>%
      purrr::flatten_chr() -> result


    matrix(result, nrow = nrows, ncol = 3, byrow = TRUE) %>%
      tibble::as_tibble() %>%
      dplyr::rename(
        blue = "V1",
        red = "V2",
        winner = "V3"
      ) -> result


    tab2 <- cbind(tab1, result)


    xml2::read_html(query_link) %>%
      rvest::html_node("table") %>%
      rvest::html_nodes("tbody") %>%
      rvest::html_nodes(xpath = "//td[@class='mhgame-bans']") %>%
      rvest::html_nodes("span") %>%
      rvest::html_attr("title") -> picks_bans


    matrix(picks_bans, nrow = nrows, ncol = 16, byrow = TRUE) %>%
      tibble::as_tibble() %>%
      tidyr::unite("ban_blue", V1:V3, sep = ",") %>%
      tidyr::unite("ban_red", V4:V6, sep = ",") %>%
      tidyr::unite("pick_blue", V7:V11, sep = ",") %>%
      tidyr::unite("pick_red", V12:V16, sep = ",") -> picks_bans


    historico <- cbind(tab2, picks_bans)


    historico <- historico %>%
      dplyr::rename(patch = p) %>%
      dplyr::mutate(info = stringr::str_extract(url, "[0-9]{4}(.*)")) %>%
      tidyr::separate(col = info, into = c("ano", "split"), sep = "/", remove = TRUE) %>%
      dplyr::select(date, split, patch, blue, red, winner, ban_blue, ban_red, pick_blue, pick_red, blue_roster, red_roster)




    return(historico)
  }

  historico <- purrr::map_dfr(links_historico, get_historico)


  historico %>%
    dplyr::na_if("") -> historico

  if (!is.null(Team)) {
    historico <- historico %>%
      tibble::as_tibble() %>%
      dplyr::filter(blue %in% Team | red %in% Team) %>%
      dplyr::mutate(league = "CBLOL")
  } else {
    historico <- historico %>%
      tibble::as_tibble() %>%
      dplyr::mutate(league = "CBLOL")
  }



  on.exit(options(old), add = TRUE)

  if (nrow(historico) == 0) {
    message("There is no data for this entry")
  } else {
    return(historico)
  }
}
