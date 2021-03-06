#' Harvest data of CBLOL rosters from Leaguepedia
#'
#' Creates a tibble containing Leaguepedia data on CBLOL rosters on each edition
#'
#' @param Playerid (character)  Its very case sensitive and the playerid(s) have to be passed exactly as it is written at Leaguepedia
#' @param Team (character)  Its very case sensitive and the name(s) have to be passed exactly as it is written at Leaguepedia
#' @param Split (character)  Use if you want to specify a Split: "Split 1" or "Split 2".
#' @param Year (numeric) The year of the edition you want to access data (2015:2020)
#' @param Role (character)  Use if you want to specify one or more roles: "Top Laner", "Jungler", "Mid Laner", "Bot Laner", "Support" or "Coach".
#'
#' @return A tibble containing: country, playerid (id), name, team, year, split, role and league. If there is no data for the entry you specified it will return a message saying "There is no data for this entry".
#'
#' @export
#'
#' @examples
#' rosters <- getData_rosters(Year = c(2015:2020),Team = "INTZ",Role = "Support", Split = c("Split 1","Split 2"))
getData_rosters <- function(Playerid = NULL, Team = NULL, Split, Year, Role) {




  if(!is.null(Playerid)){
    if(typeof(Playerid) != "character"){
      type <- typeof(Playerid)

      rlang::abort(message = paste0("Playerid should be character, not ", type),
                   class = "class error")
    }
  }

  if(!is.null(Team)){
    if(typeof(Team) != "character"){
      type <- typeof(Team)

      rlang::abort(message = paste0("Team should be character, not ", type),
                   class = "class error")
    }
  }

  if(typeof(Split) != "character"){
    type <- typeof(Split)

    rlang::abort(message = paste0("Split should be character, not ", type),
                 class = "class error")
  }


  if(typeof(Role) != "character"){
    type <- typeof(Role)

    rlang::abort(message = paste0("Role should be character, not ", type),
                 class = "class error")
  }

  if(is.numeric(Year) == FALSE){
    type <- typeof(Year)

    rlang::abort(message = paste0("Year should be numeric, not ", type),
                 class = "class error")
  }

  if(Year == 2021){
    rlang::abort(message = "The season hasn't started yet")
  }




  url <- "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"


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



  links_elencos <- tibble::as_tibble(paste0(links_edicoes, "/Team_Rosters"))

  links_elencos %>%
    dplyr::filter(!stringr::str_detect(value, "Playoffs")) -> links_elencos

  links_elencos %>%
    as.list() %>%
    purrr::flatten_chr() -> links_elencos

  links_elencos %>%
    tibble::as_tibble() %>%
    dplyr::mutate(newvars = stringr::str_extract(value, "[0-9]{4}(.*)")) %>%
    tidyr::separate(col = newvars, into = c("year","split"), sep="/") %>%
    dplyr::mutate(year = as.integer(stringr::str_remove(year, "_Season"))) %>%
    dplyr::filter(year %in% Year & split %in% Split) %>%
    dplyr::select(value) %>%
    purrr::flatten_chr() -> links_elencos


  equipes_cblol <- function(url) {

    url_lido <- xml2::read_html(url)

    url_lido %>%
      rvest::html_nodes("h3") %>%
      rvest::html_nodes("span") %>%
      rvest::html_attr("id") %>%
      tibble::as_tibble() %>%
      dplyr::filter(!stringr::str_detect(value, ".E2")) %>%
      dplyr::mutate(
        value = stringr::str_replace_all(value, "_", " ")
      ) %>%
      as.list() %>%
      purrr::flatten_chr() -> equipes


    url_lido %>%
      rvest::html_nodes("div.wide-content-scroll") %>%
      rvest::html_nodes("table") %>%
      rvest::html_table(fill = TRUE) -> tables



    time <- function(equipe, tabela) {
      tabela <- tabela[c(1, 4, 5)]

      tabela %>%
        janitor::clean_names() %>%
        tibble::as_tibble() %>%
        dplyr::rename(country = x) %>%
        dplyr::mutate(team = equipe) -> tabela

      return(tabela)
    }

    elenco <- purrr::map2_dfr(.x = equipes, .y = tables, .f = time)

    elenco %>%
      dplyr::mutate(
        year = stringr::str_extract(url, "[0-9]{4}"),
        split = stringr::str_extract(
          url,
          "Split_[0-9]"
        )
      ) -> elenco

    url_lido %>%
      rvest::html_node("table") %>%
      rvest::html_nodes("tbody") %>%
      rvest::html_nodes(xpath = "//td[@class='extended-rosters-role']") %>%
      rvest::html_nodes("span") %>%
      rvest::html_attr("title") %>%
      tibble::as_tibble() %>%
      purrr::flatten_chr() -> role


    matrix(role, nrow = nrow(elenco), ncol = 1) %>%
      tibble::as_tibble() %>%
      dplyr::rename(role = "V1") -> role

    elenco <- cbind(elenco, role)


    return(elenco)
  }

  elencos <- purrr::map_dfr(links_elencos, equipes_cblol)


  elencos <- elencos %>%
    dplyr::mutate(
      league = "CBLOL",
      team = stringr::str_sub(team, start = 3),
      year = as.numeric(year)
    )


  if (!is.null(Playerid)) {
    elencos <- elencos %>%
      dplyr::filter(role %in% Role) %>%
      dplyr::filter(id %in% Playerid) %>%
      tibble::as_tibble()
  } else if (!is.null(Team)) {
    elencos <- elencos %>%
      dplyr::filter(role %in% Role) %>%
      dplyr::filter(team %in% Team) %>%
      tibble::as_tibble()
  } else if (!is.null(Playerid) & !is.null(Team)) {
    elencos <- elencos %>%
      dplyr::filter(role %in% Role) %>%
      dplyr::filter(team %in% Team) %>%
      dplyr::filter(id %in% Playerid) %>%
      tibble::as_tibble()
  } else {
    elencos <- elencos %>%
      dplyr::filter(role %in% Role) %>%
      tibble::as_tibble()
  }




  if (nrow(elencos) == 0) {
    rlang::abort(message = "There is no data for this entry")
  } else {
    return(elencos)
  }
}


