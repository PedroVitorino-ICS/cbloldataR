#'  Harvest from Leaguepedia data of players in games of CBLOL
#'
#' Creates a tibble containing Leaguepedia data of players in CBLOL games
#'
#'
#' @param Role (character) The lane of the player. It should contain at least one of the five roles: "Top", "Jungle", "Mid", "Bot" and "Support".
#' @param Year (numeric) The year you want to access data(2015:2020).
#' @param Split (character) The split you want to access data: "Split 1", "Split 2", "Split 1 Playoffs" or "Split 2 Playoffs".
#' @param Playerid (character) Its very case sensitive and the playerid(s) have to be passed exactly as in Leaguepedia.
#' @param Team (character) Its very case sensitive and the name(s) have to be passed exactly as in Leaguepedia
#'
#' @return A tibble containing: player, team, role,  number of games played, victories, defeats, win rate, kills, deaths, assists, KDA, CS per game, CS per minute, gold per game, gold per minute, kill participation, percentage of kills/team, percentage of gold/team, number of champions played, year, split, league.
#' @export
#'
#' @examples
#' players <- getData_players(Role = "Jungle", Year = 2020, Split = c("Split 2","Split 2 Playoffs"))
getData_players <- function(Role, Year, Split, Playerid = NULL, Team = NULL){


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




  message("Be patient, it may take a while...")


  Split <- stringr::str_replace_all(Split," ","_")

  url = "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"



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


  montagem_url_edicoes <- function(base){

    link <- paste0("https://lol.gamepedia.com",base)
    return(link)
  }


  links_edicoes <- purrr::map(base_edicoes,montagem_url_edicoes)
  links_edicoes <- purrr::flatten_chr(links_edicoes)

  links_edicoes %>%
    tibble::as_tibble() %>%
    dplyr::distinct() %>%
    purrr::flatten_chr() -> links_edicoes

  links_jogadores <- paste0(links_edicoes,"/Player_Statistics")


  links_jogadores %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      ano = as.integer(stringr::str_extract(value, "[0-9]{4}")),
      split = stringr::str_extract(value, "Split(.*)"),
      split = stringr::str_remove(split, "/(.*)")
    ) %>%
    dplyr::filter(ano %in% Year) %>%
    dplyr::filter(split %in% Split) %>%
    dplyr::select(-ano, -split) %>%
    purrr::flatten_chr() -> links_jogadores




  get_estatistica_jogadores <- function(url, Roles = Role) {

    url_lido <- xml2::read_html(url)

    url_lido %>%
      rvest::html_nodes("th") %>%
      rvest::html_nodes("table") %>%
      rvest::html_nodes("td") %>%
      rvest::html_nodes("a") %>%
      rvest::html_text() -> vector_roles


    links_por_role <- url_lido %>%
      rvest::html_nodes("th") %>%
      rvest::html_nodes("table") %>%
      rvest::html_nodes("td") %>%
      rvest::html_nodes("a") %>%
      rvest::html_attr("href")

    tibble::tibble(links = links_por_role,
                   role = vector_roles) -> links_por_role

    links_por_role %>%
      dplyr::filter(role %in% Roles) %>%
      dplyr::select(-role) %>%
      purrr::flatten_chr() -> links_por_role




    get_roles <- function(url_role) {

      url_role_lido <- xml2::read_html(url_role)

      url_role_lido %>%
        rvest::html_nodes("table") %>%
        rvest::html_table(fill = TRUE, header = FALSE) -> tab

      roles <- c("Top","Jungle","Mid","Bot","Support")

      role <- purrr::flatten_chr(stringr::str_extract_all(tab[[1]]$X1[1],roles))


      url_role_lido %>%
        rvest::html_node("table") %>%
        rvest::html_nodes("tbody") %>%
        rvest::html_nodes(xpath = "//td[@class='spstats-team']") %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("title") -> times



      tab[[1]] %>%
        janitor::row_to_names(remove_row = TRUE, row_number = 3) %>%
        janitor::clean_names() %>%
        dplyr::na_if("-") %>%
        dplyr::mutate(role = role) -> dat


      dat <- dat %>%
        dplyr::select(-x,-champs) %>%
        dplyr::mutate(team = times)

      return(dat)

    }

    dat2 <- purrr::map_dfr(links_por_role,get_roles)



    dat2 %>%
      dplyr::mutate(info = stringr::str_extract(url, "[0-9]{4}(.*)")) -> dat2

    dat2 %>%
      tidyr::separate(col = info,
                      into = c("year","split"), sep = "/", remove = TRUE) %>%
      dplyr::mutate(year = stringr::str_extract(year, "[0-9]{4}")) -> dat2

    estatistica_jogadores <- dat2 %>%
      dplyr::rename(player = "x_2") %>%
      dplyr::select(1,19,18,2:17,20,21)

    return(estatistica_jogadores)

  }


  estatistica_jogadores_geral <- purrr::map_dfr(links_jogadores,get_estatistica_jogadores)


  if (!is.null(Playerid)) {

    estatistica_jogadores_geral <- estatistica_jogadores_geral %>%
      dplyr::filter(player %in% Playerid) %>%
      dplyr::mutate(league = "CBLOL") %>%
      tibble::as_tibble()


  } else if (!is.null(Team)) {

    estatistica_jogadores_geral <- estatistica_jogadores_geral %>%
      dplyr::filter(team %in% Team) %>%
      dplyr::mutate(league = "CBLOL") %>%
      tibble::as_tibble()

  } else if (!is.null(Playerid) & !is.null(Team)) {

    estatistica_jogadores_geral <- estatistica_jogadores_geral %>%
      dplyr::filter(team %in% Team) %>%
      dplyr::filter(player %in% Playerid) %>%
      dplyr::mutate(league = "CBLOL") %>%
      tibble::as_tibble()

  } else {

    estatistica_jogadores_geral <- estatistica_jogadores_geral %>%
      dplyr::mutate(league = "CBLOL") %>%
      tibble::as_tibble()

  }





  estatistica_jogadores_geral <- estatistica_jogadores_geral %>%
    dplyr::mutate(g = as.numeric(g),
                  w = as.numeric(w),
                  l = as.numeric(l),
                  k = as.numeric(k),
                  d = as.numeric(d),
                  a = as.numeric(a),
                  kda = as.numeric(kda),
                  cs = as.numeric(cs),
                  cs_m = as.numeric(cs_m),
                  g_2 = as.numeric(g_2),
                  g_m = as.numeric(g_m),
                  cp = as.numeric(cp),
                  year = as.numeric(year))



  if (nrow(estatistica_jogadores_geral) == 0) {
    rlang::abort(message = "There is no data for this entry")
  } else {
    return(estatistica_jogadores_geral)
  }

}
