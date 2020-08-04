#' Harvest from Leaguepedia data of players per Champion played in games of CBLOL
#'
#' Creates a tibble containing Leaguepedia data of players  per Champion played in games of CBLOL
#'
#' @param Role (character) The lane of the player. It should contain at least one of the five roles: "Top", "Jungle", "Mid", "AD Carry" and "Support".
#' @param Year (numeric) The year you want to access data.
#' @param Split (character) The split you want to access data: "Split_1", "Split_2", "Split_1_Playoffs" or "Split_2_Playoffs".
#' @param Playerid (character) The player you want to access data. By default it returns data on every player. Its very case sensitive and the playerid(s) should be passed exactly as in Leaguepedia
#' @param Champion (character) The champion you want to access data. By default it returns data on every champion. Its very case sensitive and the champion(s) name(s) should be passed exactly as in Leaguepedia
#'
#' @return A tibble containing: player, role, year, split, champion, number of games played with the champion, victories, defeats, win rate, kills, deaths, assists, KDA, CS per game, CS per minute, gold per game, gold per minute, kill participation, percentage of kills/team, percentage of gold/team and league.
#' @export
#'
#' @examples
#' a <- getData_playersChampion()
#' b <- getData_playersChampion(
#'   Role = "Top",
#'   Year = 2020,
#'   Split = "Split_2",
#'   Champion = "Renekton"
#'   )
#' c <- getData_playersChampion(
#' Role = c("Mid","AD Carry"),
#' Year = 2020,
#' Champion = "Syndra"
#' )
#' d <- getData_playersChampion(
#' Role = "AD Carry",
#' Playerid = "BrTT",
#' Year = 2020,
#' Split = "Split_2"
#' )

getData_playersChampion <- function(Role = c("Top","Jungle","Mid","AD Carry","Support"), Year = 2015:2020, Split = c("Split_1", "Split_1_Playoffs", "Split_2", "Split_2_Playoffs"), Playerid = NULL, Champion = NULL){

  url = "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  message("Be patient, it may take a while...")

  old <- options(warn = 0)
  options(warn=-1)


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

  # >>>>>>>>>>>>> tirando playoffs 2/2020 PORQUE AINDA NÃO ACONTECEU <<<<<<<<<<<<<<<<<<<<<<<< #
  links_jogadores_sem_playoffs_2_2020 <- links_jogadores[-4]

  links_jogadores_sem_playoffs_2_2020 %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      ano = as.integer(stringr::str_extract(value, "[0-9]{4}")),
      split = stringr::str_extract(value, "Split(.*)"),
      split = stringr::str_remove(split, "/(.*)")
    ) %>%
    dplyr::filter(ano %in% Year) %>%
    dplyr::filter(split %in% Split) %>%
    dplyr::select(-ano, -split) %>%
    purrr::flatten_chr() -> links_jogadores_sem_playoffs_2_2020

  ############ <<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>> ######################


  pb <- dplyr::progress_estimated(length(links_jogadores_sem_playoffs_2_2020))

  get_estatistica_jogadores <- function(url,Roles = Role) {

    pb$tick()$print()


    xml2::read_html(url) %>%
      rvest::html_nodes("th") %>%
      rvest::html_nodes("table") %>%
      rvest::html_nodes("td") %>%
      rvest::html_nodes("a") %>%
      rvest::html_text() -> vector_roles


    links_por_role <- xml2::read_html(url) %>%
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



    get_roles <- function(url_role){


      xml2::read_html(url_role) %>%
          rvest::html_nodes(xpath = "//td[@class='spstats-bold']") %>%
          rvest::html_nodes("a") %>%
          rvest::html_attr("href") -> url_por_jogador




      get_jogador <- function(url_jogador){


        xml2::read_html(url_jogador) %>%
          rvest::html_nodes("table") %>%
          rvest::html_table(fill = TRUE, header = FALSE) %>%
          .[[1]] -> tab2

        tab2 %>%
          dplyr::select(X1) %>%
          tidyr::separate(X1,into = c("player", "role"), sep = ";",remove = TRUE) %>%
          dplyr::slice(1) %>%
          dplyr::mutate(player = stringr::str_remove(player,"Player:"),
                 player = stringr::str_trim(player),
                 role = stringr::str_remove(role,"Role:"),
                 role = stringr::str_trim(role)) %>%
          purrr::flatten_chr() -> player_role

        player <- player_role[1]

        role <- player_role[2]


        tab2 %>%
          janitor::row_to_names(remove_row = TRUE, row_number = 3) %>%
          janitor::clean_names() %>%
          dplyr::na_if("-") %>%
          dplyr::mutate(role = role,
                 player = player) %>%
          dplyr::filter(!stringr::str_detect(champion, "Overall:")) %>%
          dplyr::filter(!stringr::str_detect(champion, "Total:")) -> dat

        return(dat)
      }

      base <- get_jogador(url_por_jogador[1])

      base %>%
        dplyr::filter(stringr::str_detect(champion,"null_")) -> base

      iter <- url_por_jogador %>%
        tibble::as_tibble()

      for (i in iter$value){
        # print(i)
        passagem <- get_jogador(i)
        base <- rbind(base,passagem)

      }

      return(base)

    }



    dat3 <- purrr::map_dfr(links_por_role,get_roles)

    dat3 %>%
      dplyr::mutate(info = stringr::str_extract(url, "[0-9]{4}(.*)")) -> dat3

    dat3 %>%
      tidyr::separate(col = info,
               into = c("year","split"), sep = "/", remove = TRUE) %>%
      dplyr::mutate(year = stringr::str_extract(year, "[0-9]{4}")) -> dat3

    estatistica_jogadores_campeao <- dat3 %>%
      dplyr::select(18,17,19,20,1:16)

    return(estatistica_jogadores_campeao)

  }

  estatistica_jogadores_campeao <- purrr::map_dfr(links_jogadores_sem_playoffs_2_2020,get_estatistica_jogadores)



  estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
    dplyr::mutate(player = stringr::str_remove(player,"\\((.*)"))




  if (!is.null(Playerid)) {
    estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
      dplyr::filter(player %in% Playerid) %>%
      dplyr::mutate(league = "CBLOL") %>%
      tibble::as_tibble()

  } else if (!is.null(Champion)) {

    estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
      tibble::as_tibble() %>%
      dplyr::filter(champion %in% Champion) %>%
      dplyr::mutate(league = "CBLOL")

  } else if (!is.null(Playerid) & !is.null(Champion)) {

    estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
      dplyr::filter(champion %in% Champion) %>%
      dplyr::mutate(league = "CBLOL") %>%
      dplyr::filter(player %in% Playerid) %>%
      tibble::as_tibble()

  } else {

    estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
      tibble::as_tibble() %>%
      dplyr::mutate(league = "CBLOL")
  }


  estatistica_jogadores_campeao <- estatistica_jogadores_campeao %>%
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
                  year = as.numeric(year))



  on.exit(options(old), add = TRUE)

  if (nrow(estatistica_jogadores_campeao) == 0) {
    message("There is no data for this entry")
  } else {
    return(estatistica_jogadores_campeao)
  }

}

