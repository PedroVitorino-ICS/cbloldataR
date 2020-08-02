getData_editions <- function(ano = NA){


  url = "https://lol.gamepedia.com/Circuit_Brazilian_League_of_Legends"

  edicoes <- read_html(url) %>%
    html_nodes(".wikitable") %>%
    html_table() %>%
    .[[1]] %>%
    mutate(
      `Runner-Up` = str_sub(`Runner-Up`, start = 3),
      First = str_sub(First, start = 3),
      `Prize Pool` =  str_remove(`Prize Pool` , "R+\\$"),
      `Prize Pool` = str_remove(`Prize Pool`, "BRL \\$ "),
      `Prize Pool` = str_replace(`Prize Pool`,",","" ),
      `Prize Pool` = str_trim(`Prize Pool`),
      Tournament = str_remove(Tournament, "CBLOL [0-9]{4} "),
      Tournament = str_remove(Tournament, "[0-9]{4}"),
      Tournament = str_remove(Tournament, "Brazilian "),
      Tournament = str_trim(Tournament),
      Year = str_extract(Start,"[0-9]{4}")
    ) %>%
    select(7,3:6) %>%
    as_tibble() %>%
    clean_names() %>%
    na_if("") %>%
    na_if("TBD")

  edicoes$league <- "Brazil"

  edicoes <- edicoes %>%
    mutate(year = as.numeric(year),
           prize_pool = as.numeric(prize_pool))

  if (!is.na(ano)){
    edicoes <- edicoes %>%
      filter(year %in% ano)
  } else{
    edicoes <- edicoes
  }

  return(edicoes)
}

