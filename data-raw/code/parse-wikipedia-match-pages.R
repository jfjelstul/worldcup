# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(lubridate)
library(rvest)

# Function to extract team tables
extract_team_data <- function(html_table) {

  # Extract and clean HTML table -----------------------------------------------

  # Convert HTML table to tibble
  tr <- html_table |>
    html_elements("tr")
  table <- list()
  j <- 1
  for (i in 1:length(tr)) {
    td <- tr[i] |>
      html_elements("td") |>
      html_text() |>
      str_squish()
    td <- td[td != ""]
    if (length(td) == 3) {
      text <- ""
    }
    if (length(td) == 4) {
      text <- td[4]
      td <- td[-4]
    }
    if (length(td) == 5) {
      text <- c(td[4], td[5])
      td <- td[-c(4, 5)]
    }
    if (length(td) == 6) {
      text <- c(td[4], td[5], td[6])
      td <- td[-c(4, 5, 6)]
    }
    if (length(td) == 7) {
      text <- c(td[4], td[5], td[6], td[7])
      td <- td[-c(4, 5, 6, 7)]
    }
    if (length(td) >= 3) {
      text <- text[text != ""]
      td[4] <- str_c(text, collapse = ", ")
      names(td) <- c("position_code", "shirt_number", "player_name", "events")
      table[[j]] <- td
      j <- j + 1
    }
  }
  table <- bind_rows(table)

  # Clean variables ------------------------------------------------------------

  table <- table |>
    mutate(
      id = 1:n(),
      starting = as.numeric(id <= 11),
      on_bench = as.numeric(id > 11),
      player_name = player_name |>
        str_remove_all("\\[.*?\\]") |>
        str_remove("\\([Cc]\\)") |>
        str_squish(),
      position_code = position_code |>
        str_extract("[A-Z]{2,3}"),
      shirt_number = shirt_number |>
        str_extract("^[0-9]+$") |>
        as.numeric(),
      events = events |>
        str_remove_all("\\[.*?\\]") |>
        str_squish(),
      events = case_when(
        events == "" ~ "none",
        TRUE ~ events
      )
    )

  # Extract links --------------------------------------------------------------

  # Make a table with links
  links <- tibble(
    player_wikipedia_link = html_table |>
      html_elements("a") |>
      html_attr("href"),
    player_name = html_table |>
      html_elements("a") |>
      html_text()
  )

  # Merge links into table
  table <- table |>
    left_join(
      links,
      by = "player_name"
    ) |>
    mutate(
      player_wikipedia_link = str_c("https://en.wikipedia.org", player_wikipedia_link),
      player_wikipedia_link = case_when(
        is.na(player_wikipedia_link) ~ "not available",
        TRUE ~ player_wikipedia_link
      )
    ) |>
    select(
      player_name, player_wikipedia_link,
      shirt_number, position_code,
      starting, on_bench,
      events
    )

  return(table)
}

# Function to make goal table
extract_event_data <- function(html_table) {

  # Extract score --------------------------------------------------------------

  # Score
  score <- html_table |>
    html_element(xpath = ".//th[@class='fscore']") |>
    html_text() |>
    str_squish()

  # Extract penalty score ------------------------------------------------------

  # Penalty score
  score_penalties <- html_table |>
    html_elements(xpath = ".//th") |>
    html_text()
  if (length(score_penalties) == 5) {
    score_penalties <- score_penalties[5]
  } else {
    score_penalties <- "0-0"
  }

  # Extract team names ---------------------------------------------------------

  # Home team name
  home_team_name <- html_table |>
    html_element(xpath = ".//th[@class='fhome']") |>
    html_text() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia") |>
    str_squish()

  # Home team link
  home_team_wikipedia_link <- html_table |>
    html_element(xpath = ".//th[@class='fhome']") |>
    html_element("a") |>
    html_attr("href") |>
    str_squish()

  # Away team name
  away_team_name <- html_table |>
    html_element(xpath = ".//th[@class='faway']") |>
    html_text() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia") |>
    str_squish()

  # Away team link
  away_team_wikipedia_link <- html_table |>
    html_element(xpath = ".//th[@class='faway']") |>
    html_element("a") |>
    html_attr("href") |>
    str_squish()

  # Extract event data ---------------------------------------------------------

  # Home team events
  home_team_event_data <- html_table |>
    html_elements(xpath = ".//td[@class='fhgoal']")

  # Identify home team goals and penalties
  if(length(home_team_event_data) == 1) {
    home_team_goal_data <- home_team_event_data[[1]]
    home_team_penalty_kick_data <- NULL
  } else if (length(home_team_event_data) == 2) {
    home_team_goal_data <- home_team_event_data[[1]]
    home_team_penalty_kick_data <- home_team_event_data[[2]]
  } else {
    home_team_goal_data <- NULL
    home_team_penalty_kick_data <- NULL
  }

  # Away team events
  away_team_event_data <- html_table |>
    html_elements(xpath = ".//td[@class='fagoal']")

  # Identify away team goals and penalties
  if(length(away_team_event_data) == 1) {
    away_team_goal_data <- away_team_event_data[[1]]
    away_team_penalty_kick_data <- NULL
  } else if (length(away_team_event_data) == 2) {
    away_team_goal_data <- away_team_event_data[[1]]
    away_team_penalty_kick_data <- away_team_event_data[[2]]
  } else {
    away_team_goal_data <- NULL
    away_team_penalty_kick_data <- NULL
  }

  # Extract goals --------------------------------------------------------------

  # Home team goals
  if(!is.null(home_team_goal_data)) {
    home_team_goals <- tibble(
      team_name = home_team_name,
      text = home_team_goal_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_remove(".mw-parser-output") |>
        str_remove(".fb.*?\\}") |>
        str_remove_all("\\[[0-9]+\\]") |>
        str_squish() |>
        (\(x) x[!str_detect(x, "plainlist ol")])(),
      player_wikipedia_link = home_team_goal_data |>
        html_elements("a") |>
        html_attr("href") |>
        str_squish() |>
        (\(x) x[!str_detect(x, "Penalty.kick|Own.goal|#cite_note")])()
    )
  } else {
    home_team_goals <- NULL
  }

  # Away team goals
  if(!is.null(away_team_goal_data)) {
    away_team_goals <- tibble(
      team_name = away_team_name,
      text = away_team_goal_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_remove(".mw-parser-output") |>
        str_remove(".fb.*?\\}") |>
        str_remove_all("\\[[0-9]+\\]") |>
        str_squish() |>
        (\(x) x[!str_detect(x, "plainlist ol")])(),
      player_wikipedia_link = away_team_goal_data |>
        html_elements("a") |>
        html_attr("href") |>
        str_squish() |>
        (\(x) x[!str_detect(x, "Penalty.kick|Own.goal|#cite_note")])()
    )
  } else {
    away_team_goals <- NULL
  }

  # Compile goal data
  goals <- bind_rows(
    home_team_goals,
    away_team_goals
  )

  # Clean goal data
  goals <- goals |>
    mutate(
      minute_label = text |>
        str_extract("[0-9+]+'.*"),
      player_name = text |>
        str_remove("[0-9+]+'.*") |>
        str_squish()
    ) |>
    separate_rows(minute_label, sep = ",") |>
    mutate(
      penalty = minute_label |>
        str_detect("pen") |>
        as.numeric(),
      own_goal = minute_label |>
        str_detect("o.g.") |>
        as.numeric(),
      minute_label = minute_label |>
        str_remove("\\(pen.\\)") |>
        str_remove("\\(o.g.\\)") |>
        str_remove("\\[[a-z]\\]") |>
        str_replace("([0-9])\\+", "\\1'+") |>
        str_squish(),
      player_wikipedia_link = str_c("https://en.wikipedia.org", player_wikipedia_link)
    ) |>
    select(
      team_name, player_name, player_wikipedia_link,
      minute_label, penalty, own_goal
    )

  # Extract penalty kicks ------------------------------------------------------

  # Home team penalties
  if(!is.null(home_team_penalty_kick_data)) {
    home_team_penalty_kicks <- tibble(
      team_name = home_team_name,
      converted = home_team_penalty_kick_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_detect("scored") |>
        as.numeric(),
      player_name = home_team_penalty_kick_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_remove("(scored|missed)") |>
        str_squish(),
      player_wikipedia_link = home_team_penalty_kick_data |>
        html_elements("a") |>
        html_attr("href") |>
        (\(x) x[!str_detect(x, "\\.svg")])()
    )
    home_team_penalty_kicks <- home_team_penalty_kicks |>
      mutate(
        player_wikipedia_link = str_c("https://en.wikipedia.org", player_wikipedia_link)
      )
  } else {
    home_team_penalty_kicks <- NULL
  }

  # Away team penalties
  if(!is.null(away_team_penalty_kick_data)) {
    away_team_penalty_kicks <- tibble(
      team_name = away_team_name,
      converted = away_team_penalty_kick_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_detect("scored") |>
        as.numeric(),
      player_name = away_team_penalty_kick_data |>
        html_text2() |>
        str_split("\\n") |>
        unlist() |>
        str_remove("(scored|missed)") |>
        str_squish(),
      player_wikipedia_link = away_team_penalty_kick_data |>
        html_elements("a") |>
        html_attr("href") |>
        (\(x) x[!str_detect(x, "\\.svg")])()
    )
    away_team_penalty_kicks <- away_team_penalty_kicks |>
      mutate(
        player_wikipedia_link = str_c("https://en.wikipedia.org", player_wikipedia_link)
      )
  } else {
    away_team_penalty_kicks <- NULL
  }

  # Compile penalty kick data
  penalty_kicks <- bind_rows(
    home_team_penalty_kicks,
    away_team_penalty_kicks
  )

  # Output ---------------------------------------------------------------------

  return(
    list(
      score = score,
      score_penalties = score_penalties,
      home_team_name = home_team_name,
      away_team_name = away_team_name,
      goals = goals,
      penalty_kicks = penalty_kicks
    )
  )
}

# Function to extract data from each HTML file
extract_match_data <- function(file) {

  # Print file name to the console
  cat(file, "\n")

  # Extract data from HTML ------------------------------------------------------

  # Read in the HTML code
  html <- read_html(file)

  # Clean the HTML code
  html <- html |>
    as.character() |>
    str_replace_all("<img alt=\"Yellow card\".*?>", " yellow card ") |>
    str_replace_all("<img alt=\"Yellow-red card\".*?>", " second yellow card ") |>
    str_replace_all("<img alt=\"Red card\".*?>", " red card ") |>
    str_replace_all("<img alt=\"soccer ball with red X\".*?>", " missed ") |>
    str_replace_all("<img alt=\"soccer ball with check mark\".*?>", " scored ") |>
    str_replace_all("<img alt=\"downward-facing red arrow\".*?>", " subbed off ") |>
    str_replace_all("<img alt=\"upward-facing green arrow\".*?>", " subbed on ") |>
    minimal_html()

  # Extract the name of the page
  page_name <- html |>
    html_elements("h1") |>
    html_text()

  # Year
  year <- page_name |>
    str_extract("[0-9]{4}") |>
    as.numeric()

  # Extract content
  content <- html |>
    html_elements(xpath = ".//div[@class='mw-parser-output']") |>
    html_children()

  # Extract team tables
  team_tables <- html |>
    html_elements(xpath = ".//table[@cellspacing='0' and @cellpadding='0' and starts-with(@style, 'font-size:')]")

  # Extract goal tables
  event_tables <- html |>
    html_elements(xpath = ".//table[@class='fevent']")

  # Extract match IDs ----------------------------------------------------------

  # Extract event data
  event_data <- map(event_tables, extract_event_data)

  # Home team
  home_teams <- event_data |>
    map("home_team_name") |>
    unlist() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia")

  # Away team
  away_teams <- event_data |>
    map("away_team_name") |>
    unlist() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia")

  # Extract date
  dates <- content |>
    html_elements(xpath = "//div[@class='fdate']") |>
    html_text() |>
    str_squish() |>
    str_replace("([A-Z][a-z]+) ([0-9]{1,2}),? ([0-9]{4})", "\\2 \\1 \\3") |>
    str_extract("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") |>
    dmy()

  # Match IDs
  match_ids <- str_c(
    home_teams, " vs ", away_teams,
    " (", dates, ")"
  )

  # Add names to list
  names(event_data) <- match_ids

  # Lineups --------------------------------------------------------------------

  # Make lineup data
  if (year >= 1970) {

    # Extract team data
    team_data <- map(team_tables, extract_team_data)

    if (length(team_data) > 0) {
      # Extract lineup data
      lineups <- team_data |>
        bind_rows(.id = "team_id") |>
        mutate(
          team_id = as.numeric(team_id)
        )

      # match template
      template <- tibble(
        match_id = rep(match_ids, each = 2),
        team_name = c(rbind(home_teams, away_teams))
      )
      template <- template |>
        mutate(
          team_id = 1:n()
        )

      # Merge into template
      lineups <- left_join(
        template,
        lineups,
        by = "team_id"
      )

      # Organize variables
      lineups <- lineups |>
        select(
          match_id, team_name,
          player_name, player_wikipedia_link,
          shirt_number, position_code,
          starting, on_bench, events
        )
    } else {
      lineups <- NULL
    }
  } else {
    lineups <- NULL
  }

  # Goals ----------------------------------------------------------------------

  # Extract goals
  goals <- event_data |>
    map("goals") |>
    bind_rows(.id = "match_id")

  if (year >= 1970 & !is.null(lineups)) {

    # Add shirt numbers
    goals <- left_join(
      goals,
      lineups |>
        select(player_wikipedia_link, shirt_number) |>
        group_by(player_wikipedia_link) |>
        summarize(
          shirt_number = shirt_number[1]
        ) |>
        ungroup(),
      by = "player_wikipedia_link"
    )

  } else {

    # Add shirt numbers
    goals <- goals |>
      mutate(
        shirt_number = 0
      )
  }

  # Organize variables
  goals <- goals |>
    select(
      match_id, team_name,
      player_name, player_wikipedia_link,
      minute_label, penalty, own_goal
    )

  # Penalty kicks --------------------------------------------------------------

  if (year >= 1970) {

    # Extract penalty kicks
    penalty_kicks <- event_data |>
      map("penalty_kicks") |>
      bind_rows(.id = "match_id")

    # Add shirt numbers to penalty kick data
    if (nrow(penalty_kicks) > 0 & !is.null(lineups)) {
      penalty_kicks <- left_join(
        penalty_kicks,
        lineups |>
          select(player_wikipedia_link, shirt_number) |>
          group_by(player_wikipedia_link) |>
          summarize(
            shirt_number = shirt_number[1]
          ) |>
          ungroup(),
        by = c("player_wikipedia_link" = "player_wikipedia_link")
      )
      penalty_kicks <- penalty_kicks |>
        select(
          match_id, team_name,
          player_name, player_wikipedia_link,
          converted
        )
    } else {
      penalty_kicks <- NULL
    }
  } else {
    penalty_kicks <- NULL
  }

  # Matches --------------------------------------------------------------------

  # Extract scores
  matches <- tibble(
    match_id = match_ids,
    score = event_data |>
      map("score") |>
      unlist(),
    score_penalties = event_data |>
      map("score_penalties") |>
      unlist()
  )

  # Code match variables
  matches <- matches |>
    mutate(
      extra_time = score |>
        str_detect("a.e.t.") |>
        as.numeric(),
      penalty_shootout = as.numeric(score_penalties != "0-0"),
      score = score |>
        str_remove("\\(a.e.t.\\)") |>
        str_remove("\\(a.e.t./g.g.\\)") |>
        str_squish(),
      home_team_score = score |>
        str_extract("^[0-9]+") |>
        as.numeric(),
      away_team_score = score |>
        str_extract("[0-9]+$") |>
        as.numeric(),
      home_team_score_penalties = score_penalties |>
        str_extract("^[0-9]+") |>
        as.numeric(),
      away_team_score_penalties = score_penalties |>
        str_extract("[0-9]+$") |>
        as.numeric(),
      home_team_name = home_teams,
      away_team_name = away_teams,
      match_date = dates,
      match_time = content |>
        html_elements(xpath = "//div[@class='ftime']") |>
        html_text() |>
        str_squish() |>
        str_extract("[0-9]+:[0-9]+")
    ) |>
    select(
      match_id, home_team_name, away_team_name, match_date, match_time,
      score, home_team_score, away_team_score,
      extra_time, penalty_shootout,
      score_penalties, home_team_score_penalties, away_team_score_penalties
    )

  # Referees -------------------------------------------------------------------

  # Make referee data
  referees <- tibble(
    match_id = match_ids,
    referee = html |>
      html_elements(xpath = "//div[@class='fright']") |>
      html_text() |>
      str_extract("Referee.*") |>
      str_remove("Referee:") |>
      str_remove("\\(.*") |>
      str_squish(),
    referee_wikipedia_link = html |>
      html_elements(xpath = "//div[@class='fright']") |>
      as.character() |>
      str_extract("Referee: *<a.*?>") |>
      str_extract("href=.*? ") |>
      str_remove_all("\"") |>
      str_remove("href=") |>
      str_squish(),
    referee_country = html |>
      html_elements(xpath = "//div[@class='fright']") |>
      html_text() |>
      str_extract("Referee.*") |>
      str_extract("\\(.*?\\)") |>
      str_remove("\\(") |>
      str_remove("\\)") |>
      str_squish()
  )

  # Clean missing links
  referees <- referees |>
    mutate(
      referee_wikipedia_link = case_when(
        is.na(referee_wikipedia_link) ~ "not available",
        str_detect(referee_wikipedia_link, "redlink") ~ "not available",
        TRUE ~ referee_wikipedia_link
      )
    )

  # Output ---------------------------------------------------------------------

  return(
    list(
      matches = matches,
      goals = goals,
      penalty_kicks = penalty_kicks,
      lineups = lineups,
      referees = referees
    )
  )
}

# Function to parse files
parse_match_files <- function(files) {
  raw <- map(files, extract_match_data)
  return(
    list(
      matches = raw |>
        map("matches") |>
        bind_rows(),
      goals = raw |>
        map("goals") |>
        bind_rows(),
      penalty_kicks = raw |>
        map("penalty_kicks") |>
        bind_rows(),
      lineups = raw |>
        map("lineups") |>
        bind_rows(),
      referees = raw |>
        map("referees") |>
        bind_rows()
    )
  )
}

# Parse match files ------------------------------------------------------------

# Get file names
files <- list.files("data-raw/Wikipedia-match-pages", full.names = TRUE)

# Parse files
data_raw <- parse_match_files(files)

# Extract tables
wikipedia_matches <- data_raw$matches
wikipedia_goals <- data_raw$goals
wikipedia_penalty_kicks <- data_raw$penalty_kicks
wikipedia_lineups <- data_raw$lineups
wikipedia_referees <- data_raw$referees

# Load squad data
load("data-raw/Wikipedia-data/wikipedia_squads.RData")

# Goals ------------------------------------------------------------------------

# Correct missing information
wikipedia_goals <- wikipedia_goals |>
  mutate(
    player_wikipedia_link = case_when(
      match_id == "Hungary vs El Salvador (1982-06-15)" & player_name == "Tóth" ~ "https://en.wikipedia.org/wiki/J%C3%B3zsef_T%C3%B3th_(footballer,_born_1951)",
      TRUE ~ player_wikipedia_link
    )
  )

# Player Wikipedia links to correct
to_correct <- wikipedia_goals |>
  filter(!(player_wikipedia_link %in% wikipedia_squads$player_wikipedia_link)) |>
  select(
    player_name, player_wikipedia_link
  ) |>
  distinct()

# Code incorrect links
wikipedia_goals <- wikipedia_goals |>
  mutate(
    player_wikipedia_link = case_when(
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Luis_de_Souza_Ferreira" ~ "https://en.wikipedia.org/wiki/Luis_Souza_Ferreira",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Iuliu_Bar%C3%A1tky" ~ "https://en.wikipedia.org/wiki/Iuliu_Baratky",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Le%C3%B4nidas_da_Silva" ~ "https://en.wikipedia.org/wiki/Le%C3%B4nidas",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Friedrich_Scherfke" ~ "https://en.wikipedia.org/wiki/Fryderyk_Scherfke",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Roberto_Em%C3%ADlio_da_Cunha" ~ "https://en.wikipedia.org/wiki/Roberto_(footballer,_born_1912)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Maneca" ~ "https://en.wikipedia.org/wiki/Manuel_Marinho_Alves",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Thomaz_Soares_da_Silva" ~ "https://en.wikipedia.org/wiki/Zizinho",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/%C3%93scar_M%C3%ADguez" ~ "https://en.wikipedia.org/wiki/Oscar_M%C3%ADguez",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Alfredo_dos_Santos" ~ "https://en.wikipedia.org/wiki/Alfredo_Ramos_dos_Santos",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Didi_(footballer,_born_1928)" ~ "https://en.wikipedia.org/wiki/Valdir_Pereira",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Jean_Vincent" ~ "https://en.wikipedia.org/wiki/Jean_Vincent_(footballer)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Theodor_Wagner" ~ "https://en.wikipedia.org/wiki/Theodor_Wagner_(footballer)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Aleksandr_Ivanov_(footballer_born_1928)" ~ "https://en.wikipedia.org/wiki/Aleksandr_Ivanov_(footballer,_born_1928)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Leonel_Sanchez" ~ "https://en.wikipedia.org/wiki/Leonel_S%C3%A1nchez",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Alfredo_del_%C3%81guila" ~ "https://en.wikipedia.org/wiki/Alfredo_del_Aguila",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Fl%C3%B3ri%C3%A1n_Albert" ~ "https://en.wikipedia.org/wiki/Fl%C3%B3ri%C3%A1n_Albert,_Sr.",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Jos%C3%A9_Ely_de_Miranda" ~ "https://en.wikipedia.org/wiki/Zito_(footballer)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Vitaliy_Khmelnytskyi" ~ "https://en.wikipedia.org/wiki/Vitaly_Khmelnitsky",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Roberto_Rivelino" ~ "https://en.wikipedia.org/wiki/Rivellino",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Roberto_Rivellino" ~ "https://en.wikipedia.org/wiki/Rivellino",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Andr%C3%A1s_T%C3%B3th_(footballer_born_1949)" ~ "https://en.wikipedia.org/wiki/Andr%C3%A1s_T%C3%B3th_(footballer,_born_1949)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Mokhtar_Dhouieb" ~ "https://en.wikipedia.org/wiki/Mokhtar_Dhouib",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Hassan_Rowshan" ~ "https://en.wikipedia.org/wiki/Hassan_Roshan",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/J%C3%B3zsef_T%C3%B3th_(footballer_born_1951)" ~ "",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Eduardo_Laing" ~ "https://en.wikipedia.org/wiki/Antonio_Laing",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Aleksandre_Chivadze" ~ "https://en.wikipedia.org/wiki/Aleksandr_Chivadze",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Khoren_Oganesian" ~ "https://en.wikipedia.org/wiki/Khoren_Hovhannisyan",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Park_Chang-Sun" ~ "https://en.wikipedia.org/wiki/Park_Chang-sun",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Kim_Jong-Boo" ~ "https://en.wikipedia.org/wiki/Kim_Jong-boo",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Choi_Soon-Ho" ~ "https://en.wikipedia.org/wiki/Choi_Soon-ho",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Huh_Jung-Moo" ~ "https://en.wikipedia.org/wiki/Huh_Jung-moo",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Cho_Kwang-Rae" ~ "https://en.wikipedia.org/wiki/Cho_Kwang-rae",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Pavlo_Yakovenko" ~ "https://en.wikipedia.org/wiki/Pavel_Yakovenko",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Igor_Belanov" ~ "https://en.wikipedia.org/wiki/Ihor_Belanov",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Oleg_Blokhin" ~ "https://en.wikipedia.org/wiki/Oleh_Blokhin",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Colin_Clarke_(footballer_born_1962)" ~ "https://en.wikipedia.org/wiki/Colin_Clarke_(footballer,_born_1962)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Abdelkrim_Merry_Krimau" ~ "https://en.wikipedia.org/wiki/Abdelkrim_Merry",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Edino_Nazareth_Filho" ~ "https://en.wikipedia.org/wiki/Edinho_(footballer,_born_1955)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Gavril_Balint" ~ "https://en.wikipedia.org/wiki/Gabi_Balint",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Lei_Clijsters" ~ "https://en.wikipedia.org/wiki/Leo_Clijsters",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Mark_Wright_(footballer_born_1963)" ~ "https://en.wikipedia.org/wiki/Mark_Wright_(footballer,_born_1963)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/M%C3%A1rcio_Roberto_dos_Santos" ~ "https://en.wikipedia.org/wiki/M%C3%A1rcio_Santos_(footballer,_born_1969)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Ion_Andoni_Goikoetxea" ~ "https://en.wikipedia.org/wiki/Jon_Andoni_Goikoetxea",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Luis_Enrique_(footballer)" ~ "https://en.wikipedia.org/wiki/Luis_Enrique",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Andreas_Herzog" ~ "https://en.wikipedia.org/wiki/Andi_Herzog",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Kiko_(footballer)" ~ "https://en.wikipedia.org/wiki/Kiko_(footballer,_born_1972)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Yoo_Sang-Chul" ~ "https://en.wikipedia.org/wiki/Yoo_Sang-chul",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Hamid_Reza_Estili" ~ "https://en.wikipedia.org/wiki/Hamid_Estili",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Edm%C3%ADlson_(footballer,_born_1976)" ~ "https://en.wikipedia.org/wiki/Edm%C3%ADlson",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Jen%C3%ADlson_%C3%82ngelo_de_Souza" ~ "https://en.wikipedia.org/wiki/J%C3%BAnior_(footballer,_born_1973)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Anders_Svensson_(footballer,_born_1976)" ~ "https://en.wikipedia.org/wiki/Anders_Svensson",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Peter_Van_Der_Heyden" ~ "https://en.wikipedia.org/wiki/Peter_Van_der_Heyden",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Valery_Karpin" ~ "https://en.wikipedia.org/wiki/Valeri_Karpin",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Seol_Ki-Hyeon" ~ "https://en.wikipedia.org/wiki/Seol_Ki-hyeon",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Ahn_Jung-Hwan" ~ "https://en.wikipedia.org/wiki/Ahn_Jung-hwan",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Yasser_Al_Qahtani" ~ "https://en.wikipedia.org/wiki/Yasser_Al-Qahtani",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Sami_Al_Jaber" ~ "https://en.wikipedia.org/wiki/Sami_Al-Jaber",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Winston_Reid_(footballer)" ~ "https://en.wikipedia.org/wiki/Winston_Reid",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Jos%C3%A9_Mar%C3%ADa_Gim%C3%A9nez" ~ "https://en.wikipedia.org/wiki/Jos%C3%A9_Gim%C3%A9nez",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Oghenekaro_Etebo" ~ "https://en.wikipedia.org/wiki/Peter_Etebo",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Aleksandar_Mitrovi%C4%87_(footballer)" ~ "https://en.wikipedia.org/wiki/Aleksandar_Mitrovi%C4%87",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Giorgian_de_Arrascaeta" ~ "https://en.wikipedia.org/wiki/Giorgian_De_Arrascaeta",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Elissandra_Regina_Cavalcanti" ~ "https://en.wikipedia.org/wiki/Nen%C3%AA_(footballer,_born_1976)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Cristiane_Rozeira" ~ "https://en.wikipedia.org/wiki/Cristiane_(footballer)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Andressa_Alves_da_Silva" ~ "https://en.wikipedia.org/wiki/Andressa_Alves",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Jennifer_Ruiz" ~ "https://en.wikipedia.org/wiki/Jenny_Ruiz-Williams",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Dominique_Bloodworth" ~ "https://en.wikipedia.org/wiki/Dominique_Janssen",
      TRUE ~ player_wikipedia_link
    )
  )

# Merge in player data
wikipedia_goals <- wikipedia_goals |>
  left_join(
    wikipedia_squads |>
      select(
        player_wikipedia_link, player_id,
        given_name, family_name
      ) |>
      distinct(),
    by = "player_wikipedia_link"
  )

# Check for missing
table(is.na(wikipedia_goals$player_id))

# Penalty kicks ----------------------------------------------------------------

# Player Wikipedia links to correct
to_correct <- wikipedia_penalty_kicks |>
  filter(!(player_wikipedia_link %in% wikipedia_squads$player_wikipedia_link)) |>
  select(
    player_name, player_wikipedia_link
  ) |>
  distinct()

# Code incorrect links
wikipedia_penalty_kicks <- wikipedia_penalty_kicks |>
  mutate(
    player_wikipedia_link = case_when(
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Cl%C3%A1udio_Ibrahim_Vaz_Leal" ~ "https://en.wikipedia.org/wiki/Branco_(footballer)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/J%C3%BAlio_C%C3%A9sar_da_Silva" ~ "https://en.wikipedia.org/wiki/J%C3%BAlio_C%C3%A9sar_(footballer,_born_1963)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/D%C4%83nu%C5%A3_Lupu" ~ "https://en.wikipedia.org/wiki/D%C4%83nu%C8%9B_Lupu",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Krassimir_Balakov" ~ "https://en.wikipedia.org/wiki/Krasimir_Balakov",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Bontcho_Guentchev" ~ "https://en.wikipedia.org/wiki/Boncho_Genchev",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/M%C3%A1rcio_Santos" ~ "https://en.wikipedia.org/wiki/M%C3%A1rcio_Santos_(footballer,_born_1969)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Joaqu%C3%ADn_(footballer)" ~ "https://en.wikipedia.org/wiki/Joaqu%C3%ADn_(footballer,_born_1981)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Konstantinos_Mitroglou" ~ "https://en.wikipedia.org/wiki/Kostas_Mitroglou",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Elissandra_Regina_Cavalcanti" ~ "https://en.wikipedia.org/wiki/Nen%C3%AA_(footballer,_born_1976)",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Cristiane_Rozeira" ~ "https://en.wikipedia.org/wiki/Cristiane_(footballer)",
      TRUE ~ player_wikipedia_link
    )
  )

# Merge in player data
wikipedia_penalty_kicks <- wikipedia_penalty_kicks |>
  left_join(
    wikipedia_squads |>
      select(
        player_wikipedia_link, player_id,
        given_name, family_name
      ) |>
      distinct(),
    by = "player_wikipedia_link"
  )

# Check for missing
table(is.na(wikipedia_penalty_kicks$player_id))

# Check totals
sum(wikipedia_matches$home_team_score_penalties) + sum(wikipedia_matches$away_team_score_penalties)
sum(wikipedia_penalty_kicks$converted)

# Check goal per tournament ----------------------------------------------------

goals_per_tournament <- wikipedia_goals |>
  mutate(
    year = match_id |>
      str_extract("[0-9]{4}") |>
      as.numeric()
  ) |>
  group_by(year) |>
  count()

# Check that goals and scores match --------------------------------------------

# Calculate the number of goals per team per match
goals_by_team <- wikipedia_goals |>
  mutate(
    year = match_id |>
      str_extract("[0-9]{4}") |>
      as.numeric(),
    date = match_id |>
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") |>
      ymd()
  ) |>
  group_by(match_id, date, team_name) |>
  count() |>
  arrange(date)

# Get the score per team per match
score_by_team <- bind_rows(
  wikipedia_matches |>
    select(match_id, home_team_name, home_team_score) |>
    rename(team_name = home_team_name, score = home_team_score),
  wikipedia_matches |>
    select(match_id, away_team_name, away_team_score) |>
    rename(team_name = away_team_name, score = away_team_score)
)
score_by_team <- score_by_team |>
  filter(score != 0)

# Merge together
goals_by_team <- left_join(
  goals_by_team,
  score_by_team,
  by = c("match_id", "team_name")
)

# Check that the totals match
table(goals_by_team$n == goals_by_team$score)

# Lineups ----------------------------------------------------------------------

# Drop matches with missing lineups
wikipedia_lineups <- wikipedia_lineups |>
  filter(!str_detect(match_id, "1999")) |>
  filter(match_id != "Japan vs United States (2011-07-17)")

# Check for missing data -------------------------------------------------------

table(is.na(wikipedia_matches))
table(is.na(wikipedia_goals))
table(is.na(wikipedia_penalty_kicks))
table(is.na(wikipedia_lineups))
table(is.na(wikipedia_referees))

# Save data --------------------------------------------------------------------

# Save tables
save(wikipedia_matches, file = "data-raw/Wikipedia-data/wikipedia_matches.RData")
save(wikipedia_goals, file = "data-raw/Wikipedia-data/wikipedia_goals.RData")
save(wikipedia_penalty_kicks, file = "data-raw/Wikipedia-data/wikipedia_penalty_kicks.RData")
save(wikipedia_lineups, file = "data-raw/Wikipedia-data/wikipedia_lineups.RData")
save(wikipedia_referees, file = "data-raw/Wikipedia-data/wikipedia_referees.RData")
