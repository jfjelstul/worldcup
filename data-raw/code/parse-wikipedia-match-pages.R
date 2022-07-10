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
  table <- html_table |>
    html_table(head = FALSE, trim = TRUE)

  # Make sure the table has 6 columns
  if(ncol(table) == 3) {
    table$X4 <- NA
    table$X5 <- NA
    table$X6 <- NA
  } else if(ncol(table) == 4) {
    table$X5 <- NA
    table$X6 <- NA
  } else if(ncol(table) == 5) {
    table$X6 <- NA
  } else if (ncol(table) == 7) {
    table$X7 <- NULL
  }

  # Extract links
  href <- html_table |>
    html_elements("a") |>
    html_attr("href")
  img <- html_table |>
    html_elements("a") |>
    as.character() |>
    str_detect("img alt")
  links <- href[href != "/wiki/Captain_(association_football)" & !img]
  links <- links[!str_detect(links, "#cite_note")]

  # Rename variables
  table = table |>
    rename(
      position_code = X1,
      shirt_number = X2,
      name = X3,
      text_1 = X4,
      text_2 = X5,
      text_3 = X6
    )

  # Clean table
  table = table |>
    mutate(
      shirt_number = shirt_number |>
        as.character(),
      text_1 = text_1 |>
        as.character() |>
        str_squish(),
      text_2 = text_2 |>
        as.character() |>
        str_squish(),
      text_3 = text_3 |>
        as.character() |>
        str_squish(),
      text_1 = case_when(
        is.na(text_1) ~ "",
        TRUE ~ text_1
      ),
      text_2 = case_when(
        is.na(text_2) ~ "",
        TRUE ~ text_2
      ),
      text_3 = case_when(
        is.na(text_3) ~ "",
        TRUE ~ text_3
      ),
      events = str_c(text_1, text_2, text_3, sep = " ") |>
        str_squish(),
      name = name |>
        str_replace("([[:lower:]]{3,})([[:upper:]])", "\\1 & \\2")
    ) |>
    separate_rows(name, sep = "&") |>
    filter(!is.na(name)) |>
    filter(name != "") |>
    filter(!str_detect(name, "[Ss]ubstitut")) |>
    filter(!str_detect(name, "[Mm]anager")) |>
    filter(!str_detect(name, "disciplinary actions")) |>
    select(
      position_code, shirt_number, name, events
    )

  # Code variables -------------------------------------------------------------

  table <- table |>
    mutate(
      link = links,
      captain = name |>
        str_detect("\\([Cc]\\)") |>
        as.numeric(),
      name = name |>
        str_remove_all("\\[.*?\\]") |>
        str_remove("\\([Cc]\\)") |>
        str_squish(),
      position_code = position_code |>
        str_extract("[A-Z]{2,3}"),
      shirt_number = shirt_number |>
        str_extract("^[0-9]+$") |>
        as.numeric(),
      shirt_number = shirt_number |>
        as.numeric(),
      manager = as.numeric(is.na(shirt_number)),
      events = events |>
        str_replace("' *second", "', second") |>
        str_replace("' *red", "', red") |>
        str_replace("' *subbed", "', subbed") |>
        str_remove_all("\\[.*?\\]") |>
        str_squish(),
      events = case_when(
        events == "" ~ "none",
        manager == 1 ~ "none",
        TRUE ~ events
      ),
    ) |>
    select(
      manager, name, shirt_number, position_code, captain, events, link
    )

  # Lineups --------------------------------------------------------------------

  lineups <- table |>
    mutate(
      id = 1:n(),
      starting = as.numeric(id <= 11),
      on_bench = as.numeric(id > 11)
    ) |>
    filter(id <= 11 | str_detect(events, "subbed")) |>
    filter(manager == 0) |>
    rename(
      player_name = name,
      player_wikipedia_link = link
    ) |>
    select(
      player_name, player_wikipedia_link,
      shirt_number, position_code,
      starting, on_bench, captain, events
    )

  # Output ---------------------------------------------------------------------

  return(lineups)
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
        str_squish(),
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
        str_squish(),
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
        str_squish()
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
  home_teams = event_data |>
    map("home_team_name") |>
    unlist() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia")

  # Away team
  away_teams = event_data |>
    map("away_team_name") |>
    unlist() |>
    str_replace("China PR", "China") |>
    str_replace("FR Yugoslavia", "Yugoslavia")

  # Extract date
  dates = content |>
    html_elements(xpath = "//div[@class='fdate']") |>
    html_text() |>
    str_squish() |>
    str_replace("([A-Z][a-z]+) ([0-9]{1,2}),? ([0-9]{4})", "\\2 \\1 \\3") |>
    str_extract("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") |>
    dmy()

  # Match IDs
  match_ids <- str_c(
    home_teams, " v ", away_teams,
    " (", dates, ")"
  )

  # Add names to list
  names(event_data) <- match_ids

  # Lineups --------------------------------------------------------------------

  if (year >= 1970) {

    # Extract team data
    team_data <- map(team_tables, extract_team_data)

    # Extract lineup data
    lineups = team_data |>
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
        starting, on_bench, captain, events
      )
  } else {
    lineups <- NULL
  }

  # Goals ----------------------------------------------------------------------

  # Extract goals
  goals = event_data |>
    map("goals") |>
    bind_rows(.id = "match_id")

  if (year >= 1970) {

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
      player_name, player_wikipedia_link, shirt_number,
      minute_label, penalty, own_goal
    )

  # Penalty kicks --------------------------------------------------------------

  if (year >= 1970) {

    # Extract penalty kicks
    penalty_kicks = event_data |>
      map("penalty_kicks") |>
      bind_rows(.id = "match_id")

    # Add shirt numbers to penalty kick data
    if (nrow(penalty_kicks) > 0) {
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
          player_name, player_wikipedia_link, shirt_number,
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

# Code missing data ------------------------------------------------------------

# Code missing shirt numbers in goal data
wikipedia_goals <- wikipedia_goals |>
  mutate(
    shirt_number = case_when(
      match_id == "Italy v Mexico (1970-06-14)" & player_name == "González" ~ 17,
      match_id == "Zaire v Brazil (1974-06-22)" & player_name == "Rivelino" ~ 10,
      match_id == "Brazil v East Germany (1974-06-26)" & player_name == "Rivelino" ~ 10,
      match_id == "Argentina v Brazil (1974-06-30)" & player_name == "Rivelino" ~ 10,
      match_id == "Peru v Iran (1978-06-11)" & player_name == "Rowshan" ~ 10,
      match_id == "Hungary v El Salvador (1982-06-15)" & player_name == "Tóth" ~ 4,
      match_id == "Belgium v Hungary (1982-06-22)" & player_name == "Varga" ~ 19,
      match_id == "West Germany v France (1982-07-08)" & player_name == "Trésor" ~ 8,
      match_id == "Soviet Union v Hungary (1986-06-02)" & player_name == "Yakovenko" ~ 8,
      match_id == "France v Soviet Union (1986-06-05)" & player_name == "Rats" ~ 21,
      match_id == "Soviet Union v Canada (1986-06-09)" & player_name == "Blokhin" ~ 11,
      match_id == "Soviet Union v Canada (1986-06-09)" & player_name == "Zavarov" ~ 9,
      match_id == "Algeria v Spain (1986-06-12)" & player_name == "Eloy" ~ 20,
      match_id == "Portugal v Morocco (1986-06-11)" & player_name == "Merry Krimau" ~ 9,
      match_id == "Soviet Union v Belgium (1986-06-15)" & player_name == "Belanov" ~ 19,
      match_id == "Denmark v Spain (1986-06-18)" & player_name == "Goikoetxea" ~ 8,
      match_id == "Yugoslavia v United Arab Emirates (1990-06-19)" & player_name == "Thani" ~ 3,
      match_id == "United States v Iran (1998-06-21)" & player_name == "Estili" ~ 9,
      match_id == "Tunisia v Saudi Arabia (2006-06-14)" & player_name == "Al-Qahtani" ~ 20,
      match_id == "Tunisia v Saudi Arabia (2006-06-14)" & player_name == "Al-Jaber" ~ 9,
      TRUE ~ shirt_number
    )
  )

# Code missing shirt numbers in penalty kick data
wikipedia_penalty_kicks <- wikipedia_penalty_kicks |>
  mutate(
    shirt_number = case_when(
      match_id == "Brazil v France (1986-06-21)" & player_name == "Branco" ~ 17,
      match_id == "Spain v Belgium (1986-06-22)" & player_name == "Eloy" ~ 20,
      match_id == "Republic of Ireland v Romania (1990-06-25)" & player_name == "Lupu" ~ 11,
      match_id == "Brazil v Italy (1994-07-17)" & player_name == "Márcio Santos" ~ 15,
      match_id == "Brazil v Netherlands (1998-07-07)" & player_name == "Cocu" ~ 11,
      TRUE ~ shirt_number
    )
  )

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
