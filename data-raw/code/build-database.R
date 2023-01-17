# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(lubridate)

# Functions --------------------------------------------------------------------

code_match_period <- function(x) {
  x <- tibble(minute_label = x) |>
    mutate(
      minute_regulation = minute_label |>
        str_extract("^[0-9]+") |>
        as.numeric(),
      stoppage_time = minute_label |>
        str_detect("\\+") |>
        as.numeric(),
      match_period = case_when(
        minute_regulation >= 1 & minute_regulation <= 45 & stoppage_time == 0 ~ "first half",
        minute_regulation == 45 & stoppage_time == 1 ~ "first half, stoppage time",
        minute_regulation >= 46 & minute_regulation <= 90 & stoppage_time == 0 ~ "second half",
        minute_regulation == 90 & stoppage_time == 1 ~ "second half, stoppage time",
        minute_regulation >= 91 & minute_regulation <= 105 & stoppage_time == 0 ~ "extra time, first half",
        minute_regulation == 105 & stoppage_time == 1 ~ "extra time, first half, stoppage time",
        minute_regulation >= 106 & minute_regulation <= 120 & stoppage_time == 0 ~ "extra time, second half",
        minute_regulation == 120 & stoppage_time == 1 ~ "extra time, second half, stoppage time",
        TRUE ~ "missing"
      )
    ) |>
    pull(match_period)
  return(x)
}

# Build tables -----------------------------------------------------------------

## Confederations --------------------------------------------------------------

# Load data
confederations <- read_csv("data-raw/hand-coded-tables/confederations.csv")

# Organize variables
confederations <- confederations |>
  arrange(
    confederation_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, confederation_id, confederation_name, confederation_code,
    confederation_wikipedia_link
  )

## Teams -----------------------------------------------------------------------

# Load data
teams <- read_csv("data-raw/hand-coded-tables/teams.csv")

# Team ID
teams <- teams |>
  arrange(team_name) |>
  mutate(
    team_id = str_c(
      "T-",
      str_pad(1:n(), width = 2, side = "left", pad = "0")
    )
  )

# Merge in confederation variables
teams <- left_join(
  teams,
  confederations |>
    select(confederation_id, confederation_name, confederation_code),
  by = "confederation_name"
)

# Organize variables
teams <- teams |>
  arrange(
    team_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, team_id, team_name, team_code, federation_name, region_name,
    confederation_id, confederation_name, confederation_code,
    team_wikipedia_link, federation_wikipedia_link
  )

## Tournaments -----------------------------------------------------------------

# Load data
tournaments <- read_csv("data-raw/hand-coded-tables/tournaments.csv")

# Create variables
tournaments <- tournaments |>
  mutate(
    tournament_id = str_c("WC-", year),
    tournament_name = str_c(year, " FIFA World Cup")
  )

# Organize variables
tournaments <- tournaments |>
  arrange(
    tournament_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    year, start_date, end_date,
    host_country, winner, host_won,
    count_teams, group_stage, second_group_stage,
    final_round, round_of_16, quarter_finals,
    semi_finals, third_place_match, final
  )

## Group standings -------------------------------------------------------------

# Load data
group_standings <- read_csv("data-raw/hand-coded-tables/group_standings.csv")

# Code tournament ID
group_standings <- group_standings |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
group_standings <- left_join(
  group_standings,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in team variables
group_standings <- left_join(
  group_standings,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
group_standings <- group_standings |>
  arrange(
    tournament_id, stage_number, group_name
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    stage_number, stage_name,
    group_name, position,
    team_id, team_name, team_code,
    played, wins, draws, losses,
    goals_for, goals_against, goal_difference,
    points, advanced
  )

## Groups ----------------------------------------------------------------------

# Collapse standings data by group
groups <- group_standings |>
  group_by(tournament_id, tournament_name, stage_number, stage_name, group_name) |>
  summarize(
    count_teams = n()
  ) |>
  ungroup()

# Organize variables
groups <- groups |>
  arrange(
    tournament_id, stage_number, group_name
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    stage_number, stage_name, group_name, count_teams
  )

## Tournament standings --------------------------------------------------------

# Load data
tournament_standings <- read_csv("data-raw/hand-coded-tables/tournament_standings.csv")

# Code tournament ID
tournament_standings <- tournament_standings |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
tournament_standings <- left_join(
  tournament_standings,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in team variables
tournament_standings <- left_join(
  tournament_standings,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
tournament_standings <- tournament_standings |>
  arrange(
    tournament_id, position
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    position, team_id, team_name, team_code
  )

## Host countries --------------------------------------------------------------

# Load data
host_countries <- read_csv("data-raw/hand-coded-tables/host_countries.csv")

# Code tournament ID
host_countries <- host_countries |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
host_countries <- left_join(
  host_countries,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in team variables
host_countries <- left_join(
  host_countries,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
host_countries <- host_countries |>
  arrange(
    tournament_id, team_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    team_id, team_name, team_code,
    performance
  )

## Tournament stages -----------------------------------------------------------

# Load data
tournament_stages <- read_csv("data-raw/hand-coded-tables/tournament_stages.csv")

# Code tournament ID
tournament_stages <- tournament_stages |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
tournament_stages <- left_join(
  tournament_stages,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Organize variables
tournament_stages <- tournament_stages |>
  arrange(
    tournament_id, stage_number
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    stage_number, stage_name,
    group_stage, knockout_stage, unbalanced_groups,
    start_date, end_date,
    count_matches, count_teams, count_scheduled,
    count_replays, count_playoffs, count_walkovers
  )

## Stadiums --------------------------------------------------------------------

# Load hand-coded match data
stadiums_hand_coded <- read_csv("data-raw/hand-coded-tables/stadiums.csv")

# Collapse by stadium
stadiums <- stadiums_hand_coded |>
  group_by(
    country_name, city_name, stadium_name,
    city_wikipedia_link, stadium_wikipedia_link
  ) |>
  summarize(
    stadium_capacity = max(stadium_capacity)
  ) |>
  ungroup()

# Check number of observations
table(duplicated(select(stadiums, country_name, city_name, stadium_name))) == nrow(stadiums)

# Stadium ID
stadiums <- stadiums |>
  arrange(country_name, city_name, stadium_name) |>
  mutate(
    stadium_id = str_c(
      "S-",
      str_pad(1:n(), width = 3, side = "left", pad = "0")
    )
  )

# Organize variables
stadiums <- stadiums |>
  arrange(
    stadium_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, stadium_id,
    stadium_name, city_name, country_name,
    stadium_capacity, stadium_wikipedia_link, city_wikipedia_link
  )

# Clean environment
rm(stadiums_hand_coded)

## Matches ---------------------------------------------------------------------

# Load hand-coded match data
load("data-raw/Wikipedia-data/wikipedia_matches.RData")
matches_hand_coded <- read_csv("data-raw/hand-coded-tables/matches.csv")
stadiums_hand_coded <- read_csv("data-raw/hand-coded-tables/stadiums.csv")

# Match name
matches <- wikipedia_matches |>
  mutate(
    match_name = str_c(
      home_team_name,
      " v ",
      away_team_name
    )
  )

# Merge in hand-coded data
matches <- left_join(
  matches,
  matches_hand_coded |>
    select(
      match_name, match_date, stage_name, knockout_stage, group_stage,
      group_name, replayed, replay
    ),
  by = c("match_name", "match_date")
)

# Merge in stadium data
matches <- left_join(
  matches,
  stadiums_hand_coded |>
    select(
      match_name, match_date, city_name, stadium_name, country_name
    ),
  by = c("match_name", "match_date")
)

# Outcome variables
matches <- matches |>
  mutate(
    home_team_win = as.numeric(home_team_score > away_team_score | home_team_score_penalties > away_team_score_penalties),
    away_team_win = as.numeric(away_team_score > home_team_score | away_team_score_penalties > home_team_score_penalties),
    replayed = case_when(
      match_name == "Italy v Spain" & match_date == ymd("1934-05-31") ~ 1,
      match_name == "Switzerland v Germany" & match_date == ymd("1938-06-04") ~ 1,
      match_name == "Cuba v Romania" & match_date == ymd("1938-06-05") ~ 1,
      match_name == "Brazil v Czechoslovakia" & match_date == ymd("1938-06-12") ~ 1,
      TRUE ~ 0
    ),
    replay = case_when(
      match_name == "Italy v Spain" & match_date == ymd("1934-06-01") ~ 1,
      match_name == "Switzerland v Germany" & match_date == ymd("1938-06-09") ~ 1,
      match_name == "Cuba v Romania" & match_date == ymd("1938-06-09") ~ 1,
      match_name == "Brazil v Czechoslovakia" & match_date == ymd("1938-06-14") ~ 1,
      TRUE ~ 0
    ),
    draw = as.numeric((group_stage == 1 | replayed == 1) & home_team_score == away_team_score),
    result = case_when(
      draw == 1 ~ "draw",
      home_team_win == 1 ~ "home team win",
      away_team_win == 1 ~ "away team win"
    ),
    home_team_score_margin = home_team_score - away_team_score,
    away_team_score_margin = away_team_score - home_team_score
  )

# Check results
table(matches$result)
table(matches$home_team_win)
table(matches$away_team_win)
table(matches$draw)
table(matches$replayed)
table(matches$replayed, matches$draw)
table(matches$replay)

# Check match stages and groups
table(matches$stage_name, matches$group_name)

# Tournament ID
matches <- matches |>
  mutate(
    year = year(match_date),
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
matches <- left_join(
  matches,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in home team variables
matches <- left_join(
  matches,
  teams |>
    select(team_id, team_name, team_code) |>
    rename(
      home_team_id = team_id,
      home_team_name = team_name,
      home_team_code = team_code
    ),
  by = "home_team_name"
)

# Merge in away team variables
matches <- left_join(
  matches,
  teams |>
    select(team_id, team_name, team_code) |>
    rename(
      away_team_id = team_id,
      away_team_name = team_name,
      away_team_code = team_code
    ),
  by = "away_team_name"
)

# Merge in stadium variables
matches <- left_join(
  matches,
  stadiums |>
    select(stadium_id, stadium_name, city_name, country_name),
  by = c("stadium_name", "city_name", "country_name")
)

# Create match ID
matches <- matches |>
  rename(merge_match_id = match_id) |>
  arrange(match_date, match_time, group_name, home_team_name) |>
  group_by(tournament_id) |>
  mutate(
    match_number = 1:n(),
    match_id = str_c(
      "M-", year, "-",
      str_pad(match_number, side = "left", width = 2, pad = "0")
    )
  ) |>
  ungroup()

# Organize data
matches <- matches |>
  arrange(
    match_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    match_id, match_name, stage_name, group_name,
    group_stage, knockout_stage, replayed, replay,
    match_date, match_time,
    stadium_id, stadium_name, city_name, country_name,
    home_team_id, home_team_name, home_team_code,
    away_team_id, away_team_name, away_team_code,
    score, home_team_score, away_team_score,
    home_team_score_margin, away_team_score_margin,
    extra_time, penalty_shootout, score_penalties,
    home_team_score_penalties, away_team_score_penalties,
    result, home_team_win, away_team_win, draw, merge_match_id
  )

# Check for missing data
table(is.na(matches))

# Clean environment
rm(matches_hand_coded, stadiums_hand_coded)

## Team appearances --------------------------------------------------------

# Home team appearances
home_team_appearances <- select(
  matches,
  match_id, tournament_id, tournament_name,
  match_id, match_name, stage_name, group_name,
  group_stage, knockout_stage, replayed, replay,
  match_date, match_time,
  stadium_id, stadium_name, city_name, country_name,
  home_team_id, home_team_name, home_team_code,
  home_team_score, home_team_score_penalties,
  away_team_id, away_team_name, away_team_code,
  away_team_score, away_team_score_penalties,
  result, penalty_shootout, extra_time,
  merge_match_id
)

# Away team appearances
away_team_appearances <- select(
  matches,
  match_id, tournament_id, tournament_name,
  match_id, match_name, stage_name, group_name,
  group_stage, knockout_stage, replayed, replay,
  match_date, match_time,
  stadium_id, stadium_name, city_name, country_name,
  home_team_id, home_team_name, home_team_code,
  home_team_score, home_team_score_penalties,
  away_team_id, away_team_name, away_team_code,
  away_team_score, away_team_score_penalties,
  result, penalty_shootout, extra_time,
  merge_match_id
)

# Rename home team variables
home_team_appearances <- home_team_appearances |>
  mutate(
    home_team = 1,
    away_team = 0
  ) |>
  rename(
    team_id = home_team_id,
    team_name = home_team_name,
    team_code = home_team_code,
    opponent_id = away_team_id,
    opponent_name = away_team_name,
    opponent_code = away_team_code,
    goals_for = home_team_score,
    goals_against = away_team_score,
    penalties_for = home_team_score_penalties,
    penalties_against = away_team_score_penalties
  )

# Rename away team variables
away_team_appearances <- away_team_appearances |>
  mutate(
    home_team = 0,
    away_team = 1
  ) |>
  rename(
    team_id = away_team_id,
    team_name = away_team_name,
    team_code = away_team_code,
    opponent_id = home_team_id,
    opponent_name = home_team_name,
    opponent_code = home_team_code,
    goals_for = away_team_score,
    goals_against = home_team_score,
    penalties_for = away_team_score_penalties,
    penalties_against = home_team_score_penalties
  )

# Stack tibbles
team_appearances <- bind_rows(home_team_appearances, away_team_appearances)

# Code variables
team_appearances <- team_appearances |>
  mutate(
    goal_differential = goals_for - goals_against,
    win = as.numeric(
      (result == "home team win" & home_team == 1) |
        (result == "away team win" & away_team == 1)
    ),
    lose = as.numeric(
      (result == "home team win" & home_team == 0) |
        (result == "away team win" & away_team == 0)
    ),
    draw = as.numeric(result == "draw"),
    result = case_when(
      win == 1 ~ "win",
      lose == 1 ~ "lose",
      draw == 1 ~ "draw"
    )
  )

# Check outcomes
table(team_appearances$result)
table(team_appearances$win)
table(team_appearances$lose)
table(team_appearances$draw)
table(team_appearances$replayed)
table(team_appearances$replay)

# Organize variables
team_appearances <- team_appearances |>
  arrange(
    match_id, desc(home_team)
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    match_id, match_name, stage_name, group_name,
    group_stage, knockout_stage, replayed, replay,
    match_date, match_time,
    stadium_id, stadium_name, city_name, country_name,
    team_id, team_name, team_code,
    opponent_id, opponent_name, opponent_code,
    home_team, away_team,
    goals_for, goals_against, goal_differential,
    extra_time, penalty_shootout, penalties_for, penalties_against,
    result, win, lose, draw, merge_match_id
  )

# Check for missing data
table(is.na(team_appearances))

# Clean environment
rm(home_team_appearances, away_team_appearances)

## Qualified teams -------------------------------------------------------------

# Collapse by tournament
qualified_teams <- team_appearances |>
  arrange(match_date) |>
  group_by(
    tournament_id, tournament_name,
    team_id, team_name, team_code
  ) |>
  summarize(
    count_matches = n(),
    performance = stage_name[n()]
  ) |>
  ungroup()

# Organize variables
qualified_teams <- qualified_teams |>
  arrange(
    tournament_id, team_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    team_id, team_name, team_code,
    count_matches, performance
  )

## Squads ----------------------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_squads.RData")

# Create tournament ID
squads <- wikipedia_squads |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
squads <- left_join(
  squads,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in team variables
squads <- left_join(
  squads,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
squads <- squads |>
  arrange(
    tournament_id, team_name, shirt_number, family_name
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    team_id, team_name, team_code,
    player_id, family_name, given_name,
    shirt_number, position_name, position_code,
    birth_date, player_wikipedia_link
  )

## Players ---------------------------------------------------------------------

# Collapse squad data
players <- squads |>
  mutate(
    year = tournament_id |>
      str_extract("[0-9]{4}")
  ) |>
  group_by(player_id, family_name, given_name, player_wikipedia_link) |>
  summarize(
    position_name = str_c(unique(position_name), collapse = ", "),
    birth_date = str_c(unique(birth_date), collapse = ", "),
    list_tournaments = str_c(year, collapse = ", ")
  ) |>
  ungroup() |>
  mutate(
    goal_keeper = position_name |>
      str_detect("goal keeper") |>
      as.numeric(),
    defender = position_name |>
      str_detect("defender") |>
      as.numeric(),
    midfielder = position_name |>
      str_detect("midfielder") |>
      as.numeric(),
    forward = position_name |>
      str_detect("forward") |>
      as.numeric(),
    count_tournaments = list_tournaments |>
      str_count("[0-9]{4}")
  )

# Check dummy variables
table(
  players$goal_keeper +
    players$defender +
    players$midfielder +
    players$forward
)

# Check that player ID is unique
length(unique(players$player_id)) == nrow(players)

# Organize variables
players <- players |>
  arrange(
    family_name, given_name, birth_date
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, player_id,
    family_name, given_name, birth_date,
    goal_keeper, defender, midfielder, forward,
    count_tournaments, list_tournaments,
    player_wikipedia_link
  )

## Player appearances ----------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_lineups.RData")

# Merge lineup data into match data
player_appearances <- left_join(
  team_appearances |>
    filter(
      match_date > ymd("1970-01-01")
    ) |>
    select(
      merge_match_id, tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code,
      home_team, away_team
    ),
  wikipedia_lineups,
  by = c("merge_match_id" = "match_id", "team_name")
)

# Keeper starters and substitutes
player_appearances <- player_appearances |>
  group_by(
    match_id, home_team
  ) |>
  mutate(
    player_number = 1:n(),
    starter = starting,
    substitute = events |>
      str_detect("subbed on") |>
      as.numeric()
  ) |>
  ungroup() |>
  filter(starting == 1 | substitute == 1)

# Code positions
player_appearances <- player_appearances |>
  mutate(
    position_code = case_when(
      position_code == "CD" ~ "CB", # center back
      position_code == "LCM" ~ "LM", # left center midfielder
      position_code == "RCM" ~ "RM", # right center midfielder
      position_code == "AMF" ~ "AM", # attacking midfielder
      position_code == "WF" ~ "MF", # midfielder
      position_code == "AA" ~ "MF", # midfielder
      TRUE ~ position_code
    ),
    position_name = case_when(
      position_code == "GK" ~ "goal keeper",
      position_code == "DF" ~ "defender",
      position_code == "LB" ~ "left back",
      position_code == "CB" ~ "center back",
      position_code == "RB" ~ "right back",
      position_code == "LWB" ~ "left wing back",
      position_code == "RWB" ~ "right wing back",
      position_code == "SW" ~ "sweeper",
      position_code == "MF" ~ "midfielder",
      position_code == "DM" ~ "defensive midfielder",
      position_code == "AM" ~ "attacking midfielder",
      position_code == "LM" ~ "left midfielder",
      position_code == "CM" ~ "center midfielder",
      position_code == "RM" ~ "right midfielder",
      position_code == "LW" ~ "left winger",
      position_code == "RW" ~ "right winger",
      position_code == "FW" ~ "forward",
      position_code == "SS" ~ "second striker",
      position_code == "LF" ~ "left forward",
      position_code == "CF" ~ "center forward",
      position_code == "RF" ~ "right forward"
    )
  )

# Check positions
table(player_appearances$position_name)
table(player_appearances$position_code)

# Check total number of observations
nrow(player_appearances) == 700 * 2 * 11 + 3223

# Merge in player IDs
player_appearances <- left_join(
  player_appearances,
  squads |>
    select(
      tournament_id, shirt_number, team_name,
      player_id, family_name, given_name
    ),
  by = c("tournament_id", "shirt_number", "team_name")
)

# Organize variables
player_appearances <- player_appearances |>
  arrange(
    match_id, desc(home_team), desc(starter), shirt_number
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code, home_team, away_team,
    player_id, family_name, given_name,
    shirt_number, position_name, position_code,
    starter, substitute, captain
  )

## Goals -----------------------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_goals.RData")

# Merge in match variables
goals <- left_join(
  wikipedia_goals |>
    rename(
      merge_match_id = match_id
    ),
  team_appearances |>
    select(
      merge_match_id,
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code,
      home_team, away_team
    ),
  by = c("merge_match_id", "team_name")
)

# Add URL stem
goals <- goals |>
  mutate(
    player_wikipedia_link = str_c(
      "https://en.wikipedia.org",
      player_wikipedia_link
    )
  )

# Goals before 1970
goals_1930_1966 <- goals |>
  filter(match_date < ymd("1970-01-01") & own_goal == 0)

# Goals since 1970
goals_1970_2018 <- goals |>
  filter(match_date > ymd("1970-01-01") & own_goal == 0)

# Own goals
own_goals <- goals |>
  filter(own_goal == 1)

# Get links to standardize
goals_1930_1966 |>
  filter(!(player_wikipedia_link %in% squads$player_wikipedia_link)) |>
  pull(player_wikipedia_link) |>
  unique()

# Standardize player Wikipedia links
goals_1930_1966 <- goals_1930_1966 |>
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
      TRUE ~ player_wikipedia_link
    )
  )

# Merge in player variables
goals_1930_1966 <- left_join(
  goals_1930_1966,
  squads |>
    select(
      tournament_id, team_id, team_name, team_code,
      player_id, family_name, given_name, player_wikipedia_link
    ) |>
    rename(
      player_team_id = team_id,
      player_team_name = team_name,
      player_team_code = team_code
    ),
  by = c("tournament_id", "player_wikipedia_link")
)

# Merge in player variables
goals_1970_2018 <- left_join(
  goals_1970_2018,
  squads |>
    select(
      tournament_id, team_id, team_name, team_code,
      player_id, family_name, given_name, shirt_number
    ) |>
    rename(
      player_team_id = team_id,
      player_team_name = team_name,
      player_team_code = team_code
    ) |>
    mutate(
      team_name = player_team_name
    ),
  by = c("tournament_id", "team_name", "shirt_number")
)

# Standardize player Wikipedia links
own_goals <- own_goals |>
  mutate(
    player_wikipedia_link = case_when(
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Cho_Kwang-Rae" ~ "https://en.wikipedia.org/wiki/Cho_Kwang-rae",
      player_wikipedia_link == "https://en.wikipedia.org/wiki/Oghenekaro_Etebo" ~ "https://en.wikipedia.org/wiki/Peter_Etebo",
      TRUE ~ player_wikipedia_link
    )
  )

# Merge in player variables
own_goals <- left_join(
  own_goals,
  squads |>
    select(
      tournament_id, team_id, team_name, team_code,
      player_id, family_name, given_name, player_wikipedia_link
    ) |>
    rename(
      player_team_id = team_id,
      player_team_name = team_name,
      player_team_code = team_code
    ),
  by = c("tournament_id", "player_wikipedia_link")
)

# Stack tibbles
goals <- bind_rows(
  goals_1930_1966,
  goals_1970_2018,
  own_goals
)

# Code match period
goals <- goals |>
  mutate(
    minute_regulation = minute_label |>
      str_extract("^[0-9]+") |>
      as.numeric(),
    minute_stoppage = minute_label |>
      str_extract("\\+[0-9]+") |>
      str_extract("[0-9]+") |>
      as.numeric(),
    minute_stoppage = case_when(
      is.na(minute_stoppage) ~ 0,
      TRUE ~ minute_stoppage
    ),
    match_period = code_match_period(minute_label)
  )

# Check player names
check <- goals |>
  select(
    merge_match_id, player_name, family_name, own_goal
  )
rm(check)

# Organize variables
goals <- goals |>
  arrange(match_id, minute_regulation, minute_stoppage) |>
  mutate(
    key_id = 1:n(),
    goal_id = str_c(
      "G-",
      str_pad(1:n(), width = 4, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, goal_id, tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code,
    home_team, away_team,
    player_id, family_name, given_name, shirt_number,
    player_team_id, player_team_name, player_team_code,
    minute_label, minute_regulation, minute_stoppage, match_period,
    own_goal, penalty
  )

# Check team names
table(goals$own_goal, goals$team_name == goals$player_team_name)

# Clean environment
rm(goals_1930_1966, goals_1970_2018, own_goals)

## Penalty kicks ---------------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_penalty_kicks.RData")

# Merge in match variables
penalty_kicks <- left_join(
  wikipedia_penalty_kicks |>
    rename(merge_match_id = match_id),
  team_appearances |>
    select(
      merge_match_id,
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code,
      home_team, away_team
    ),
  by = c("merge_match_id", "team_name")
)

# Merge in player variables
penalty_kicks <- left_join(
  penalty_kicks,
  squads |>
    select(
      tournament_id, team_name, player_id,
      family_name, given_name, shirt_number
    ),
  by = c("tournament_id", "team_name", "shirt_number")
)

# Organize variables
penalty_kicks <- penalty_kicks |>
  arrange(match_id) |>
  mutate(
    key_id = 1:n(),
    penalty_kick_id = str_c(
      "PK-",
      str_pad(1:n(), width = 3, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, penalty_kick_id,
    tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code,
    home_team, away_team,
    player_id, family_name, given_name, shirt_number,
    converted
  )

# Check for missing values
table(is.na(penalty_kicks))

## Bookings --------------------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_lineups.RData")

# Fix parsing error
wikipedia_lineups <- wikipedia_lineups |>
  mutate(
    events = case_when(
      player_name == "Khalil Ghanim yellow card 36' second yellow card 76'" ~ "yellow card 36', second yellow card 76'",
      player_name == "Mal Donaghy red card 60'" ~ "red card 60'",
      TRUE ~ events
    ),
    player_name = case_when(
      player_name == "Khalil Ghanim yellow card 36' second yellow card 76'" ~ "Khalil Ghanim",
      player_name == "Mal Donaghy red card 60'" ~ "Mal Donaghy",
      TRUE ~ player_name
    )
  )

# Expand to have one row per booking
bookings <- wikipedia_lineups |>
  filter(str_detect(events, "card [0-9]+")) |>
  separate_rows(events, sep = ",") |>
  filter(str_detect(events, "card")) |>
  rename(text = events) |>
  mutate(
    text = text |>
      str_squish(),
    yellow_card = text |>
      str_detect("yellow card") |>
      as.numeric(),
    red_card = text |>
      str_detect("red card") |>
      as.numeric(),
    minute_label = text |>
      str_extract("[0-9]+'?(\\+[0-9]+')?") |>
      str_replace("([0-9]+)\\+", "\\1'+"),
    minute_regulation = minute_label |>
      str_extract("^[0-9]+") |>
      as.numeric(),
    minute_stoppage = minute_label |>
      str_extract("\\+[0-9]+") |>
      str_extract("[0-9]+") |>
      as.numeric(),
    minute_stoppage = case_when(
      is.na(minute_stoppage) ~ 0,
      TRUE ~ minute_stoppage
    ),
    match_period = code_match_period(minute_label),
    second_yellow_card = text |>
      str_detect("second") |>
      as.numeric(),
    sending_off = as.numeric(second_yellow_card == 1 | red_card == 1)
  ) |>
  filter(yellow_card == 1 | red_card == 1) |>
  select(
    match_id, team_name, player_name, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    yellow_card, red_card, second_yellow_card, sending_off
  )

# Check number of sendings off
table(bookings$second_yellow_card)
table(bookings$red_card)
table(bookings$sending_off)

# Check number of sendings off by tournament
173 - 23 - 2 == 59 + 89

# Merge in match variables
bookings <- left_join(
  bookings |>
    rename(
      merge_match_id = match_id
    ),
  team_appearances |>
    select(
      merge_match_id,
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code,
      home_team, away_team
    ),
  by = c("merge_match_id", "team_name")
)

# Merge in player variables
bookings <- left_join(
  bookings,
  squads |>
    select(
      tournament_id, team_name,
      player_id, family_name, given_name, shirt_number
    ),
  by = c("tournament_id", "team_name", "shirt_number")
)

# Organize variables
bookings <- bookings |>
  arrange(
    match_id, minute_regulation, minute_stoppage
  ) |>
  mutate(
    key_id = 1:n(),
    booking_id = str_c(
      "B-",
      str_pad(1:n(), width = 4, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, booking_id,
    tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code, home_team, away_team,
    player_id, family_name, given_name, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    yellow_card, red_card, second_yellow_card, sending_off
  )

## Substitutions ---------------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_lineups.RData")

# Correct substitution arrows
indexes <- wikipedia_lineups$on_bench == 1 & str_detect(wikipedia_lineups$events, "subbed off") & !str_detect(wikipedia_lineups$events, "subbed on")
wikipedia_lineups$events[indexes] <- str_replace(wikipedia_lineups$events[indexes], "subbed off", "subbed on")

# Fix parsing error
wikipedia_lineups <- wikipedia_lineups |>
  mutate(
    events = case_when(
     events == "yellow card 68', subbed on 56' subbed off 90+1'" ~ "yellow card 68', subbed on 56', subbed off 90+1'",
     match_id == "Peru v Iran (1978-06-11)" & player_name == "Hassan Roshan" ~ "subbed off 66';",
     match_id == "Peru v Iran (1978-06-11)" & player_name == "Hossein Faraki" ~ "subbed off 51'",
     TRUE ~ events
    )
  )

# Expand to have one row per player per substitution
substitutions <- wikipedia_lineups |>
  filter(str_detect(events, "subbed")) |>
  separate_rows(events, sep = ",") |>
  filter(str_detect(events, "subbed")) |>
  rename(text = events) |>
  mutate(
    text = text |>
      str_remove("^.*?card") |>
      str_squish(),
    minute_label = text |>
      str_extract("[0-9]+'?(\\+[0-9]+')?") |>
      str_replace("([0-9]+)\\+", "\\1'+"),
    minute_regulation = minute_label |>
      str_extract("^[0-9]+") |>
      as.numeric(),
    minute_stoppage = minute_label |>
      str_extract("\\+[0-9]+") |>
      str_extract("[0-9]+") |>
      as.numeric(),
    minute_stoppage = case_when(
      is.na(minute_stoppage) ~ 0,
      TRUE ~ minute_stoppage
    ),
    match_period = code_match_period(minute_label),
    coming_on = text |>
      str_detect("subbed on") |>
      as.numeric(),
    going_off = text |>
      str_detect("subbed off") |>
      as.numeric()
  ) |>
  select(
    match_id, team_name, player_name, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    going_off, coming_on
  )

# Merge in match variables
substitutions <- left_join(
  substitutions |>
    rename(
      merge_match_id = match_id
    ),
  team_appearances |>
    select(
      merge_match_id,
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code,
      home_team, away_team
    ),
  by = c("merge_match_id", "team_name")
)

# Merge in player variables
substitutions <- left_join(
  substitutions,
  squads |>
    select(
      tournament_id, team_name,
      player_id, family_name, given_name, shirt_number
    ),
  by = c("tournament_id", "team_name", "shirt_number")
)

# Organize variables
substitutions <- substitutions |>
  arrange(
    match_id, minute_regulation, minute_stoppage, desc(going_off)
  ) |>
  mutate(
    key_id = 1:n(),
    substitution_id = str_c(
      "S-",
      str_pad(1:n(), width = 4, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, substitution_id,
    tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code, home_team, away_team,
    player_id, family_name, given_name, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    going_off, coming_on
  )

# Check the number of players going off and coming on
table(substitutions$going_off)
table(substitutions$coming_on)

# Check subs per game
substitutions |>
  group_by(match_id) |>
  summarize(
    check = sum(going_off) == sum(coming_on)
  ) |>
  pull() |>
  table()

## Referees --------------------------------------------------------------------

# Load data
referee_names_cleaned <- read_csv("data-raw/referee-names/referee_names_cleaned.csv")

# Collapse by name
referees <- referee_names_cleaned |>
  group_by(
    family_name, given_name, referee_wikipedia_link,
    country_name, confederation_code
  ) |>
  summarize() |>
  ungroup()

# Check for duplicates
table(duplicated(select(referees, family_name, given_name)))

# Merge in confederation variables
referees <- left_join(
  referees,
  confederations |>
    select(
      confederation_id, confederation_name, confederation_code
    ),
  by = "confederation_code"
)

# Organize variables
referees <- referees |>
  arrange(
    family_name, given_name
  ) |>
  mutate(
    key_id = 1:n(),
    referee_id = str_c(
      "R-",
      str_pad(1:n(), width = 3, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, referee_id,
    family_name, given_name,
    country_name, confederation_id, confederation_name, confederation_code,
    referee_wikipedia_link
  )

# Check that links are unique
check <- referees |>
  filter(referee_wikipedia_link != "not available") |>
  mutate(
    duplicate = duplicated(referee_wikipedia_link)
  )
table(check$duplicate)
rm(check)

## Referee appearances ---------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_referees.RData")
referee_names_cleaned <- read_csv("data-raw/referee-names/referee_names_cleaned.csv")

# Referee appearances
referee_appearances <- wikipedia_referees |>
  select(
    match_id, referee
  ) |>
  rename(
    referee_name = referee
  )

# Organize variables
referee_names_cleaned <- referee_names_cleaned |>
  select(
    referee_name, family_name, given_name,
    country_name, confederation_code, referee_wikipedia_link
  )

# Merge in cleaned names
referee_appearances <- left_join(
  referee_appearances,
  referee_names_cleaned |>
    select(
      referee_name, family_name, given_name
    ),
  by = "referee_name"
)

# Merge in match variables
referee_appearances <- left_join(
  referee_appearances |>
    rename(
      merge_match_id = match_id
    ),
  matches |>
    select(
      merge_match_id,
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name
    ),
  by = c("merge_match_id")
)

# Merge in referee variables
referee_appearances <- left_join(
  referee_appearances,
  referees |>
    select(
      referee_id, family_name, given_name,
      country_name, confederation_id, confederation_name, confederation_code
    ),
  by = c("family_name", "given_name")
)

# Organize variables
referee_appearances <- referee_appearances |>
  arrange(
    match_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    referee_id, family_name, given_name,
    country_name, confederation_id, confederation_name, confederation_code
  )

# Check for missing values
table(is.na(referee_appearances))

## Referee appointments --------------------------------------------------------

# Collapse by tournament
referee_appointments <- referee_appearances |>
  group_by(
    tournament_id, tournament_name,
    referee_id, family_name, given_name,
    country_name, confederation_id, confederation_name, confederation_code
  ) |>
  summarize() |>
  ungroup()

# Organize variables
referee_appointments <- referee_appointments |>
  arrange(
    tournament_id, referee_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    referee_id, family_name, given_name,
    country_name, confederation_id, confederation_name, confederation_code
  )

## Managers --------------------------------------------------------------------

# Load data
manager_names_cleaned <- read_csv("data-raw/manager-names/manager_names_cleaned.csv")

# Collapse by name
managers <- manager_names_cleaned |>
  group_by(
    family_name, given_name, manager_wikipedia_link,
    country_name
  ) |>
  summarize() |>
  ungroup()

# Check for duplicates
table(duplicated(select(managers, family_name, given_name)))

# Organize variables
managers <- managers |>
  arrange(
    family_name, given_name
  ) |>
  mutate(
    key_id = 1:n(),
    manager_id = str_c(
      "M-",
      str_pad(1:n(), width = 3, side = "left", pad = "0")
    )
  ) |>
  select(
    key_id, manager_id,
    family_name, given_name,
    country_name, manager_wikipedia_link
  )

## Manager appointments --------------------------------------------------------

# Load data
load("data-raw/Wikipedia-data/wikipedia_managers.RData")
manager_names_cleaned <- read_csv("data-raw/manager-names/manager_names_cleaned.csv")

# Drop one observation
wikipedia_managers <- wikipedia_managers |>
  filter(manager_name != "Munich air disaster")

# Clean team names
wikipedia_managers <- wikipedia_managers |>
  mutate(
    team_name = case_when(
      team_name == "China PR" ~ "China",
      TRUE ~ team_name
    )
  )

# Merge in cleaned manager names
manager_appointments <- left_join(
  wikipedia_managers |>
    select(year, team_name, manager_name),
  manager_names_cleaned |>
    select(manager_name, family_name, given_name, country_name),
  by = "manager_name"
)

# Merge in manager ID
manager_appointments <- left_join(
  manager_appointments,
  managers |>
    select(manager_id, family_name, given_name, country_name),
  by = c("family_name", "given_name", "country_name")
)

# Merge in tournament variables
manager_appointments <- left_join(
  manager_appointments |>
    mutate(
      tournament_id = str_c("WC-", year)
    ),
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in team variables
manager_appointments <- left_join(
  manager_appointments,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
manager_appointments <- manager_appointments |>
  arrange(
    tournament_id, manager_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    team_id, team_name, team_code,
    manager_id, family_name, given_name, country_name
  )

## Manager appearances ---------------------------------------------------------

# Merge appointments data into match data
manager_appearances <- left_join(
  team_appearances |>
    select(
      tournament_id, tournament_name,
      match_id, match_name, match_date, stage_name, group_name,
      team_id, team_name, team_code, home_team, away_team
    ),
  manager_appointments |>
    select(
      tournament_name, team_name,
      manager_id, family_name, given_name, country_name
    ),
  by = c("tournament_name", "team_name")
)

# Organize variables
manager_appearances <- manager_appearances |>
  arrange(
    match_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    match_id, match_name, match_date, stage_name, group_name,
    team_id, team_name, team_code, home_team, away_team,
    manager_id, family_name, given_name, country_name
  )

# Check for missing values
table(is.na(manager_appearances))

## Awards ----------------------------------------------------------------------

# Load data
awards <- read_csv("data-raw/hand-coded-tables/awards.csv")

# Organize variables
awards <- awards |>
  arrange(
    award_id
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, award_id, award_name,
    award_description, year_introduced
  )

## Award winners ---------------------------------------------------------------

# Load data
award_winners <- read_csv("data-raw/hand-coded-tables/award_winners.csv")

# Code tournament ID
award_winners <- award_winners |>
  mutate(
    tournament_id = str_c("WC-", year)
  )

# Merge in tournament variables
award_winners <- left_join(
  award_winners,
  tournaments |>
    select(tournament_id, tournament_name),
  by = "tournament_id"
)

# Merge in player variables
award_winners <- left_join(
  award_winners,
  players |>
    select(player_id, family_name, given_name),
  by = c("family_name", "given_name")
)

# Merge in team variables
award_winners <- left_join(
  award_winners,
  teams |>
    select(team_id, team_name, team_code),
  by = "team_name"
)

# Organize variables
award_winners <- award_winners |>
  arrange(
    tournament_id, award_id, family_name
  ) |>
  mutate(
    key_id = 1:n()
  ) |>
  select(
    key_id, tournament_id, tournament_name,
    award_id, award_name, shared,
    player_id, family_name, given_name,
    team_id, team_name, team_code
  )

# Check for missing values -----------------------------------------------------

table(is.na(tournaments))
table(is.na(confederations))
table(is.na(teams))
table(is.na(players))
table(is.na(managers))
table(is.na(referees))
table(is.na(stadiums))
table(is.na(matches))
table(is.na(awards))

table(is.na(qualified_teams))
table(is.na(squads))
table(is.na(manager_appointments))
table(is.na(referee_appointments))

table(is.na(team_appearances))
table(is.na(player_appearances))
table(is.na(manager_appearances))
table(is.na(referee_appearances))

table(is.na(goals))
table(is.na(penalty_kicks))
table(is.na(bookings))
table(is.na(substitutions))

table(is.na(host_countries))
table(is.na(tournament_standings))
table(is.na(groups))
table(is.na(group_standings))
table(is.na(tournament_standings))
table(is.na(award_winners))

# Check the number of variables ------------------------------------------------

names(tournaments)
names(confederations)
names(teams)
names(players)
names(managers)
names(referees)
names(stadiums)
names(matches)
names(awards)

names(qualified_teams)
names(squads)
names(manager_appointments)
names(referee_appointments)

names(team_appearances)
names(player_appearances)
names(manager_appearances)
names(referee_appearances)

names(goals)
names(penalty_kicks)
names(bookings)
names(substitutions)

names(host_countries)
names(tournament_stages)
names(groups)
names(group_standings)
names(tournament_standings)
names(award_winners)

# Drop variables
matches <- matches |>
  select(-merge_match_id)
squads <- squads |>
  select(-birth_date, -player_wikipedia_link)
team_appearances <- team_appearances |>
  select(-merge_match_id)

# Save data --------------------------------------------------------------------

save(tournaments, file = "data/tournaments.RData")
save(confederations, file = "data/confederations.RData")
save(teams, file = "data/teams.RData")
save(players, file = "data/players.RData")
save(managers, file = "data/managers.RData")
save(referees, file = "data/referees.RData")
save(stadiums, file = "data/stadiums.RData")
save(matches, file = "data/matches.RData")
save(awards, file = "data/awards.RData")

save(qualified_teams, file = "data/qualified_teams.RData")
save(squads, file = "data/squads.RData")
save(manager_appointments, file = "data/manager_appointments.RData")
save(referee_appointments, file = "data/referee_appointments.RData")

save(team_appearances, file = "data/team_appearances.RData")
save(player_appearances, file = "data/player_appearances.RData")
save(manager_appearances, file = "data/manager_appearances.RData")
save(referee_appearances, file = "data/referee_appearances.RData")

save(goals, file = "data/goals.RData")
save(penalty_kicks, file = "data/penalty_kicks.RData")
save(bookings, file = "data/bookings.RData")
save(substitutions, file = "data/substitutions.RData")

save(host_countries, file = "data/host_countries.RData")
save(tournament_stages, file = "data/tournament_stages.RData")
save(groups, file = "data/groups.RData")
save(group_standings, file = "data/group_standings.RData")
save(tournament_standings, file = "data/tournament_standings.RData")
save(award_winners, file = "data/award_winners.RData")
