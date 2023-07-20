# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(RSQLite)
library(DBI)

# Read in data -----------------------------------------------------------------

load("data/tournaments.RData")
load("data/confederations.RData")
load("data/teams.RData")
load("data/players.RData")
load("data/managers.RData")
load("data/referees.RData")
load("data/stadiums.RData")
load("data/matches.RData")
load("data/awards.RData")

load("data/qualified_teams.RData")
load("data/squads.RData")
load("data/manager_appointments.RData")
load("data/referee_appointments.RData")

load("data/team_appearances.RData")
load("data/player_appearances.RData")
load("data/manager_appearances.RData")
load("data/referee_appearances.RData")

load("data/goals.RData")
load("data/penalty_kicks.RData")
load("data/bookings.RData")
load("data/substitutions.RData")

load("data/host_countries.RData")
load("data/tournament_stages.RData")
load("data/groups.RData")
load("data/group_standings.RData")
load("data/tournament_standings.RData")
load("data/award_winners.RData")

# Select variables -------------------------------------------------------------

# Tournaments
tournaments <- tournaments |>
  mutate(
    host_won = as.logical(host_won),
    group_stage = as.logical(group_stage),
    second_group_stage = as.logical(second_group_stage),
    final_round = as.logical(final_round),
    round_of_16 = as.logical(round_of_16),
    quarter_finals = as.logical(quarter_finals),
    semi_finals = as.logical(semi_finals),
    third_place_match = as.logical(third_place_match),
    final = as.logical(final),
  ) |>
  select(
    tournament_id, tournament_name,
    year, start_date, end_date,
    host_country, winner, host_won, count_teams,
    group_stage, second_group_stage, final_round,
    round_of_16, quarter_finals, semi_finals,
    third_place_match, final
  )

# Confederations
confederations <- confederations |>
  select(
    confederation_id, confederation_name, confederation_code,
    confederation_wikipedia_link
  )

# Teams
teams <- teams |>
  mutate(
    mens_team = as.logical(mens_team),
    womens_team = as.logical(womens_team)
  ) |>
  select(
    team_id, team_name, team_code,
    mens_team, womens_team,
    federation_name, region_name, confederation_id,
    mens_team_wikipedia_link, womens_team_wikipedia_link,
    federation_wikipedia_link
  )

# Players
players <- players |>
  mutate(
    goal_keeper = as.logical(goal_keeper),
    defender = as.logical(defender),
    midfielder = as.logical(midfielder),
    forward = as.logical(forward),
    female = as.logical(female)
  ) |>
  select(
    player_id, family_name, given_name, birth_date, female,
    goal_keeper, defender, midfielder, forward,
    count_tournaments, list_tournaments,
    player_wikipedia_link
  )

# Managers
managers <- managers |>
  mutate(
    female = as.logical(female)
  ) |>
  select(
    manager_id, family_name, given_name, female,
    country_name, manager_wikipedia_link
  )

# Referees
referees <- referees |>
  mutate(
    female = as.logical(female)
  ) |>
  select(
    referee_id, family_name, given_name, female,
    country_name, confederation_id, referee_wikipedia_link
  )

# Stadiums
stadiums <- stadiums |>
  select(
    stadium_id, stadium_name, city_name, country_name,
    stadium_capacity, stadium_wikipedia_link, city_wikipedia_link
  )

# Matches
matches <- matches |>
  mutate(
    group_stage = as.logical(group_stage),
    knockout_stage = as.logical(knockout_stage),
    replayed = as.logical(replayed),
    replay = as.logical(replay),
    extra_time = as.logical(extra_time),
    penalty_shootout = as.logical(penalty_shootout),
    home_team_win = as.logical(home_team_win),
    away_team_win = as.logical(away_team_win),
    draw = as.logical(draw)
  ) |>
  select(
    tournament_id, match_id, match_name, stage_name, group_name,
    group_stage, knockout_stage, replayed, replay,
    match_date, match_time, stadium_id,
    home_team_id, away_team_id,
    score, home_team_score, away_team_score,
    home_team_score_margin, away_team_score_margin,
    extra_time, penalty_shootout, score_penalties,
    home_team_score_penalties, away_team_score_penalties,
    result, home_team_win, away_team_win, draw
  )

# Awards
awards <- awards |>
  select(
    award_id, award_name,
    award_description, year_introduced
  )

# Qualified teams
qualified_teams <- qualified_teams |>
  select(
    tournament_id, team_id,
    count_matches, performance
  )

# Squads
squads <- squads |>
  select(
    tournament_id, team_id, player_id,
    shirt_number, position_name, position_code
  )

# Manager appointments
manager_appointments <- manager_appointments |>
  select(
    tournament_id, team_id, manager_id
  )

# Referee appointments
referee_appointments <- referee_appointments |>
  select(
    tournament_id, referee_id
  )

# Team appearances
team_appearances <- team_appearances |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    extra_time = as.logical(extra_time),
    penalty_shootout = as.logical(penalty_shootout),
    win = as.logical(win),
    lose = as.logical(lose),
    draw = as.logical(draw)
  ) |>
  select(
    tournament_id, match_id, team_id, opponent_id,
    home_team, away_team,
    goals_for, goals_against, goal_differential,
    extra_time, penalty_shootout,
    penalties_for, penalties_against,
    result, win, lose, draw
  )

# Player appearances
player_appearances <- player_appearances |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    starter = as.logical(starter),
    substitute = as.logical(substitute)
  ) |>
  select(
    tournament_id, match_id, team_id,
    home_team, away_team,
    player_id,
    shirt_number, position_name, position_code,
    starter, substitute
  )

# Manager appearances
manager_appearances <- manager_appearances |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team)
  ) |>
  select(
    tournament_id, match_id, team_id,
    home_team, away_team,
    manager_id
  )

# Referee appearances
referee_appearances <- referee_appearances |>
  select(
    tournament_id, match_id, referee_id
  )

# Goals
goals <- goals |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    own_goal = as.logical(own_goal),
    penalty = as.logical(penalty)
  ) |>
  select(
    goal_id, tournament_id, match_id,
    team_id, home_team, away_team,
    player_id, shirt_number, player_team_id,
    minute_label, minute_regulation, minute_stoppage, match_period,
    own_goal, penalty
  )

# Penalty kicks
penalty_kicks <- penalty_kicks |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    converted = as.logical(converted)
  ) |>
  select(
    penalty_kick_id, tournament_id, match_id,
    team_id, home_team, away_team,
    player_id, shirt_number, converted
  )

# Bookings
bookings <- bookings |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    yellow_card = as.logical(yellow_card),
    red_card = as.logical(red_card),
    second_yellow_card = as.logical(second_yellow_card),
    sending_off = as.logical(sending_off)
  ) |>
  select(
    booking_id, tournament_id, match_id,
    team_id, home_team, away_team,
    player_id, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    yellow_card, red_card, second_yellow_card, sending_off
  )

# Substitutions
substitutions <- substitutions |>
  mutate(
    home_team = as.logical(home_team),
    away_team = as.logical(away_team),
    going_off = as.logical(going_off),
    coming_on = as.logical(coming_on)
  ) |>
  select(
    substitution_id, tournament_id, match_id,
    team_id, home_team, away_team,
    player_id, shirt_number,
    minute_label, minute_regulation, minute_stoppage, match_period,
    going_off, coming_on
  )

# Host countries
host_countries <- host_countries |>
  select(
    tournament_id, team_id, performance
  )

# Tournament stages
tournament_stages <- tournament_stages |>
  mutate(
    group_stage = as.logical(group_stage),
    knockout_stage = as.logical(knockout_stage),
    unbalanced_groups = as.logical(unbalanced_groups)
  ) |>
  select(
    tournament_id,
    stage_number, stage_name,
    group_stage, knockout_stage, unbalanced_groups,
    start_date, end_date,
    count_matches, count_teams, count_scheduled,
    count_replays, count_playoffs, count_walkovers
  )

# Groups
groups <- groups |>
  select(
    tournament_id,
    stage_number, stage_name, group_name, count_teams
  )

# Group standings
group_standings <- group_standings |>
  mutate(
    advanced = as.logical(advanced)
  ) |>
  select(
    tournament_id,
    stage_number, stage_name, group_name,
    position, team_id,
    played, wins, draws, losses,
    goals_for, goals_against, goal_difference,
    points, advanced
  )

# Tournament standings
tournament_standings <- tournament_standings |>
  select(
    tournament_id, position, team_id
  )

# Award winners
award_winners <- award_winners |>
  mutate(
    shared = as.logical(shared)
  ) |>
  select(
    tournament_id, award_id, shared,
    player_id, team_id
  )

# Create an SQL database -------------------------------------------------------

con <- dbConnect(SQLite(), "data-sqlite/worldcup.db", extended_types = TRUE)

# Load tables to database ------------------------------------------------------

# Turn on foreign keys
dbExecute(con, "PRAGMA foreign_keys = ON")

# Read in SQL schema
statement <- readLines("data-sqlite/SQL-schema.txt")
statement <- str_c(statement, collapse = "\n")
statement <- str_split(statement, ";")
statement <- unlist(statement)
statement <- statement[-length(statement)]

# Create schema
for(i in 1:length(statement)) {
  dbExecute(con, statement[i])
}

# Check tables
dbListTables(con)

# Add data
dbAppendTable(con, "tournaments", tournaments, overwrite = TRUE)
dbAppendTable(con, "confederations", confederations, overwrite = TRUE)
dbAppendTable(con, "teams", teams, overwrite = TRUE)
dbAppendTable(con, "players", players, overwrite = TRUE)
dbAppendTable(con, "managers", managers, overwrite = TRUE)
dbAppendTable(con, "referees", referees, overwrite = TRUE)
dbAppendTable(con, "stadiums", stadiums, overwrite = TRUE)
dbAppendTable(con, "matches", matches, overwrite = TRUE)
dbAppendTable(con, "awards", awards, overwrite = TRUE)

dbAppendTable(con, "qualified_teams", qualified_teams, overwrite = TRUE)
dbAppendTable(con, "squads", squads, overwrite = TRUE)
dbAppendTable(con, "manager_appointments", manager_appointments, overwrite = TRUE)
dbAppendTable(con, "referee_appointments", referee_appointments, overwrite = TRUE)

dbAppendTable(con, "team_appearances", team_appearances, overwrite = TRUE)
dbAppendTable(con, "player_appearances", player_appearances, overwrite = TRUE)
dbAppendTable(con, "manager_appearances", manager_appearances, overwrite = TRUE)
dbAppendTable(con, "referee_appearances", referee_appearances, overwrite = TRUE)

dbAppendTable(con, "goals", goals, overwrite = TRUE)
dbAppendTable(con, "penalty_kicks", penalty_kicks, overwrite = TRUE)
dbAppendTable(con, "bookings", bookings, overwrite = TRUE)
dbAppendTable(con, "substitutions", substitutions, overwrite = TRUE)

dbAppendTable(con, "host_countries", host_countries, overwrite = TRUE)
dbAppendTable(con, "tournament_stages", tournament_stages, overwrite = TRUE)
dbAppendTable(con, "groups", groups, overwrite = TRUE)
dbAppendTable(con, "group_standings", group_standings, overwrite = TRUE)
dbAppendTable(con, "tournament_standings", tournament_standings, overwrite = TRUE)
dbAppendTable(con, "award_winners", award_winners, overwrite = TRUE)

# Test queries -----------------------------------------------------------------

dbListTables(con)
test <- dbGetQuery(con, "SELECT * FROM tournaments")
test <- dbGetQuery(con, "SELECT * FROM goals")

# Close database ---------------------------------------------------------------

dbDisconnect(con)
