# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(jsonlite)

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

# Convert tables to JSON -------------------------------------------------------

database_json <- toJSON(
  list(
    tournaments = tournaments,
    confederations = confederations,
    teams = teams,
    players = players,
    managers = managers,
    referees = referees,
    stadiums = stadiums,
    matches = matches,
    awards = awards,

    qualified_teams = qualified_teams,
    squads = squads,
    manager_appointments = manager_appointments,
    referee_appointments = referee_appointments,

    team_appearances = team_appearances,
    player_appearances = player_appearances,
    manager_appearances = manager_appearances,
    referee_appearances = referee_appearances,

    goals = goals,
    penalty_kicks = penalty_kicks,
    bookings = bookings,
    substitutions = substitutions,

    host_countries = host_countries,
    tournament_stages = tournament_stages,
    groups = groups,
    group_standings = group_standings,
    tournament_standings = tournament_standings,
    award_winners = award_winners
  )
)

# Write a JSON file ------------------------------------------------------------

write_json(database_json, "data-json/worldcup.json")
