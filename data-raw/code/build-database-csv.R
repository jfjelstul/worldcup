# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)

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

# Write CSV files --------------------------------------------------------------

write_csv(tournaments, "data-csv/tournaments.csv")
write_csv(confederations, "data-csv/confederations.csv")
write_csv(teams, "data-csv/teams.csv")
write_csv(players, "data-csv/players.csv")
write_csv(managers, "data-csv/managers.csv")
write_csv(referees, "data-csv/referees.csv")
write_csv(stadiums, "data-csv/stadiums.csv")
write_csv(matches, "data-csv/matches.csv")
write_csv(awards, "data-csv/awards.csv")

write_csv(qualified_teams, "data-csv/qualified_teams.csv")
write_csv(squads, "data-csv/squads.csv")
write_csv(manager_appointments, "data-csv/manager_appointments.csv")
write_csv(referee_appointments, "data-csv/referee_appointments.csv")

write_csv(team_appearances, "data-csv/team_appearances.csv")
write_csv(player_appearances, "data-csv/player_appearances.csv")
write_csv(manager_appearances, "data-csv/manager_appearances.csv")
write_csv(referee_appearances, "data-csv/referee_appearances.csv")

write_csv(goals, "data-csv/goals.csv")
write_csv(penalty_kicks, "data-csv/penalty_kicks.csv")
write_csv(bookings, "data-csv/bookings.csv")
write_csv(substitutions, "data-csv/substitutions.csv")

write_csv(host_countries, "data-csv/host_countries.csv")
write_csv(tournament_stages, "data-csv/tournament_stages.csv")
write_csv(groups, "data-csv/groups.csv")
write_csv(group_standings, "data-csv/group_standings.csv")
write_csv(tournament_standings, "data-csv/tournament_standings.csv")
write_csv(award_winners, "data-csv/award_winners.csv")
