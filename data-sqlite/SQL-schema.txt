-- SQL schema for the Fjelstul World Cup Database

-- the order of the datasets for creating primary and foreign keys:

-- group 1:
-- tournaments
-- confederations
-- teams
-- players
-- managers
-- referees
-- stadiums
-- matches
-- awards

-- group 2:
-- qualified_teams
-- squads
-- manager_appointments
-- referee_appointments

-- group 3:
-- team_appearances
-- player_appearances
-- manager_appearances
-- referee_appearances

-- group 4:
-- goals
-- penalty_kicks
-- bookings
-- substitutions

-- group 5:
-- host_countries
-- tournament_stages
-- groups
-- group_standings
-- tournament_standings
-- award_winners

-- start by dropping all of the tables without primary keys
-- group 2:
DROP TABLE IF EXISTS qualified_teams;
DROP TABLE IF EXISTS squads;
DROP TABLE IF EXISTS manager_appointments;
DROP TABLE IF EXISTS referee_appointments;

-- group 3:
DROP TABLE IF EXISTS team_appearances;
DROP TABLE IF EXISTS player_appearances;
DROP TABLE IF EXISTS manager_appearances;
DROP TABLE IF EXISTS referee_appearances;

-- group 5:
DROP TABLE IF EXISTS host_countries;
DROP TABLE IF EXISTS tournament_stages;
DROP TABLE IF EXISTS groups;
DROP TABLE IF EXISTS group_standings;
DROP TABLE IF EXISTS tournament_standings;
DROP TABLE IF EXISTS award_winners;

-- then drop tables with primary keys but without dependencies
-- group 4:
DROP TABLE IF EXISTS goals;
DROP TABLE IF EXISTS penalty_kicks;
DROP TABLE IF EXISTS bookings;
DROP TABLE IF EXISTS substitutions;

-- then drop tables with primary keys and dependencies in reverse order
-- group 1:
DROP TABLE IF EXISTS awards;
DROP TABLE IF EXISTS matches;
DROP TABLE IF EXISTS stadiums;
DROP TABLE IF EXISTS referees;
DROP TABLE IF EXISTS managers;
DROP TABLE IF EXISTS players;
DROP TABLE IF EXISTS teams;
DROP TABLE IF EXISTS confederations;
DROP TABLE IF EXISTS tournaments;

-- tournaments
-- one record per tournament
CREATE TABLE tournaments(
  tournament_id TEXT NOT NULL,
  tournament_name TEXT,
  year INTEGER,
  start_date DATE,
  end_date DATE,
  host_country TEXT,
  winner TEXT,
  host_won BOOLEAN,
  count_teams INTEGER,
  group_stage BOOLEAN,
  second_group_stage BOOLEAN,
  final_round BOOLEAN,
  round_of_16 BOOLEAN,
  quarter_finals BOOLEAN,
  semi_finals BOOLEAN,
  third_place_match BOOLEAN,
  final BOOLEAN,
  PRIMARY KEY (tournament_id)
);

-- confederations
-- one record per confederation
CREATE TABLE confederations(
  confederation_id TEXT NOT NULL,
  confederation_name TEXT,
  confederation_code TEXT,
  confederation_wikipedia_link TEXT,
  PRIMARY KEY (confederation_id)
);

-- teams
-- one record per team
CREATE TABLE teams(
  team_id TEXT NOT NULL,
  team_name TEXT,
  team_code TEXT,
  federation_name TEXT,
  region_name TEXT,
  confederation_id TEXT NOT NULL,
  team_wikipedia_link TEXT,
  federation_wikipedia_link TEXT,
  PRIMARY KEY (team_id),
  FOREIGN KEY (confederation_id) REFERENCES confederations (confederation_id)
);

-- players
-- one record per player
CREATE TABLE players(
  player_id TEXT NOT NULL,
  family_name TEXT,
  given_name TEXT,
  birth_date DATE,
  goal_keeper BOOLEAN,
  defender BOOLEAN,
  midfielder BOOLEAN,
  forward BOOLEAN,
  count_tournaments INTEGER,
  list_tournaments TEXT,
  player_wikipedia_link TEXT,
  PRIMARY KEY (player_id)
);

-- managers
-- one record per manager
CREATE TABLE managers(
  manager_id TEXT NOT NULL,
  family_name TEXT,
  given_name TEXT,
  country_name TEXT,
  manager_wikipedia_link TEXT,
  PRIMARY KEY (manager_id)
);

-- referees
-- one record per referee
CREATE TABLE referees(
  referee_id TEXT NOT NULL,
  family_name TEXT,
  given_name TEXT,
  country_name TEXT,
  confederation_id TEXT,
  referee_wikipedia_link TEXT,
  PRIMARY KEY (referee_id),
  FOREIGN KEY (confederation_id) REFERENCES confederations (confederation_id)
);

-- stadiums
-- one record per stadium
CREATE TABLE stadiums(
  stadium_id TEXT NOT NULL,
  stadium_name TEXT,
  city_name TEXT,
  country_name TEXT,
  stadium_capacity INTEGER,
  stadium_wikipedia_link TEXT,
  city_wikipedia_link TEXT,
  PRIMARY KEY (stadium_id)
);

-- matches
-- one record per match
CREATE TABLE matches(
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  match_name TEXT,
  stage_name TEXT,
  group_name TEXT,
  group_stage BOOLEAN,
  knockout_stage BOOLEAN,
  replayed BOOLEAN,
  replay BOOLEAN,
  match_date TEXT,
  match_time TEXT,
  stadium_id TEXT NOT NULL,
  home_team_id TEXT NOT NULL,
  away_team_id TEXT NOT NULL,
  score TEXT,
  home_team_score INTEGER,
  away_team_score INTEGER,
  home_team_score_margin INTEGER,
  away_team_score_margin INTEGER,
  extra_time BOOLEAN,
  penalty_shootout BOOLEAN,
  score_penalties TEXT,
  home_team_score_penalties INTEGER,
  away_team_score_penalties INTEGER,
  result TEXT,
  home_team_win BOOLEAN,
  away_team_win BOOLEAN,
  draw BOOLEAN,
  PRIMARY KEY (match_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (stadium_id) REFERENCES stadiums (stadium_id),
  FOREIGN KEY (home_team_id) REFERENCES teams (team_id),
  FOREIGN KEY (away_team_id) REFERENCES teams (team_id)
);

-- awards
-- one record per award
CREATE TABLE awards(
  award_id TEXT NOT NULL,
  award_name TEXT,
  award_description TEXT,
  year_introduced INTEGER,
  PRIMARY KEY (award_id)
);

-- qualified_teams
-- one observation per team per tournament
CREATE TABLE qualified_teams(
  tournament_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  count_matches INTEGER,
  performance TEXT,
  PRIMARY KEY (tournament_id, team_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id)
);

-- squads
-- one observation per player per team per tournament
CREATE TABLE squads(
  tournament_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  position_name TEXT,
  position_code TEXT,
  PRIMARY KEY (tournament_id, team_id, player_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id)
);

-- manager_appointments
-- one observation per manager per team per tournament
CREATE TABLE manager_appointments(
  tournament_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  manager_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, team_id, manager_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (manager_id) REFERENCES managers (manager_id)
);

-- referee_appointments
-- one observation per referee per tournament
CREATE TABLE referee_appointments(
  tournament_id TEXT NOT NULL,
  referee_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, referee_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (referee_id) REFERENCES referees (referee_id)
);

-- team_appearances
-- one observation per team per match
CREATE TABLE team_appearances(
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  opponent_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  goals_for INTEGER,
  goals_against INTEGER,
  goal_differential INTEGER,
  extra_time BOOLEAN,
  penalty_shootout BOOLEAN,
  penalties_for INTEGER,
  penalties_against INTEGER,
  result TEXT,
  win BOOLEAN,
  lose BOOLEAN,
  draw BOOLEAN,
  PRIMARY KEY (tournament_id, match_id, team_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (opponent_id) REFERENCES teams (team_id)
);

-- player_appearances
-- one observation per player per team per match per tournament
CREATE TABLE player_appearances(
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  position_name TEXT,
  position_code TEXT,
  starter BOOLEAN,
  substitute BOOLEAN,
  captain BOOLEAN,
  PRIMARY KEY (tournament_id, match_id, team_id, player_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id)
);

-- manager_appearances
-- one observation per manager per team per match per tournament
CREATE TABLE manager_appearances(
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  manager_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, match_id, team_id, manager_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (manager_id) REFERENCES managers (manager_id)
);

-- referee_appearances
-- one observaton per referee per match per tournament
CREATE TABLE referee_appearances(
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  referee_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, match_id, referee_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (referee_id) REFERENCES referees (referee_id)
);

-- goals
-- one observation per goal
CREATE TABLE goals(
  goal_id TEXT NOT NULL,
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  player_team_id TEXT NOT NULL,
  minute_label TEXT,
  minute_regulation INTEGER,
  minute_stoppage INTEGER,
  match_period TEXT,
  own_goal BOOLEAN,
  penalty BOOLEAN,
  PRIMARY KEY (goal_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id),
  FOREIGN KEY (player_team_id) REFERENCES teams (team_id)
);

-- penalty_kicks
-- one observation per penalty kick in a penalty shootout
CREATE TABLE penalty_kicks(
  penalty_kick_id TEXT NOT NULL,
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  converted BOOLEAN,
  PRIMARY KEY (penalty_kick_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id)
);

-- bookings
-- one observation per booking
CREATE TABLE bookings(
  booking_id TEXT NOT NULL,
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  minute_label TEXT,
  minute_regulation INTEGER,
  minute_stoppage INTEGER,
  match_period TEXT,
  yellow_card BOOLEAN,
  red_card BOOLEAN,
  second_yellow_card BOOLEAN,
  sending_off BOOLEAN,
  PRIMARY KEY (booking_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id)
);

-- substitutions
-- one observation per player per substitution
CREATE TABLE substitutions(
  substitution_id TEXT NOT NULL,
  tournament_id TEXT NOT NULL,
  match_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  home_team BOOLEAN,
  away_team BOOLEAN,
  player_id TEXT NOT NULL,
  shirt_number INTEGER,
  minute_label TEXT,
  minute_regulation INTEGER,
  minute_stoppage INTEGER,
  match_period TEXT,
  going_off BOOLEAN,
  coming_on BOOLEAN,
  PRIMARY KEY (substitution_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (match_id) REFERENCES matches (match_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id)
);

-- host_countries
-- one record per host country
CREATE TABLE host_countries(
  tournament_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  performance TEXT,
  PRIMARY KEY (tournament_id, team_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id)
);

-- tournament_stages
-- one record per stage per tournament
CREATE TABLE tournament_stages(
  tournament_id TEXT NOT NULL,
  stage_number INTEGER,
  stage_name TEXT,
  group_stage BOOLEAN,
  knockout_stage BOOLEAN,
  unbalanced_groups BOOLEAN,
  start_date DATE,
  end_date DATE,
  count_matches INTEGER,
  count_teams INTEGER,
  count_scheduled INTEGER,
  count_replays INTEGER,
  count_playoffs INTEGER,
  count_walkovers INTEGER,
  PRIMARY KEY (tournament_id, stage_number),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id)
);

-- groups
-- one record per group per group stage per tournament
CREATE TABLE groups(
  tournament_id TEXT NOT NULL,
  stage_number INTEGER,
  stage_name TEXT,
  group_name TEXT,
  count_teams INTEGER,
  PRIMARY KEY (tournament_id, stage_number, group_name),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id)
);

-- group_standings
-- one record per team per group per group stage per tournament
CREATE TABLE group_standings(
  tournament_id TEXT NOT NULL,
  stage_number INTEGER,
  stage_name TEXT,
  group_name TEXT,
  position INTEGER,
  team_id TEXT NOT NULL,
  played INTEGER,
  wins INTEGER,
  draws INTEGER,
  losses INTEGER,
  goals_for INTEGER,
  goals_against INTEGER,
  goal_difference INTEGER,
  points INTEGER,
  advanced BOOLEAN,
  PRIMARY KEY (tournament_id, stage_number, group_name, position),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id)
);

-- tournament_standings
-- one record per ranked position per tournament
CREATE TABLE tournament_standings(
  tournament_id TEXT NOT NULL,
  position INTEGER,
  team_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, position),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id)
);

-- award_winners
-- one record per award winner per tournament
CREATE TABLE award_winners(
  tournament_id TEXT NOT NULL,
  award_id TEXT NOT NULL,
  shared BOOLEAN,
  player_id TEXT NOT NULL,
  team_id TEXT NOT NULL,
  PRIMARY KEY (tournament_id, award_id, player_id),
  FOREIGN KEY (tournament_id) REFERENCES tournaments (tournament_id),
  FOREIGN KEY (award_id) REFERENCES awards (award_id),
  FOREIGN KEY (player_id) REFERENCES players (player_id),
  FOREIGN KEY (team_id) REFERENCES teams (team_id)
);
