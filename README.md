# The Fjelstul World Cup Database

The Fjelstul World Cup Database is a comprehensive database about the FIFA World Cup created by Joshua C. Fjelstul, Ph.D. that covers all `21` World Cup tournaments (1930-2018). The database includes `27` datasets (approximately 1.1 million data points) that cover all aspects of the World Cup. The data has been extensively cleaned and cross-validated. 

Users can use data from the Fjelstul World Cup Database to calculate statistics about teams, players, managers, and referees. Users can also use the data to predict match results. With many units of analysis and opportunities for merging and reshaping data, the database is also an excellent resource for teaching data science skills, especially in `R`.

## Overview of the database

The `27` datasets in the Fjelstul World Cup Database are organized into `5` groups:

1. A first group of datasets (containing `9` datasets) includes information about each of the `9` basic units of observation in the database: tournaments (`tournaments`), including the host country, the winner, the dates of the tournament, and information about the format of each tournament; the FIFA confederations (`confederations`); teams (`teams`); players (`players`); managers (`managers`), including their team and home country; referees (`referees`), including their home country and confederation; stadiums that have hosted World Cup matches (`stadiums`); matches (`matches`), including the stage of the tournament, the location of the match (country, city, stadium), the teams involved, and the result; and the individual awards that are handed out to players at each tournament (`awards`). Each of these units of observation has a unique ID number.

2. A second group of datasets (containing `4` datasets) maps teams, players, managers, and referees to tournaments. There is a dataset about which teams qualified (`qualified teams`), which indicates how each team performed in the tournament; a dataset about squads (`squads`), which indicates the name, position, and shirt number of each player; a dataset about manager appointments (`manager_appointments`), which indicates the team and home country of each manager; and a dataset about referee appointments (`referee_appointments`), which indicates the home country and confederation of each referee. 

3. A third group of datasets (containing `4` datasets) maps teams, players, managers, and referees to individual matches. There are datasets about team appearances (`team_appearances`), player appearances (`player_apperances`), manager appearances (`manager appearances`), and referee appearances (`referee appearances`). Players who start a game on the bench but who are not substituted in appear in the `squads` dataset but not the `player_appearances` dataset.

4. A fourth group of datasets (containing `4` datasets) cover in-match events, including: all goals (`goals`); all attempted penalty kicks in penalty shootouts and their outcomes (`penalty_kicks`); all bookings (`bookings`), including yellow cards and red cards; and all substitutions (`substitutions`). Each dataset includes the minute of the event and the player(s) and team involved. Each of these `4` types of in-match events has a unique ID number.

5. A fifth group of datasets (containing `6` datasets) cover tournament-level attributes. There a dataset about host countries (`host_countries`), including the performance of each host country; a dataset about the stages in each tournament (`tournament_stages`), which records each stage of the tournament, the dates of the stage, and key features of the stage; a dataset about the groups in each group stage (`groups`), which indicates the name of each group and the number of teams in each group; a dataset about the final standings in each group (`group_standings`); a dataset about the final standings for each tournament (`tournament_standings`); and a dataset about all individual player awards handed out at each tournament (`award_winners`).

## Downloading the data

The Fjelstul World Cup Database is available via the `R` package `worldcup`, which you can install from this repository (instructions below). Note that this repository is structured as a repository for an `R` package. You can also download the database directly from this repository in `4` formats: an `.RData` version of the database is available in the `data/` folder, a `.csv` version is available in the `data-csv/` folder, a `.json` version is available in the `data-json/` folder, and a relational database version (`SQLite`) is available in the `data-sqlite/` folder.

The `.Rdata`, `.csv`, and `.json` versions of the database are all identical except for the file format. These versions of the database are not technically relational because many tables already include variables that have been merged in from other tables for convenience (i.e., some data exists in multiple tables). The `SQLite` version includes all of the same variables, but variables from other tables are not already merged in. Dummy variables that are coded `0` or `1` are converted to `FALSE` and `TRUE`. Users can use the primary and foreign keys in the tables to merge in data from other tables. See the `README.md` file in the `data-sqlite/` folder for more details on using the relational database.

## Downloading the codebook

The codebook for the Fjelstul World Cup Database is available in `.pdf` format in the `codebook/pdf/` folder. 
The codebook is also available in `.csv` format in the `codebook/csv/` folder. There are `2` files: `datasets.csv`, which describes the contents of each dataset, and `variables.csv`, which describes each variable. 

The codebook for the database is also included in the `R` package: `worldcup::datasets` and `worldcup::variables`. The same information is also available as part of the `R` documentation for each dataset. For example, you can see the codebook for the `worldcup::matches` dataset by running `?worldcup::matches`.

## The license

The copyright for the original structure and organization of the Fjelstul World Cup Database and for all of the documentation and replication code for the database is owned by Joshua C. Fjelstul, Ph.D.

The Fjelstul World Cup Database and the `worldcup` package are both published under a [CC-BY-SA 4.0 license](https://creativecommons.org/licenses/by-sa/4.0/legalcode). This means that you can distribute, modify, and use all or part of the database for commercial or non-commercial purposes as long as (1) you provide proper attribution and (2) any new works you produce based on this database also carry the CC-BY-SA 4.0 license. 

To provide proper attribution, according to the CC-BY-SA 4.0 license, you must provide the name of the author ("Joshua C. Fjelstul, Ph.D."), a notice that the database is copyrighted ("© 2022 Joshua C. Fjelstul, Ph.D."), a link to the CC-BY-SA 4.0 license (https://creativecommons.org/licenses/by-sa/4.0/legalcode), and a link to this repository (https://www.github.com/jfjelstul/worldcup). You must also indicate any modifications you have made to the database.

Consistent with the CC-BY-SA 4.0 license, I provide this database as-is and as-available, and make no representations or warranties of any kind concerning the database, whether express, implied, statutory, or other. This includes, without limitation, warranties of title, merchantability, fitness for a particular purpose, non-infringement, absence of latent or other defects, accuracy, or the presence or absence of errors, whether or not known or discoverable. 

## The datasets

- `tournaments`: This dataset records all World Cup tournaments. There is one observation per tournament. It includes the host of the tournament, the winner of the tournament, the start and end dates of the tournament, and information about the format of the tournament. There are `18` variables and `21` observations.

- `confederations`: This dataset records all FIFA confederations. There is one observation per confederation. There are `5` variables and `6` observations.

- `teams`: This dataset records all teams who have participated in a World Cup match. There is one observation per team. It includes the 3-letter ISO code for each country and information about each country's national federation and FIFA confederation. There are `11` variables and `84` observations.

- `players`: This dataset records all players who have participated in a World Cup match, including players on the bench. There is one observation per player. It includes their name, their birth date, and a link to their Wikipedia page, if they have one. Note that it does not include their team, as a small number of players represent different countries in different tournaments. There are `12` variables and `7907` observations.

- `managers`: This dataset records all managers who have participated in a World Cup match. There is one observation per manager. It includes their name, their home country, and a link to their Wikipedia page, if they have one. There are `6` variables and `357` observations.

- `referees`: This dataset records all referees who have participated in a World Cup match. There is one observation per referee. It includes their name, their home country, their confederation, and a link to their Wikipedia page, if they have one. There are `9` variables and `380` observations.

- `stadiums`: This dataset records all stadiums that have hosted a World Cup match. There is one observation per stadium. It includes the country and city of the stadium, the approximate capacity of the stadium, and the link to the Wikipedia pages for the city and the stadium. There are `8` variables and `185` observations.

- `matches`: This dataset records all World Cup matches. There is one observation per match per tournament. It includes the home team, the away team, the date of the match, the country, city, and stadium that the match was played in, the final score, the score margin for each team, whether the match went to extra time, whether there was a penalty shootout, the number of penalties scored in the shootout (if applicable), the result of the match (home team win, away team win, draw, replayed), and the winner (if applicable). There are `37` variables and `900` observations.

- `awards`: This dataset records all individual awards that are handed out to players. There is one observation per award. It includes the name of the award, the year the award was first introduced, and a description of the award. There are `5` variables and `8` observations.

- `qualified_teams`: This dataset records all qualified teams. There is one observation per team per tournament. It includes the tournament, the team, and the performance of each team (the furthest stage reached). There are `8` variables and `457` observations.

- `squads`: This dataset records the composition of each squad. There is one observation per player per team per tournament. It includes the position of each player, the shirt number of each player (from 1954), the current club of each player, and a link to the Wikipedia page for the club, if it has one. There are `12` variables and `10142` observations.

- `manager_appointments`: This dataset records all manager appointments. There is one observation per manager per team per tournament. There are some teams that have co-managers. There are `10` variables and `469` observations.

- `referee_appointments`: This dataset records all referee appointments. There is one observation per referee per tournament. This dataset only includes the main referee, not assistant referees, fourth officials, or video assistant referees.There are `10` variables and `511` observations.

- `team_appearances`: This dataset records all team appearances. There is one observation per team per match per tournament. It includes whether the team is the home team or the away team, the number of goals for and against, the goal difference, whether there was a penalty shootout, penalties for and against (if applicable), and whether the team wins, loses, or draws. There are `36` variables and `1800` observations.

- `player_appearances`: This dataset records all player appearances since 1970. There is one observation per player per team per match per tournament. It includes players who play in the match, including players who are in the starting eleven and players who come in as substitutes. FIFA match reports do not include information about substitutions before 1970. There are `22` variables and `18623` observations.

- `manager_appearances`: This dataset records all manager appearances. There is one observation per manager per team per match per tournament. There are some teams that have co-managers. There are `17` variables and `1842` observations.

- `referee_appearances`: This dataset records all referee appearances. There is one observation per referee per match per tournament. There are `15` variables and `900` observations.

- `goals`: This dataset records all goals. There is one observation per goal. It indicates the team that scored the goal, player who scored the goal, the team of the player who scored the goal (to account for own goals), minute of the goal, and whether the goal was scored in the run of play by the opposition, was an own goal, or was a penalty. This dataset does not include converted penalties in a penalty shootout. There are `27` variables and `2548` observations.

- `penalty_kicks`: This dataset records all penalty kicks taken during penalty shootouts. There is one observation per penalty kick. This dataset does not include attempted penalty kicks during matches. It indicates minute of each kick, the player who took the kick, and whether the penalty was converted. There are `19` variables and `279` observations.

- `bookings`: This dataset records all bookings, including yellow cards and red cards, since 1970. The modern system of yellow and red cards was introduced in 1970. There is one observation per booking. It indicates the minute of each booking, the player who was booked, whether the booking was a yellow card or a red card, whether the card was a second yellow card, and whether the player was sent off as a result of the booking. There are `26` variables and `2466` observations.

- `substitutions`: This dataset records all substitutions since 1970. FIFA match reports do not include information about substitutions before 1970. There is one observation per player per substitution. It indicates the minute of the substitution, the player who went off, and the player who came on. There are `24` variables and `6464` observations.

- `host_countries`: This dataset records all host countries. There is one observation per host country per tournament. A tournament can have multiple host countries. It indicates the performance of each host country (the furthest stage reached). There are `7` variables and `22` observations.

- `tournament_stages`: This dataset records the stages in each tournament. There is one observation per stage per tournament. It indicates the name of the stage, whether the stage was a group stage or a knockout stage, if the stage was a group stage, whether there were unbalanced groups, the start and end dates of the stage, and how many matches there were in the stage, how many teams participated in each stage, how many games were scheduled, how many replays there were, how many walkovers there were, and how many playoffs there were. There are `16` observations and `107` observations.

- `groups`: This dataset records the names of the groups for each group stage. There is one observation per group per group stage per tournament. Some tournaments have multiple group stages. It indicates the stage, the name of the group, and how many teams were in the group. There are `7` variables and `117` observations.

- `group_standings`: This dataset records group standings for each group stage. There is one observation per team per group per group stage per tournament. Some tournaments have multiple group stages. It includes the final position of the team (factoring in tie breakers), the name of the team, the number of matches played, the number of wins, the number of losses, the number of draws, the number of goals for, the number of goals against, the goal difference, the total number of points earned, and whether the team advanced out of the group. There are `19` variables and `458` observations.

- `tournament_standings`: This dataset records the final standings for each tournament. There is one observation per position per tournament. The top four teams are ranked. In most tournaments, these are the winner of the final, the loser of the final, the winner of the third-place match, and the loser of the third-place match. There are `7` variables and `84` observations.

- `award_winners`: This dataset records all award winners. There is one observation per player per award per tournament. Some awards are shared by multiple players. It indicates the name of the award, the player(s) who won the award, the team of the player(s) who won the award, and whether the award was shared. There are `12` variables and `132` observations.

## Data sources and replication code

The data in the Fjelstul World Cup Database is coded based on information from Wikipedia. Some of this information is cross-referenced with the official FIFA match reports to check for accuracy. The Wikipedia pages used to code the data are archived in the `data-raw/` folder. Data on tournaments is hand-coded coded based on the pages in `data-raw/Wikipedia-tournament-pages/`, data on squads is machine-coded based on the pages in `data-raw/Wikipedia-squad-pages/`, data on matches is machine-coded based on the pages in `data-raw/Wikipedia-match-pages/`, and data on awards is hand-coded based on the page in `data-raw/Wikipedia-awards-page/`. The raw data for all of the datasets that are hand-coded are available in `data-raw/hand-coded-tables/`. 

The replication code for downloading the Wikipedia pages used to code the database is available in `data-raw/code/download-wikipedia-pages.R`. The replication code for extracting data from these pages is also available in `data-raw/code/`. The file `data-raw/code/parse-wikipedia-match-pages.R` extracts data from the match pages in `data-raw/Wikipedia-match-pages/` and the file `data-raw/code/parse-wikipedia-squad-pages.R` extracts data from the squad pages in `data-raw/Wikipedia-squad-pages/`. The working files produced by this code are stored in `data-raw/Wikipedia-data/`. The file `data-raw/code/build-database.R` compiles the database from the parsed data in `data-raw/Wikipedia-data/` and the hand-coded data in `data-raw/hand-coded-tables/`. 

The files `data-raw/code/build-database-csv.R`, `data-raw/code/build-database-json.R`, and `data-raw/code/build-database-sqlite.R` build the `.csv`, `.json`, and `SQLite` versions of the database, respectively. 

The replication code for the codebook is available in `codebook/code/`. 

## Data notes

- **Player names.** The official FIFA match reports include numerous errors relating to player names, including incorrect first names, misspellings, and incorrect, inconsistent, or out-dated transliterations. Wikipedia has cleaner names. Not all names are uniquely identifying. There can be multiple validate Wikipedia links for a player, due to redirects. I have standardized the links in the squad pages to each player has a unique link in the `players` dataset. Some players (and managers) are commonly known by a single name (e.g., Pelé, Ronaldinho, etc.). For these players, the `family_name` variable records the name they are commonly known by and the `given_name` variable is coded `not applicable`.

- **Shirt numbers.** Prior to 1950, players did not wear numbers on their shirts. In the 1950 World Cup, players had numbers, but they were not consistent from match to match.  Consequently, the `shirt_number` variable in the `squads` dataset is coded `0` for tournaments prior to the 1954 World Cup.

- **Substitutions.** The data on substitutions starts in 1970. Prior to 1970, the match reports do not indicate substitutions. When comparing the data to the online versions of the FIFA match reports, note that the match reports sometimes incorrectly switch the player going off and the player coming on. In addition, sometimes the match reports will have the incorrect player coming on.

- **Time.** In football matches, minute `1'` refers to the minute from `00:00` to `00:59`, minute `2'` refers to the minute from `01:00` to `01:59`, and so on. The first half goes from minute `1'` (`00:00` to `00:59`) to minute `45'` (`44:00` to `44:59`). The second half goes from minute `46'` (`45:00` to `45:59`) to minute `90'` (`89:00` to `89:59`). Thus, an event that occurs in minute `45'` is in the last minute of regulation time in the first half, not the first minute of regulation time in the second half, which would be minute `46'`. The first minute of stoppage time (as opposed to regulation time) in the first half is `45' + 1'` and refers to the minute from `45:00` to `45:59` of the first half. The first minute of stoppage time in the second half is `90' + 1'` and refers to the minute from `90:00` to `90:59` of the second half. The same pattern applies to the first half (`91'` to `105'`) and second half (`106'` to `120'`) of extra time. Times for penalties in a shootout are recorded as `AET`, which stands for "after extra time". The `time` variable records the time of an event in this format. The `minute_regulation` variable records the regulation component of the time (the number before the `+`) and the `minute_stoppage` variable records the stoppage component of the time (the number after the `+`). For all events that occur in stoppage time, the `minute_regulation` variable will be `45`, `90`, `105`, or `120`, and the `minute_stoppage` variable will be greater than or equal to `1`. For all events that occur in regulation time, the `minute_stoppage` variable is coded `0`.

- **Event times.** For goals, substitutions, and bookings, I use the times recorded by Wikipedia. These times can differ slightly from the times in the online versions fo the official FIFA match reports, which sometimes contain inconsistencies are errors. It is common for the exact minute of in-match events to vary slightly between various online sources.

- **Tournament formats.** The 1934 and 1938 tournaments did not include a group stage, so the `groups` dataset does not include observations for these tournaments. In the 1950 tournament, there was a first round and second round, which both had a group stage format. Through 1970, group names used numbers. The 1974, 1978, and 1982 tournaments had two group stages. The `groups` dataset includes the groups for both stages. Groups names in the first stage use numbers and group names in the second stage use letters. Since 1986, group names have used letters.

- **Country codes.** The database uses ISO Alpha-3 country codes. The codes for the home countries are `ENG` for England, `WAL` for Wales, `SCO` for Scotland, and `NIR` for Northern Ireland. The codes for former countries are `SCG` for Serbia and Montenegro, `YUG` for Yugoslavia, `DDR` for East Germany, `CSK` for Czechoslovakia, and `SUN` for the Soviet Union. There is no ISO Alpha-3 code for the Dutch East Indies, so the database uses `IDN`, the code for Indonesia. West Germany uses the code for Germany, `DEU`, but is labeled as `West Germany`.

## Installing the R package

You can install the latest development version of the `worldcup` package from GitHub:

```
# install.packages("devtools")
devtools::install_github("jfjelstul/worldcup")
```

## Citating the database

If you use the database in a paper or project, please cite the database:

> Fjelstul, Joshua C. "The Fjelstul World Cup Database v.1.0." July 8, 2022. https://www.github.com/jfjelstul/worldcup.

The `BibTeX` entry for the database is:

```
@Manual{Fjelstul2022,
  author = {Fjelstul, Joshua C.},
  title = {The Fjelstul World Cup Database v.1.0},
  year = {2022}
}
```

If you access the database via the `worldcup` package, please also cite the package:

> Joshua C. Fjelstul (2022). worldcup: The Fjelstul World Cup Database. R package version 0.1.0.

The `BibTeX` entry for the `R` package is:

```
@Manual{,
  title = {worldcup: The Fjelstul World Cup Database},
  author = {Fjelstul, Joshua C.},
  year = {2022},
  note = {R package version 0.1.0},
}
```

## Reporting problems

If you notice an error in the data or a bug in the `R` package, please report it [here](https://github.com/jfjelstul/worldcup/issues).
