# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(downloadr)

# Tournament pages -------------------------------------------------------------

urls <- c(
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1934_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1938_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup"
)

files <- urls |>
  str_extract("[0-9]{4}.*") |>
  str_replace("_FIFA_World_Cup", "-tournament") |>
  str_c(".html")

download_files(
  urls = urls,
  files = files,
  delay_min = 0.5,
  delay_max = 1,
  folder_path = "data-raw/Wikipedia-tournament-pages"
)

# Match pages ------------------------------------------------------------------

urls <- c(

  # 1930
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_knockout_stage",

  # 1934
  "https://en.wikipedia.org/wiki/1934_FIFA_World_Cup_final_tournament",

  # 1938
  "https://en.wikipedia.org/wiki/1938_FIFA_World_Cup_final_tournament",

  # 1950
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_final_round",

  # 1954
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_knockout_stage",

  # 1958
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_knockout_stage",

  # 1962
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_knockout_stage",

  # 1966
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_knockout_stage",

  # 1970
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_knockout_stage",

  # 1974
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_knockout_stage",

  # 1978
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_knockout_stage",

  # 1982
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_1",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_2",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_3",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_4",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_5",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_6",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_knockout_stage",

  # 1986
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_knockout_stage",

  # 1990
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_knockout_stage",

  # 1994
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_knockout_stage",

  # 1998
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_knockout_stage",

  # 2002
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_knockout_stage",

  # 2006
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_knockout_stage",

  # 2010
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_knockout_stage",

  # 2014
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_knockout_stage",

  # 2018
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_knockout_stage",

  # 2022
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_A",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_B",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_C",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_D",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_E",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_F",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_G",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_Group_H",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_knockout_stage"
)

files <- urls |>
  str_extract("[0-9]{4}.*") |>
  str_replace("_FIFA_World_Cup_", "-") |>
  str_replace("_", "-") |>
  str_replace("Group", "group") |>
  str_c(".html")

download_files(
  urls = urls,
  files = files,
  delay_min = 0.5,
  delay_max = 1,
  folder_path = "data-raw/Wikipedia-match-pages"
)

# Squad pages ------------------------------------------------------------------

urls <- c(
  "https://en.wikipedia.org/wiki/1930_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1934_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1938_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1950_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1954_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1958_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1962_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1966_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1970_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1974_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1978_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1982_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1986_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1990_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1994_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/1998_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2002_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2006_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2010_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2014_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2018_FIFA_World_Cup_squads",
  "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_squads"
)

files <- urls |>
  str_extract("[0-9]{4}.*") |>
  str_replace("_FIFA_World_Cup_squads", "-squads") |>
  str_c(".html")

download_files(
  urls = urls,
  files = files,
  delay_min = 0.5,
  delay_max = 1,
  folder_path = "data-raw/Wikipedia-squad-pages"
)

# Awards page ------------------------------------------------------------------

download_files(
  urls = "https://en.wikipedia.org/wiki/FIFA_World_Cup_awards",
  files = "awards.html",
  delay_min = 0.5,
  delay_max = 1,
  folder_path = "data-raw/Wikipedia-awards-page"
)
