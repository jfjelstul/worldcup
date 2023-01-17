# Joshua C. Fjelstul, Ph.D.
# worldcup R package

# Packages
library(tidyverse)
library(rvest)
library(xml2)
library(lubridate)

# Function to extract countries ------------------------------------------------

extract_countries <- function(html) {

  # Number of h2 nodes
  count_h2 <- html |>
    html_elements("h2") |>
    length()

  # Number of h3 nodes
  count_h3 <- html |>
    html_elements("h3") |>
    length()

  # Extract the text of the most frequent heading node
  if (count_h2 > count_h3) {
    countries <- html |>
      html_elements("h2") |>
      html_text()
  } else {
    countries <- html |>
      html_elements("h3") |>
      html_text()
  }

  # Extract the number of countries
  countries <- countries |>
    str_remove("\\[edit\\]") |>
    str_squish()
  drop <- c(
    "Contents", "Search",
    "References", "External links", "Notes",
    "Player representation by league",
    "Player representation by league system",
    "Player representation by club",
    "Player representation by age",
    "Average age of squads", "Age",
    "Coaches representation by country",
    "Player representation by club confederation",
    "Player statistics",
    "Navigation menu",
    "Personal tools", "Namespaces", "Views", "Navigation", "Contribute",
    "Tools", "Print/export", "In other projects", "Languages"
  )
  countries <- countries[!(countries %in% drop)]

  return(countries)
}

# Function to extract squads ---------------------------------------------------

extract_squads <- function(file) {

  # Print file name to the console
  cat(file, "\n")

  # Read in HTML
  html <- read_html(file)

  # Extract countries
  countries <- extract_countries(html)

  # Extract tables
  tbody <- html |> html_elements("tbody")

  # Create a counter
  counter <- 0

  # Create an empty list to hold squad tables
  squads <- list()

  # Loop through teams
  for(i in 1:length(tbody)) {

    # Extract HTML tables
    table_html <- tbody[[i]]

    # Convert HTML table to a tibble
    table <- table_html |>
      html_table()

    # Stop if not a table of players
    if (nrow(table) < 15 | ncol(table) < 6) {
      next
    }

    # Increment counter
    counter <- counter + 1

    # Extract table rows
    tr <- html_elements(table_html, "tr")
    tr <- tr[-1]

    # Create empty vectors to hold table data
    players <- rep(NA, nrow(table))
    player_links <- rep(NA, nrow(table))

    # Loop through table rows
    for(j in 1:nrow(table)) {

      # Extract links from the table
      a <- tr[j] |>
        html_elements("a")

      # Extract text and URL
      a_text <- a |> html_text()
      a_href <- a |> html_attr("href")
      a_href[is.na(a_href)] <- ""

      # Drop position links
      drop <- str_detect(a_href, "Goalkeeper|Defender|Midfielder|Forward|Captain")
      a_text <- a_text[!drop]
      a_href <- a_href[!drop]

      # Drop football association links
      drop <- a_text == ""
      a_text <- a_text[!drop]
      a_href <- a_href[!drop]

      # Drop footnote links
      drop <- str_detect(a_href, "^#")
      a_text <- a_text[!drop]
      a_href <- a_href

      # Extract players and player links
      players[j] <- a_text[1]
      player_links[j] <- a_href[1]

    }

    # Rename columns
    if(ncol(table) == 6) {
      names(table) <- c(
        "shirt_number", "position_code", "player_name",
        "birth_date", "caps", "club_name"
      )
    } else if(ncol(table) == 7) {
      names(table) <- c(
        "shirt_number", "position_code", "player_name",
        "birth_date", "caps", "goals", "club_name"
      )
    }

    # Select variables
    table <- table |>
      select(
        shirt_number, position_code, player_name, birth_date
      )

    # Clean table
    table <- table |>
      mutate(
        file = file,
        squad_id = counter,
        team_name = countries[counter],
        shirt_number = as.character(shirt_number),
        player_wikipedia_link = player_links
      )

    # Add to list
    squads[[i]] <- table
  }

  # Stack tables
  squads <- bind_rows(squads)

  return(squads)
}

# Function to extract managers -------------------------------------------------

extract_managers <- function(file) {

  # Print file name to the console
  cat(file, "\n")

  # Read in HTML
  html <- read_html(file)

  # Extract countries
  countries <- extract_countries(html)

  # Extract paragraph elements
  p <- html |> html_elements("p")

  # Extract text of paragraph elements
  text <- p |>
    html_text() |>
    str_squish()
  text <- text[!is.na(text)]

  # Identify elements to keep
  indexes <- text |>
    str_detect("(Head coach(es)?|Coach(es)?|Managers?):")
  p <- p[indexes]

  # Fix error in Wikipedia
  if (file == "data-raw/Wikipedia-squad-pages/1934-squads.html") {
    countries <- countries[countries != "Sweden"]
  }

  # Loop through teams
  managers <- list()
  for(i in 1:length(countries)) {

    # Extract children of paragraph element
    children <- p[i] |>
      html_children()

    # Attributes
    children_type <- children |>
      html_name()

    # Select elements
    children <- children[!(children_type %in% c("style", "link", "sup"))]

    # Country name
    country_name <- children[1] |>
      html_elements("a") |>
      html_attr("href") |>
      str_remove("/wiki/")
    if (length(country_name) == 0) {
      country_name = countries[i]
    } else {
      children <- children[-1]
    }

    # Manager name
    manager_name <- children |>
      html_text()

    # Manager link
    manager_wikipedia_link <- children |>
      html_attr("href")
    manager_wikipedia_link <- str_c(
      "https://en.wikipedia.org",
      manager_wikipedia_link
    )

    # Make a tibble
    managers[[i]] <- tibble(
      year = file |>
        str_extract("[0-9]{4}") |>
        as.numeric(),
      team_name = countries[i],
      manager_name = manager_name,
      country_name = country_name,
      manager_wikipedia_link = manager_wikipedia_link
    )
  }

  # Stack tibbles
  managers <- bind_rows(managers)

  return(managers)
}

# Parse HTML -------------------------------------------------------------------

# Get a list of files
files <- list.files("data-raw/Wikipedia-squad-pages", full.names = TRUE)

# Parse HTML files
squad_data_raw <- map(files, extract_squads)

# Stack tibbles
wikipedia_squads <- bind_rows(squad_data_raw)

# Parse HTML files
manager_data_raw <- map(files, extract_managers)

# Stack tibbles
wikipedia_managers <- bind_rows(manager_data_raw)

# Code missing manager data ----------------------------------------------------

# Add Sweden (1934)
wikipedia_managers <- wikipedia_managers |>
  add_row(
    year = 1934,
    team_name = "Sweden",
    manager_name = "John Pettersson",
    country_name = "Sweden",
    manager_wikipedia_link = "https://en.wikipedia.org/wiki/John_Pettersson_(football_manager)"
  )

# Remove one observation
wikipedia_managers <- wikipedia_managers |>
  filter(manager_name != "Kim Pyung-seok") |>
  filter(!is.na(manager_wikipedia_link))

# Clean country names
wikipedia_managers <- wikipedia_managers |>
  mutate(
    country_name = country_name |>
      str_replace_all("_", " "),
    country_name = case_when(
      country_name == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      country_name == "Federal Republic of Yugoslavia" ~ "Yugoslavia",
      country_name == "China PR" ~ "China",
      country_name == "West Germany" ~ "Germany",
      TRUE ~ country_name
    )
  )

# Correct country
wikipedia_managers <- wikipedia_managers |>
  mutate(
    country_name = case_when(
      manager_name == "Henri Michel" ~ "France",
      manager_name == "Karl Rappan" ~ "Austria",
      TRUE ~ country_name
    )
  )

# Code squad variables ---------------------------------------------------------

wikipedia_squads <- wikipedia_squads |>
  mutate(
    year = file |>
      str_extract("[0-9]{4}") |>
      as.numeric(),
    team_name = case_when(
      team_name == "China PR" ~ "China",
      TRUE ~ team_name
    ),
    position_code = position_code |>
      str_extract("[A-Z]{2}"),
    position_name = case_when(
      position_code == "GK" ~ "goal keeper",
      position_code == "DF" ~ "defender",
      position_code == "MF" ~ "midfielder",
      position_code == "FW" ~ "forward"
    ),
    birth_date = birth_date |>
      str_extract("[0-9]+ [A-Z][a-z]+ [0-9]+") |>
      dmy() |>
      as.character(),
    birth_date = case_when(
      is.na(birth_date) ~ "not available",
      TRUE ~ birth_date
    ),
    shirt_number = shirt_number |>
      str_extract("[0-9]+"),
    shirt_number = case_when(
      is.na(shirt_number) ~ 0,
      year <= 1950 ~ 0,
      TRUE ~ as.numeric(shirt_number)
    ),
    player_name = player_name |>
      str_remove("\\(.*?\\)") |>
      str_remove("\\[.*?\\]") |>
      str_remove("[*]") |>
      str_squish(),
    player_wikipedia_link = case_when(
      str_detect(player_wikipedia_link, "redlink") ~ "not available",
      is.na(player_wikipedia_link) ~ "not available",
      TRUE ~ player_wikipedia_link
    )
  ) |>
  select(
    year, team_name, shirt_number,
    position_name, position_code, player_name, birth_date,
    player_wikipedia_link
  )

# Clean player names and links -------------------------------------------------

# Check for duplicate players
# Collapse by team and birth date
possible_duplicates <- wikipedia_squads |>
  filter(birth_date != "not available") |>
  group_by(team_name, birth_date) |>
  summarize(
    players = str_c(unique(player_name), collapse = ", "),
    links = str_c(unique(player_wikipedia_link), collapse = ", "),
  ) |>
  filter(str_detect(players, ","))

# Correct inconsistent names
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Jorge Olguin" ~ "Jorge Olguín",
      player_name == "Roberto Sensini" ~ "Roberto Néstor Sensini",
      player_name == "André Vandeweyer" ~ "André Vandewyer",
      player_name == "Lei Clijsters" ~ "Leo Clijsters",
      player_name == "Michel de Wolf" ~ "Michel De Wolf",
      player_name == "Franky Van Der Elst" ~ "Franky Van der Elst",
      player_name == "Eric Deflandre" ~ "Éric Deflandre",
      player_name == "Castilho" ~ "Carlos José Castilho",
      player_name == "Bellini" ~ "Hilderaldo Bellini",
      player_name == "Mauro" ~ "Mauro Ramos",
      player_name == "Zagallo" ~ "Mário Zagallo",
      player_name == "Rivelino" ~ "Rivellino",
      player_name == "Valdir Peres" ~ "Waldir Peres",
      player_name == "Silas" ~ "Paulo Silas",
      player_name == "Borislav Mikhailov" ~ "Borislav Mihaylov",
      player_name == "Emile M'Bouh" ~ "Émile Mbouh",
      player_name == "Wilmer Cabrera" ~ "Wílmer Cabrera",
      player_name == "Edison Méndez" ~ "Édison Méndez",
      player_name == "Gary M. Stevens" ~ "Gary Stevens",
      player_name == "Kader Keïta" ~ "Abdul Kader Keïta",
      player_name == "Alex" ~ "Alessandro Santos",
      player_name == "Jesús Ramón Ramírez" ~ "Ramón Ramírez",
      player_name == "Jesús Corona" ~ "José de Jesús Corona",
      player_name == "Francisco Rodríguez" ~ "Francisco Javier Rodríguez",
      player_name == "Ladislau Raffinsky" ~ "László Raffinsky",
      player_name == "Yuriy Nikiforov" ~ "Yuri Nikiforov",
      player_name == "Khaled Al-Muwallid" ~ "Khalid Al-Muwallid",
      player_name == "Fuad Anwar Amin" ~ "Fuad Anwar",
      player_name == "Ahmed Dokhi" ~ "Ahmed Al-Dokhi",
      player_name == "Ahmed Dokhi Al-Dosari" ~ "Ahmed Al-Dokhi",
      player_name == "Oleg Blokhin" ~ "Oleh Blokhin",
      player_name == "Volodymyr Bessonov" ~ "Volodymyr Bezsonov",
      player_name == "Vasiliy Rats" ~ "Vasyl Rats",
      player_name == "Gennadiy Lytovchenko" ~ "Gennadiy Litovchenko",
      player_name == "Gelson Fernandes" ~ "Gélson Fernandes",
      player_name == "Fernando Alvez" ~ "Fernando Álvez",
      player_name == "Ivan Horvat" ~ "Ivica Horvat",
      player_name == "Vladimir Popović" ~ "Vladica Popović",
      TRUE ~ player_name
    )
  )

# Check for duplicate players again
# These are all players from the same country with the same birth date
possible_duplicates <- wikipedia_squads |>
  filter(birth_date != "not available") |>
  group_by(team_name, birth_date) |>
  summarize(
    players = str_c(unique(player_name), collapse = ", "),
    links = str_c(unique(player_wikipedia_link), collapse = ", "),
  ) |>
  filter(str_detect(players, ","))

# Identify inconsistent names to correct
# Collapse by link
names_to_correct <- wikipedia_squads |>
  filter(player_wikipedia_link != "not available") |>
  group_by(player_wikipedia_link) |>
  summarize(
    player_name = str_c(unique(player_name), collapse = ", ")
  ) |>
  filter(str_detect(player_name, ","))

# Correct names
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Attilio Demaria" ~ "Attilio Demaría",
      player_name == "Mazzola" ~ "José Altafini",
      TRUE ~ player_name
    )
  )

# Players with the same name but different links
# Collapse by name and birth date
names_to_check <- wikipedia_squads |>
  group_by(player_name, birth_date) |>
  summarize(
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; ")
  ) |>
  filter(str_detect(player_wikipedia_link, ";"))

# Standardize links
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_wikipedia_link = case_when(
      player_wikipedia_link == "/wiki/Anders_Svensson_(footballer,_born_1976)" ~ "/wiki/Anders_Svensson",
      player_wikipedia_link == "/wiki/Andr%C3%A9_Vandeweyer" ~ "/wiki/Andr%C3%A9_Vandewyer",
      player_wikipedia_link == "/wiki/Jo%C3%A3o_Batista_da_Silva_(footballer)" ~ "/wiki/Jo%C3%A3o_Batista_da_Silva",
      player_wikipedia_link == "/wiki/Jos%C3%A9_Carlos_Bauer" ~ "/wiki/Bauer_(footballer)",
      player_wikipedia_link == "/wiki/Billy_Wright_(footballer,_born_1924)" ~ "/wiki/Billy_Wright_(footballer_born_1924)",
      player_wikipedia_link == "/wiki/David_James_(footballer,_born_1970)" ~ "/wiki/David_James_(footballer)",
      player_wikipedia_link == "/wiki/Den%C3%ADlson_(footballer,_born_1977)" ~ "/wiki/Den%C3%ADlson_de_Oliveira_Ara%C3%BAjo",
      player_wikipedia_link == "/wiki/Eddie_Lewis_(American_soccer)" ~ "/wiki/Eddie_Lewis_(soccer)",
      player_wikipedia_link == "/wiki/Edison_M%C3%A9ndez" ~ "/wiki/%C3%89dison_M%C3%A9ndez",
      player_wikipedia_link == "/wiki/Emile_M%27Bouh" ~ "/wiki/%C3%89mile_Mbouh",
      player_wikipedia_link == "/wiki/%C3%89ric_Deflandre" ~ "/wiki/Eric_Deflandre",
      player_wikipedia_link == "/wiki/Fernando_%C3%81lvez" ~ "/wiki/Fernando_Alvez",
      player_wikipedia_link == "/wiki/Fuad_Anwar_Amin" ~ "/wiki/Fuad_Anwar",
      player_wikipedia_link == "/wiki/Franky_Van_Der_Elst" ~ "/wiki/Franky_Van_der_Elst",
      player_wikipedia_link == "/wiki/G%C3%A9lson_Fernandes" ~ "/wiki/Gelson_Fernandes",
      player_wikipedia_link == "/wiki/Hennadiy_Lytovchenko" ~ "/wiki/Gennadiy_Litovchenko",
      player_wikipedia_link == "/wiki/H%C3%A9ctor_Moreno_(footballer)" ~ "/wiki/H%C3%A9ctor_Moreno",
      player_wikipedia_link == "/wiki/John_Barnes_(footballer)" ~ "/wiki/John_Barnes",
      player_wikipedia_link == "/wiki/Jorge_Olguin" ~ "/wiki/Jorge_Olgu%C3%ADn",
      player_wikipedia_link == "/wiki/Jos%C3%A9_Mar%C3%ADa_Gim%C3%A9nez" ~ "/wiki/Jos%C3%A9_Gim%C3%A9nez",
      player_wikipedia_link == "/wiki/J%C3%B3zsef_Varga_(footballer,_born_1954)" ~ "/wiki/J%C3%B3zsef_Varga_(footballer_born_1954)",
      player_wikipedia_link == "/wiki/Kim_Tae-young_(footballer,_born_1970)" ~ "/wiki/Kim_Tae-young_(footballer_born_1970)",
      player_wikipedia_link == "/wiki/Ladislau_Raffinsky" ~ "/wiki/L%C3%A1szl%C3%B3_Raffinsky",
      player_wikipedia_link == "/wiki/Lei_Clijsters" ~ "/wiki/Leo_Clijsters",
      player_wikipedia_link == "/wiki/Luis_Enrique_(footballer)" ~ "/wiki/Luis_Enrique",
      player_wikipedia_link == "/wiki/Luis_Su%C3%A1rez_(footballer,_born_1935)" ~ "/wiki/Luis_Su%C3%A1rez_Miramontes",
      player_wikipedia_link == "/wiki/Luis_Ubi%C3%B1as_(footballer)" ~ "/wiki/Luis_Ubi%C3%B1a",
      player_wikipedia_link == "/wiki/Luisinho_(footballer,_born_1911)" ~ "/wiki/Lu%C3%ADs_Mesquita_de_Oliveira",
      player_wikipedia_link == "/wiki/Packie_Bonner" ~ "/wiki/Pat_Bonner",
      player_wikipedia_link == "/wiki/Rodolpho_Barteczko" ~ "/wiki/Patesko",
      player_wikipedia_link == "/wiki/Roberto_Rivellino" ~ "/wiki/Rivellino",
      player_wikipedia_link == "/wiki/Roberto_Sensini" ~ "/wiki/Roberto_N%C3%A9stor_Sensini",
      player_wikipedia_link == "/wiki/Roberto_Rivelino" ~ "/wiki/Rivellino",
      player_wikipedia_link == "/wiki/Vasiliy_Rats" ~ "/wiki/Vasyl_Rats",
      player_wikipedia_link == "/wiki/Vladimir_Popovi%C4%87_(footballer_born_1935)" ~ "/wiki/Vladica_Popovi%C4%87",
      player_wikipedia_link == "/wiki/Valdir_Peres" ~ "/wiki/Waldir_Peres",
      player_wikipedia_link == "/wiki/Volodymyr_Bessonov" ~ "/wiki/Volodymyr_Bezsonov",
      player_wikipedia_link == "/wiki/Wilmer_Cabrera" ~ "/wiki/W%C3%ADlmer_Cabrera",
      player_wikipedia_link == "/wiki/Yuri_Nikiforov" ~ "/wiki/Yuriy_Nikiforov",
      player_wikipedia_link == "/wiki/Z%C3%A9_Maria_(footballer,_born_1949)" ~ "/wiki/Jos%C3%A9_Maria_Rodrigues_Alves",
      player_wikipedia_link == "/wiki/Jos%C3%A9_Ely_de_Miranda" ~ "/wiki/Zito_(footballer)",
      player_wikipedia_link == "/wiki/Aleksandar_Mitrovi%C4%87_(footballer)" ~ "/wiki/Aleksandar_Mitrovi%C4%87",
      player_wikipedia_link == "/wiki/Munir_Mohand_Mohamedi" ~ "/wiki/Munir_Mohamedi",
      TRUE ~ player_wikipedia_link
    )
  )

# Check names again
names_to_check <- wikipedia_squads |>
  group_by(player_name, birth_date) |>
  summarize(
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; ")
  ) |>
  filter(str_detect(player_wikipedia_link, ";"))

# Clean player names with quotes
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Isaak \"Tjaak\" Pattiwael" ~ "Isaak Pattiwael",
      player_name == "Tan \"Bing\" Mo Heng" ~ "Tan Mo Heng",
      player_name == "Hendrikus V. \"Henk\" Zomers" ~ "Hendrikus Zomers",
      player_name == "Sadok \"Attouga\" Sassi" ~ "Sadok Sassi",
      player_name == "Mohsen \"Jendoubi\" Labidi" ~ "Mohsen Labidi",
      player_name == "Abdelkrim Merry \"Krimau\"" ~ "Abdelkrim Merry",
      TRUE ~ player_name
    )
  )

# Clean player names with periods
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Gábor P. Szabó" ~ "Gábor Szabó",
      player_name == "Frans G. Hu Kon" ~ "Frans Hu Kon",
      player_name == "M.J. Hans Taihuttu" ~ "Hans Taihuttu",
      player_name == "G. Van Den Burgh" ~ "Van Den Burgh",
      player_name == "Juan F. Lombardo" ~ "Juan Lombardo",
      player_name == "Gary A. Stevens" ~ "Gary Stevens",
      TRUE ~ player_name
    )
  )

# Correct inconsistent birth dates
dates_to_check <- wikipedia_squads |>
  group_by(player_name, player_wikipedia_link, team_name) |>
  summarize(
    birth_date = str_c(unique(birth_date), collapse = "; ")
  ) |>
  filter(str_detect(birth_date, ";"))

# Clean player dates
wikipedia_squads <- wikipedia_squads |>
  mutate(
    birth_date = case_when(
      player_name == "Alain Giresse" ~ "1952-08-02",
      player_name == "Antal Szentmihályi" ~ "1939-06-13",
      player_name == "Brad Friedel" ~ "1971-05-18",
      player_name == "Carlos Blanco" ~ "1927-03-05",
      player_name == "Daniel Borimirov" ~ "1970-01-15",
      player_name == "Dario Vidošić" ~ "1987-04-08",
      player_name == "Dimitar Yakimov" ~ "1941-08-12",
      player_name == "Donis Escober" ~ "1981-02-03",
      player_name == "Enrique Borja" ~ "1945-12-30",
      player_name == "Ferdinand Daučík" ~ "1910-05-30",
      player_name == "Gerhard Hanappi" ~ "1929-02-16",
      player_name == "Guillermo Sepúlveda" ~ "",
      player_name == "Hassan Yebda" ~ "1984-05-14",
      player_name == "Hugo Sotil" ~ "1949-05-18",
      player_name == "Igor Chislenko" ~ "1939-01-04",
      player_name == "Isidoro Díaz" ~ "1938-03-14",
      player_name == "Ján Popluhár" ~ "1935-09-12",
      player_name == "János Dudás" ~ "1911-02-13",
      player_name == "Javad Nekounam" ~ "1980-09-07",
      player_name == "Jean Beausejour" ~ "1984-06-01",
      player_name == "Jerry Palacios" ~ "1981-11-01",
      player_name == "Jim Platt" ~ "1952-01-26",
      player_name == "Jimmy Armfield" ~ "1935-09-21",
      player_name == "Jimmy Dickinson" ~ "1925-04-25",
      player_name == "Jimmy Nicholl" ~ "1956-12-28",
      player_name == "Jorge Valdivia" ~ "1983-10-19",
      player_name == "José Ramos Delgado" ~ "1935-08-25",
      player_name == "József Bozsik" ~ "1925-11-28",
      player_name == "Karl Koller" ~ "1929-02-09",
      player_name == "Leônidas" ~ "1913-09-06",
      player_name == "Lorenzo Staelens" ~ "1964-04-30",
      player_name == "Luke Wilkshire" ~ "1981-10-02",
      player_name == "Mark Milligan" ~ "1985-08-04",
      player_name == "Mehdi Lacen" ~ "1984-03-15",
      player_name == "Mohammed Al-Khilaiwi" ~ "1971-08-21",
      player_name == "Nacer Chadli" ~ "1989-08-02",
      player_name == "Nawaf Al-Temyat" ~ "1976-06-28",
      player_name == "Nicolae Kovács" ~ "1911-12-29",
      player_name == "Norbert Eschmann" ~ "1933-09-19",
      player_name == "Oldřich Nejedlý" ~ "1909-12-26",
      player_name == "Oliver Eggimann" ~ "1919-01-28",
      player_name == "Pablo Mastroeni" ~ "1976-08-29",
      player_name == "Rafael Albrecht" ~ "1941-08-23",
      player_name == "Rajko Mitić" ~ "1922-11-19",
      player_name == "Ronaldo" ~ "1976-09-18",
      player_name == "Simon Mignolet" ~ "1988-03-06",
      player_name == "Tommy Docherty" ~ "1928-04-24",
      player_name == "Vangelis Moras" ~ "1981-08-26",
      player_name == "Victor N'Dip" ~ "1967-08-18",
      player_name == "Viv Anderson" ~ "1956-07-29",
      player_name == "Vladimir Stojković" ~ "1983-07-28",
      player_name == "Walter Zenga" ~ "1960-04-28",
      player_name == "Yaya Touré" ~ "1983-05-13",
      player_name == "Yozhef Sabo" ~ "1940-02-29",
      player_name == "Zlatko Yankov" ~ "1966-06-07",
      TRUE ~ birth_date
    )
  )

# Check birth dates again
dates_to_check <- wikipedia_squads |>
  group_by(player_name, player_wikipedia_link, team_name) |>
  summarize(
    birth_date = str_c(unique(birth_date), collapse = "; ")
  ) |>
  filter(str_detect(birth_date, ";"))

# Collapse by link
names_to_check <- wikipedia_squads |>
  group_by(player_wikipedia_link) |>
  summarize(
    player_name = str_c(unique(player_name), collapse = "; ")
  ) |>
  filter(str_detect(player_name, ";"))

# Add supplement ---------------------------------------------------------------

# Load data
squads_supplement <- read_csv("data-raw/player-names/squads_supplement.csv")

# Add to data
wikipedia_squads <- bind_rows(
  wikipedia_squads,
  squads_supplement
)

# Add URL stem to links --------------------------------------------------------

wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_wikipedia_link = str_c(
      "https://en.wikipedia.org",
      player_wikipedia_link
    ),
    player_wikipedia_link = case_when(
      str_detect(player_wikipedia_link, "not available") ~ "not available",
      TRUE ~ player_wikipedia_link
    ),
  )

# Create player IDs ------------------------------------------------------------

# Disambiguate players with the same name
wikipedia_squads <- wikipedia_squads |>
  mutate(
    temp_player_id = str_c(player_name, " ", birth_date),
    temp_player_id = temp_player_id |>
      as.factor() |>
      as.numeric()
  )

# Make player IDs
set.seed(12345)
player_ids <- wikipedia_squads |>
  group_by(temp_player_id) |>
  summarize() |>
  ungroup() |>
  mutate(
    player_id = sample(1:10000, size = n(), replace = FALSE),
    player_id = str_c(
      "P-",
      str_pad(player_id, width = 5, side = "left", pad = "0")
    )
  )

# Merge in player IDs
wikipedia_squads <- left_join(
  wikipedia_squads,
  player_ids,
  by = "temp_player_id"
)

# Separate player names --------------------------------------------------------

# Collapse by player
player_names <- wikipedia_squads |>
  group_by(player_name, player_id, player_wikipedia_link) |>
  summarize(
    team_name = str_c(unique(team_name), collapse = ", "),
  ) |>
  ungroup() |>
  mutate(
    count_spaces = player_name |>
      str_count(" ")
  )

# Save player names and family name and given name by hand

# Merge in cleaned player names ------------------------------------------------

# Load data
player_names_cleaned <- read_csv("data-raw/player-names/player_names_cleaned.csv")

# Make a list of names to clean
player_names_to_clean <- wikipedia_squads |>
  filter(
    !(player_wikipedia_link %in% player_names_cleaned$player_wikipedia_link)
  ) |>
  select(
    player_wikipedia_link, player_name, team_name
  ) |>
  mutate(
    given_name = player_name |>
      str_extract("^.*? ") |>
      str_squish(),
    family_name = player_name |>
      str_extract(" .*") |>
      str_squish(),
  )

# Get unique names
player_names_cleaned <- player_names_cleaned |>
  group_by(player_name, family_name, given_name) |>
  summarize()

# Check for missing names
table(wikipedia_squads$player_name %in% player_names_cleaned$player_name)

# Merge names into squad table
wikipedia_squads <- left_join(
  wikipedia_squads,
  player_names_cleaned,
  by = "player_name"
)

# Organize data ----------------------------------------------------------------

# Organize squad variables
wikipedia_squads <- wikipedia_squads |>
  select(
    year, team_name, shirt_number, position_name, position_code,
    player_id, player_name, family_name, given_name,
    birth_date, player_wikipedia_link
  )

# Check for missing data
table(is.na(wikipedia_squads))

# Organize manager variables
wikipedia_managers <- wikipedia_managers |>
  arrange(year, team_name) |>
  select(
    year, team_name, manager_name,
    country_name, manager_wikipedia_link
  )

# Check for missing data
table(is.na(wikipedia_managers))

# Save data
save(wikipedia_squads, file = "data-raw/Wikipedia-data/wikipedia_squads.RData")
save(wikipedia_managers, file = "data-raw/Wikipedia-data/wikipedia_managers.RData")
