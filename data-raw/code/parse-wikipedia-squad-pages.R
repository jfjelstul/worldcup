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
  if (file == "data-raw/Wikipedia-squad-pages/men-1934-squads.html") {
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
      file = file,
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

# Code squad variables ---------------------------------------------------------

wikipedia_squads <- wikipedia_squads |>
  mutate(
    tournament = case_when(
      str_detect(file, "/men-") ~ "FIFA Men's World Cup",
      str_detect(file, "/women-") ~ "FIFA Women's World Cup",
    ),
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
    tournament, year, team_name, shirt_number,
    position_name, position_code, player_name, birth_date,
    player_wikipedia_link
  )

# Correct known Wikipedia errors -----------------------------------------------

# Correct names for players who played for multiple countries
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Attilio Demaria" ~ "Attilio Demaría",
      player_name == "Mazzola" ~ "José Altafini",
      TRUE ~ player_name
    )
  )

# Clean player names and links -------------------------------------------------

# Correct links
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_wikipedia_link = case_when(
      str_detect(player_wikipedia_link, "Sports_Club") ~ "not available",
      TRUE ~ player_wikipedia_link
    )
  )

## Check 1: multiple names -----------------------------------------------------

# Group by link and check for duplicate player names
to_check <- wikipedia_squads |>
  group_by(player_wikipedia_link, team_name) |>
  summarize(
    players = str_c(unique(player_name), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(str_detect(players, ", "))

# Correct inconsistent names
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(

      # Men
      player_name == "Kader Keïta" ~ "Abdul Kader Keïta",
      player_name == "Ahmed Dokhi" ~ "Ahmed Al-Dokhi",
      player_name == "Ahmed Dokhi Al-Dosari" ~ "Ahmed Al-Dokhi",
      player_name == "Alex" ~ "Alessandro Santos",
      player_name == "Borislav Mikhailov" ~ "Borislav Mihaylov",
      player_name == "Castilho" ~ "Carlos José Castilho",
      player_name == "Francisco Rodríguez" ~ "Francisco Javier Rodríguez",
      player_name == "Gary M. Stevens" ~ "Gary Stevens",
      player_name == "Bellini" ~ "Hilderaldo Bellini",
      player_name == "Ivan Horvat" ~ "Ivica Horvat",
      player_name == "Jesús Corona" ~ "José de Jesús Corona",
      player_name == "Khaled Al-Muwallid" ~ "Khalid Al-Muwallid",
      player_name == "Zagallo" ~ "Mário Zagallo",
      player_name == "Mauro" ~ "Mauro Ramos",
      player_name == "Michel de Wolf" ~ "Michel De Wolf",
      player_name == "Oleg Blokhin" ~ "Oleh Blokhin",
      player_name == "Silas" ~ "Paulo Silas",
      player_name == "Jesús Ramón Ramírez" ~ "Ramón Ramírez",

      # Women
      player_name == "Ane Stangeland" ~ "Ane Stangeland Horpestad",
      player_name == "Célia Okoyino da Mbabi" ~ "Célia Šašić",
      player_name == "Carin Jennings" ~ "Carin Jennings-Gabarra",
      player_name == "Carla Werden" ~ "Carla Overbeck",
      player_name == "Cat Reddick" ~ "Cat Whitehill",
      player_name == "Christie Pearce" ~ "Christie Rampone",
      player_name == "Faith Ikidi" ~ "Faith Michael",
      player_name == "Guro Knutsen" ~ "Guro Knutsen Mienna",
      player_name == "Joy Biefeld" ~ "Joy Fawcett",
      player_name == "Julie Johnston" ~ "Julie Ertz",
      player_name == "Kate Sobrero" ~ "Kate Markgraf",
      player_name == "Katie Hoyle" ~ "Katie Duncan",
      player_name == "Kristine Wigdahl Hegland" ~ "Kristine Minde",
      player_name == "Lauren Cheney" ~ "Lauren Holiday",
      player_name == "Michelle Akers-Stahl" ~ "Michelle Akers",
      player_name == "Ngozi Okobi" ~ "Ngozi Okobi-Okeoghene",
      player_name == "Stephanie Lopez" ~ "Stephanie Cox",
      player_name == "Tameka Butt" ~ "Tameka Yallop",
      player_name == "Tomomi Mitsui" ~ "Tomomi Miyamoto",
      player_name == "Verena Faißt" ~ "Verena Schweers",
      player_name == "Yūki Nagasato" ~ "Yūki Ōgimi",

      TRUE ~ player_name
    )
  )

# Check again
to_check <- wikipedia_squads |>
  group_by(player_wikipedia_link, team_name) |>
  summarize(
    players = str_c(unique(player_name), collapse = ", "),
    .groups = "drop"
  ) |>
  filter(str_detect(players, ", "))

## Check 2: multiple names -----------------------------------------------------

# Group by birth date and team and check for duplicate player names
to_check <- wikipedia_squads |>
  group_by(birth_date, team_name) |>
  summarize(
    player_name = str_c(unique(player_name), collapse = "; "),
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(player_name, ";")) |>
  filter(birth_date != "not available")

# Clean links
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_name = case_when(
      player_name == "Ladislau Raffinsky" ~ "László Raffinsky",
      player_name == "André Vandeweyer" ~ "André Vandewyer",
      player_name == "Vladimir Popović" ~ "Vladica Popović",
      player_name == "Rivelino" ~ "Rivellino",
      player_name == "Valdir Peres" ~ "Waldir Peres",
      player_name == "Jorge Olguin" ~ "Jorge Olguín",
      player_name == "Lei Clijsters" ~ "Leo Clijsters",
      player_name == "Volodymyr Bessonov" ~ "Volodymyr Bezsonov",
      player_name == "Fernando Alvez" ~ "Fernando Álvez",
      player_name == "Vasiliy Rats" ~ "Vasyl Rats",
      player_name == "Franky Van Der Elst" ~ "Franky Van der Elst",
      player_name == "Gennadiy Lytovchenko" ~ "Gennadiy Litovchenko",
      player_name == "Emile M'Bouh" ~ "Émile Mbouh",
      player_name == "Roberto Sensini" ~ "Roberto Néstor Sensini",
      player_name == "Wilmer Cabrera" ~ "Wílmer Cabrera",
      player_name == "Yuriy Nikiforov" ~ "Yuri Nikiforov",
      player_name == "Fuad Anwar Amin" ~ "Fuad Anwar",
      player_name == "Eric Deflandre" ~ "Éric Deflandre",
      player_name == "Edison Méndez" ~ "Édison Méndez",
      player_name == "Gelson Fernandes" ~ "Gélson Fernandes",
      player_name == "Ricardo Rodriguez" ~ "Ricardo Rodríguez",
      player_name == "Andressa" ~ "Andressa Alves",
      player_name == "Giorgian de Arrascaeta" ~ "Giorgian De Arrascaeta",
      TRUE ~ player_name
    )
  )

# Check birth dates again
to_check <- wikipedia_squads |>
  group_by(birth_date, team_name) |>
  summarize(
    player_name = str_c(unique(player_name), collapse = "; "),
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(player_name, ";")) |>
  filter(birth_date != "not available")

## Check 3: multiple links -----------------------------------------------------

# Group by player (name and birth date) and check for duplicate links
to_check <- wikipedia_squads |>
  group_by(player_name, birth_date, team_name) |>
  summarize(
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(player_wikipedia_link, ";"))

# Standardize links
wikipedia_squads <- wikipedia_squads |>
  mutate(
    player_wikipedia_link = case_when(

      # Men
      player_name == "Aleksandar Mitrović" ~ "/wiki/Aleksandar_Mitrovi%C4%87",
      player_name == "Anders Svensson" ~ "/wiki/Anders_Svensson",
      player_name == "André Vandewyer" ~ "/wiki/Andr%C3%A9_Vandewyer",
      player_name == "Batista" ~ "/wiki/Jo%C3%A3o_Batista_da_Silva",
      player_name == "Bauer" ~ "/wiki/Bauer_(footballer)",
      player_name == "Billy Wright" ~ "/wiki/Billy_Wright_(footballer_born_1924)",
      player_name == "David James" ~ "/wiki/David_James_(footballer)",
      player_name == "Denílson" & birth_date == "1977-08-24" ~ "/wiki/Den%C3%ADlson_de_Oliveira_Ara%C3%BAjo",
      player_name == "Eddie Lewis" ~ "/wiki/Eddie_Lewis_(soccer)",
      player_name == "Édison Méndez" ~ "/wiki/%C3%89dison_M%C3%A9ndez",
      player_name == "Émile Mbouh" ~ "/wiki/%C3%89mile_Mbouh",
      player_name == "Éric Deflandre" ~ "/wiki/Eric_Deflandre",
      player_name == "Fernando Álvez" ~ "/wiki/Fernando_Alvez",
      player_name == "Franky Van der Elst" ~ "/wiki/Franky_Van_der_Elst",
      player_name == "Fuad Anwar" ~ "/wiki/Fuad_Anwar",
      player_name == "Gélson Fernandes" ~ "/wiki/Gelson_Fernandes",
      player_name == "Gennadiy Litovchenko" ~ "/wiki/Gennadiy_Litovchenko",
      player_name == "Giorgian De Arrascaeta" ~ "/wiki/Giorgian_De_Arrascaeta",
      player_name == "Héctor Moreno" ~ "/wiki/H%C3%A9ctor_Moreno",
      player_name == "John Barnes" ~ "/wiki/John_Barnes",
      player_name == "Jorge Olguín" ~ "/wiki/Jorge_Olgu%C3%ADn",
      player_name == "José Giménez" ~ "/wiki/Jos%C3%A9_Gim%C3%A9nez",
      player_name == "József Varga" ~ "/wiki/J%C3%B3zsef_Varga_(footballer_born_1954)",
      player_name == "Kim Tae-young" ~ "/wiki/Kim_Tae-young_(footballer_born_1970)",
      player_name == "László Raffinsky" ~ "/wiki/L%C3%A1szl%C3%B3_Raffinsky",
      player_name == "Leo Clijsters" ~ "/wiki/Leo_Clijsters",
      player_name == "Luis Enrique" ~ "/wiki/Luis_Enrique",
      player_name == "Luis Suárez" & birth_date == "1935-05-02" ~ "/wiki/Luis_Su%C3%A1rez_Miramontes",
      player_name == "Luis Ubiña" ~ "/wiki/Luis_Ubi%C3%B1a",
      player_name == "Luisinho" ~ "/wiki/Lu%C3%ADs_Mesquita_de_Oliveira",
      player_name == "Munir Mohamedi" ~ "/wiki/Munir_Mohamedi",
      player_name == "Pat Bonner" ~ "/wiki/Pat_Bonner",
      player_name == "Patesko" ~ "/wiki/Patesko",
      player_name == "Ricardo Rodríguez" ~ "/wiki/Ricardo_Rodr%C3%ADguez_(footballer)",
      player_name == "Rivellino" ~ "/wiki/Rivellino",
      player_name == "Roberto Néstor Sensini" ~ "/wiki/Roberto_N%C3%A9stor_Sensini",
      player_name == "Vasyl Rats" ~ "/wiki/Vasyl_Rats",
      player_name == "Vladica Popović" ~ "/wiki/Vladica_Popovi%C4%87",
      player_name == "Volodymyr Bezsonov" ~ "/wiki/Volodymyr_Bezsonov",
      player_name == "Waldir Peres" ~ "/wiki/Waldir_Peres",
      player_name == "Wílmer Cabrera" ~ "/wiki/W%C3%ADlmer_Cabrera",
      player_name == "Yuri Nikiforov" ~ "/wiki/Yuriy_Nikiforov",
      player_name == "Zé Maria" ~ "/wiki/Jos%C3%A9_Maria_Rodrigues_Alves",
      player_name == "Zito" ~ "/wiki/Zito_(footballer)",

      # Women
      player_name == "Andressinha" ~ "/wiki/Andressinha",
      player_name == "Andressa Alves" ~ "/wiki/Andressa_Alves",
      player_name == "Beatriz" ~ "/wiki/Bia_Zaneratto",
      player_name == "Cristiane" ~ "/wiki/Cristiane_(footballer)",
      player_name == "Letícia Izidoro" ~ "/wiki/Let%C3%ADcia_Izidoro",
      player_name == "Poliana" ~ "/wiki/Poliana_(footballer)",
      player_name == "Tamires" ~ "/wiki/Tamires",
      player_name == "Tayla" ~ "/wiki/Tayla_(footballer)",

      TRUE ~ player_wikipedia_link
    )
  )

# Check again
to_check <- wikipedia_squads |>
  group_by(player_name, birth_date, team_name) |>
  summarize(
    player_wikipedia_link = str_c(unique(player_wikipedia_link), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(player_wikipedia_link, ";"))

## Check 4: multiple dates -----------------------------------------------------

# Group by player (name and link) and check for multiple dates
to_check <- wikipedia_squads |>
  group_by(player_name, player_wikipedia_link, team_name) |>
  summarize(
    birth_date = str_c(unique(birth_date), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(birth_date, ";"))

# Clean player dates
wikipedia_squads <- wikipedia_squads |>
  mutate(
    birth_date = case_when(

      # Men
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

      # Women
      player_name == "Ainon Phancha" ~ "1992-01-26",
      player_name == "Alex Scott" ~ "1984-10-14",
      player_name == "Aline" ~ "1982-07-06",
      player_name == "Anita Rapp" ~ "1977-07-24",
      player_name == "Ann Mukoro" ~ "1975-05-27",
      player_name == "Anneli Andelén" ~ "1968-06-21",
      player_name == "Annette Ngo Ndom" ~ "1985-06-02",
      player_name == "Asako Takakura" ~ "1968-04-19",
      player_name == "Bárbara" ~ "1988-07-04",
      player_name == "Candace Chapman" ~ "1983-04-02",
      player_name == "Cenira" ~ "1965-02-12",
      player_name == "Clare Polkinghorne" ~ "1989-02-01",
      player_name == "Daniela" ~ "1984-01-12",
      player_name == "Diana Matheson" ~ "1984-04-06",
      player_name == "Elane" ~ "1968-06-04",
      player_name == "Ester" ~ "1982-12-09",
      player_name == "Etsuko Handa" ~ "1965-05-10",
      player_name == "Fabiana Vallejos" ~ "1985-07-30",
      player_name == "Genevive Clottey" ~ "1969-04-25",
      player_name == "Grazielle" ~ "1981-03-28",
      player_name == "Homare Sawa" ~ "1978-09-06",
      player_name == "Hwang Bo-ram" ~ "1987-10-06",
      player_name == "Ivana Andrés" ~ "1994-07-13",
      player_name == "Jung Seol-bin" ~ "1990-01-06",
      player_name == "Kae Nishina" ~ "1972-12-07",
      player_name == "Kaori Nagamine" ~ "1968-06-03",
      player_name == "Khwanrudi Saengchan" ~ "1991-05-16",
      player_name == "Liu Ailing" ~ "1967-05-02",
      player_name == "Liu Yali" ~ "1980-02-09",
      player_name == "Louise Hansen" ~ "1975-05-04",
      player_name == "Malin Andersson" ~ "1973-05-04",
      player_name == "Malin Lundgren" ~ "1967-03-09",
      player_name == "Man Yanling" ~ "1972-11-09",
      player_name == "Márcia Taffarel" ~ "1968-03-15",
      player_name == "Maren Meinert" ~ "1973-08-05",
      player_name == "Marianne Pettersen" ~ "1975-04-12",
      player_name == "Maureen Mmadu" ~ "1975-05-07",
      player_name == "Ngozi Ezeocha" ~ "1973-10-12",
      player_name == "Niu Lijie" ~ "1969-04-12",
      player_name == "Nkechi Mbilitam" ~ "1974-04-05",
      player_name == "Nkiru Okosieme" ~ "1972-03-01",
      player_name == "Ogonna Chukwudi" ~ "1988-09-14",
      player_name == "Omo-Love Branch" ~ "1974-01-10",
      player_name == "Onome Ebi" ~ "1983-05-08",
      player_name == "Patience Avre" ~ "1976-06-10",
      player_name == "Patience Sackey" ~ "1975-04-22",
      player_name == "Raissa Feudjio" ~ "1995-10-29",
      player_name == "Rie Yamaki" ~ "1975-10-02",
      player_name == "Rita Nwadike" ~ "1974-11-03",
      player_name == "Rosana Gómez" ~ "1980-07-13",
      player_name == "Roseli" ~ "1969-09-07",
      player_name == "Sacha Wainwright" ~ "1972-02-06",
      player_name == "Sarah Cooper" ~ "1969-10-08",
      player_name == "Silvana Burtini" ~ "1969-05-10",
      player_name == "Sissi" ~ "1967-06-02",
      player_name == "Sun Wen" ~ "1973-04-06",
      player_name == "Suzanne Muir" ~ "1970-07-06",
      player_name == "Tânia" ~ "1974-03-10",
      player_name == "Therese Sjögran" ~ "1977-04-08",
      player_name == "Tochukwu Oluehi" ~ "1987-05-02",
      player_name == "Valeria" ~ "1968-03-09",
      player_name == "Wang Liping" ~ "1973-11-12",
      player_name == "Wei Haiying" ~ "1971-01-05",
      player_name == "Wen Lirong" ~ "1969-10-02",
      player_name == "Yūki Ōgimi" ~ "1987-07-15",
      player_name == "Yumi Tomei" ~ "1972-06-01",
      player_name == "Zhou Hua" ~ "1969-10-03",
      player_name == "Zhou Yang" ~ "1971-01-02",

      TRUE ~ birth_date
    )
  )

# Check again
to_check <- wikipedia_squads |>
  group_by(player_name, player_wikipedia_link, team_name) |>
  summarize(
    birth_date = str_c(unique(birth_date), collapse = "; "),
    .groups = "drop"
  ) |>
  filter(str_detect(birth_date, ";"))

## Clean quotes ----------------------------------------------------------------

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
      player_name == "Mahmoud 'El-Tetsh' Mokhtar" ~ "Mahmoud Mokhtar",
      TRUE ~ player_name
    )
  )

## Clean periods ---------------------------------------------------------------

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
      player_name == "C. J. Bott" ~ "CJ Bott",
      TRUE ~ player_name
    )
  )

# Add supplement ---------------------------------------------------------------

# Load data
squads_supplement <- read_csv(
  "data-raw/player-names/squads_supplement.csv",
  show_col_types = FALSE,
  progress = FALSE
)

# Add to data
wikipedia_squads <- bind_rows(
  wikipedia_squads,
  squads_supplement
)

# Select variables
wikipedia_squads <- wikipedia_squads |>
  mutate(
    tournament, year, team_name, shirt_number,
    position_name, position_code, player_name,
    birth_date, player_wikipedia_link
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
    temp_player_id = str_c(player_name, birth_date, player_wikipedia_link),
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
    player_id = sample(1:99999, size = n(), replace = FALSE),
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

# Merge in cleaned player names ------------------------------------------------

# Load names for men
player_names_men_cleaned <- read_csv(
  "data-raw/player-names/player_names_men_cleaned.csv",
  show_col_types = FALSE,
  progress = FALSE
)

# Load names for women
player_names_women_cleaned <- read_csv(
  "data-raw/player-names/player_names_women_cleaned.csv",
  show_col_types = FALSE,
  progress = FALSE
)

# Combine tibbles
player_names_cleaned <- bind_rows(
  player_names_men_cleaned,
  player_names_women_cleaned
)

# Get unique names
player_names_cleaned <- player_names_cleaned |>
  group_by(player_name, family_name, given_name) |>
  summarize(
    .groups = "drop"
  )

# Check for missing names
table(wikipedia_squads$player_name %in% player_names_cleaned$player_name)
table(player_names_cleaned$player_name %in% wikipedia_squads$player_name)

# Merge names into squad table
wikipedia_squads <- left_join(
  wikipedia_squads,
  player_names_cleaned,
  by = "player_name"
)

# Clean manager data -----------------------------------------------------------

# Add Sweden (1934)
wikipedia_managers <- wikipedia_managers |>
  add_row(
    file = "data-raw/Wikipedia-squad-pages/men-1934-squads.html",
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
    tournament = case_when(
      str_detect(file, "/men-") ~ "FIFA Men's World Cup",
      str_detect(file, "/women-") ~ "FIFA Women's World Cup",
    ),
    country_name = country_name |>
      str_replace_all("_", " "),
    country_name = case_when(
      country_name == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      country_name == "Federal Republic of Yugoslavia" ~ "Yugoslavia",
      country_name == "China PR" ~ "China",
      country_name == "West Germany" ~ "Germany",
      TRUE ~ country_name
    ),
    team_name = case_when(
      team_name == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      team_name == "Federal Republic of Yugoslavia" ~ "Yugoslavia",
      team_name == "China PR" ~ "China",
      team_name == "West Germany" ~ "Germany",
      TRUE ~ team_name
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

# Organize data ----------------------------------------------------------------

# Organize squad variables
wikipedia_squads <- wikipedia_squads |>
  arrange(tournament, year) |>
  select(
    tournament, year, team_name, shirt_number, position_name, position_code,
    player_id, player_name, family_name, given_name,
    birth_date, player_wikipedia_link
  )

# Check for missing data
table(is.na(wikipedia_squads))

# Organize manager variables
wikipedia_managers <- wikipedia_managers |>
  arrange(tournament, year, team_name) |>
  select(
    tournament, year, team_name, manager_name,
    country_name, manager_wikipedia_link
  )

# Check for missing data
table(is.na(wikipedia_managers))

# Save data
save(wikipedia_squads, file = "data-raw/Wikipedia-data/wikipedia_squads.RData")
save(wikipedia_managers, file = "data-raw/Wikipedia-data/wikipedia_managers.RData")
