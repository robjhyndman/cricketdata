library(cricketdata)
library(rvest)
library(stringr)
library(dplyr)

# List of the first 1000 ESPN teams
get_espn_countries <- function(n = 1000) {
  countries <- tibble(id = seq(n), country = character(n))
  for(i in countries$id) {
    cat(i," ")
    countries$country[i] <- cricketdata:::get_espn_country(i)
  }
  return(countries)
}

espn_countries <- get_espn_countries()
usethis::use_data(espn_countries, overwrite = TRUE, internal = TRUE)


# Create tibble of cricinfo meta data for all players who are on both cricsheet and cricinfo.
# Using start_again = TRUE in case some data has been corrected online.
# Much more efficient to set start_again = FALSE
new_player_meta <- update_player_meta(start_again = FALSE)
new_player_meta <- new_player_meta |> 
  as_tibble() |>
  mutate(
    country = if_else(str_detect(country, "P.N.G."), "Papua New Guinea", country),
    country = if_else(str_detect(country, "U.A.E."), "United Arab Emirates", country),
    country = if_else(str_detect(country, "U.S.A."), "United States of America", country),
    country = if_else(str_detect(country, "Czech Rep."), "Czech Republic", country),
    country = if_else(str_detect(country, "Cayman Is"), "Cayman Islands", country),
  )

# Check all character fields are ascii
for (j in seq_len(NCOL(new_player_meta))) {
  if(inherits(new_player_meta[,j], "character")) {
    new_player_meta[,j] <- iconv(new_player_meta[,j], from="utf8", to="ascii")
  }
}
player_meta <- new_player_meta
usethis::use_data(player_meta, overwrite = TRUE)

# Need to update date and size of object in following files
# fetch_player_meta.R
# update_player_meta.R
# data.R

# Find all csv2 files on cricsheet
downloads <- read_html("https://cricsheet.org/downloads/")
names <- downloads |>
  html_elements("td") |>
  html_text2() |>
  str_trim()
names <- names[names != ""]
names <- names[!str_detect(names, "JSON")]
names <- names[!grepl("^[0-9,]*$", names)]
names <- names[!str_detect(names, "[Added|Played] in the previous")]
first <- which(str_detect(names, "^All matches"))[2]
names <- names[first:length(names)]
names <- names[!str_detect(names, "Original\nNew")]
names <- str_remove(names, " [0-9]* matches withheld \\(\\?\\)")

acronyms <- downloads |>
  html_elements("a") |>
  as.character()
acronyms <- acronyms[grepl("csv2", acronyms)]
acronyms <- acronyms[!grepl("_male_csv2", acronyms)]
acronyms <- acronyms[!grepl("_female_csv2", acronyms)]
acronyms <- tail(acronyms, length(names))
acronyms <- str_extract(acronyms, "[a-zA-Z0-9]*_csv2")
acronyms <- str_remove(acronyms, "_csv2")

cricsheet_codes <- tibble::tibble(
  competition = names,
  code = acronyms
)
usethis::use_data(cricsheet_codes, overwrite = TRUE)
