library(cricketdata)
library(rvest)
library(stringr)

# Create tibble of cricinfo meta data for all players who are on both cricsheet and cricinfo.
# Using start_again = TRUE in case some data has been corrected online.
# Much more efficient to set start_again = FALSE
player_meta <- update_player_meta(start_again = TRUE)
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
names <- names[!str_detect(names, "Original\nNew")]
names <- names[!grepl("^[0-9,]*$", names)]
names <- names[tail(which(names == "All matches"), 1):length(names)]
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
