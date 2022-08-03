# Create tibble of cricinfo meta data for all players who are on both cricsheet and cricinfo.
library(cricketdata)
library(tidyverse)

# Read people file from cricsheet
people <- readr::read_csv("https://cricsheet.org/register/people.csv") |>
  select(cricsheet_id = identifier,
         cricinfo_id = key_cricinfo,
         cricinfo2 = key_cricinfo_2,
         name, unique_name) |>
  # Remove people not on Cricinfo
  filter(!is.na(cricinfo_id))

# Compare existing version of player_meta and find missing players
missing_df <- people |>
  anti_join(player_meta, by = "cricinfo_id") |>
  anti_join(player_meta, by = c("cricinfo2" = "cricinfo_id"))

# Now update it with any missing players
updates <- fetch_player_meta(missing_df$cricinfo_id) |>
  filter(!is.na(full_name))
player_meta <- player_meta |>
  bind_rows(updates)

# For people missing on cricinfo, try the other cricinfo id
missing_df <- people |>
  right_join(player_meta |> filter(is.na(full_name)), by='cricinfo_id') |>
  filter(!is.na(cricinfo2))
cricinfo2 <- fetch_player_meta(missing_df$cricinfo2)
player_meta <- player_meta |>
  filter(!is.na(full_name)) |>
  bind_rows(cricinfo2) |>
  arrange(full_name)

# Clean up some fields
player_meta <- player_meta |>
  distinct() |>
  mutate(country = str_remove(country, " Wmn"))

# Make it available in the package
usethis::use_data(player_meta, overwrite = TRUE)
