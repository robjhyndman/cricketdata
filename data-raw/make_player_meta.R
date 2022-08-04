# Create tibble of cricinfo meta data for all players who are on both cricsheet and cricinfo.
library(cricketdata)
# Using start_again = TRUE in case some data has been corrected online.
# Much more efficient to set start_again = FALSE
player_meta <- update_player_meta(start_again = TRUE)
usethis::use_data(player_meta, overwrite = TRUE)

# Need to update date and size of object in following files
# fetch_player_meta.R
# update_player_meta.R
# data.R

