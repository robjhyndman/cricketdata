# Create tibble of cricinfo meta data for all players who are on both cricsheet and cricinfo.
library(cricketdata)
player_meta <- update_player_meta()
usethis::use_data(player_meta, overwrite = TRUE)

# Need to update date in help files for 
# player_meta()
# fetch_player_meta()
# update_player_meta()

