#' Fetch Player Meta Data
#'
#' Fetch player meta data from ESPNCricinfo and return a tibble with one line
#' per player. To identify the players, use their Cricinfo player IDs.
#' The simplest way to find this is to look up their Cricinfo Profile page. The number
#' at the end of the URL is the ID. For example, Meg Lanning's profile page is
#' https://www.espncricinfo.com/cricketers/meg-lanning-329336,
#' so her ID is 329336.
#'
#' @param playerid A vector of player IDs as given in Cricinfo profiles. Integer or character.
#'
#' @return A tibble containing meta data on the selected players, with one row for
#' each player.
#' @author Hassan Rafique and Rob J Hyndman
#' @seealso It is usually simpler to just use the saved data set [player_meta]
#' which contains the meta data for all players on ESPNCricinfo as at 24 March 2025.
#' To find a player ID, use [find_player_id()].
#' Use [fetch_player_data()] to download playing statistics for a player.
#' @examples
#' \dontrun{
#' # Download meta data on Meg Lanning and Ellyse Perry
#' aus_women <- fetch_player_meta(c(329336, 275487))
#' }
#' @export
fetch_player_meta <- function(playerid) {
  output <- NULL
  pb <- cli::cli_progress_bar(total = length(playerid))
  for (j in seq_along(playerid)) {
    cli::cli_progress_update()
    output <- rbind(output, fetch_player_meta_individual(playerid[j]))
  }
  cli::cli_progress_done()
  return(output)
}

fetch_player_meta_individual <- function(playerid) {
  # Set up empty output
  output <- data.frame(
    cricinfo_id = playerid,
    name = NA_character_,
    full_name = NA_character_,
    country = NA_character_,
    dob = as.Date(NA),
    batting_style = NA_character_,
    bowling_style = NA_character_
  )
  # Read JSON file with player meta data
  url <- paste0("http://core.espnuk.org/v2/sports/cricket/athletes/", playerid)
  json <- try(jsonlite::read_json(url), silent = TRUE)
  if ("try-error" %in% class(json)) {
    warning(paste(
      "Cannot read player information from ESPNCricinfo for ID",
      playerid
    ))
  } else {
    output$full_name <- json$fullName 
    output$name <- json$name
    output$country <- get_espn_country(json$country)
    if(is.na(output$country)) {
      stop(paste("Country not found. Player",playerid,output$full_name))
    }
    output$dob <- as.Date(json$dateOfBirth)
    if(length(json$style) > 0) {
      output$batting_style <- json$style[[1]]$description
    }
    if(length(json$style) > 1) {
      output$bowling_style <- json$style[[2]]$description
    }
  }
  return(output)
}

# Find country from ESPN team id
get_espn_country <- function(i) {
  if(i %in% espn_countries$id) {
    return(espn_countries$country[espn_countries$id == i])
  }
  json <- try(jsonlite::read_json(paste0("http://core.espnuk.org/v2/sports/cricket/teams/", i)), silent = TRUE)
  if(!("try-error" %in% class(json))) {
    # Update data set to avoid repeated calls
    espn_countries <- rbind(espn_countries, data.frame(id = i, country = json$name)) 
    return(json$name)
  } else {
    return(NA_character_)
  }
}

utils::globalVariables(c(
  "cricinfo_id", "full_name", "country", "dob", "birthplace",
  "batting_style", "bowling_style", "playing_role",
  "title", "values"
))
