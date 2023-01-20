#' Fetch Player Meta Data
#'
#' Fetch player meta data from ESPNCricinfo and return a tibble with one line
#' per player. To identify the players, use their Cricinfo player IDs.
#' The simplest way to find this is to look up their Cricinfo Profile page. The number
#' at the end of the URL is the ID. For example, Meg Lanning's profile page is
#' http://www.espncricinfo.com/australia/content/player/329336.html,
#' so her ID is 329336.
#'
#' @param playerid A vector of player IDs as given in Cricinfo profiles. Integer or character.
#'
#' @return A tibble containing meta data on the selected players, with one row for
#' each player.
#' @author Hassan Rafique and Rob J Hyndman
#' @seealso It is usually simpler to just use the saved data set [player_meta]
#' which contains the meta data for all players on ESPNCricinfo as at 20 January 2023.
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
  for (j in seq_along(playerid)) {
    # print(j)
    output <- rbind(output, fetch_player_meta_individual(playerid[j]))
  }
  return(output)
}

fetch_player_meta_individual <- function(playerid) {
  # Read URL with player meta data
  url <- paste0("https://www.espncricinfo.com/*/content/player/", playerid, ".html")
  raw <- try(rvest::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    warning(paste(
      "Cannot read player information from ESPNCricinfo for ID",
      playerid
    ))
    # Empty data frame with one row
    output <- data.frame(
      cricinfo_id = playerid,
      full_name = NA_character_,
      country = NA_character_
    )
  } else {
    # Parse html from ESPNCricinfo
    html <- rvest::read_html(url)
    player.col <- html |>
      rvest::html_elements(".ds-grid p") |>
      rvest::html_text(trim = TRUE) |>
      stringr::str_squish()
    keep_cols <- which(player.col %in%
      c("Full Name", "Born", "Age", "Batting Style", "Bowling Style", "Playing Role"))
    player.col <- player.col[keep_cols]
    player.info <- html |>
      rvest::html_nodes(".ds-text-title-s") |>
      rvest::html_text(trim = TRUE)
    player.info <- player.info[keep_cols]
    p.country.raw <- html |>
      rvest::html_nodes(".ds-text-comfortable-s") |>
      rvest::html_text(trim = TRUE)

    # data frame with one row
    output <- data.frame(title = player.col, values = player.info) |>
      tidyr::pivot_wider(names_from = title, values_from = values) |>
      janitor::clean_names()
    output$cricinfo_id <- playerid
    output$country <- p.country.raw[1]
  }
  # Extract DOB and Birthplace
  output$dob <- as.Date(NA)
  output$birthplace <- NA_character_
  if ("born" %in% colnames(output)) {
    output$dob <- stringr::str_extract(output$born, "[A-Za-z0-9 ,]*[1-2][0-9][0-9][0-9]")
    if (!is.na(output$dob)) {
      # Is there a date or only a month and year?
      if (stringr::str_detect(output$dob, ",")) {
        output$dob <- lubridate::mdy(output$dob)
      } else {
        # Set date to first of month
        output$dob <- lubridate::dmy(paste("01", output$dob))
      }
    }
    output$birthplace <- stringr::str_remove(output$born, "[A-Za-z0-9 ,]*[0-9]")
    output$born <- NULL
    if (output$birthplace == "") {
      output$birthplace <- NA_character_
    } else {
      output$birthplace <- stringr::str_remove(output$birthplace, "^[, ]*")
      # Fix missing countries
      if (is.na(output$country) & output$birthplace == "South Korea") {
        output$country <- "South Korea"
      }
    }
  }
  if (!("batting_style" %in% colnames(output))) {
    output$batting_style <- NA_character_
  }
  if (!("bowling_style" %in% colnames(output))) {
    output$bowling_style <- NA_character_
  }
  if (!("playing_role" %in% colnames(output))) {
    output$playing_role <- NA_character_
  }

  tibble::as_tibble(output) |>
    dplyr::select(
      cricinfo_id, full_name, country, dob, birthplace, batting_style,
      bowling_style, playing_role
    )
}

utils::globalVariables(c(
  "cricinfo_id", "full_name", "country", "dob", "birthplace",
  "batting_style", "bowling_style", "playing_role",
  "title", "values"
))
