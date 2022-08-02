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
#' @seealso [find_player_id()]
#' @examples
#' \dontrun{
#' # Download meta data on Meg Lanning and Ellyse Perry
#' aus_women <- fetch_player_meta(c(329336, 275487))
#'}
#' @export
fetch_player_meta <- function(playerid) {
  output <- NULL
  for(j in seq_along(playerid)) {
    output <- rbind(output, fetch_player_meta_individual(playerid[j]))
  }
  return(output)
}

fetch_player_meta_individual <- function(playerid) {
  # Read URL with player meta data
  url <- paste0("https://www.espncricinfo.com/*/content/player/", playerid, ".html")
  raw <- try(rvest::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    stop(paste("Cannot read player information from ESPNCricinfo for ID",
               playerid))
  }
  
  # Parse html from ESPNCricinfo
  html <- rvest::read_html(url)
  player.col <- html |>
                rvest::html_elements(".ds-grid p") |>
                rvest::html_text(trim = T) |>
                stringr::str_squish()
  player.info <- html |>
    rvest::html_nodes(".ds-text-title-s") |>
    rvest::html_text(trim = T) |>
    stringr::str_squish()
  player.info <- player.info[seq_along(player.col)]
  p.country.raw <- html |>
    rvest::html_nodes(".ds-text-comfortable-s") |>
    rvest::html_text(trim = T) |>
    stringr::str_squish()
  
  # Return data frame with one row
  output <- tibble::tibble(
    cricinfo_id = playerid,
    name = player.info[player.col == "Full Name"],
    country = p.country.raw[1],
    dob = player.info[player.col == "Born"],
    birthplace = player.info[player.col == "Born"],
    age = player.info[player.col == "Age"],
    batting_style = player.info[player.col == "Batting Style"],
    bowling_style = player.info[player.col == "Bowling Style"],
    playing_role = player.info[player.col == "Playing Role"]
  )
  output$dob <- lubridate::mdy(paste0(unlist(strsplit(output$dob, ","))[1:2], collapse=""))
  output$birthplace <- paste0(unlist(strsplit(output$birthplace, ","))[-(1:2)], collapse="") |> 
    stringr::str_squish()
  if(output$birthplace == "")
    output$birthplace <- NA_character_
  
  return(output)
}
