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
#' @seealso It is usually simpler to just use the saved data set \code{\link{player_meta}}
#' which contains the meta data for all players on ESPNCricinfo as at 3 August 2022.
#' To find a player ID, use \code{\link{find_player_id}()}. 
#' Use \code{\link{fetch_player_data}()} to download playing statistics for a player.
#' @examples
#' \dontrun{
#' # Download meta data on Meg Lanning and Ellyse Perry
#' aus_women <- fetch_player_meta(c(329336, 275487))
#'}
#' @export
fetch_player_meta <- function(playerid) {
  output <- NULL
  for(j in seq_along(playerid)) {
    #print(j)
    output <- rbind(output, fetch_player_meta_individual(playerid[j]))
  }
  return(output)
}

fetch_player_meta_individual <- function(playerid) {
  # Read URL with player meta data
  url <- paste0("https://www.espncricinfo.com/*/content/player/", playerid, ".html")
  raw <- try(rvest::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    warning(paste("Cannot read player information from ESPNCricinfo for ID",
               playerid))
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
    player.col <- player.col[player.col %in% 
      c("Full Name", "Born", "Age", "Batting Style", "Bowling Style", "Playing Role")]
    player.info <- html |>
      rvest::html_nodes(".ds-text-title-s") |>
      rvest::html_text(trim = TRUE) 
    player.info <- player.info[seq_along(player.col)]
    p.country.raw <- html |>
      rvest::html_nodes(".ds-text-comfortable-s") |>
      rvest::html_text(trim = TRUE)
    
    # data frame with one row
    output <- data.frame(title = player.col, values = player.info) %>%
      tidyr::pivot_wider(names_from = title, values_from = values) %>%
      janitor::clean_names()
    output$cricinfo_id <- playerid
    output$country <- p.country.raw[1]
  }
  if("born" %in% colnames(output)) {
    output$dob <- lubridate::mdy(paste0(unlist(strsplit(output$born, ","))[1:2], collapse=""))
    output$birthplace <- paste0(unlist(strsplit(output$born, ","))[-(1:2)], collapse="") |> 
      stringr::str_squish()
    output$born <- NULL
    if(output$birthplace == "")
      output$birthplace <- NA_character_
  } else {
    output$dob <- as.Date(NA)
    output$birthplace <- NA_character_
  }
  if(!("batting_style" %in% colnames(output)))
    output$batting_style <- NA_character_
  if(!("bowling_style" %in% colnames(output)))
    output$bowling_style <- NA_character_
  if(!("playing_role" %in% colnames(output)))
    output$playing_role <- NA_character_
  
  tibble::as_tibble(output) |> 
    dplyr::select(cricinfo_id, full_name, country, dob, birthplace, batting_style,
           bowling_style, playing_role)
}

utils::globalVariables(c("cricinfo_id","full_name","country","dob","birthplace",
                         "batting_style","bowling_style","playing_role",
                         "title","values"))