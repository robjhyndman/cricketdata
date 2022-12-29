#' Fetch Player Data
#'
#' Fetch individual player data from all matches played. The function will scrape
#' the data from ESPNCricinfo and return a tibble with one line per innings for all
#' games a player has played. To identify a player, use their Cricinfo player ID.
#' The simplest way to find this is to look up their Cricinfo Profile page. The number
#' at the end of the URL is the ID. For example, Meg Lanning's profile page is
#' http://www.espncricinfo.com/australia/content/player/329336.html,
#' so her ID is 329336.
#'
#' @param playerid The player ID as given in the Cricinfo profile. Integer or character.
#' @param matchtype Which type of cricket matches do you want? Tests, ODIs or T20s? Not case-sensitive.
#' @param activity Which type of activities do you want? Batting, Bowling or Fielding? Not case-sensitive.
#'
#' @return A tibble containing data on the selected player, with one row for every innings
#' of every match in which they have played.
#' @author Rob J Hyndman and Sayani Gupta
#' @seealso [find_player_id()] to find a player ID by searching on their name,
#' and [fetch_player_meta()] to download meta data for players.
#' @examples
#' \dontrun{
#' # Download data on some players
#' EllysePerry <- fetch_player_data(275487, "T20", "batting")
#' RahulDravid <- fetch_player_data(28114, "ODI", "fielding")
#' LasithMalinga <- fetch_player_data(49758, "Test", "bowling")
#'
#' # Create a plot for Ellyse Perry's T20 scores
#' library(dplyr)
#' library(ggplot2)
#' EllysePerry |>
#'   filter(!is.na(Runs)) |>
#'   ggplot(aes(x = Start_Date, y = Runs, col = Dismissal, na.rm = TRUE)) +
#'   geom_point() +
#'   ggtitle("Ellyse Perry's T20 Scores")
#' }
#' @export
fetch_player_data <- function(playerid,
                              matchtype = c("test", "odi", "t20"),
                              activity = c("batting", "bowling", "fielding")) {
  matchtype <- tolower(matchtype)
  matchtype <- match.arg(matchtype)

  activity <- tolower(activity)
  activity <- match.arg(activity)

  # First figure out if player is female or male
  profile <- paste(
    "http://www.espncricinfo.com/ci/content/player/",
    playerid, ".html",
    sep = ""
  )
  raw <- try(xml2::read_html(profile), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    stop("Player not found")
  }
  female <- length(grep("format=women", as.character(raw))) > 0

  matchclass_male <- match(matchtype, c("test", "odi", "t20"))
  matchclass_female <- match(matchtype, c("test", "odi", "t20")) + 7

  url <- paste(
    "http://stats.espncricinfo.com/ci/engine/player/",
    playerid,
    ".html?class=",
    matchclass_male,
    ";template=results;type=", activity, ";view=innings;wrappertype=print",
    sep = ""
  )
  raw <- try(xml2::read_html(url), silent = TRUE)
  check1 <- rvest::html_table(raw)

  if ("No records available to match this query" %in% unlist(check1)) {
    url <- paste(
      "http://stats.espncricinfo.com/ci/engine/player/",
      playerid,
      ".html?class=",
      matchclass_female,
      ";template=results;type=", activity, ";view=innings;wrappertype=print",
      sep = ""
    )
  }

  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    stop("Problem with URL")
  }
  # closing previous html connections
  # Grab relevant table
  tab_all_rec <- rvest::html_table(raw)
  tab <- tab_all_rec[[4]]
  tab_no_rec <- tab_all_rec[[3]]

  if ("No records available to match this query" %in% unlist(tab_no_rec)) {
    stop(paste("Player has never played", matchtype, "format", sep = " "), call. = F)
  }
  # Remove redundant missings columns
  tab <- tibble::as_tibble(tab[, colSums(is.na(tab)) != NROW(tab)], .name_repair = "check_unique")

  # Convert "-" to NA
  tab[tab == "-"] <- NA

  # Convert some columns to numeric or Date
  tab$Innings <- as.integer(tab$Inns)
  tab$Date <- lubridate::dmy(tab$`Start Date`)
  tab$`Start Date` <- NULL
  tab$Opposition <- substring(tab$Opposition, 3)
  tab$Ground <- as.character(tab$Ground)
  if ("Mins" %in% colnames(tab)) {
    tab$Mins <- as.numeric(tab$Mins)
  }

  # Make tidy column names columns
  tidy.col <- make.names(colnames(tab), unique = TRUE)
  colnames(tab) <- gsub(".", "_", tidy.col, fixed = TRUE)
  tidy.col <- colnames(tab)

  ## order the elements, no difference for different activities
  com_col <- c("Date", "Innings", "Opposition", "Ground")

  ## Removing "*" in the column `Runs` and converting it to numeric
  if ("Runs" %in% colnames(tab)) {
    tab$Runs <- suppressWarnings(as.numeric(gsub("*", "", x = tab$Runs, fixed = TRUE)))
  }

  # Reorder columns
  return(
    tab[, c(com_col, tidy.col[!tidy.col %in% com_col])]
  )
}
