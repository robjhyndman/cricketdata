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
#'
#' @return A tibble containing data on the selected player, with one row for every innings
#' of every match in which they have played.
#' @author Rob J Hyndman
#' @examples
#' \dontrun{
#' ElyssePerry <- fetch_player_data(275487, "T20", "batting")
#' RahulDravid <- fetch_player_data(32242, "ODI", "fielding")
#' LasithMalinga <- fetch_player_data(49758, "Test", "bowling")
#' BethMooney <- fetch_player_data(381258, "test", "batting")
#' IanHarvey <- fetch_player_data(5597, "test", "batting")

#'
#' library(ggplot2)
#' ggplot(MegLanning) + geom_point(aes(x=Date, y=Score, col=NotOut)) +
#'   ggtitle("Meg Lanning ODI Scores")
#' }
#'
#' @export
fetch_player_data <- function(playerid,
                              matchtype=c("test", "odi", "t20"),
                              activity =c("batting", "bowling", "fielding")) {
  matchtype <- tolower(matchtype)
  matchtype <- match.arg(matchtype)

  activity <- tolower(activity)
  activity <- match.arg(activity)

  # First figure out if player is female or male
  profile <- paste(
    "http://www.espncricinfo.com/ci/content/player/",
    playerid, ".html", sep = ""
  )
  raw <- try(xml2::read_html(profile), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    stop("Player not found")
  }
  female <- length(grep("format=women", as.character(raw))) > 0

  matchclass <- match(matchtype, c("test", "odi", "t20")) + (female * 7)

  url <- paste(
    "http://stats.espncricinfo.com/ci/engine/player/",
    playerid,
    ".html?class=",
    matchclass,
    ";template=results;type=", activity, ";view=innings;wrappertype=print",
    sep = ""
  )


  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw)) {
    stop("Problem with URL")
  }

  # Grab relevant table
  tab_all_rec <- rvest::html_table(raw)
  tab <- tab_all_rec[[4]]
  tab_no_rec <- tab_all_rec[[3]]

  if ("No records available to match this query" %in% unlist(tab_no_rec)) {
    stop(paste("Player has never played", matchtype, "format", sep = " "), call. = F)
  }
  # Remove redundant missings columns

  tab <- tibble::as_tibble(tab[, colSums(is.na(tab)) != NROW(tab)])

  # Convert "-" to NA
  tab[tab == "-"] <- NA

  # Make tidy column names columns
  tidy.col <- make.names(colnames(tab), unique = TRUE)
  colnames(tab) <- gsub(".", "_", tidy.col, fixed = TRUE)
  tidy.col <- colnames(tab)

  # Convert some columns to numeric or Date

  tab$Innings <- as.integer(tab$Inns)
  tab$Start_Date <- lubridate::dmy(tab$Start_Date)
  tab$Opposition <- substring(tab$Opposition, 3)
  tab$Ground <- as.character(tab$Ground)


  ## order the elements, no difference for different activities

  com_col <- c("Start_Date", "Innings", "Opposition", "Ground")

  ## order the elements, no difference for different activities

  # Reorder columns
  return(
    tab[, c(com_col, tidy.col[!tidy.col %in% com_col])]
  )
}
