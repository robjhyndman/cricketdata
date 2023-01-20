#' Fetch Data from Cricinfo
#'
#' Fetch data from ESPNCricinfo and return a tibble.
#' All arguments are case-insensitive and partially matched.
#'
#' @param matchtype Character indicating test (default), odi, or t20.
#' @param sex Character indicating men (default) or women.
#' @param activity Character indicating batting (default), bowling or fielding.
#' @param type Character indicating innings-by-innings or career (default) data
#' @param country Character indicating country. The default is to fetch data for all countries.
#'
#' @author Rob J Hyndman, Timothy Hyndman, Charles Gray
#' @return A \code{tibble} object, similar to a \code{data.frame}.
#' @examples
#' \dontrun{
#' auswt20 <- fetch_cricinfo("T20", "Women", country = "Aust")
#' IndiaODIBowling <- fetch_cricinfo("ODI", "men", "bowling", country = "india")
#' }
#'
#' @export

fetch_cricinfo <- function(matchtype = c("test", "odi", "t20"),
                           sex = c("men", "women"),
                           activity = c("batting", "bowling", "fielding"),
                           type = c("career", "innings"),
                           country = NULL) {
  matchtype <- tolower(matchtype)
  sex <- tolower(sex)
  type <- tolower(type)
  activity <- tolower(activity)
  if (!is.null(country)) {
    country <- tolower(country)
  }

  matchtype <- match.arg(matchtype)
  sex <- match.arg(sex)
  activity <- match.arg(activity)
  type <- match.arg(type)

  # Get the raw data
  this_data <- fetch_cricket_data(matchtype, sex, country, activity, type)

  # Clean it up
  if (activity == "batting") {
    this_data <- clean_batting_data(this_data)
  } else if (activity == "bowling") {
    this_data <- clean_bowling_data(this_data)
  } else {
    this_data <- clean_fielding_data(this_data)
  }

  return(this_data)
}
