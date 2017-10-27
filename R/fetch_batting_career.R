#' Fetch and clean batting career dataset.
#'
#' Retrieve career batting data for each person on ESPNCricinfo
#'
#' @param matchtype Character indicating test, odi, or t20.
#' @param sex Character indicating men or women.
#'
#' @examples
#' wt20_batting_career <- fetch_batting_career("t20", "women")
#'
#' @export

fetch_batting_career <- function(matchtype = c("test", "odi", "t20"),
                                sex = c("men", "women")) {

  # Check arguments given by user match the type (class?) of the default
  # arguments of the function.
  matchtype <- match.arg(matchtype)
  sex <- match.arg(sex)

  # Get the data.
  this_data <- fetch_cricket_data(matchtype, sex, "batting", "career")

  return(clean_batting_data(this_data))
}

