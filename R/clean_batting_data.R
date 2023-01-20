# Function to clean batting data.
# Works with career or innings data

clean_batting_data <- function(x) {
  # Make names easier to interpret
  vars <- colnames(x)
  vars[vars == "Mat"] <- "Matches"
  vars[vars == "Inns"] <- "Innings"
  vars[vars == "NO"] <- "NotOuts"
  vars[vars == "HS"] <- "HighScore"
  vars[vars == "Ave"] <- "Average"
  vars[vars == "100"] <- "Hundreds"
  vars[vars == "50"] <- "Fifties"
  vars[vars == "0"] <- "Ducks"
  vars[vars == "SR"] <- "StrikeRate"
  vars[vars == "BF"] <- "BallsFaced"
  vars[vars == "4s"] <- "Fours"
  vars[vars == "6s"] <- "Sixes"
  vars[vars == "Mins"] <- "Minutes"
  vars[vars == "Start Date"] <- "Date"
  colnames(x) <- vars

  career <- ("Matches" %in% vars)
  if (career) {
    x$Matches <- as.integer(x$Matches)
    x$NotOuts <- as.integer(x$NotOuts)
    x$Hundreds <- as.integer(x$Hundreds)
    x$Fifties <- as.integer(x$Fifties)
    x$Ducks <- as.integer(x$Ducks)
    # Add not out column and remove annotations from highscore
    x$HighScoreNotOut <- seq(NROW(x)) %in% grep("*", x$HighScore, fixed = TRUE)
    x$HighScore <- as.numeric(gsub("*", "", x$HighScore, fixed = TRUE))
    if ("Span" %in% vars) {
      x$Start <- as.integer(substr(x$Span, 1, 4))
      x$End <- as.integer(substr(x$Span, 6, 9))
    }
    x$Runs <- as.integer(x$Runs)
    x$Innings <- as.integer(x$Innings)
    x$Average <- x$Runs / (x$Innings - x$NotOuts)
  } else {
    x$Innings <- as.integer(x$Innings)
    x$Minutes <- as.numeric(x$Minutes)
    x$Date <- lubridate::dmy(x$Date)
    x$Opposition <- stringr::str_replace_all(x$Opposition, "v | Women| Wmn", "")
    x$Opposition <- rename_countries(x$Opposition)
    # Add not out and participation column and remove annotations from runs
    x$NotOut <- seq(NROW(x)) %in% grep("*", x$Runs, fixed = TRUE)
    x$Runs <- gsub("*", "", x$Runs, fixed = TRUE)
    x$Participation <- participation_status(x$Runs)
    x$Runs[x$Participation != "B"] <- NA
    x$Runs <- as.integer(x$Runs)
  }

  if ("BallsFaced" %in% vars) {
    x$BallsFaced <- as.integer(x$BallsFaced)
    x$StrikeRate <- x$Runs / x$BallsFaced * 100
  }
  if ("Fours" %in% vars) {
    x$Fours <- as.integer(x$Fours)
    x$Sixes <- as.integer(x$Sixes)
  }

  # Extract country information if it is present
  # This should only be required when multiple countries are included
  country <- (length(grep("\\(", x$Player) > 0))
  if (country) {
    x$Country <- stringr::str_extract(x$Player, "\\([a-zA-Z /\\-extends]+\\)")
    x$Country <- stringr::str_replace_all(x$Country, "\\(|\\)|-W", "")
    x$Country <- rename_countries(x$Country)
    x$Player <- stringr::str_replace(x$Player, "\\([a-zA-Z /\\-]+\\)", "")
  }

  # Re-order and select columns
  vars <- colnames(x)
  if (career) {
    varorder <- c(
      "Player", "Country", "Start", "End", "Matches", "Innings", "NotOuts", "Runs", "HighScore", "HighScoreNotOut",
      "Average", "BallsFaced", "StrikeRate", "Hundreds", "Fifties", "Ducks", "Fours", "Sixes"
    )
  } else {
    varorder <- c(
      "Date", "Player", "Country", "Runs", "NotOut", "Minutes", "BallsFaced", "Fours", "Sixes",
      "StrikeRate", "Innings", "Participation", "Opposition", "Ground"
    )
  }
  varorder <- varorder[varorder %in% vars]

  return(x[, varorder])
}


# Convert bowling/batting batting category to a character variable.

participation_status <- function(status) {
  absent <- grep("absent", status)
  dnb <- grep("^DNB", status)
  tdnb <- grep("TDNB", status)
  sub <- grep("sub", status)

  status[seq(NROW(status))] <- "B"
  status[absent] <- "Absent"
  status[dnb] <- "DNB"
  status[tdnb] <- "TDNB"
  status[sub] <- "Sub"

  return(status)
}
