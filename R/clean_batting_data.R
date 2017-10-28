clean_batting_career_data <- function(x)
{
  # Make names easier to interpret
  vars <- colnames(x)
  vars[vars=="Mat"] <- "Matches"
  vars[vars=="Inns"] <- "Innings"
  vars[vars=="NO"] <- "NotOuts"
  vars[vars=="HS"] <- "HighScore"
  vars[vars=="Ave"] <- "Average"
  vars[vars=="100"] <- "Hundreds"
  vars[vars=="50"] <- "Fifties"
  vars[vars=="0"] <- "Ducks"
  vars[vars=="SR"] <- "StrikeRate"
  vars[vars=="BF"] <- "BallsFaced"
  vars[vars=="4s"] <- "Fours"
  vars[vars=="6s"] <- "Sixes"
  colnames(x) <- vars

  # Add not out column
  notout <- seq(NROW(x)) %in% grep("*", x$HighScore, fixed=TRUE)
  x$HighScore <- as.numeric(gsub("*", "", x$HighScore, fixed=TRUE))
  x$HighScoreNotOut <- as.logical(notout)

  if("Matches" %in% vars)
    x$Matches <- as.integer(x$Matches)
  if("Innings" %in% vars)
    x$Innings <- as.integer(x$Innings)
  if("Runs" %in% vars)
    x$Runs <- as.integer(x$Runs)
  if("NotOuts" %in% vars)
    x$NotOuts <- as.integer(x$NotOuts)
  x$Average <- x$Runs / (x$Innings - x$NotOuts)
  if("Hundreds" %in% vars)
    x$Hundreds <- as.integer(x$Hundreds)
  if("Fifties" %in% vars)
    x$Fifties <- as.integer(x$Fifties)
  if("Ducks" %in% vars)
    x$Ducks <- as.integer(x$Ducks)
  if("BallsFaced" %in% vars)
  {
    x$BallsFaced <- as.integer(x$BallsFaced)
    x$StrikeRate <- x$Runs / x$BallsFaced * 100
  }
  if("Fours" %in% vars)
    Fours <- as.integer(x$Fours)
  if("Sixes" %in% vars)
    Sixes <- as.integer(x$Sixes)

  # Span
  x$Start <- as.integer(substr(x$Span, 1, 4))
  x$End <- as.integer(substr(x$Span, 6, 9))
  x$Span <- NULL

  # Country
  if(length(grep("\\(", x[1,1])) > 0)
  {
    x$Country <- stringr::str_extract(x$Player, "\\([a-zA-Z \\-extends]+\\)")
    x$Country <- stringr::str_replace_all(x$Country, "\\(|\\)|-W", "")
    x$Country <- rename_countries(x$Country)
    x$Player <- stringr::str_replace(x$Player, "\\([a-zA-Z  \\-]+\\)", "")
  }

  return(x)

}

clean_batting_data <- function(x)
{
  # Add not out and participation column
  notout <- seq(NROW(x)) %in% grep("*", x$Runs)
  x$Runs <- gsub("*", "", x$Runs, fixed=TRUE)
  x <- dplyr::mutate(x,
    NotOut = seq(NROW(x)) %in% notout,
    Participation = participation_status(x$Runs))

  # Change DNB etc to NA
  absent <- grep("absent", x$Runs)
  dnbat <- grep("^DNB", x$Runs)
  tdnbat <- grep("TDNB", x$Runs)
  sub <- grep("sub", x$Runs)
  x$Runs[dnbat] <- NA
  x$Runs[tdnbat] <- NA
  x$Runs[absent] <- NA
  x$Runs[sub] <- NA

  # Convert some columns to numeric or date
  x <- dplyr::mutate(x,
    Runs = as.numeric(Runs),
    Mins = as.numeric(Mins),
    BallsFaced = as.numeric(BF),
    Fours = as.numeric(`4s`),
    Sixes = as.numeric(`6s`),
    StrikeRate = as.numeric(SR),
    Innings = as.integer(Inns),
    Date = lubridate::dmy(`Start Date`))

  # Tim's code -----------------------------------------------------------------
  x <- dplyr::mutate(x,
    Country = stringr::str_extract(Player, "\\([a-zA-Z \\-extends]+\\)"),
    Country = stringr::str_replace_all(Country, "\\(|\\)|-W", ""),
    Player = stringr::str_replace(Player, "\\([a-zA-Z  \\-]+\\)", ""),
    Opposition = stringr::str_replace_all(Opposition, "v | Women| Wmn", ""),
    Country = rename_countries(Country),
    Opposition = rename_countries(Opposition))

  x <- x[,c("Date","Player", "Country", "Runs", "NotOut", "Participation", "Mins", "BallsFaced", "Fours", "Sixes",
            "StrikeRate","Innings","Opposition","Ground")]

  return(x)

  # ----------------------------------------------------------------------------
}

rename_countries <- function(countries){
  countries %>%
    stringr::str_replace("AFG", "Afghanistan") %>%
    stringr::str_replace("Afr$", "Africa XI") %>%
    stringr::str_replace("AUS", "Australia") %>%
    stringr::str_replace("Bdesh|BDESH|BD", "Bangladesh") %>%
    stringr::str_replace("BMUDA", "Bermuda") %>%
    stringr::str_replace("CAN", "Canada") %>%
    stringr::str_replace("DnWmn|Denmk", "Denmark") %>%
    stringr::str_replace("EAf", "East (and Central) Africa") %>%
    stringr::str_replace("ENG", "England") %>%
    stringr::str_replace("HKG", "Hong Kong") %>%
    stringr::str_replace("ICC$", "ICC World XI") %>%
    stringr::str_replace("INDIA|IND", "India") %>%
    stringr::str_replace("IntWn|Int XI", "International XI") %>%
    stringr::str_replace("Ire$|IRELAND|IRE", "Ireland") %>%
    stringr::str_replace("JamWn", "Jamaica") %>%
    stringr::str_replace("JPN", "Japan") %>%
    stringr::str_replace("KENYA", "Kenya") %>%
    stringr::str_replace("NAM", "Namibia") %>%
    stringr::str_replace("NEPAL", "Nepal") %>%
    stringr::str_replace("Neth$|NL", "Netherlands") %>%
    stringr::str_replace("NZ", "New Zealand") %>%
    stringr::str_replace("OMAN", "Oman") %>%
    stringr::str_replace("PAK", "Pakistan") %>%
    stringr::str_replace("PNG|P\\.N\\.G\\.", "Papau New Guinea") %>%
    stringr::str_replace("^SA", "South Africa") %>%
    stringr::str_replace("SCOT|SCO|Scot$", "Scotland") %>%
    stringr::str_replace("SL", "Sri Lanka") %>%
    stringr::str_replace("TTWmn|T \\& T", "Trinidad and Tobago") %>%
    stringr::str_replace("UAE|U\\.A\\.E\\.", "United Arab Emirates") %>%
    stringr::str_replace("USA|U\\.S\\.A\\.", "United States of America") %>%
    stringr::str_replace("World$|World-XI", "World XI") %>%
    stringr::str_replace("WI", "West Indies") %>%
    stringr::str_replace("YEWmn|Y\\. Eng", "Young England") %>%
    stringr::str_replace("ZIM", "Zimbabwe")
}


