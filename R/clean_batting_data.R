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
    stringr::str_replace("AUS", "Australia") %>%
    stringr::str_replace("Bdesh|BDESH|BD", "Bangladesh") %>%
    stringr::str_replace("BMUDA", "Bermuda") %>%
    stringr::str_replace("CAN", "Canada") %>%
    stringr::str_replace("ENG", "England") %>%
    stringr::str_replace("HKG", "Hong Kong") %>%
    stringr::str_replace("INDIA|IND", "India") %>%
    stringr::str_replace("Ire$|IRELAND|IRE", "Ireland") %>%
    stringr::str_replace("KENYA", "Kenya") %>%
    stringr::str_replace("NEPAL", "Nepal") %>%
    stringr::str_replace("Neth$|NL", "Netherlands") %>%
    stringr::str_replace("NZ", "New Zealand") %>%
    stringr::str_replace("OMAN", "Oman") %>%
    stringr::str_replace("PAK", "Pakistan") %>%
    stringr::str_replace("PNG|P\\.N\\.G\\.", "Papau New Guinea") %>%
    stringr::str_replace("SA", "South Africa") %>%
    stringr::str_replace("SCOT", "Scotland") %>%
    stringr::str_replace("SL", "Sri Lanka") %>%
    stringr::str_replace("UAE|U\\.A\\.E\\.", "United Arab Emirates") %>%
    stringr::str_replace("WI", "West Indies") %>%
    stringr::str_replace("ZIM", "Zimbabwe")
}


