# Function to clean bowling data.
# Works with career or innings data

clean_bowling_data <- function(x)
{
  # Make names easier to interpret
  vars <- colnames(x)
  vars[vars=="Mat"] <- "Matches"
  vars[vars=="Inns"] <- "Innings"
  vars[vars=="Mdns"] <- "Maidens"
  vars[vars=="Wkts"] <- "Wickets"
  vars[vars=="BBI"] <- "BestBowlingInnings"
  vars[vars=="BBM"] <- "BestBowlingMatch"
  vars[vars=="Ave"] <- "Average"
  vars[vars=="Econ"] <- "Economy"
  vars[vars=="SR"] <- "StrikeRate"
  vars[vars=="4"] <- "FourWickets"
  vars[vars=="5"] <- "FiveWickets"
  vars[vars=="10"] <- "TenWickets"
  vars[vars=="Start Date"] <- "Date"
  colnames(x) <- vars

  # Fix classes for all variables
  if("Maidens" %in% vars)
    x$Maidens <- as.integer(x$Maidens)
  if("Balls" %in% vars)
    x$Balls <- as.integer(x$Balls)
  x$Runs <- as.integer(x$Runs)
  x$Wickets <- as.integer(x$Wickets)
  x$Innings <- as.integer(x$Innings)

  career <- ("Matches" %in% vars)

  if(career)
  {
    x$Matches <- as.integer(x$Matches)
    if("Span" %in% vars)
    {
      x$Start <- as.integer(substr(x$Span, 1, 4))
      x$End <- as.integer(substr(x$Span, 6, 9))
    }
    if("FourWickets" %in% vars)
      x$FourWickets <- as.integer(x$FourWickets)
    if("FiveWickets" %in% vars)
      x$FiveWickets <- as.integer(x$FiveWickets)
    if("TenWickets" %in% vars)
      x$TenWickets <- as.integer(x$TenWickets)
  }
  else
  {
    # Add participation column
    if("Overs" %in% vars)
    {
      x$Participation <- participation_status(x$Overs)
      x$Overs[x$Participation!="B"] <- NA
    }
    x$Date <- lubridate::dmy(x$Date)
    x$Opposition <- stringr::str_replace_all(x$Opposition, "v | Women| Wmn", "")
    x$Opposition <- rename_countries(x$Opposition)
  }
  if("Overs" %in% vars)
    x$Overs <- as.numeric(x$Overs)

  # Recompute average to avoid rounding errors
  if("Average" %in% vars)
    x$Average <- x$Runs / x$Wickets

  # Recompute economy rate to avoid rounding errors
  if("Balls" %in% vars)
    balls <- x$Balls
  else
    balls <- trunc(x$Overs)*6 + (x$Overs %% 1)*10
  if("Economy" %in% vars)  {
    x$Economy <- as.numeric(x$Economy)
    ER <- x$Runs / (balls/6)
    # Don't modify values if they differ by more than 0.05
    differ <- abs(round(ER,2) - x$Economy) > 0.05
    differ[is.na(differ)] <- FALSE
    x$Economy[!differ] <- ER[!differ]
  }

  # Recompute strike rate
  if("StrikeRate" %in% vars)
    x$StrikeRate <- balls / x$Wickets

  # Extract country information if it is present
  # This should only be required when multiple countries are included
  country <- (length(grep("\\(", x[1,1])) > 0)
  if(country)
  {
    x$Country <- stringr::str_extract(x$Player, "\\([a-zA-Z \\-extends]+\\)")
    x$Country <- stringr::str_replace_all(x$Country, "\\(|\\)|-W", "")
    x$Country <- rename_countries(x$Country)
    x$Player <- stringr::str_replace(x$Player, " \\([a-zA-Z  \\-]+\\)", "")
  }

  # Re-order and select columns
  vars <- colnames(x)
  if(career)
    varorder <- c("Player","Country","Start","End","Matches","Innings","Overs","Balls","Maidens","Runs","Wickets",
      "Average","Economy","StrikeRate","BestBowlingInnings","BestBowlingMatch","FourWickets","FiveWickets","TenWickets")
  else
    varorder <- c("Date","Player", "Country", "Overs","Balls","Maidens","Runs","Wickets",
      "Economy","Innings","Participation", "Opposition","Ground")
  varorder <- varorder[varorder %in% vars]

  return(x[,varorder])

}
