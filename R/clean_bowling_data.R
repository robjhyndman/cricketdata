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
  if(!career)
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
  {
    x$Overs <- as.integer(x$Overs)
    x$Economy <- x$Runs / x$Overs
  }
  else
    x$Economy <- x$Runs / (x$Balls/6)
  if(career)
  {
    x$Matches <- as.integer(x$Matches)
    x$Average <- x$Runs / x$Wickets
    if("Span" %in% vars)
    {
      x$Start <- as.integer(substr(x$Span, 1, 4))
      x$End <- as.integer(substr(x$Span, 6, 9))
    }
    if("Overs" %in% vars)
      x$StrikeRate <- x$Overs*6 / x$Wickets
    else
      x$StrikeRate <- x$Balls / x$Wickets
    if("FourWickets" %in% vars)
      x$FourWickets <- as.integer(x$FourWickets)
    if("FiveWickets" %in% vars)
      x$FiveWickets <- as.integer(x$FiveWickets)
    if("TenWickets" %in% vars)
      x$TenWickets <- as.integer(x$TenWickets)
  }

  # Extract country information if it is present
  # This should only be required when multiple countries are included
  country <- (length(grep("\\(", x[1,1])) > 0)
  if(country)
  {
    x$Country <- stringr::str_extract(x$Player, "\\([a-zA-Z \\-extends]+\\)")
    x$Country <- stringr::str_replace_all(x$Country, "\\(|\\)|-W", "")
    x$Country <- rename_countries(x$Country)
    x$Player <- stringr::str_replace(x$Player, "\\([a-zA-Z  \\-]+\\)", "")
  }

  # Re-order and select columns
  vars <- colnames(x)
  if(career)
    varorder <- c("Player","Country","Start","End","Matches","Innings","Overs","Balls","Maidens","Runs","Wickets",
      "Average","Economy","StrikeRate","BestBowlingInnings","FourWickets","FiveWickets","TenWickets")
  else
    varorder <- c("Date","Player", "Country", "Overs","Balls","Maidens","Runs","Wickets",
      "Economy","Innings","Participation", "Opposition","Ground")
  varorder <- varorder[varorder %in% vars]

  # # Re-order by date and player
  # if("Date" %in% vars)
  # {
  #   if(country)
  #     roworder <- order(x$Date, x$Country, x$Player)
  #   else
  #     roworder <- order(x$Date, x$Player)
  # }
  # else if("Start" %in% vars)
  # {
  #   if(country)
  #     roworder <- order(x$Start, x$End, x$Country, x$Player)
  #   else
  #     roworder <- order(x$Start, x$End, x$Player)
  # }
  # else
  # {
  #   if(country)
  #     roworder <- order(x$Country, x$Player)
  #   else
  #     roworder <- order(x$Player)
  # }

  return(x[,varorder])

}
