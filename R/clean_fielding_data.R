# Function to clean bowling data.
# Works with career or innings data

clean_fielding_data <- function(x)
{
  # Make names easier to interpret
  vars <- colnames(x)
  vars[vars=="Mat"] <- "Matches"
  vars[vars=="Inns"] <- "Innings"
  vars[vars=="Start Date"] <- "Date"
  vars[vars=="Dis"] <- "Dismissals"
  vars[vars=="Ct"] <- "Caught"
  vars[vars=="St"] <- "Stumped"
  vars[vars=="Ct Wk"] <- "CaughtBehind"
  vars[vars=="Ct Fi"] <- "CaughtFielder"
  vars[vars=="MD"] <- "MaxDismissalsInnings"

  colnames(x) <- vars

  # Fix classes for all variables
  x$Innings <- as.integer(x$Innings)
  x$Dismissals <- as.integer(x$Dismissals)
  x$Caught <- as.integer(x$Caught)
  x$Stumped <- as.integer(x$Stumped)
  x$CaughtBehind <- as.integer(x$CaughtBehind)
  x$CaughtFielder <- as.integer(x$CaughtFielder)

  career <- ("Matches" %in% vars)
  if(career)
  {
    x$Matches <- as.integer(x$Matches)
    if("Span" %in% vars)
    {
      x$Start <- as.integer(substr(x$Span, 1, 4))
      x$End <- as.integer(substr(x$Span, 6, 9))
    }
    x$MaxDismissalsInnings <- unlist(lapply(
      strsplit(x$MaxDismissalsInnings,"\\("), function(x){as.numeric(x[1])}
      ))
  }
  else
  {
    x$Date <- lubridate::dmy(x$Date)
    x$Opposition <- stringr::str_replace_all(x$Opposition, "v | Women| Wmn", "")
    x$Opposition <- rename_countries(x$Opposition)
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
    varorder <- c("Player","Country","Start","End","Matches","Innings",
      "Dismissals","Caught","CaughtFielder","CaughtBehind","Stumped","MaxDismissalsInnings")
  else
    varorder <- c("Date","Player", "Country",
      "Dismissals","Caught","CaughtFielder","CaughtBehind","Stumped","Innings","Opposition","Ground")
  varorder <- varorder[varorder %in% vars]

  return(x[,varorder])

}
