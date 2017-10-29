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
#' ElyssePerry <- fetch_player_data(275487, "T20")
#' MegLanning <- fetch_player_data(329336, "ODI")
#' SteveSmith <- fetch_player_data(267192, "Test")
#' 
#' library(ggplot2)
#' ggplot(MegLanning) + geom_point(aes(x=Date, y=Score, col=NotOut)) +
#'   ggtitle("Meg Lanning ODI Scores")
#' }
#'
#' @export
fetch_player <- function(playerid,
        matchtype=c("test","odi","t20"))
{
  matchtype <- tolower(matchtype)
  matchtype <- match.arg(matchtype)

  # First figure out if player is female or male
  profile <- paste("http://www.espncricinfo.com/australia/content/player/",
                   playerid, ".html", sep="")
  raw <- try(xml2::read_html(profile), silent=TRUE)
  if("try-error" %in% class(raw))
    stop("Player not found")
  female <- length(grep("format=women", as.character(raw))) > 0

  matchclass <- match(matchtype, c("test","odi","t20")) + (female * 7)
  url <- paste("http://stats.espncricinfo.com/ci/engine/player/",
               playerid,
               ".html?class=",
               matchclass,
              ";template=results;type=allround;view=innings;wrappertype=print",
               sep="")
  raw <- try(xml2::read_html(url), silent=TRUE)
  if("try-error" %in% class(raw))
    stop("Problem with URL")

  # Grab relevant table
  tab <- rvest::html_table(raw)[[4]]

  # Remove redundant missings columns
  tab <- tibble::as_tibble(tab[, colSums(is.na(tab)) != NROW(tab)])
  
  # Convert "-" to NA
  tab[tab=="-"] <- NA

  # Rename columns
  colnames(tab)[10] <- "Date"

  # Add not out column
  notout <- seq(NROW(tab)) %in% grep("*",tab$Score)
  dnbat <- grep("DNB",tab$Score)
  tab$Score <- gsub("*","",tab$Score, fixed=TRUE)
  tab$Score[dnbat] <- NA
  dnbowl <- grep("DNB", tab$Overs)
  tab$Overs[dnbowl] <- NA

  # Convert some columns to numeric or Date
  tab$Score <- as.numeric(tab$Score)
  tab$NotOut <- seq(NROW(tab)) %in% notout
  tab$DidNotBat <- seq(NROW(tab)) %in% dnbat
  tab$DidNotBowl <- seq(NROW(tab)) %in% dnbowl
  tab$Overs <- as.numeric(tab$Overs)
  tab$Conc <- as.integer(tab$Conc)
  tab$Wkts <- as.integer(tab$Wkts)
  tab$Ct <- as.integer(tab$Ct)
  tab$St <- as.integer(tab$St)
  tab$Innings <- as.integer(tab$Inns)
  tab$Date <- lubridate::dmy(tab$Date)

  # Reorder columns
  return(
  tab[,c("Date","Opposition","Ground","Innings",
         "Score","NotOut","DidNotBat","Overs","Conc","Wkts","DidNotBowl","Ct","St")]
  )
}

