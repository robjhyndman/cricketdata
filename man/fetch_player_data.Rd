% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_player.R
\name{fetch_player_data}
\alias{fetch_player_data}
\title{Fetch Player Data}
\usage{
fetch_player_data(
  playerid,
  matchtype = c("test", "odi", "t20"),
  activity = c("batting", "bowling", "fielding")
)
}
\arguments{
\item{playerid}{The player ID as given in the Cricinfo profile. Integer or character.}

\item{matchtype}{Which type of cricket matches do you want? Tests, ODIs or T20s? Not case-sensitive.}

\item{activity}{Which type of activities do you want? Batting, Bowling or Fielding? Not case-sensitive.}
}
\value{
A tibble containing data on the selected player, with one row for every innings
of every match in which they have played.
}
\description{
Fetch individual player data from all matches played. The function will scrape
the data from ESPNCricinfo and return a tibble with one line per innings for all
games a player has played. To identify a player, use their Cricinfo player ID.
The simplest way to find this is to look up their Cricinfo Profile page. The number
at the end of the URL is the ID. For example, Meg Lanning's profile page is
http://www.espncricinfo.com/australia/content/player/329336.html,
so her ID is 329336.
}
\examples{
\dontrun{
# Download data on some players
EllysePerry <- fetch_player_data(275487, "T20", "batting")
RahulDravid <- fetch_player_data(28114, "ODI", "fielding")
LasithMalinga <- fetch_player_data(49758, "Test", "bowling")

# Create a plot for Ellyse Perry's T20 scores
library(dplyr)
library(ggplot2)
EllysePerry |>
  filter(!is.na(Runs)) |>
  ggplot(aes(x = Start_Date, y = Runs, col = Dismissal, na.rm = TRUE)) +
  geom_point() +
  ggtitle("Ellyse Perry's T20 Scores")
}
}
\seealso{
\code{\link[=find_player_id]{find_player_id()}} to find a player ID by searching on their name,
and \code{\link[=fetch_player_meta]{fetch_player_meta()}} to download meta data for players.
}
\author{
Rob J Hyndman and Sayani Gupta
}
