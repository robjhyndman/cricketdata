# Fetch Player Data

Fetch individual player data from all matches played. The function will
scrape the data from ESPNCricinfo and return a tibble with one line per
innings for all games a player has played. To identify a player, use
their Cricinfo player ID. The simplest way to find this is to look up
their Cricinfo Profile page. The number at the end of the URL is the ID.
For example, Meg Lanning's profile page is
http://www.espncricinfo.com/australia/content/player/329336.html, so her
ID is 329336.

## Usage

``` r
fetch_player_data(
  playerid,
  matchtype = c("test", "odi", "t20"),
  activity = c("batting", "bowling", "fielding")
)
```

## Arguments

- playerid:

  The player ID as given in the Cricinfo profile. Integer or character.

- matchtype:

  Which type of cricket matches do you want? Tests, ODIs or T20s? Not
  case-sensitive.

- activity:

  Which type of activities do you want? Batting, Bowling or Fielding?
  Not case-sensitive.

## Value

A tibble containing data on the selected player, with one row for every
innings of every match in which they have played.

## See also

[`find_player_id()`](http://pkg.robjhyndman.com/cricketdata/reference/find_player_id.md)
to find a player ID by searching on their name, and
[`fetch_player_meta()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_meta.md)
to download meta data for players.

## Author

Rob J Hyndman and Sayani Gupta

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
