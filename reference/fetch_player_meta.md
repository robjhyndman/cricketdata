# Fetch Player Meta Data

Fetch player meta data from ESPNCricinfo and return a tibble with one
line per player. To identify the players, use their Cricinfo player IDs.
The simplest way to find this is to look up their Cricinfo Profile page.
The number at the end of the URL is the ID. For example, Meg Lanning's
profile page is
https://www.espncricinfo.com/cricketers/meg-lanning-329336, so her ID is
329336.

## Usage

``` r
fetch_player_meta(playerid)
```

## Arguments

- playerid:

  A vector of player IDs as given in Cricinfo profiles. Integer or
  character.

## Value

A tibble containing meta data on the selected players, with one row for
each player.

## See also

It is usually simpler to just use the saved data set
[player_meta](http://pkg.robjhyndman.com/cricketdata/reference/player_meta.md)
which contains the meta data for all players on ESPNCricinfo as at 24
March 2025. To find a player ID, use
[`find_player_id()`](http://pkg.robjhyndman.com/cricketdata/reference/find_player_id.md).
Use
[`fetch_player_data()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_data.md)
to download playing statistics for a player.

## Author

Hassan Rafique and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
# Download meta data on Meg Lanning and Ellyse Perry
aus_women <- fetch_player_meta(c(329336, 275487))
} # }
```
