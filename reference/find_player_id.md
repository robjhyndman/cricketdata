# Find a player id from cricinfo.com

Find a player id from cricinfo.com

## Usage

``` r
find_player_id(searchstring)
```

## Arguments

- searchstring:

  Part of a player name(s) to search for. Can be a character vector.

## Value

A table of matching players, their ids, and teams they played for.

## See also

[`fetch_player_data()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_data.md)
to download playing statistics for a player, and
[`fetch_player_meta()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_meta.md)
to download meta data on players.

## Author

Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
(perry <- find_player_id("Perry"))
EllysePerry <- fetch_player_data(perry[2, "ID"], "test")
} # }
```
