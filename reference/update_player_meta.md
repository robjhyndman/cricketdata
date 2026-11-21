# Update player_meta

The
[player_meta](http://pkg.robjhyndman.com/cricketdata/reference/player_meta.md)
data set contains the names and other attributes of players who appear
on both [cricsheet](https://cricsheet.org) and
[ESPNCricinfo](https://www.espncricinfo.com) as at 24 March 2025. This
function returns an updated version of the data set based on information
currently available online.

## Usage

``` r
update_player_meta(start_again = FALSE)
```

## Arguments

- start_again:

  If TRUE, downloads all data from ESPNCricinfo without using
  player_meta as a starting point. This can take a long time.

## Value

A tibble containing meta data on cricket players.

## See also

[player_meta](http://pkg.robjhyndman.com/cricketdata/reference/player_meta.md),
[`fetch_player_meta()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_meta.md).

## Author

Hassan Rafique and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
# Update data to current
new_player_meta <- update_player_meta()
} # }
```
