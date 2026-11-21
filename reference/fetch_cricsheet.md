# Fetch ball-by-ball, match and player data from Cricsheet and return a tibble.

Download csv data from Cricsheet <https://cricsheet.org/downloads/>.
Data must be specified by three factors: (a) type of data: `bbb`
(ball-by-ball), `match` or `player`. (b) gender; (c) competition
specified as a Cricsheet code. See
[`cricsheet_codes`](http://pkg.robjhyndman.com/cricketdata/reference/cricsheet_codes.md)
for the competitions and codes available.

## Usage

``` r
fetch_cricsheet(
  type = c("bbb", "match", "player"),
  gender = c("female", "male"),
  competition = "tests"
)
```

## Arguments

- type:

  Character string giving type of data: ball-by-ball, match info or
  player info.

- gender:

  Character string giving player gender: female or male.

- competition:

  Character string giving code corresponding to competition. See
  [`cricsheet_codes`](http://pkg.robjhyndman.com/cricketdata/reference/cricsheet_codes.md)
  for the competitions and codes available.

## Value

A `tibble` object, similar to a `data.frame`.

## Author

Jacquie Tran, Hassan Rafique and Rob J Hyndman

## Examples

``` r
if (FALSE) { # \dontrun{
wbbl_bbb <- fetch_cricsheet(competition = "wbbl", type = "bbb")
wbbl_match <- fetch_cricsheet(competition = "wbbl", type = "match")
wbbl_player <- fetch_cricsheet(competition = "wbbl", type = "player")
} # }
```
