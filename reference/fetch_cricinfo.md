# Fetch Data from Cricinfo

Fetch data from ESPNCricinfo and return a tibble. All arguments are
case-insensitive and partially matched.

## Usage

``` r
fetch_cricinfo(
  matchtype = c("test", "odi", "t20"),
  sex = c("men", "women"),
  activity = c("batting", "bowling", "fielding"),
  type = c("career", "innings"),
  country = NULL
)
```

## Arguments

- matchtype:

  Character indicating test (default), odi, or t20.

- sex:

  Character indicating men (default) or women.

- activity:

  Character indicating batting (default), bowling or fielding.

- type:

  Character indicating innings-by-innings or career (default) data

- country:

  Character indicating country. The default is to fetch data for all
  countries.

## Value

A `tibble` object, similar to a `data.frame`.

## Author

Rob J Hyndman, Timothy Hyndman, Charles Gray

## Examples

``` r
if (FALSE) { # \dontrun{
auswt20 <- fetch_cricinfo("T20", "Women", country = "Aust")
IndiaODIBowling <- fetch_cricinfo("ODI", "men", "bowling", country = "india")
} # }
```
