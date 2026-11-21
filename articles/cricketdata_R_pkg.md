# cricketdata: An Open Source R package

Abstract

Open and accessible data streams are crucial for reproducible research
and further development. Cricket data sources are limited and are
usually not in a format ready for analysis. [`cricketdata`
R](http://pkg.robjhyndman.com/cricketdata/) package allows the users to
download the data as a tibble ready for analysis from two primary
sources: ESPNCricinfo and Cricsheet.
[fetch_cricinfo()](http://pkg.robjhyndman.com/cricketdata/reference/fetch_cricinfo.md)
and
[fetch_player_data()](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_data.md)
functions allow the user to download the data from ESPNCricinfo for
different formats of international cricket (tests, odis, T20), player
position (batter, bowler, fielding), and whole career or innings wise.
Cricsheet is another data source, primarily for ball-by-ball data.
[fetch_cricsheet()](http://pkg.robjhyndman.com/cricketdata/reference/fetch_cricsheet.md)
function downloads the ball-by-ball, match, and player data for
different competitions/formats (tests, odis, T20 internationals, T20
leagues). The T20 data is further processed by adding more features
(columns) using the raw data. Some other
[functions](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_meta.md)
provide access to the individual players’ playing career data and
information about their playing style, country of origin, etc. The
package essentially provides (almost) all publicly available cricket
data ready for analysis. The package saves the user significant time in
building the data pipeline, which may now be used for analysis. Here’s
an example of project built using `cricketdata`:
<https://dazzalytics.shinyapps.io/cricwar/>

``` r

library(cricketdata)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

## Introduction

The coverage of cricket as a sport has been limited compared to other
global sports. [ESPN Cricinfo](https://www.espncricinfo.com) is the
major and one of the few online platforms dedicated to cricket coverage.
It started as
[Cricinfo](https://en.wikipedia.org/wiki/ESPNcricinfo#/media/File:Cricinfo_in_1995.jpg)
in the late 90s, and it was maintained by students and cricket fans who
had immigrated to North America but were eager to keep tabs on the
cricket activity around the globe. [ESPN acquired
Cricinfo](https://www.espncricinfo.com/story/espn-acquires-cricinfo-297655)
in 2007, becoming ESPN Cricinfo. It is the most extensive repository of
open cricket data with the caveat that data is not in an accessible
format to be downloaded easily. You would have to copy-paste (tables) or
write programming scripts to access the data in a format suitable for
analysis. Recently they have added a search tool,
[Statsguru](https://stats.espncricinfo.com/ci/engine/stats/index.html),
that lets you parse through their database, presenting results usually
in a table format.

[Cricsheet](https://cricsheet.org/) is another open data source for
ball-by-ball data maintained by a great fan of the game, [Stephen
Rushe](https://twitter.com/srushe). The cricsheet provides raw
ball-by-ball data for all formats (tests, odis, T20) and both Men’s and
Women’s games. It is an extensive project to produce ball-by-ball data,
and we hugely appreciate Stephen Rushe’s work. The data is available in
different formats, such as JSON, YAML, and CSV.

### Why `cricketdata`

The `cricketdata` (open-source) package aims to be a one-stop shop for
most cricket data from all primary sources, available in an accessible
form and ready for analysis. Different functions in the package allow us
to download the data from Cricinfo and cricsheet as a data frame
(tibble) in R. The user can access data from different formats of the
game, e,g, tests, odis, international T20, league T20, etc. In
particular, the

- ball-by-ball data,
- individual player play by innings data,
- player play by team wrt career or innings data,
- player id, dob, batting/bowling hand, bowling type.

[cricWAR](https://dazzalytics.shinyapps.io/cricwar/)
<https://dazzalytics.shinyapps.io/cricwar/> is an example of sports
analytic project based on `cricketdata` resources.

`cricketdata` as an open-source project is inspired primarily from the
open-source work done by `Rstats` community and sports analytics
projects such as [`nflfastR`](https://www.nflfastr.com/) (Carl and
Baldwin, n.d.), [`sportsdataverse`](https://sportsdataverse.org/)
(Gilani, n.d.).

In the following sections, we will show how to install the package and
take full advantage of the package functionality with numerous examples.

## Installation

`cricketdata` is available on CRAN and the *stable* version can be
installed.

``` r

install.packages("cricketdata", dependencies = TRUE)
```

You may also download the *development* version from
[Github](https://github.com/robjhyndman/cricketdata)

``` r

install.packages("devtools")
devtools::install_github("robjhyndman/cricketdata")
```

## Functions

There are six main functions,

- [`fetch_cricinfo()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_cricinfo.md)
- [`find_player_id()`](http://pkg.robjhyndman.com/cricketdata/reference/find_player_id.md)
- [`fetch_player_data()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_data.md)
- [`fetch_cricsheet()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_cricsheet.md)
- [`fetch_player_meta()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_player_meta.md)
- [`update_player_meta()`](http://pkg.robjhyndman.com/cricketdata/reference/update_player_meta.md)

and a data file containing the player meta data.

- `player_meta`

We show the use of each function with examples below.

### `fetch_cricinfo()`

Fetch team data on international cricket matches provided by
ESPNCricinfo. It downloads data for international T20, ODI or Test
matches, for men or women, and for batting, bowling or fielding. By
default, it downloads career-level statistics for individual players.

*Arguments*

- matchtype: Character indicating test (default), odi, or t20.
- sex: Character indicating men (default) or women.
- activity: Character indicating batting (default), bowling or fielding.
- type: Character indicating innings-by-innings or career (default)
  data.
- country: Character indicating country. The default is to fetch data
  for all countries.

**Women’s T20 Bowling Data**

``` r

# Fetch all Women's Bowling data for T20 format
wt20 <- fetch_cricinfo("T20", "Women", "Bowling")
```

``` r

# Looking at data
wt20 |>
  glimpse()
#> Rows: 2,753
#> Columns: 16
#> $ Player             <chr> "M Schutt", "Nida Dar", "DB Sharma", "S Ecclestone"…
#> $ Country            <chr> "Australia", "Pakistan", "India", "England", "THA",…
#> $ Start              <int> 2013, 2010, 2016, 2016, 2018, 2008, 2008, 2007, 200…
#> $ End                <int> 2025, 2024, 2024, 2025, 2025, 2025, 2021, 2023, 202…
#> $ Matches            <int> 122, 160, 124, 96, 99, 167, 117, 113, 145, 98, 112,…
#> $ Innings            <int> 121, 152, 121, 95, 98, 137, 113, 112, 122, 93, 111,…
#> $ Overs              <dbl> 413.1, 510.2, 436.5, 355.2, 331.4, 407.5, 395.3, 39…
#> $ Maidens            <int> 10, 13, 12, 10, 23, 8, 6, 21, 6, 21, 17, 30, 4, 13,…
#> $ Runs               <int> 2632, 2910, 2642, 2089, 1363, 2385, 2206, 2291, 227…
#> $ Wickets            <int> 149, 144, 138, 137, 126, 126, 125, 123, 118, 116, 1…
#> $ Average            <dbl> 17.664430, 20.208333, 19.144928, 15.248175, 10.8174…
#> $ Economy            <dbl> 6.370311, 5.702155, 6.048073, 5.878987, 4.109548, 5…
#> $ StrikeRate         <dbl> 16.63758, 21.26389, 18.99275, 15.56204, 15.79365, 1…
#> $ BestBowlingInnings <chr> "5/15", "5/21", "4/10", "4/18", "5/5", "4/12", "5/1…
#> $ FourWickets        <int> 4, 1, 1, 2, 5, 4, 4, 0, 1, 6, 1, 2, 4, 1, 3, 2, 3, …
#> $ FiveWickets        <int> 1, 1, 0, 0, 1, 0, 3, 2, 0, 2, 0, 0, 0, 2, 3, 1, 0, …

# Table showing certain features of the data
wt20 |>
  select(Player, Country, Matches, Runs, Wickets, Economy, StrikeRate) |>
  head() |>
  knitr::kable(
    digits = 2, align = "c",
    caption = "Women Player career profile for international T20"
  )
```

|    Player    |  Country  | Matches | Runs | Wickets | Economy | StrikeRate |
|:------------:|:---------:|:-------:|:----:|:-------:|:-------:|:----------:|
|   M Schutt   | Australia |   122   | 2632 |   149   |  6.37   |   16.64    |
|   Nida Dar   | Pakistan  |   160   | 2910 |   144   |  5.70   |   21.26    |
|  DB Sharma   |   India   |   124   | 2642 |   138   |  6.05   |   18.99    |
| S Ecclestone |  England  |   96    | 2089 |   137   |  5.88   |   15.56    |
| N Boochatham |    THA    |   99    | 1363 |   126   |  4.11   |   15.79    |
|   EA Perry   | Australia |   167   | 2385 |   126   |  5.85   |   19.42    |

Women Player career profile for international T20 {.table}

``` r

# Plotting Data
wt20 |>
  filter(Wickets >= 50) |>
  ggplot(aes(y = StrikeRate, x = Average)) +
  geom_point(alpha = 0.3, col = "blue") +
  ggtitle("Women International T20 Bowlers") +
  ylab("Balls bowled per wicket") +
  xlab("Runs conceded per wicket")
```

![Strike Rate (balls bowled per wicket) Vs Average (runs conceded per
wicket) for Women international T20 bowlers. Each observation represents
one player, who has taken at least 50 international
wickets.](cricketdata_R_pkg_files/figure-html/fig-wt20SRvER-1.png)

Strike Rate (balls bowled per wicket) Vs Average (runs conceded per
wicket) for Women international T20 bowlers. Each observation represents
one player, who has taken at least 50 international wickets.

**USA men’s ODI data by innings**

``` r

# Fetch all USA Men's ODI data by innings
menODI <- fetch_cricinfo("ODI", "Men", "Batting",
  type = "innings",
  country = "United States of America"
)
```

``` r

# Table of USA player who have scored a century
menODI |>
  filter(Runs >= 100) |>
  select(Player, Runs, BallsFaced, Fours, Sixes, Opposition) |>
  knitr::kable(digits = 2)
```

| Player          | Runs | BallsFaced | Fours | Sixes | Opposition           |
|:----------------|-----:|-----------:|------:|------:|:---------------------|
| JS Malhotra     |  173 |        124 |     4 |    16 | Papau New Guinea     |
| Milind Kumar    |  155 |        110 |    16 |     5 | United Arab Emirates |
| MD Patel        |  130 |        101 |    11 |     6 | Oman                 |
| Aaron Jones     |  123 |         87 |     9 |     6 | Scotland             |
| MD Patel        |  121 |         95 |     7 |     5 | Canada               |
| SR Mukkamalla   |  120 |        114 |    11 |     0 | United Arab Emirates |
| SR Taylor       |  114 |        123 |    11 |     3 | Nepal                |
| SJ Modani       |  111 |        133 |     9 |     0 | Oman                 |
| SR Mukkamalla   |  107 |         99 |    15 |     1 | United Arab Emirates |
| Shayan Jahangir |  104 |         97 |    12 |     4 | Nepal                |
| G Singh         |  101 |        109 |     8 |     2 | West Indies          |
| Shayan Jahangir |  100 |         79 |    10 |     3 | Nepal                |
| MD Patel        |  100 |        114 |     9 |     1 | Nepal                |

### `fetch_player_id`

Each player has a player id on ESPNCricinfo, which is useful to access a
individual player’s data. This function given a string of players name
or part of the name would return the name of corresponding player(s),
their cricinfo id(s), and some other information.

*Argument*

- searchstring: string of a player’s name or part of the name

``` r

# Fetching a player, Meg Lanning's, ID
meg_lanning_id <- find_player_id("Meg Lanning")$ID
```

``` r

meg_lanning_id
#> [1] 329336
```

### `fetch_player_data`

Fetch individual player data from all matches played. The function will
scrape the data from ESPNCricinfo and return a tibble with one line per
innings for all games a player has played. To identify a player, use
their Cricinfo player ID. The simplest way to find this is to look up
their Cricinfo Profile page. The number at the end of the URL is the ID.
For example, Meg Lanning’s profile page is
<https://www.espncricinfo.com/cricketers/meg-lanning-329336>, so her ID
is 329336. Or you may use the `find_player_id` function.

*Argument*

- playerid
- matchtype: Character indicating test (default), odi, or t20.
- activity: Character indicating batting (default), bowling or fielding.

``` r

# Fetching the player Meg Lanning's playing data
MegLanning <- fetch_player_data(meg_lanning_id, "ODI") |>
  mutate(NotOut = (Dismissal == "not out"))
```

``` r

dim(MegLanning)
#> [1] 103  14
names(MegLanning)
#>  [1] "Date"       "Innings"    "Opposition" "Ground"     "Runs"      
#>  [6] "Mins"       "BF"         "X4s"        "X6s"        "SR"        
#> [11] "Pos"        "Dismissal"  "Inns"       "NotOut"

# Compute batting average
MLave <- MegLanning |>
  filter(!is.na(Runs)) |>
  summarise(Average = sum(Runs) / (n() - sum(NotOut))) |>
  pull(Average)
names(MLave) <- paste("Average =", round(MLave, 2))

# Plot ODI scores
ggplot(MegLanning) +
  geom_hline(aes(yintercept = MLave), col = "gray") +
  geom_point(aes(x = Date, y = Runs, col = NotOut)) +
  ggtitle("Meg Lanning ODI Scores") +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = MLave))
```

![Meg Lanning, Australian captain, has shown amazing consistency over
her career, with centuries scored in every year of her career except for
2021, when her highest score from 6 matches was
53.](cricketdata_R_pkg_files/figure-html/fig-meglanning-1.png)

Meg Lanning, Australian captain, has shown amazing consistency over her
career, with centuries scored in every year of her career except for
2021, when her highest score from 6 matches was 53.

### `fetch_cricsheet()`

[Cricsheet](https://cricsheet.org/) is the only open accessible source
for cricket ball-by-ball data.
[`fetch_cricsheet()`](http://pkg.robjhyndman.com/cricketdata/reference/fetch_cricsheet.md)
download csv data from cricsheet. Data must be specified by three
factors: (a) type of data: bbb (ball-by-ball), match or player. (b)
gender; (c) competition. See <https://cricsheet.org/downloads/> for what
the competition character codes mean.

The raw T20 data from cricsheet is further processed to add more columns
(features) to facilitate analysis.

*Arguments*

- type: Character string giving type of data: ball-by-ball, match info
  or player info.

- gender: Character string giving player gender: female or male.

- competition: Character string giving name of competition. e.g. ipl for
  Indiana Premier League, psl for Pakistan Super League, tests for
  international test matches, etc.

**Indian Premier League (IPL) Ball-by-Ball Data**

``` r

# Fetch all IPL ball-by-ball data
ipl_bbb <- fetch_cricsheet("bbb", "male", "ipl")
```

``` r

ipl_bbb |>
  glimpse()
#> Rows: 260,920
#> Columns: 33
#> $ match_id               <int> 335982, 335982, 335982, 335982, 335982, 335982,…
#> $ season                 <chr> "2007/08", "2007/08", "2007/08", "2007/08", "20…
#> $ start_date             <date> 2008-04-18, 2008-04-18, 2008-04-18, 2008-04-18…
#> $ venue                  <chr> "M Chinnaswamy Stadium", "M Chinnaswamy Stadium…
#> $ innings                <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ over                   <dbl> 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3,…
#> $ ball                   <int> 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 1, 2, 3,…
#> $ batting_team           <chr> "Kolkata Knight Riders", "Kolkata Knight Riders…
#> $ bowling_team           <chr> "Royal Challengers Bangalore", "Royal Challenge…
#> $ striker                <chr> "SC Ganguly", "BB McCullum", "BB McCullum", "BB…
#> $ non_striker            <chr> "BB McCullum", "SC Ganguly", "SC Ganguly", "SC …
#> $ bowler                 <chr> "P Kumar", "P Kumar", "P Kumar", "P Kumar", "P …
#> $ runs_off_bat           <int> 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 6, 4, 0, 0, 0, 0,…
#> $ extras                 <int> 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,…
#> $ ball_in_over           <int> 1, 2, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 1, 2, 3,…
#> $ extra_ball             <lgl> FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,…
#> $ balls_remaining        <dbl> 119, 118, 118, 117, 116, 115, 114, 113, 112, 11…
#> $ runs_scored_yet        <int> 1, 1, 2, 2, 2, 2, 3, 3, 7, 11, 17, 21, 21, 21, …
#> $ wicket                 <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE…
#> $ wickets_lost_yet       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ innings1_total         <int> 222, 222, 222, 222, 222, 222, 222, 222, 222, 22…
#> $ innings2_total         <int> 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82,…
#> $ target                 <dbl> 223, 223, 223, 223, 223, 223, 223, 223, 223, 22…
#> $ wides                  <int> NA, NA, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ noballs                <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ byes                   <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ legbyes                <int> 1, NA, NA, NA, NA, NA, 1, NA, NA, NA, NA, NA, N…
#> $ penalty                <int> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ wicket_type            <chr> "", "", "", "", "", "", "", "", "", "", "", "",…
#> $ player_dismissed       <chr> "", "", "", "", "", "", "", "", "", "", "", "",…
#> $ other_wicket_type      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ other_player_dismissed <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
#> $ .groups                <chr> "drop", "drop", "drop", "drop", "drop", "drop",…
```

``` r

# Top 20 batters wrt Boundary and Dot % in IPL 2022 season
ipl_bbb |>
  filter(season == "2022") |>
  group_by(striker) |>
  summarize(
    Runs = sum(runs_off_bat), BallsFaced = n() - sum(!is.na(wides)),
    StrikeRate = Runs / BallsFaced, DotPercent = sum(runs_off_bat == 0) * 100 / BallsFaced,
    BoundaryPercent = sum(runs_off_bat %in% c(4, 6)) * 100 / BallsFaced
  ) |>
  arrange(desc(Runs)) |>
  rename(Batter = striker) |>
  slice(1:20) |>
  ggplot(aes(y = BoundaryPercent, x = DotPercent, size = BallsFaced)) +
  geom_point(color = "red", alpha = 0.3) +
  geom_text(aes(label = Batter),
    vjust = -0.5, hjust = 0.5, color = "#013369",
    position = position_dodge(0.9), size = 3
  ) +
  ylab("Boundary Percent") +
  xlab("Dot Percent") +
  ggtitle("IPL 2022: Top 20 Batters")
```

![Top 20 prolific batters in IPL 2022. We show what percentage of balls
they hit for a boundary (4 or 6) against percentage of how many balls
they do not score off of (dot percent). Ideally we want to be in top
left quadrant, high boundary % and low dot
%.](cricketdata_R_pkg_files/figure-html/fig-iplbatter-1.png)

Top 20 prolific batters in IPL 2022. We show what percentage of balls
they hit for a boundary (4 or 6) against percentage of how many balls
they do not score off of (dot percent). Ideally we want to be in top
left quadrant, high boundary % and low dot %.

``` r

# Top 10 prolific batters in IPL 2022 season.
ipl_bbb |>
  filter(season == "2022") |>
  group_by(striker) |>
  summarize(
    Runs = sum(runs_off_bat), BallsFaced = n() - sum(!is.na(wides)),
    StrikeRate = Runs / BallsFaced,
    DotPercent = sum(runs_off_bat == 0) * 100 / BallsFaced,
    BoundaryPercent = sum(runs_off_bat %in% c(4, 6)) * 100 / BallsFaced
  ) |>
  arrange(desc(Runs)) |>
  rename(Batter = striker) |>
  slice(1:10) |>
  knitr::kable(digits = 1, align = "c")
```

|    Batter    | Runs | BallsFaced | StrikeRate | DotPercent | BoundaryPercent |
|:------------:|:----:|:----------:|:----------:|:----------:|:---------------:|
|  JC Buttler  | 863  |    579     |    1.5     |    42.7    |      22.3       |
|   KL Rahul   | 616  |    455     |    1.4     |    36.9    |      16.5       |
|  Q de Kock   | 508  |    341     |    1.5     |    36.4    |      20.5       |
|  HH Pandya   | 487  |    371     |    1.3     |    36.4    |      16.4       |
| Shubman Gill | 483  |    365     |    1.3     |    34.5    |      17.0       |
|  DA Miller   | 481  |    337     |    1.4     |    31.5    |      16.3       |
| F du Plessis | 468  |    367     |    1.3     |    42.2    |      16.9       |
|   S Dhawan   | 460  |    375     |    1.2     |    42.9    |      15.7       |
|  SV Samson   | 458  |    312     |    1.5     |    44.2    |      22.1       |
|   DJ Hooda   | 451  |    330     |    1.4     |    34.5    |      16.4       |

### `player_meta`

It is a data set containing player’s and cricket officials meta data
such as full name, country of representation, data of birth, bowling and
batting hand, bowling style, and playing role. More than 11,000 player’s
and officials data is available. This data was scraped from ESPNCricinfo
website.

``` r

player_meta |>
  filter(!is.na(playing_role)) |>
  select(-cricinfo_id, -unique_name, -name) |>
  head() |>
  knitr::kable(
    digits = 1, align = "c", format = "pipe",
    col.names = c(
      "ID", "FullName", "Country", "DOB", "BirthPlace",
      "BattingStyle", "BowlingStyle", "PlayingRole"
    )
  )
```

| ID | FullName | Country | DOB | BirthPlace | BattingStyle | BowlingStyle | PlayingRole |
|:--:|:--:|:--:|:--:|:--:|:--:|:--:|:--:|
| 9dbc77b3 | Aaftab Alam Khan | Malta | 1986-01-31 | Right hand Bat | Right arm Medium fast | NA | Wicketkeeper Batter |
| 797f52cc | Aahan Gopinath Achar | Singapore | 1999-03-30 | Left hand Bat | Slow Left arm Orthodox | NA | Bowler |
| e249fdaa | Aakash Chopra | India | 1977-09-19 | Right hand Bat | Right arm Medium, Right arm Offbreak | Agra, Uttar Pradesh | Batter |
| 4b0e3049 | Aaliyah Alicia Alleyne | West Indies | 1994-11-11 | Right hand Bat | Right arm Medium | NA | Bowler |
| f1733e13 | Aaliyah Williams | West Indies | 1998-02-28 | Right hand Bat | Right arm Medium | NA | Allrounder |
| a8e54ef4 | Aamer Jamal | Pakistan | 1996-07-05 | Right hand Bat | Right arm Medium | Mianwali | Allrounder |

### `fetch_player_meta()`

Fetch the player’s meta data such as full name, country of
representation, data of birth, bowling and batting hand, bowling style,
and playing role. This meta data is useful for advance modeling, e,g,
age curves, batter profile against bowling types etc.

*Argument*

- playerid: A vector of player IDs as given in Cricinfo profiles.
  Integer or character.

The cricinfo player ids can be accessed in multiple ways, e.g. use
fetch_player_id() function, get the id from the player’s cricinfo page
or consult the `player_meta` data frame which has player meta data of
more than 11,000 players.

``` r

# Download meta data on Meg Lanning and Ellyse Perry
aus_women <- fetch_player_meta(c(329336, 275487))
```

``` r

aus_women |>
  select(-name) |>
  knitr::kable(
    digits = 1, align = "c", format = "pipe",
    col.names = c(
      "ID", "FullName", "Country", "DOB", "BattingStyle", "BowlingStyle"
    )
  )
```

| ID | FullName | Country | DOB | BattingStyle | BowlingStyle |
|:--:|:--:|:--:|:--:|:--:|:--:|
| 329336 | Meghann Moira Lanning | Australia | 1992-03-25 | Right-hand bat | Right-arm medium |
| 275487 | Ellyse Alexandra Perry | Australia | 1990-11-03 | Right-hand bat | Right-arm fast-medium |

### `update_player_meta()`

This function is supposed to consult the directory of all players
available on cricsheet website and include the meta data of new players
into the `player_meta` data frame. The data for new players will be
scraped from the ESPNCricinfo.

## References

Carl, Sebastian, and Ben Baldwin. n.d. “nflfastR: Functions to
Efficiently Access NFL Play by Play Data.” In *R Package*.
<https://CRAN.R-project.org/package=nflfastR>.

Gilani, Saiem. n.d. *Sports Dataverse*. <https://sportsdataverse.org/>.
