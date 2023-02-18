# For retrieving ball-by-ball data ---------------------------------------------

#' Fetch ball-by-ball, match and player data from Cricsheet and return a tibble.
#'
#' Download csv data from Cricsheet \url{https://cricsheet.org/downloads/}.
#' Data must be specified by three factors:
#' (a) type of data: `bbb` (ball-by-ball), `match` or `player`.
#' (b) gender;
#' (c) competition specified as a Cricsheet code. See \code{\link{cricsheet_codes}} for the
#' competitions and codes available.
#'
#' @param type Character string giving type of data: ball-by-ball, match info or player info.
#' @param gender Character string giving player gender: female or male.
#' @param competition Character string giving code corresponding to competition. See \code{\link{cricsheet_codes}} for the
#' competitions and codes available.
#' @author Jacquie Tran, Hassan Rafique and Rob J Hyndman
#' @return A \code{tibble} object, similar to a \code{data.frame}.
#' @examples
#' \dontrun{
#' wbbl_bbb <- fetch_cricsheet(competition = "wbbl", type = "bbb")
#' wbbl_match <- fetch_cricsheet(competition = "wbbl", type = "match")
#' wbbl_player <- fetch_cricsheet(competition = "wbbl", type = "player")
#' }
#' @export

fetch_cricsheet <- function(
    type = c("bbb", "match", "player"),
    gender = c("female", "male"),
    competition = "tests") {
  # Match arguments
  type <- match.arg(type)
  gender <- match.arg(gender)

  # Convert code for backwards compatibility
  competition <- dplyr::recode(competition,
    county = "cch",
    edwards_cup = "cec",
    heyhoe_flint_trophy = "rhf",
    multi_day = "mdms",
    sheffield_shield = "ssh",
    super_smash = "ssm",
    the_hundred = "hnd",
    t20_blast = "ntb",
    t20is = "t20s",
    t20is_unofficial = "it20s",
    wbbl = "wbb",
    wt20c = "wtc"
  )
  # Construct file names and url
  destfile <- paste0(competition, "_", gender, "_csv2.zip")
  url <- paste0("https://cricsheet.org/downloads/", destfile)
  subdir <- paste0(sub("_csv2.zip", "", destfile), "_bbb")
  destfile <- file.path(tempdir(), destfile)

  # Download zip file from Cricsheet.org if it hasn't already been downloaded
  if (!file.exists(destfile)) {
    download.file(url, destfile)
  }

  # List all files in zip
  check_files <- unzip(destfile, exdir = tempdir(), list = TRUE)$Name
  check_files <- data.frame(check_files = check_files)
  check_files$check_files <- as.character(check_files$check_files)
  check_files$file_type <- dplyr::case_when(
    stringr::str_detect(check_files$check_files, "txt") ~ "txt",
    stringr::str_detect(check_files$check_files, "_info") ~ "info",
    stringr::str_detect(check_files$check_files, "all_matches") ~ "allbbb",
    TRUE ~ "bbb"
  )

  # Identify the required files
  if (type == "bbb") {
    if ("all_matches.csv" %in% check_files$check_files) {
      match_files <- "all_matches.csv"
    } else {
      match_files <- check_files$check_files[check_files$file_type == "bbb"]
    }
  } else {
    match_files <- check_files$check_files[check_files$file_type == "info"]
  }
  # Unzip files into sub directory
  unzip(destfile, match_files, exdir = file.path(tempdir(), subdir))

  # List match files with full file paths
  match_filepaths <- file.path(tempdir(), subdir, match_files)

  if (type == "bbb") {
    # Read data from CSVs stored in the temp directory
    all_matches <- do.call(
      "rbind",
      lapply(match_filepaths, FUN = function(files) {
        read.csv(files)
      })
    )
  } else {
    all_matches <- suppressWarnings(
      readr::read_csv(
        match_filepaths,
        id = "path", guess_max = 100,
        col_names = c("col_to_delete", "key", "value"),
        skip = 1, show_col_types = FALSE,
        col_types = readr::cols(.default = readr::col_character())
      )
    )
    # Note: Warning suppressed because the source data
    # changes format slightly when displaying player metadata compared to match data
    # Match metadata is in key-value pairs,
    # while player metadata contains additional value columns
    # We can safely suppress the warning(s) here and deal with the different
    # formats below.

    # Tidy up and subset to match metadata only
    # (i.e., excluding player / people metadata)
    # Note: Warning suppressed again as per note above.
    all_matches$col_to_delete <- NULL
    all_matches$match_id <- stringr::str_extract(all_matches$path, "[a-zA-Z0-9_\\-\\.]*$")
    all_matches$match_id <- sub("_info.csv", "", all_matches$match_id)
    all_matches$path <- NULL
    if (type == "match") {
      all_matches <- all_matches[!(all_matches$key %in% c("player", "players", "registry")), ]
      # Find columns with multiple values per key/match_id
      # Rows with second teams named
      j <- which(all_matches$key == "team")
      j <- j[seq(2, length(j), by = 2)]
      all_matches$key[j] <- "team2"
      all_matches$key[all_matches$key == "team"] <- "team1"
      # Rows with second umpires named
      j <- which(all_matches$key == "umpire")
      j <- j[seq(2, length(j), by = 2)]
      all_matches$key[j] <- "umpire2"
      all_matches$key[all_matches$key == "umpire"] <- "umpire1"
      # Make into wide form
      all_matches <- tidyr::pivot_wider(all_matches,
        id_cols = "match_id",
        names_from = "key",
        values_from = "value",
        values_fill = NA,
        values_fn = ~ head(.x, 1) # To remove duplicated values such as date
      )
      all_matches <- dplyr::mutate_all(all_matches, ~ replace(., . == "NULL", NA_character_))
    } else {
      all_matches <- all_matches[all_matches$key %in% c("player", "players"), ]
      all_matches$key <- NULL
      all_matches <- tidyr::separate(all_matches, value, sep = ",", c("team", "player"))
    }
  }
  output <- tibble::as_tibble(all_matches)

  # Clean data
  # Was it a T20 match?
  if (!("ball" %in% colnames(output))) {
    t20 <- FALSE
  } else {
    t20 <- max(output$ball, na.rm = TRUE) <= 21
  }
  if (type == "bbb" & t20) {
    output <- cleaning_bbb_t20_cricsheet(output)
  }

  return(output)
}

# Function to clean raw t20 bbb data from cricsheet
# Provided by
cleaning_bbb_t20_cricsheet <- function(df) {
  df <- df |>
    dplyr::mutate(
      # Wicket lost
      wicket = !(wicket_type %in% c("", "retired hurt")),
      # Over number
      over = ceiling(ball),
      # Extra ball to follow
      extra_ball = (!is.na(wides) | !is.na(noballs))
    ) |>
    dplyr::group_by(match_id, innings, over) |>
    # Adjusting the ball values by introducing raw_balls, so that 1.1 and 1.10
    # are correctly differentiated as the first & tenth ball, respectively
    dplyr::mutate(ball = dplyr::row_number()) |>
    dplyr::ungroup()

  # Evaluating and joining runs scored, wickets lost at each stage of an innings
  df <- df |>
    dplyr::inner_join(
      df |>
        dplyr::group_by(match_id, innings) |>
        dplyr::reframe(
          runs_scored_yet = cumsum(runs_off_bat + extras),
          wickets_lost_yet = cumsum(wicket),
          ball = ball, over = over,
          .groups = "drop"
        ),
      by = c("match_id", "innings", "over", "ball")
    )

  # Evaluating the balls in over after adjusting for extra balls and balls remaining in an innings
  remaining_balls <- df |>
    dplyr::group_by(match_id, innings, over) |>
    dplyr::reframe(ball = ball, extra_ball = cumsum(extra_ball)) |>
    dplyr::mutate(
      ball_in_over = ball - extra_ball,
      balls_remaining = ifelse(innings %in% c(1, 2), 120 - ((over - 1) * 6 + ball_in_over), 6 - ball_in_over)
    ) |>
    dplyr::select(-extra_ball)

  # Evaluating innings totals using ball-by-ball data
  innings_total <- df |>
    dplyr::group_by(match_id, innings) |>
    dplyr::reframe(total_score = sum(runs_off_bat + extras)) |>
    tidyr::pivot_wider(
      names_from  = "innings",
      values_from = c("total_score")
    ) |>
    dplyr::rename(innings1_total = "1", innings2_total = "2") |>
    dplyr::select(match_id, innings1_total, innings2_total)

  # Joining all the dfs
  df <- df |>
    dplyr::inner_join(remaining_balls, by = c("match_id", "innings", "over", "ball")) |>
    dplyr::inner_join(innings_total, by = "match_id") |>
    dplyr::mutate(target = innings1_total + 1) |> 
    dplyr::mutate(start_date = as.Date(start_date))

  # Re-ordering the columns in the df
  df <- df |>
    dplyr::select(
      match_id, season, start_date, venue, innings, over, ball, batting_team,
      bowling_team, striker, non_striker, bowler, runs_off_bat, extras,
      ball_in_over, extra_ball, balls_remaining, runs_scored_yet,
      wicket, wickets_lost_yet, innings1_total, innings2_total, target,
      wides, noballs, byes, legbyes, penalty, wicket_type, player_dismissed,
      other_wicket_type, other_player_dismissed, dplyr::everything()
    )

  return(df)
}

utils::globalVariables(c(
  "ball_in_over",
  "ball",
  "balls_remaining",
  "batting_team",
  "bowler",
  "bowling_team",
  "byes",
  "col_to_delete",
  "competition_type",
  "exclude_flag",
  "extra_ball",
  "extras",
  "glued_url",
  "innings",
  "innings1_total",
  "innings2_total",
  "key",
  "legbyes",
  "match_id",
  "noballs",
  "non_striker",
  "other_player_dismissed",
  "other_wicket_type",
  "over",
  "path_start",
  "path",
  "penalty",
  "player_dismissed",
  "runs_off_bat",
  "runs_scored_yet",
  "season",
  "start_date",
  "striker",
  "target",
  "value",
  "venue",
  "wicket_type",
  "wicket",
  "wickets_lost_yet",
  "wides"
))
