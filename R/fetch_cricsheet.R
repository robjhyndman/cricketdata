# For retrieving ball-by-ball data ---------------------------------------------

#' Fetch ball-by-ball, match and player data from Cricsheet and return a tibble.
#'
#' Download csv data from Cricsheet \url{https://cricsheet.org/downloads/}.
#' Data must be specified by three factors:
#' (a) type of data: `bbb` (ball-by-ball), `match` or `player`.
#' (b) gender;
#' (c) competition.
#' See \url{https://cricsheet.org/downloads/} for what the competition character codes mean.
#'
#' @param type Character string giving type of data: ball-by-ball, match info or player info.
#' @param gender Character string giving player gender: female or male.
#' @param competition Character string giving name of competition.
#' @author Jacquie Tran and Rob J Hyndman
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
    competition = c("tests", "multi_day", "odis", "odms",
                    "t20is", "t20is_unofficial",
                    "apl", "bbl", "bpl", "county", "edwards_cup", "cpl",
                    "the_hundred", "ipl", "lpl", "msl", "t20_blast",
                    "psl", "heyhoe_flint_trophy", "sheffield_shield", "super_smash",
                    "wbbl", "wt20c")
  ) {
  # Match arguments
  type <- match.arg(type)
  gender <- match.arg(gender)
  competition <- match.arg(competition)

  # Construct codes for cricsheet files
  formal.args <- formals(sys.function(sysP <- sys.parent()))
  choices <- eval(formal.args[["competition"]], envir = sys.frame(sysP))
  code_table <- data.frame(competition = choices)
  code_table$code <- dplyr::recode(code_table$competition,
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
  code <- code_table[code_table$competition == competition, "code"]
  # Construct file names and url
  destfile <- paste0(code, "_", gender, "_csv2.zip")
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
      all_matches <- do.call("rbind",
        lapply(match_filepaths, FUN = function(files) { read.csv(files) })
      )
  } else {
    all_matches <- suppressWarnings(
      readr::read_csv(
        match_filepaths, id = "path", guess_max = 100,
        col_names = c("col_to_delete", "key", "value"),
        skip = 1, show_col_types = FALSE,
        col_types = readr::cols(.default = readr::col_character()))
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
    all_matches$match_id <- sub(file.path(tempdir(), subdir,""), "", all_matches$path)
    all_matches$match_id <- sub("_info.csv", "", all_matches$match_id)
    all_matches$path <- NULL
    if(type == "match") {
      all_matches <- all_matches[!(all_matches$key %in% c("player", "players", "registry")),]
      # Find columns with multiple values per key/match_id
      # Rows with second teams named
      j <- which(all_matches$key == "team")
      j <- j[seq(2, length(j), by=2)]
      all_matches$key[j] <- "team2"
      all_matches$key[all_matches$key == "team"] <- "team1"
      # Rows with second umpires named
      j <- which(all_matches$key == "umpire")
      j <- j[seq(2, length(j), by=2)]
      all_matches$key[j] <- "umpire2"
      all_matches$key[all_matches$key == "umpire"] <- "umpire1"
      # Make into wide form    
      all_matches <- tidyr::pivot_wider(all_matches, 
         id_cols = "match_id",
         names_from = "key",
         values_from = "value",
          values_fill = NA,
        ) 
      all_matches <- dplyr::mutate_all(all_matches, ~replace(., .=="NULL", NA_character_))
      
    } else {
      all_matches <- all_matches[all_matches$key %in% c("player", "players"),]
      all_matches$key <- NULL
      all_matches <- tidyr::separate(all_matches, value, sep = ",", c("team", "player"))
    }
  }

  return(tibble::as_tibble(all_matches))
}

utils::globalVariables(c("competition_type", "col_to_delete", "exclude_flag", "glued_url", "key", "match_id", "path_start", "path", "value"))
