# Main function to scrape the data from cricinfo
# Not user-visible. Called by fetch_cricinfo.

fetch_cricket_data <- function(matchtype = c("test", "odi", "t20"),
                               sex = c("men", "women"),
                               country = NULL,
                               activity = c("batting", "bowling", "fielding"),
                               view = c("innings", "career")) {
  # Check arguments given by user match the type (class?) of the default
  # arguments of the function.
  matchtype <- tolower(matchtype)
  sex <- tolower(sex)
  matchtype <- match.arg(matchtype)
  sex <- match.arg(sex)
  activity <- match.arg(activity)
  view <- match.arg(view)

  # Set view text.
  view_text <- if (view == "innings") {
    ";view=innings"
  } else {
    NULL
  }

  # Define url signifier for match type.
  matchclass <-
    match(matchtype, c("test", "odi", "t20")) + 7 * (sex == "women")

  # Find country code
  if (!is.null(country)) {
    if (sex == "men") {
      team <- men$team[pmatch(country, tolower(men$name))]
    } else {
      team <- women$team[pmatch(country, tolower(women$name))]
    }
    if (is.na(team)) {
      stop("Country not found")
    }
  }

  # Set starting page to read from.
  page <- 1L
  alldata <- NULL

  # Read each page in turn and bind the rows.
  theend <- FALSE # Initialise.
  while (!theend) {
    # Create url string.
    url <-
      paste0(
        "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=",
        matchclass,
        ifelse(is.null(country), "", paste0(";team=", team)),
        ";page=",
        format(page, scientific = FALSE),
        ";template=results;type=",
        activity,
        view_text,
        ";size=200;wrappertype=print"
      )

    # Get raw page data from page using xml2::read_html() with url string.
    raw <- try(xml2::read_html(url), silent = TRUE)
    if ("try-error" %in% class(raw)) {
      stop("Error in URL")
    }

    # Grab relevant table using rvest::html_table() on the raw page data.
    tables <- rvest::html_table(raw)
    tab <- tables[[3]]
    # Check to see if the dataset extracted is empty
    if (identical(dim(tab), c(1L, 1L))) {
      theend <- TRUE
    }
    if (page == 1L) {
      if (theend) {
        stop("No data available")
      }
      maxpage <- as.numeric(strsplit(dplyr::pull(tables[[2]][1, 1]), "Page 1 of ")[[1]][2])
      pb <- progress::progress_bar$new(total = maxpage)
      pb$tick(0)
      Sys.sleep(1 / 1000)
    }
    if (!theend) {
      # Make allcolumns characters for now.
      tab <- suppressMessages(tibble::as_tibble(apply(tab, 2, as.character), .name_repair = "unique"))

      # Bind the data extracted from this page to all data collected so far.
      alldata <- dplyr::bind_rows(alldata, tab)

      # Update progress bar
      pb$tick()
      Sys.sleep(1 / 1000)

      # Increment page counter.
      page <- page + 1L
    }
  }

  # Remove redundant missings columns.
  alldata <-
    suppressMessages(tibble::as_tibble(alldata[, colSums(is.na(alldata)) != NROW(alldata)], .name_repair = "check_unique"))
  # Convert "-" to NA
  alldata[alldata == "-"] <- NA

  return(alldata)
}
