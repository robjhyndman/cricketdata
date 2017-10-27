#' Get cricket data.
#'
#' Retrieve cricket data from stats cricinfo based on various categories.
#'
#' @param matchtype Character indicating test, odi, or t20.
#' @param sex Character indicating men or women.
#' @param activity Character indicating batting, bowling, or fielding.
#' @param view Character indicating innings or career.
#'
#' @examples
#' wt20_bowling <- fetch_cricket_data("t20", "women", "bowling", "career")
#' @export

fetch_cricket_data <- function(matchtype = c("test", "odi", "t20"),
                               sex = c("men", "women"),
                               activity = c("batting", "bowling", "fielding"),
                               view = c("innings", "career"),
                               team = NULL)
{
  # Check arguments given by user match the type (class?) of the default
  # arguments of the function.
  matchtype <- match.arg(matchtype)
  sex <- match.arg(sex)
  activity <- match.arg(activity)
  view <- match.arg(view)

  # Set view text.
  view_text <- if (view == "innings") {";view=innings"} else {NULL}

  # Define url signifier for match type.
  matchclass <-
    match(matchtype, c("test", "odi", "t20")) + 7 * (sex == "women")

  # Find country code
#  if(!is.null(team))
#    country <- match(team, c(
#      "Australia",
#      ))

  # Set starting page to read from.
  page <- 1L

  # Don't know what this variable does yet.
  alldata <- NULL

  # Read each page in turn and bind the rows.
  theend <- FALSE # Initialise.
  while (!theend)
  {
    # Create url string.
    # http://stats.espncricinfo.com/ci/engine/stats/index.html?class=10;page=2;template=results;type=bowling;view=innings
    # http://stats.espncricinfo.com/ci/engine/stats/index.html?class=10;page=2;template=results;type=bowling
    url <-
      paste(
        "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=",
        matchclass,
        ";page=",
        format(page, scientific = FALSE),
        ";template=results;type=",
        activity,
        view_text,
        ";wrappertype=print",
        sep = ""
      )

    # Get raw page data from page using xml2::read_html() with url string.
    raw <- xml2::read_html(url)

    # Grab relevant table using rvest::html_table() on the raw page data.
    # Produces a list of things or tables.
    tables <- rvest::html_table(raw)
    if(page==1L)
    {
      maxpage <- as.numeric(strsplit(tables[[2]][1,1], "Page 1 of ")[[1]][2])
      pb <- progress::progress_bar$new(total = maxpage)
      pb$tick(0)
      Sys.sleep(1/1000)
    }
    tab <- tables[[3]]

    # Check to see if the dataset extracted from the page data has nothing in
    # it.
    # NB: 1L is an integer; 1 is a numeric.
    # ToDo: See why the table has dimension 1 x 1.
    if (identical(dim(tab), c(1L, 1L)))
      theend <- TRUE

    else
    {
      # Make columns characters for now.
      tab <- tibble::as_tibble(apply(tab, 2, as.character))

      # Bind the data extracted from this page to all data collected so far.
      alldata <- dplyr::bind_rows(alldata, tab)

      # Update progress bar
      pb$tick()
      Sys.sleep(1/1000)

      # Increment page counter.
      page <- page + 1L
    }
  }

  # Remove redundant missings columns.
  alldata <-
    tibble::as_tibble(alldata[, colSums(is.na(alldata)) != NROW(alldata)])
  # Convert "-" to NA
  alldata[alldata == "-"] <- NA

  # ToDo: Return the relevant columns.
  return(alldata)
}

#1 England
#2 Australia
#3 South Africa
#4 West Indies
#5 New Zealand
#6 India
#7 Pakistan
#8 Sri Lanka
#9 Zimbabwe
#25 Bangladesh
#40 Afghanistan

#140 ICC World XI
