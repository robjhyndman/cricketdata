#' Find a player id from cricinfo.com
#'
#' @param searchstring Part of a player name(s) to search for. Can be a character vector.
#' @return A table of matching players, their ids, and teams they played for.
#' @seealso [fetch_player_data()] to download playing statistics for
#' a player, and [fetch_player_meta()] to download meta data on players.
#' @examples
#' \dontrun{
#' (perry <- find_player_id("Perry"))
#' EllysePerry <- fetch_player_data(perry[2, "ID"], "test")
#' }
#' @author Rob J Hyndman
#' @export

find_player_id <- function(searchstring) {
  url <- paste0(
    "http://stats.espncricinfo.com/ci/engine/stats/analysis.html?search=",
    searchstring, ";template=analysis"
  )
  url <- gsub(" ", "%20", url)
  raw <- lapply(url, function(x) try(xml2::read_html(x), silent = TRUE))
  if ("try-error" %in% lapply(raw, class)) {
    stop("This shouldn't happen")
  }
  # Extract table of names
  tab <- lapply(raw, function(x) rvest::html_table(x)[[1]])
  # Make into a table
  tab <- lapply(tab, function(x) {
    x <- tibble::as_tibble(x, .name_repair = "check_unique")
    x <- x[x$X1 != "", ]
  })

  for (i in seq_along(searchstring)) {
    x <- tab[[i]]
    # Check if any players returned
    if (x[1, 1] == "No matching players found") {
      warning(paste("No matching players found for search:", searchstring[i]))
    }
    # Check if return exceeds 100
    if (grepl("Search restricted", x[nrow(x), 1])) {
      warning(paste0(
        "Only the first 100 results returned for search: '", searchstring[i],
        "'. Try a more specific search"
      ))
      tab[[i]] <- x[1:100, ]
    }
  }

  tab <- dplyr::bind_rows(tab)

  # Name columns
  colnames(tab) <- c("Name", "Country", "Played")
  # Now to find the ids
  ids <- lapply(raw, rvest::html_nodes, css = "a")
  ids <- lapply(ids, function(x) as.character(x[grep("/ci/engine/player/", x)]))
  ids <- lapply(ids, function(x) gsub("([a-zA-Z= \\\"/<>]*)", "", x))
  ids <- lapply(ids, function(x) {
    unlist(lapply(strsplit(x, ".", fixed = TRUE), function(x) {
      x[1]
    }))
  })
  # Insert NA for those without matches
  ids <- lapply(ids, unique)
  ids <- lapply(ids, function(x) if (is.null(x)) NA else as.numeric(x))
  tab$ID <- unlist(ids)
  tab$searchstring <- rep(searchstring, times = unlist(lapply(ids, length)))
  return(tab[, c("ID", "Name", "searchstring", "Country", "Played")])
}
