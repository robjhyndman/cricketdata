#' Find a player id from cricinfo.com
#'
#' @param searchstring Part of a player name to search for.
#' @return A table of matching players, their ids, and teams they played for.
#' @examples
#' (perry <- find_player_id("Perry"))
#' ElyssePerry <- fetch_player(perry[2,"ID"], "test")
#' @export

find_player_id <- function(searchstring) {
  url <- paste0("http://stats.espncricinfo.com/ci/engine/stats/analysis.html?search=",
                searchstring,";template=analysis")
  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw))
    stop("Player not found")
  # Extract table of names
  tab <- rvest::html_table(raw)[[1]]
  # Make into a table
  tab <- tibble::as_tibble(tab)
  # Remove empty rows
  tab <- tab[tab$X1 != "",]
  # Name columns
  colnames(tab) <- c("Name","Country","Played")
  # Now to find the ids
  ids <- rvest::html_nodes(raw, 'a')
  ids <- as.character(ids[grep("/ci/engine/player/", ids)])
  ids <- unique(substring(ids,28,32))
  tab$ID <- as.integer(ids)
  dplyr::select(tab, ID, Name, Country, Played)
}


