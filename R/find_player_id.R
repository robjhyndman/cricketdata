#' Find a player id from cricinfo.com
#'
#' @param searchstring Part of a player name to search for.
#' @return A table of matching players, their ids, and teams they played for.
#' @examples
#' (perry <- find_player_id("Perry"))
#' EllysePerry <- fetch_player_data(perry[2,"ID"], "test")
#' @export

find_player_id <- function(searchstring) {
  url <- paste0("http://stats.espncricinfo.com/ci/engine/stats/analysis.html?search=",
                searchstring,";template=analysis")
  url <- gsub(" ", "%20", url)
  raw <- try(xml2::read_html(url), silent = TRUE)
  if ("try-error" %in% class(raw))
    stop("This shouldn't happen")
  # Extract table of names
  tab <- rvest::html_table(raw)[[1]]
  # Make into a table
  tab <- tibble::as_tibble(tab)
  # Check if player exists
  if(unlist(tab[1,1])=="No matching players found")
    stop("No matching players found")
  # Name columns
  colnames(tab) <- c("Name","Country","Played")
  # Remove empty rows
  tab <- tab[tab$Name != "",]
  checkrestrict <- grep("Search restricted", utils::tail(tab,1)[[1]])
  if(length(checkrestrict) == 0L)
    checkrestrict <- FALSE
  if(checkrestrict)
  {
    warning("Only 100 results returned. Please try a more specific search.")
    tab <- utils::head(tab,100)
  }
  # Now to find the ids
  ids <- rvest::html_nodes(raw, 'a')
  ids <- as.character(ids[grep("/ci/engine/player/", ids)])
  ids <- gsub("([a-zA-Z= \\\"/<>]*)","", ids)
  ids <- unlist(lapply(strsplit(ids,".", fixed=TRUE), function(x){x[1]}))
  tab$ID <- as.integer(unique(ids))
  return(tab[,c("ID","Name","Country","Played")])
}


