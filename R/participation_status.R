#' Did not bat function.
#'
#' Convert bowling/batting batting category to a character variable.
#'
#' @param status A number, DNB, or TDNB.
#'

participation_status <- function(status) {
  
  absent <- grep("absent", status)
  dnb <- grep("^DNB", status)
  tdnb <- grep("TDNB", status)
  sub <- grep("sub", status)
  
  status[seq(NROW(status))] <- "B"
  status[absent] <- "Absent"
  status[dnb] <- "DNB"
  status[tdnb] <- "TDNB"
  status[sub] <- "Sub"

  status
}
