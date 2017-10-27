#' Did not bat function.
#'
#' Convert bowling/batting batting category to a character variable.
#'
#' @param status A number, DNB, or TDNB.
#'
#'

participation_status <- function(status) {
  # Create new variable.
  # this_data$Participation <- map_chr(this_data$Overs, participation_status)
  #
  # Convert Overs to a numeric with characters converted to NA.
  # this_data <- this_data %>%
  #   mutate(Overs =
  #            map_chr(
  #              Overs,
  #              .f = function(x)
  #                if (x == "DNB" || x == "TDNB")
  #                  NA
  #              else
  #                x
  #            ) %>% as.numeric())

  if (status != "DNB" & status != "TDNB") {
    return("B")
  } else {
    return(status)
    }
}
