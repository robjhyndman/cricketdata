#' Update player_meta
#'
#' The [player_meta] data set contains the names and other 
#' attributes of players who appear on both [cricsheet](https://cricsheet.org) 
#' and [ESPNCricinfo](https://www.espncricinfo.com) as at 3 August 2022.
#' This function returns an updated version of the data set based on information
#' currently available online. 
#'
#' @return A tibble containing meta data on cricket players.
#' @author Hassan Rafique and Rob J Hyndman
#' @seealso [player_meta], [fetch_player_meta()].
#' @examples
#' \dontrun{
#' # Update data to current
#' new_player_meta <- update_player_meta()
#'}
#' @export
update_player_meta <- function() {
  store_warning <- options(warn = -1)$warn
  # Read people file from cricsheet
  people <- readr::read_csv("https://cricsheet.org/register/people.csv", 
                      col_types = "ccccccccccccccc", lazy=FALSE) |>
    dplyr::select(cricsheet_id = identifier,
           cricinfo_id = key_cricinfo,
           cricinfo2 = key_cricinfo_2,
           name, unique_name) |>
    # Remove people not on Cricinfo
    dplyr::filter(!is.na(cricinfo_id))

  # Compare existing version of player_meta and find missing players
  missing_df <- people |>
    dplyr::anti_join(player_meta, by = "cricinfo_id") |>
    dplyr::anti_join(player_meta, by = c("cricinfo2" = "cricinfo_id"))

  # Now update it with any missing players
  updates <- fetch_player_meta(missing_df$cricinfo_id) |>
    dplyr::filter(!is.na(full_name))
  new_player_meta <- player_meta |>
    dplyr::bind_rows(updates)

  # For people missing on cricinfo, try the other cricinfo id
  missing_df <- people |>
    dplyr::right_join(new_player_meta |> 
                        dplyr::filter(is.na(full_name)), by='cricinfo_id') |>
    dplyr::filter(!is.na(cricinfo2))
  cricinfo2 <- fetch_player_meta(missing_df$cricinfo2)
  new_player_meta <- new_player_meta |>
    dplyr::filter(!is.na(full_name)) |>
    dplyr::bind_rows(cricinfo2) |>
    dplyr::arrange(full_name)

  # Clean up some fields and return the result
  output <- new_player_meta |>
    dplyr::distinct() |>
    dplyr::mutate(country = stringr::str_remove(country, " Wmn"))
  options(warn = store_warning)
  return(output)
}

utils::globalVariables(c("identifier","key_cricinfo","key_cricinfo_2","name","unique_name","player_meta"))
