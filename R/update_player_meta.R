#' Update player_meta
#'
#' The [player_meta] data set contains the names and other
#' attributes of players who appear on both [cricsheet](https://cricsheet.org)
#' and [ESPNCricinfo](https://www.espncricinfo.com) as at 24 March 2025.
#' This function returns an updated version of the data set based on information
#' currently available online.
#'
#' @param start_again If TRUE, downloads all data from ESPNCricinfo without
#' using player_meta as a starting point. This can take a long time.
#' @return A tibble containing meta data on cricket players.
#' @author Hassan Rafique and Rob J Hyndman
#' @seealso [player_meta], [fetch_player_meta()].
#' @examples
#' \dontrun{
#' # Update data to current
#' new_player_meta <- update_player_meta()
#' }
#' @export
update_player_meta <- function(start_again = FALSE) {
  store_warning <- options(warn = -1)$warn
  # Remove people with no country from existing player_meta
  player_meta <- player_meta |> 
    dplyr::filter(!is.na(country))
  # Read people file from cricsheet
  people <- readr::read_csv("https://cricsheet.org/register/people.csv",
    col_types = "ccccccccccccccc", lazy = FALSE
  ) |>
    dplyr::select(
      cricsheet_id = identifier,
      cricinfo_id = key_cricinfo,
      cricinfo_id2 = key_cricinfo_2,
      name, unique_name
    ) |>
    # Remove people not on Cricinfo
    dplyr::filter(!is.na(cricinfo_id))

  # Compare existing version of player_meta and find missing players
  if (start_again) {
    missing_df <- people
  } else {
    missing_df <- people |>
      dplyr::anti_join(player_meta, by = "cricinfo_id") |>
      dplyr::anti_join(player_meta, by = c("cricinfo_id2" = "cricinfo_id")) 
  }

  # Now download meta data for new players
  new_player_meta <- fetch_player_meta(missing_df$cricinfo_id)

  # For people missing on cricinfo, try the other cricinfo id
  cricinfo2 <- new_player_meta |>
    dplyr::left_join(people |> dplyr::select(-name), by = "cricinfo_id") |>
    dplyr::filter(is.na(full_name) & !is.na(cricinfo_id2)) |>
    dplyr::pull(cricinfo_id2)
  if (length(cricinfo2) > 0) {
    new_player_meta <- new_player_meta |>
      dplyr::bind_rows(fetch_player_meta(cricinfo2)) |>
      dplyr::filter(!is.na(full_name))
  }

  # Add in cricsheet id
  new_player_meta <- dplyr::bind_rows(
    new_player_meta |>
      dplyr::left_join(people |> dplyr::select(-name),
        by = "cricinfo_id"
      ) |>
      dplyr::select(-cricinfo_id2) |>
      dplyr::filter(!is.na(cricsheet_id)),
    new_player_meta |>
      dplyr::left_join(people |> dplyr::select(-name, -cricinfo_id),
        by = c("cricinfo_id" = "cricinfo_id2")
      ) |>
      dplyr::filter(!is.na(cricsheet_id))
  ) |>
    # Organize by column and row
    dplyr::select(
      cricinfo_id, cricsheet_id, unique_name, full_name,
      dplyr::everything()
    ) |>
    # Remove missing people
    dplyr::filter(!is.na(full_name))
  
  # Add to existing player_meta
  if (!start_again) {
    new_player_meta <- new_player_meta |>
      dplyr::mutate(dob = as.Date(dob)) |> 
      dplyr::bind_rows(player_meta)
  }

  # Clean up and arrange
  new_player_meta <- new_player_meta |>
    # Fix country names
    dplyr::mutate(country = stringr::str_remove(country, " Wmn")) |>
    # Arrange in alphabetic order
    dplyr::arrange(full_name) |>
    # Remove duplicates
    dplyr::distinct()

  options(warn = store_warning)
  return(new_player_meta)
}

utils::globalVariables(c(
  "identifier", "key_cricinfo", "key_cricinfo_2", "name", "unique_name",
  "player_meta", "cricinfo_id", "cricinfo_id2", "cricsheet_id"
))
