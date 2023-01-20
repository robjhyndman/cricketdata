men <- data.frame(
  team = c(1:9, 11, 12, 14, 15, 17, 19, 20, 25, 26, 27, 28, 29, 30, 32, 37, 40),
  name = c(
    "England",
    "Australia",
    "South Africa",
    "West Indies",
    "New Zealand",
    "India",
    "Pakistan",
    "Sri Lanka",
    "Zimbabwe",
    "United States of America",
    "Bermuda",
    "East Africa",
    "Netherlands",
    "Canada",
    "Hong Kong",
    "Papua New Guinea",
    "Bangladesh",
    "Kenya",
    "United Arab Emirates",
    "Namibia",
    "Ireland",
    "Scotland",
    "Nepal",
    "Oman",
    "Afghanistan"
  )
)

women <- data.frame(
  team = c(289, 1026, 1863, 4240, 3379, 3672, 2614, 2285, 3022, 2461, 3867, 825, 3808, 2331, 3505, 3843),
  name = c(
    "Australia",
    "England",
    "India",
    "Bangladesh",
    "South Africa",
    "Sri Lanka",
    "New Zealand",
    "Ireland",
    "Pakistan",
    "Netherlands",
    "West Indies",
    "Denmark",
    "Jamaica",
    "Japan",
    "Scotland",
    "Trinidad & Tobago"
  )
)

men <- tibble::as_tibble(men[order(men$name), ], .name_repair = "check_unique") |>
  dplyr::mutate(
    team = as.integer(team),
    name = as.character(name)
  )
women <- tibble::as_tibble(women[order(women$name), ], .name_repair = "check_unique") |>
  dplyr::mutate(
    team = as.integer(team),
    name = as.character(name)
  )


## Men
# 1 England
# 2 Australia
# 3 South Africa
# 4 West Indies
# 5 New Zealand
# 6 India
# 7 Pakistan
# 8 Sri Lanka
# 9 Zimbabwe
# 11 United States of America
# 12 Bermuda
# 14 East Africa
# 15 Netherlands
# 17 Canada
# 19 Hong Kong
# 20 Papua New Guinea
# 25 Bangladesh
# 26 Kenya
# 27 United Arab Emirates
# 28 Namibia
# 29 Ireland
# 30 Scotland
# 32 Nepal
# 37 Oman
# 40 Afghanistan

## WOMEN

# 289 Australia Women
# 1026 England Women
# 1863  India Women
# 4240 Bangladesh Women
# 3379 South Africa Women
# 3672 Sri Lanka Women
# 2614  New Zealand Women
# 2285  Ireland women
# 3022 Pakistan Women
# 2461 Netherlands Women
# 3867 West Indies Women
# 825 Denmark Women
# 3808 Jamaica Women
# 2331 Japan Women
# 3505 Scotland Women
# 3843 Trinidad & Tobago Women



rename_countries <- function(countries) {
  countries |>
    stringr::str_replace("AFG", "Afghanistan") |>
    stringr::str_replace("Afr$", "Africa XI") |>
    stringr::str_replace("AUS", "Australia") |>
    stringr::str_replace("Bdesh|BDESH|BD", "Bangladesh") |>
    stringr::str_replace("BMUDA", "Bermuda") |>
    stringr::str_replace("CAN", "Canada") |>
    stringr::str_replace("DnWmn|Denmk", "Denmark") |>
    stringr::str_replace("EAf", "East (and Central) Africa") |>
    stringr::str_replace("ENG", "England") |>
    stringr::str_replace("HKG", "Hong Kong") |>
    stringr::str_replace("ICC$", "ICC World XI") |>
    stringr::str_replace("INDIA|IND", "India") |>
    stringr::str_replace("IntWn|Int XI", "International XI") |>
    stringr::str_replace("Ire$|IRELAND|IRE", "Ireland") |>
    stringr::str_replace("JamWn", "Jamaica") |>
    stringr::str_replace("JPN", "Japan") |>
    stringr::str_replace("KENYA", "Kenya") |>
    stringr::str_replace("NAM", "Namibia") |>
    stringr::str_replace("NEPAL", "Nepal") |>
    stringr::str_replace("Neth$|NL", "Netherlands") |>
    stringr::str_replace("NZ", "New Zealand") |>
    stringr::str_replace("OMAN", "Oman") |>
    stringr::str_replace("PAK", "Pakistan") |>
    stringr::str_replace("PNG|P\\.N\\.G\\.", "Papau New Guinea") |>
    stringr::str_replace("^SA", "South Africa") |>
    stringr::str_replace("SCOT|SCO|Scot$", "Scotland") |>
    stringr::str_replace("SL", "Sri Lanka") |>
    stringr::str_replace("TTWmn|T \\& T", "Trinidad and Tobago") |>
    stringr::str_replace("UAE|U\\.A\\.E\\.", "United Arab Emirates") |>
    stringr::str_replace("USA|U\\.S\\.A\\.", "United States of America") |>
    stringr::str_replace("World$|World-XI", "World XI") |>
    stringr::str_replace("WI", "West Indies") |>
    stringr::str_replace("YEWmn|Y\\. Eng", "Young England") |>
    stringr::str_replace("ZIM", "Zimbabwe")
}
