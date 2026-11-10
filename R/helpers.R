#' Formatting names for tables and plots
#' @param text String to be formatted
#' @keywords internal
#' @noRd
custom_title_case <- function(text) {

  title_text <- stringr::str_to_title(text)

  # Force select words to LOWERCASE (unless they appear at the very start)
  lower_words <- c("of", "the", "and", "in", "on", "at",
                   "for", "to", "a", "an")
  lower_pattern <- base::paste0(
    "(?<!^)\\b(",
    base::paste(stringr::str_to_title(lower_words), collapse = "|"),
    ")\\b"
  )

  title_text <- stringr::str_replace_all(
    title_text,
    stringr::regex(lower_pattern),
    function(m) stringr::str_to_lower(m)
  )

  # Force acronyms to UPPERCASE
  upper_acronyms <- c("U.S.", "ABC", "TAC", "IFQ", "TED", "NMFS", "ITQ",
                      "NOAA", "EEZ", "FMP", "FMFC", "MPA", "SSBR")
  upper_pattern <- base::paste0(
    "\\b(",
    base::paste(stringr::str_to_title(upper_acronyms), collapse = "|"),
    ")\\b"
  )

  title_text <- stringr::str_replace_all(
    title_text,
    stringr::regex(upper_pattern, ignore_case = TRUE),
    function(m) stringr::str_to_upper(m)
  )

  return(title_text)
}
