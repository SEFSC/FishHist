#' Definitions Table
#'
#' List containing separate tables of Definitions for each FMP with associated FR notice for reference
#'
#' @param data default=fishhist
#' @return A list of tables
#'
#' @examples tab_definitions(fishhist)
#'
#' @export
tab_definitions <- function(data=fishhist){

  # Format acronyms to stay all CAPS in the table
  acronyms <- c("U.S.", "ABC", "TAC", "IFQ", "TED", "NMFS", "NOAA", "EEZ", "FMP", "FMFC", "MPA", "SSBR")

  replacements <- stats::setNames(acronyms,
                           paste0("\\b", stringr::str_to_title(acronyms), "\\b"))

  str_to_title_acronyms <- function(text, replace_map) {
    title_text <- stringr::str_to_title(text)
    stringr::str_replace_all(title_text, replace_map)}

  custom_title_case <- function(text) {
    # 1. Convert everything to title case first using stringr::str_to_title
    title_text <- stringr::str_to_title(text)

    # 2. Words to force to LOWERCASE (unless they appear at the very start)
    lower_words <- c("of", "the", "and", "in", "on", "at", "for", "to", "a", "an")
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

    # 3. Acronyms to force to UPPERCASE
    upper_acronyms <- c("FMP", "EEZ", "U.S.")
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

  dat <- data |> dplyr::filter(MANAGEMENT_TYPECLEAN == 'DEFINITION') |>
    dplyr::select(FMP, VALUE, FR_CITATION, FR_URL) |> dplyr::distinct() |>
    dplyr::arrange(VALUE) |>
    dplyr::mutate(FMP = custom_title_case(FMP),
                  VALUE = str_to_title_acronyms(VALUE, replacements))

  tabs <- dat |>
    dplyr::group_split(FMP) |>
    purrr::map(~ {
      fmp_name <- .x$FMP[1]

      flextable::flextable(.x, col_keys = c("VALUE", "FR_CITATION")) |>
        flextable::compose(j = "FR_CITATION",
                           value = flextable::as_paragraph(flextable::hyperlink_text(x = FR_CITATION, url = FR_URL, props = flextable::fp_text_default(color = "blue", underline = TRUE)))) |>
        flextable::set_header_labels(VALUE = "Defintion",
                                     FR_CITATION = "FR Citation") |>
        flextable::add_header_lines(values = fmp_name) |>
        flextable::merge_v(j = "VALUE") |> # Only merge visible columns in col_keys
        flextable::valign(valign = "top") |>
        flextable::theme_box() |>
        flextable::fontsize(part = "all", size = 12) |>
        flextable::font(part = "all", fontname = "Times New Roman") |>
        flextable::line_spacing(space = 0.8, part = "body") |>
        flextable::width(j=1, width= 5) |>
        flextable::width(j=2, width= 2)
    }) |>
    purrr::set_names(sort(unique(dat$FMP)))

  return(tabs)
}
