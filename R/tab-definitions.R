#' List containing separate tables of Definitions for each FMP with associated FR notice for reference
#'
#' @param data default=fishhist
#' @return A list of tables
#'
#' @examples tab_definitions(fishhist)

tab_definitions <- function(data=fishhist, spp, region){

  # Format acronyms to stay all CAPS in the talbe
  acronyms <- c("U.S.", "ABC", "TAC", "IFQ", "TED", "NMFS", "NOAA", "EEZ", "FMP", "FMFC", "MPA", "SSBR")

  replacements <- setNames(acronyms,
                           paste0("\\b", stringr::str_to_title(acronyms), "\\b"))

  str_to_title_acronyms <- function(text, replace_map) {
    title_text <- stringr::str_to_title(text)
    stringr::str_replace_all(title_text, replace_map)}

  dat <- fishhist |> dplyr::filter(MANAGEMENT_TYPECLEAN == 'DEFINITION') |>
    dplyr::select(FMP, VALUE, FR_CITATION, FR_URL) |> dplyr::distinct() |>
    dplyr::arrange(VALUE) |>
    dplyr::mutate(FMP = stringr::str_to_title(FMP),
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
