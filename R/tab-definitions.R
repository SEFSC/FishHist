#' Table of definitions provided under each Federal Registrar (FR) notice
#'
#' List containing separate tables of Definitions for each FMP with associated
#' FR notice for reference
#'
#' @inheritParams common_args
#'
#' @return A flextable object
#'
#' @examples tab_definitions(fishhist,
#'    FMP_filter = "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION")
#'
#' @export
tab_definitions <- function(data=fishhist, FMP_filter){

  dat <- data |> dplyr::filter(MANAGEMENT_TYPECLEAN == 'DEFINITION',
                               FMP == FMP_filter) |>
    dplyr::select(FMP, VALUE, FR_CITATION, FR_URL) |> dplyr::distinct() |>
    dplyr::arrange(VALUE) |>
    dplyr::mutate(FMP = custom_title_case(FMP),
                  VALUE = custom_title_case(VALUE))

  tab <- flextable::flextable(dat[2:4], col_keys = c("VALUE", "FR_CITATION")) |>
    flextable::compose(
      j = "FR_CITATION",
      value = flextable::as_paragraph(
        flextable::hyperlink_text(
          x = FR_CITATION, url = FR_URL,
          props = flextable::fp_text_default(color = "blue",
                                             underline = TRUE)))
    ) |>
    flextable::set_header_labels(VALUE = "Defintion",
                                 FR_CITATION = "FR Citation") |>
    # Only merge visible columns in col_keys
    flextable::merge_v(j = "VALUE") |>
    flextable::valign(valign = "top") |>
    flextable::theme_box() |>
    flextable::fontsize(part = "all", size = 12) |>
    flextable::font(part = "all", fontname = "Times New Roman") |>
    flextable::line_spacing(space = 0.8, part = "body") |>
    flextable::width(j=1, width= 5) |>
    flextable::width(j=2, width= 2) |>
    flextable::set_caption(caption = unique(dat$FMP))

  return(tab)
}
