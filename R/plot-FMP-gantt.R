#' Gantt chart of FMPs
#'
#' Gantt chart of the first and last Federal Register (FR)
#' published within a Fishery Management Plan (FMP) within the
#' Southeast Fisheries Management History dataset.
#'
#' @inheritParams common_args
#'
#' @return A ggplot object
#'
#' @examples gantt_FMP(fishhist)
#' @export
gantt_FMP <- function(data = fishhist) {

  # FMPs by region
  fmp_reg <- data |> dplyr::select(FMP, REGION) |>
    dplyr::distinct() |> dplyr::arrange(REGION)

  data_summary <- data |>
    dplyr::group_by(FMP) |>
    dplyr::summarise(
      start_year = min(lubridate::year(STARTDATE_CLEAN)),
      end_year = max(lubridate::year(END_DATE), na.rm = TRUE)
    ) |>
    # Arrange by region
    # If FMP in multiple regions, take first row using slice
    dplyr::left_join(fmp_reg, by = dplyr::join_by(FMP)) |>
    dplyr::group_by(FMP) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(REGION, start_year) |>
    dplyr::mutate(
      # Wrap long titles
      FMP = stringr::str_wrap(custom_title_case(FMP), width = 40),
      FMP = factor(FMP, levels = unique(FMP))
    )

  plot <- ggplot2::ggplot(data_summary, ggplot2::aes(y = FMP)) +
    ggplot2::geom_segment(ggplot2::aes(x = start_year,
                                       xend = end_year,
                                       yend = FMP),
      color = "steelblue", linewidth = 6, lineend = "round"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(data_summary$start_year, na.rm = TRUE),
      max(data_summary$end_year, na.rm = TRUE),
      by = 5
    )) +
    ggplot2::labs(x = "Year", y = "Fishery Management Plan") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold")
    )

  return(plot)
}
