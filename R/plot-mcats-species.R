#' Plot of management categories recorded under an FMP for all managed species
#'
#' Number of FRs published by management type and species
#'
#' @param data default=fishhist
#' @param FMP_filter Filter to plot only a single FMP of interest.
#'
#' @return A plot
#'
#' @examples plot_mcats_spp(fishhist, FMP_filter = "REEF FISH RESOURCES OF THE
#'   GULF OF MEXICO")
#'
#' @export
plot_mcats_spp <- function(data = fishhist, FMP_filter) {
  dt <- data |>
    dplyr::filter(!is.na(END_DATE)) |>
    dplyr::group_by(END_DATE) |>
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n == max(n)) |>
    dplyr::pull(END_DATE)

  # Get currently managed species
  managed_spp <- data |>
    dplyr::ungroup() |>
    dplyr::filter(END_DATE == dt) |>
    dplyr::distinct(COMMON_NAME_CLEAN) |>
    dplyr::pull(COMMON_NAME_CLEAN)

  dat2 <- data |>
    dplyr::filter(FMP == FMP_filter, COMMON_NAME_CLEAN %in% managed_spp, MANAGEMENT_TYPECLEAN != "DEFINITION") |>
    dplyr::group_by(FMP, MANAGEMENTCATEGORY, COMMON_NAME_CLEAN) |>
    dplyr::summarise(nFRs = dplyr::n_distinct(FR_CITATION))

  if (nrow(dat2) == 0) {
    stop("Check filters")
  }

  plot <- ggplot2::ggplot(
    dat2,
    ggplot2::aes(
      x = nFRs, y = factor(COMMON_NAME_CLEAN, levels = rev(sort(unique(COMMON_NAME_CLEAN)))),
      fill = MANAGEMENTCATEGORY
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = c("#999999", "#CC79A7", "#D55E00", "#0072B2", "#F0E442", "#009E73", "#56B4E9", "#E69F00")) +
    ggplot2::labs(fill = "Management Category", x = "Number of Federal Register Notices Published", y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 5),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.length.y = grid::unit(0.1, "cm")
    ) +
    ggplot2::ggtitle(dat2$FMP)

  return(plot)
}
