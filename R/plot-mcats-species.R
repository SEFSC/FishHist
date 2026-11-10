#' Plot of management categories recorded under an FMP for all managed species
#'
#' Number of FRs published by management type and species
#'
#' @inheritParams common_args
#'
#' @return A ggplot object
#'
#' @examples plot_mcats_spp(fishhist,
#'     FMP_filter = "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION")
#'
#' @export
plot_mcats_spp <- function(data = fishhist, FMP_filter) {

  # Get end date for filter
  dt <- with(na.omit(data["END_DATE"]), {
    tab <- table(END_DATE)
    as.Date(names(tab)[tab == max(tab)])
  })

  # Get currently managed species
  managed_spp <- data |>
    dplyr::ungroup() |>
    dplyr::filter(END_DATE == dt) |>
    dplyr::distinct(COMMON_NAME_CLEAN) |>
    dplyr::pull(COMMON_NAME_CLEAN)

  dat2 <- data |>
    dplyr::filter(FMP == FMP_filter,
                  COMMON_NAME_CLEAN %in% managed_spp,
                  MANAGEMENT_TYPECLEAN != "DEFINITION") |>
    dplyr::group_by(FMP, MANAGEMENTCATEGORY, COMMON_NAME_CLEAN) |>
    dplyr::summarise(nFRs = dplyr::n_distinct(FR_CITATION))

  if (nrow(dat2) == 0) {
    stop("Check filters")
  }

  plot <- ggplot2::ggplot(
    dat2,
    ggplot2::aes(
      x = nFRs, y = factor(COMMON_NAME_CLEAN,
                           levels = rev(sort(unique(COMMON_NAME_CLEAN)))),
      fill = MANAGEMENTCATEGORY
    )
  ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      values = c("#999999", "#CC79A7", "#D55E00", "#0072B2",
                 "#F0E442", "#009E73", "#56B4E9", "#E69F00")) +
    ggplot2::labs(fill = "Management Category",
                  x = "Number of Federal Register Notices Published",
                  y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 5),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.ticks.length.y = grid::unit(0.1, "cm")
    ) +
    ggplot2::ggtitle(dat2$FMP)

  return(plot)
}
