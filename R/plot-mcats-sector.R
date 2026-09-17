#' FMP plot by sector
#'
#' Percent of regulations by management type and sector (recreational vs. commerical)
#'
#' @param data default=fishhist
#' @param FMP Filter to plot only a single FMP of interest.
#'
#' @return A plot
#'
#' @examples plot_mtypes(fishhist, FMP_filter="REEF FISH RESOURCES OF THE GULF OF MEXICO")
#'
#' @export

plot_mcats_sector <- function(data=fishhist, FMP_filter){

    dat2 <- data |>
      dplyr::filter(FMP == FMP_filter) |>
      dplyr::group_by(FMP, SECTOR_CLEAN, MANAGEMENTCATEGORY) |>
      dplyr::summarise(nrecs = dplyr::n_distinct(REG_ID)) |>
      dplyr::group_by(FMP, SECTOR_CLEAN) |>
      dplyr::mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%"))

    if(nrow(dat2) == 0) {stop("Check filters")}

    plot <- ggplot2::ggplot(dat2,
                            ggplot2::aes(x = "" , y = perc, fill = MANAGEMENTCATEGORY)) +
           ggplot2::geom_col(width = 1, color = 1) +
           ggplot2::coord_polar(theta = "y") +
           ggplot2::facet_wrap(~SECTOR_CLEAN) +
           ggplot2::geom_text(ggplot2::aes(x = 1.7, label = labels), size=4.5,
                              position = ggplot2::position_stack(vjust = 0.5))+
           ggplot2::guides(fill = ggplot2::guide_legend(title = "Management Category")) +
           ggplot2::theme_void() +
           ggplot2::ggtitle(dat2$FMP)

  return(plot)
}
