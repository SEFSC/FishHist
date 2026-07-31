#' Number of FRs published by management type and species
#'
#' @param data default=fishhist
#' @param FMP Filter to plot only a single FMP of interest.
#' \subsection{FMP:} {
#'    \describe{
#'      \item{ATLANTIC COAST RED DRUM FISHERY}
#'      \item{COASTAL MIGRATORY PELAGIC RESOURCES}
#'      \item{DOLPHIN AND WAHOO FISHERY OFF THE ATLANTIC STATES}
#'      \item{FMP FOR THE EEZ AROUND PUERTO RICO}
#'      \item{FMP FOR THE EEZ AROUND ST. CROIX}
#'      \item{FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN}
#'      \item{GOLDEN CRAB FISHERY OF THE SOUTH ATLANTIC}
#'      \item{QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{RED DRUM FISHERY OF THE GULF OF MEXICO}
#'      \item{REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{REEF FISH RESOURCES OF THE GULF OF MEXICO}
#'      \item{SHRIMP FISHERY OF THE GULF OF MEXICO}
#'      \item{SHRIMP FISHERY OF THE SOUTH ATLANTIC}
#'      \item{SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION}
#'      \item{SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{SPINY LOBSTER FISHERY OF THE GULF OF MEXICO AND SOUTH ATLANTIC}
#'    }
#' }
#'
#' @return A plot
#'
#' @examples plot_mtypes(fishhist, FMP_filter="REEF FISH RESOURCES OF THE GULF OF MEXICO")
plot_mcats_spp <- function(data=fishhist, FMP_filter){

  dt <- data |> dplyr::group_by(END_DATE2) |> dplyr::summarize(n= n()) |>
    dplyr::filter(n == max(n)) |> dplyr::pull(END_DATE2)

  # Get currently managed species
  managed_spp <- data |> dplyr::ungroup() |> dplyr::filter(END_DATE2 == dt) |>
    dplyr::distinct(COMMON_NAME_USE) |>
    pull(COMMON_NAME_USE)

  dat2 <- data |>
    dplyr::filter(FMP == FMP_filter, COMMON_NAME_USE %in% managed_spp, MANAGEMENT_TYPE != 'DEFINITION') |>
    dplyr::group_by(FMP, MANAGEMENT_CATEGORY, COMMON_NAME_USE) |>
    dplyr::summarise(nFRs = dplyr::n_distinct(FR_CITATION))

  plot <- ggplot2::ggplot(dat2,
                          ggplot2::aes(x = nFRs, y = factor(COMMON_NAME_USE, levels = rev(sort(unique(COMMON_NAME_USE)))),
                                       fill = MANAGEMENT_CATEGORY)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_x_continuous(expand = c(0, 0))+
    ggplot2::scale_fill_manual(values = c("#999999", "#CC79A7", "#D55E00",  "#0072B2",  "#F0E442",  "#009E73", "#56B4E9",  "#E69F00")) +
    ggplot2::labs(fill="Management Category", x = "Number of Federal Register Notices Published", y="")+
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size=5),
                   panel.grid.major.y = ggplot2::element_blank(),
                   axis.ticks.length.y = grid::unit(0.1, "cm")) +
    ggplot2::ggtitle(dat2$FMP)

  return(plot)
}
