#' Timeline of management actions publised for a species within a FMP
#'
#' @param data default=fishhist
#' @param species Filter to species of interest (common name).
#' @param region Filter to region of interest
#' @return A plot
#'
#' @examples process_closures(fishhist, spp = "SNAPPER, RED", region = "SOUTH ATLANTIC")

plot_species_timeline <- function(data=fishhist, spp, region){

  spp_dat <- fishhist |>
    dplyr::filter(COMMON_NAME_CLEAN == 'SNAPPER, RED',
                  REGION == 'SOUTH ATLANTIC',
                  !MANAGEMENT_TYPECLEAN %in% c('DEFINITION', 'IGNORABLE')) |>
    dplyr::mutate(date = lubridate::ymd(EFFECTIVE_DATE), start_year = lubridate::year(EFFECTIVE_DATE),
                  MANAGEMENT_USE = stringr::str_to_title(dplyr::case_when(MANAGEMENT_TYPECLEAN %in% c("BAG LIMIT", "CREW BAG LIMIT") ~ 'BAG LIMITS',
                                                                        MANAGEMENT_TYPECLEAN %in% c("IFQ PROGRAM ESTABLISHED", "ITQ PROGRAM ESTABLISHED") ~ "IFQ/ITQ PROGRAM ESTABLISHED",
                                             MANAGEMENT_TYPECLEAN %in% c("PROHIBITED SPECIES", "PROHIBITED SALE AND PURCHASE") ~ 'PROHIBITED SPECIES',
                                             MANAGEMENT_TYPECLEAN %in% c("MINIMUM SIZE LIMIT", "MAXIMUM SIZE LIMIT") ~ "SIZE LIMIT",
                                             MANAGEMENT_TYPECLEAN %in% c("FISHING YEAR", "FISHING SEASON")  ~ "FISHING YEAR/SEASON",
                                             MANAGEMENTCATEGORY == 'OTHER' ~ 'REPORTING REQUIREMENTS',
                                             MANAGEMENTCATEGORY == 'GEAR REQUIREMENTS' ~ 'GEAR REQUIREMENTS',
                                             MANAGEMENTCATEGORY == 'CATCH LIMITS' ~ 'CATCH LIMITS',
                                             TRUE ~ MANAGEMENT_TYPECLEAN)),
                  # Fix IFQ and ITF that need to stay all cap
                  MANAGEMENT_USE = dplyr::case_when(MANAGEMENT_USE == "Ifq Program Established" ~ "IFQ Program Established",
                                                    MANAGEMENT_USE == "Itq Program Established" ~ "ITQ Program Established",
                                                    TRUE ~ MANAGEMENT_USE)) |>
    dplyr::group_by(COMMON_NAME_CLEAN, start_year, MANAGEMENTCATEGORY, MANAGEMENT_USE) |>
    dplyr::summarize(date = dplyr::first(date), .groups = "drop") |>
    dplyr::group_by(COMMON_NAME_CLEAN, date) |>
    dplyr::mutate(month_offset = dplyr::row_number() - 1,
                  date_label = lubridate::`%m+%`(date, months(month_offset)),
                  date_label = dplyr::case_when(lubridate::month(date) != 2 & lubridate::month(date_label) != 29 & lubridate::month(date_label) < lubridate::month(date) & lubridate::year(date_label) > lubridate::year(date) ~
                                  as.Date(paste0(lubridate::year(date), "-", sprintf("%02d", lubridate::month(date_label)), "-", sprintf("%02d", lubridate::day(date_label)))),
                                TRUE ~ date_label),
                  date_label = dplyr::case_when(lubridate::month(date_label) == 2 & lubridate::day(date_label) == 29 ~ (as.Date(paste0(lubridate::year(date), "-", lubridate::month(date_label), "-", (date_label - lubridate::days(1))))),
                                TRUE ~ date_label)) |>
    dplyr::group_by(MANAGEMENT_USE) |>
    dplyr::mutate(position = dplyr::cur_group_id(),
           ymin = position - 0.5,
           ymax = position + 0.5) |>
    dplyr::arrange(MANAGEMENTCATEGORY, MANAGEMENT_USE)

  # Define the color palette for MANAGEMENT_USE
  management_colors <- c("#88CCEE", "#999933", "#117733", "#332288", "#882255",
                         "#AA4499", "#CC6677", "#DDCC77", "#44AA99", "#117766",
                         "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                         "#D55E00", "#CC79A7", "#999999", "#6699CC", "#888888")


  # Modify the timeline plot
  plot <- ggplot2::ggplot(spp_dat) +
    ggplot2::theme_classic() +
    # Add colored bands for COMMON_NAME_USE
    ggplot2::geom_rect(ggplot2::aes(xmin = as.Date(paste0(min(spp_dat$start_year), "-01-01")), xmax = as.Date(paste0(max(spp_dat$start_year), "-12-31")),
                  ymin = ymin, ymax = ymax, alpha = 0.1, fill="white"),
                  show.legend = FALSE) +
    # Apply fill colors
    ggplot2::scale_fill_identity() +
    # Add vertical lines for each year
    ggplot2::geom_vline(xintercept = seq(as.Date(paste0(min(spp_dat$start_year), "-01-01")), as.Date(paste0(max(spp_dat$start_year), "-12-31")), by = "1 year"),
               color = "gray", linetype = "dotted", linewidth = 0.25) +
    # Add lines from points to the x-axis
    ggplot2::geom_segment(ggplot2::aes(x = date_label, xend = date_label,
                     y = position + jitter(0.2), yend = 0),
                 color = "black", linewidth = 0.5) +
    # Add points for MANAGEMENT_USE
    ggplot2::geom_point(ggplot2::aes(x = date_label + jitter(0.3), y = position + jitter(0.2), color = MANAGEMENT_USE), size = 4,
               position = ggplot2::position_jitter(width = 0.5, height = 0)) +
    # Apply the custom color palette
    ggplot2::scale_color_manual(values = management_colors, name = "Management Type") +
    # Add y-axis labels
    ggplot2::scale_y_continuous(breaks = spp_dat$position,
                       labels = spp_dat$MANAGEMENT_USE,
                       expand = ggplot2::expansion(mult = c(0, 0.01))) +
    ggplot2::expand_limits(y=0) +
    # Set x-axis limits and labels
    ggplot2::scale_x_date(limits = c(as.Date(paste0(min(spp_dat$start_year), "-01-01")), as.Date(paste0(max(spp_dat$start_year), "-12-31"))),
                 date_breaks = "1 year",
                 date_labels = "%Y",
                 expand = c(0,0)) +
    ggplot2::coord_cartesian(xlim = c(as.Date(paste0(min(spp_dat$start_year), "-01-01")), as.Date(paste0(max(spp_dat$start_year), "-12-31")))) +
    # Customize the theme
    ggplot2::theme(axis.line.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = 15, face = "plain", hjust = 1),
          axis.text.x = ggplot2::element_text(size = 13, face = "plain", angle = -90, hjust = 1),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          legend.position = "none")

  return(plot)

}
