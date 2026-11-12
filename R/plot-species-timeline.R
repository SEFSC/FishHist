#' Timeline of management types recorded for a species within a region
#'
#' Timeline of management actions published for a species within a FMP
#' (filter set to region)
#'
#' @inheritParams common_args
#'
#' @return A ggplot object
#'
#' @examples plot_species_timeline(fishhist, spp = "SNAPPER, RED",
#'   region = "SOUTH ATLANTIC")
#'
#' @export
plot_species_timeline <- function(data = fishhist, spp, region) {
  spp_dat <- data |>
    dplyr::filter(
      COMMON_NAME_CLEAN == spp,
      REGION == region,
      !MANAGEMENT_TYPECLEAN %in% c("DEFINITION", "IGNORABLE")
    ) |>
    dplyr::mutate(
      date = lubridate::ymd(EFFECTIVE_DATE),
      start_year = lubridate::year(EFFECTIVE_DATE),
      MANAGEMENT_USE = custom_title_case(dplyr::case_when(
        MANAGEMENT_TYPECLEAN %in%
          c("BAG LIMIT", "CREW BAG LIMIT") ~
          "BAG LIMITS",
        MANAGEMENT_TYPECLEAN %in%
          c("IFQ PROGRAM ESTABLISHED", "ITQ PROGRAM ESTABLISHED") ~
          "IFQ/ITQ PROGRAM ESTABLISHED",
        MANAGEMENT_TYPECLEAN %in%
          c("PROHIBITED SPECIES", "PROHIBITED SALE AND PURCHASE") ~
          "PROHIBITED SPECIES",
        MANAGEMENT_TYPECLEAN %in%
          c("MINIMUM SIZE LIMIT", "MAXIMUM SIZE LIMIT") ~
          "SIZE LIMIT",
        MANAGEMENT_TYPECLEAN %in%
          c("FISHING YEAR", "FISHING SEASON") ~
          "FISHING YEAR/SEASON",
        MANAGEMENTCATEGORY == "OTHER" ~ "REPORTING REQUIREMENTS",
        MANAGEMENTCATEGORY == "GEAR REQUIREMENTS" ~ "GEAR REQUIREMENTS",
        MANAGEMENTCATEGORY == "CATCH LIMITS" ~ "CATCH LIMITS",
        TRUE ~ MANAGEMENT_TYPECLEAN
      ))) |>
    dplyr::group_by(COMMON_NAME_CLEAN, start_year,
                    MANAGEMENTCATEGORY, MANAGEMENT_USE) |>
    dplyr::summarize(date = dplyr::first(date), .groups = "drop") |>
    dplyr::group_by(COMMON_NAME_CLEAN, date) |>
    dplyr::mutate(
      month_offset = dplyr::row_number() - 1,
      date_label = lubridate::`%m+%`(date, months(month_offset)),
      date_label = dplyr::case_when(lubridate::month(date) != 2 &
        lubridate::day(date_label) != 29 &
        lubridate::month(date_label) < lubridate::month(date) &
        lubridate::year(date_label) > lubridate::year(date) ~
        lubridate::make_date(
          year = lubridate::year(date),
          month = lubridate::month(date_label),
          day = lubridate::day(date_label)
        ), TRUE ~ date_label),
      # Leap day handling (Subtracts 1 day if Feb 29)
      date_label = dplyr::case_when(
        lubridate::month(date_label) == 2 &
          lubridate::day(date_label) == 29 ~ date_label - lubridate::days(1),
        TRUE ~ date_label
      )
    ) |>
    dplyr::group_by(MANAGEMENT_USE) |>
    dplyr::mutate(
      position = dplyr::cur_group_id(),
      ymin = position - 0.5,
      ymax = position + 0.5
    ) |>
    dplyr::arrange(MANAGEMENTCATEGORY, MANAGEMENT_USE)

  if (nrow(spp_dat) == 0) {
    stop("Check filters")
  }

  # Define the color palette for MANAGEMENT_USE
  management_colors <- c(
    "#88CCEE", "#999933", "#117733", "#332288", "#882255",
    "#AA4499", "#CC6677", "#DDCC77", "#44AA99", "#117766",
    "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
    "#D55E00", "#CC79A7", "#999999", "#6699CC", "#888888"
  )

  min_yr <- min(
    c(spp_dat$start_year,
      as.numeric(format(spp_dat$date_label, "%Y"))), na.rm = TRUE)
  max_yr <- max(
    c(spp_dat$start_year,
      as.numeric(format(spp_dat$date_label, "%Y"))), na.rm = TRUE)

  x_min <- as.Date(paste0(min_yr, "-01-01"))
  x_max <- as.Date(paste0(max_yr, "-12-31"))

  # Modify the timeline plot
  plot <- ggplot2::ggplot(spp_dat) +
    ggplot2::theme_classic() +
    # Add vertical lines for each year
    ggplot2::geom_vline(
      xintercept = seq(x_min, x_max, by = "1 year"),
      color = "gray", linetype = "dotted", linewidth = 0.25
    ) +
    # Add lines from points to the x-axis
    ggplot2::geom_segment(
      ggplot2::aes(x = date_label, xend = date_label, y = position, yend = 0),
      color = "black", linewidth = 0.5
    ) +
    # Add points for MANAGEMENT_USE
    ggplot2::geom_point(
      ggplot2::aes(x = date_label, y = position, color = MANAGEMENT_USE),
      size = 4
    ) +
    # Apply the custom color palette
    ggplot2::scale_color_manual(
      values = management_colors, name = "Management Type") +
    # Add y-axis labels
    ggplot2::scale_y_continuous(
      breaks = spp_dat$position,
      labels = spp_dat$MANAGEMENT_USE,
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::expand_limits(y = 0) +
    # Set x-axis limits and labels
    ggplot2::scale_x_date(
      breaks = seq(x_min, x_max, by = "1 year"),
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::coord_cartesian(xlim = c(x_min, x_max)) +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 15, face = "plain", hjust = 1),
      axis.text.x = ggplot2::element_text(
        size = 13, face = "plain", angle = -90, hjust = 0, vjust = 0.5),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      legend.position = "none"
    )

  return(plot)
}
