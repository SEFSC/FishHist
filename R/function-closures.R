#' Process and connect all closure-related management types for a species
#' within a region
#'
#' Process closures to expand and incorporate all related mtypes for closures
#' to know for a single day whether the fishery is open or closed.
#'
#' @inheritParams common_args
#' @param start Process all closure-related management types starting from
#'    a defined start date (formatted using \code{as.Date}).
#' @param end Process all closure-related management types starting from
#'    a defined end date (formatted using \code{as.Date}).
#'
#' @return A list containing 6 items:
#'    \itemize{
#'      \item \strong{data} is the full closure dataset where each row represents
#'      a day within a time series. An additional field is added to indicate
#'      whether that day the fishery is OPEN or CLOSED.
#'      \item \strong{tab_sum_days_open} is a dataframe summarizing the number of
#'      days the fishery is open within a strata
#'      (year, region, zone, sector, subsector).
#'      \item \strong{plot_days_open} is a ggplot plot object of the number of
#'      days the fishery is open by year within a strata. The plot is faceted
#'      to show a plot for each zone and sector-subsector.
#'      \item \strong{tab_fr_dateranges} is a dataframe of the start and end date
#'      ranges for the open and closed seasons by region, zone,
#'      sector, subsector, management type, and FR citation.
#'      \item \strong{heatmap_open} is a ggplot object of the percent days open
#'      within a month and year (NOTE: this plot applies to zone ALL).
#'      \item \strong{heatmap_closed} is a ggplot object of the percent days closed
#'      within a month and year (NOTE: this plot applies to zone ALL).
#'    }
#'
#'    The heatmap_open and heatmap_closed plots show the same information
#'    two different ways and color coding.
#'
#'
#' @examples process_closures(fishhist,
#'   spp = "SNAPPER, RED", region = "SOUTH ATLANTIC",
#'   start = "2000-01-01", end = "2025-12-31"
#' )
#'
#' @export
process_closures <- function(data = fishhist, spp, region, start, end) {
  # Filter data to species, region, and all management types related to
  # closures
  mh_spp_closure <- data |>
    dplyr::filter(
      COMMON_NAME_CLEAN == spp,
      REGION == region,
      MANAGEMENT_TYPECLEAN %in% c(
        "CLOSURE", "FISHING SEASON", "FISHING YEAR",
        "PROHIBITED SALE AND PURCHASE",
        "PROHIBITED SPECIES"
      ),
      STARTDATE_CLEAN <= as.Date(end, "%Y-%m-%d"),
      END_DATE >= as.Date(start, "%Y-%m-%d")
    ) |>
    dplyr::mutate(
      STARTDATE_CLEAN = pmax(STARTDATE_CLEAN, as.Date(start, "%Y-%m-%d")),
      END_DATE = pmin(END_DATE, as.Date(end, "%Y-%m-%d")),
      VALUE = dplyr::case_when(
        MANAGEMENT_TYPECLEAN %in% c("FISHING SEASON", "FISHING YEAR") ~ "OPEN",
        MANAGEMENT_TYPECLEAN == "PROHIBITED SALE AND PURCHASE" &
          FLAG == "YES" ~ "OPEN",
        MANAGEMENT_TYPECLEAN == "PROHIBITED SALE AND PURCHASE" &
          FLAG == "NO" ~ "CLOSE",
        MANAGEMENT_TYPECLEAN == "PROHIBITED SPECIES" ~ "CLOSE",
        TRUE ~ VALUE
      )
    ) |>
    dplyr::arrange(SECTOR_CLEAN, STARTDATE_CLEAN)

  if (nrow(mh_spp_closure) == 0) {
    stop("Check filters")
  }

  unique_types <- unique(mh_spp_closure$MANAGEMENT_TYPECLEAN)

  # Process all types dynamically (long form)
  start <- Sys.time()
  all_spp_closures <- purrr::map_dfr(unique_types,
                                     ~ expand_status(mh_spp_closure, .x))
  runtime <- Sys.time() - start
  runtime

  spp_closure_story <- all_spp_closures |>
    dplyr::select(
      FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
      SUBSECTOR_CLEAN, date_sequence, MANAGEMENT_TYPECLEAN, FR_CITATION,
      TIME, VALUE
    ) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, date_sequence) |>
    # Keep ONLY the row with the most recent FR_CITATION within each group
    # IF there are two then take the CLOSE record because that overwrites OPEN
    dplyr::slice_max(
      order_by = tibble::tibble(
        # First - pick the most recent citation first
        FR_CITATION,
        # Second - tie-breaker
        is_close = dplyr::case_when(
          # For fishing year, prioritize CLOSE over OPEN
          MANAGEMENT_TYPECLEAN == "FISHING YEAR" ~ (VALUE == "CLOSE"),
          # For closures, prioritize OPEN over CLOSE because these are the
          # reopenings
          MANAGEMENT_TYPECLEAN == "CLOSURE" ~ (VALUE == "OPEN"),
          # All else prioritize CLOSE
          TRUE ~ (VALUE == "CLOSE")
        )
      ),
      n = 1, with_ties = FALSE
    ) |>
    dplyr::ungroup()

  # Summarize the open and closed periods for each year
  summ_spp_closures <- spp_closure_story |>
    dplyr::mutate(YEAR = lubridate::year(date_sequence)) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, YEAR) |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                   SUBSECTOR_CLEAN, date_sequence) |>
    dplyr::mutate(
      change = VALUE != dplyr::lag(VALUE, default = dplyr::first(VALUE)) |
      YEAR != dplyr::lag(YEAR, default = dplyr::first(YEAR))) |>
    dplyr::mutate(group = cumsum(change)) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, YEAR, VALUE, group) |>
    dplyr::summarize(
      ndays = dplyr::n(),
      start = min(date_sequence),
      end = max(date_sequence),
      .groups = "drop"
    ) |>
    dplyr::mutate(VALUE = dplyr::case_when(
      is.na(VALUE) ~ "OPEN",
      TRUE ~ VALUE
    )) |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                   SUBSECTOR_CLEAN, YEAR, start)

  # Summarize the number of days open per year
  summ_days_open <- summ_spp_closures |>
    dplyr::filter(VALUE == "OPEN") |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, YEAR) |>
    dplyr::summarize(Days_Open = sum(ndays), .groups = "drop") |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                   SUBSECTOR_CLEAN, YEAR)

  # Combine sector and subsector for plot label
  summ_days_open_plotlab <- summ_days_open |> dplyr::mutate(
    ZONE_SECT_SUBSECT = paste0(
      "Zone (", ZONE_CLEAN, ") ",
      "Sector (", SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN, ")")
    )
  # Plot number of Days Open
  plot_days_open <- ggplot2::ggplot(summ_days_open_plotlab,
    ggplot2::aes(x = YEAR, y = Days_Open)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(
      ggplot2::aes(label = Days_Open), vjust = -0.5, size = 2.5) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_x_continuous(
      n.breaks = (max(summ_days_open$YEAR) - min(summ_days_open$YEAR)) / 3) +
    ggplot2::labs(x = "", y = "Number of Days Open") +
    ggplot2::facet_wrap(~ZONE_SECT_SUBSECT, ncol = 1) +
    ggplot2::theme_bw()

  # Summarize table for start-end dates by management type and values
  mtype_fr_tab <- spp_closure_story |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                   SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, FR_CITATION, MANAGEMENT_TYPECLEAN,
                    VALUE) |>
    # Create a gap identifier column (increments when dates are not consecutive)
    dplyr::mutate(
      date_gap = date_sequence > (dplyr::lag(date_sequence) + 1),
      date_gap = ifelse(is.na(date_gap), FALSE, date_gap),
      # cumsum() creates a running ID (e.g., 77 FR 51939 episode 1
      # for 9/14-9/16, episode 2 for 9/21-9/23)
      episode_id = cumsum(date_gap)
    ) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN,
                    SUBSECTOR_CLEAN, FR_CITATION, MANAGEMENT_TYPECLEAN, VALUE,
                    episode_id) |>
    dplyr::summarise(
      START = min(date_sequence),
      END = max(date_sequence), .groups = "drop"
    ) |>
    dplyr::arrange(SECTOR_CLEAN, ZONE_CLEAN, START) |>
    # For this summary table change closures - value (open) to REOPENING
    dplyr::mutate(MANAGEMENT_TYPECLEAN = dplyr::case_when(
      VALUE == "OPEN" & MANAGEMENT_TYPECLEAN == "CLOSURE" ~ "REOPENING",
      TRUE ~ MANAGEMENT_TYPECLEAN
    )) |>
    dplyr::select(-VALUE)


  # Heatmap of Days Open (ZONE ALL)
  heatmap_o <- spp_closure_story |>
    dplyr::mutate(
      date_clean = as.Date(date_sequence),
      year = lubridate::year(date_clean),
      month = lubridate::month(date_clean, label = TRUE, abbr = TRUE),
      # Calculate days in that specific month (handles leap years
      # automatically)
      days_in_m = lubridate::days_in_month(date_clean)
    ) |>
    dplyr::filter(ZONE_CLEAN == "ALL") |>
    dplyr::group_by(SECTOR_CLEAN, SUBSECTOR_CLEAN, year, month) |>
    # Count open days
    dplyr::summarise(
      n_open = sum(VALUE == "OPEN", na.rm = TRUE),
      total_days = max(days_in_m), # Get total days in month
      .groups = "drop"
    ) |>
    # Ensure all year x month grid combinations exist for every sector
    tidyr::complete(tidyr::nesting(SECTOR_CLEAN, SUBSECTOR_CLEAN), year,
      month = factor(month.abb, levels = month.abb, ordered = TRUE),
      fill = list(n_open = 0)
    ) |>
    dplyr::mutate( # Recalculate total_days for filled rows if needed
      total_days = ifelse(is.na(total_days),
                          lubridate::days_in_month(as.Date(
                            paste(year, month, "01", sep = "-"), "%Y-%b-%d")),
                          total_days),
      # Calculate proportion of open days (0.0 to 1.0)
      prop_open = n_open / total_days,
      SECT_SUBSECT = paste0(SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN)
    )

  plot_heatmap_o <- ggplot2::ggplot(heatmap_o,
                                    ggplot2::aes(
                                      x = factor(month, levels = month.abb),
                                      y = year,
                                      fill = prop_open)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(
      option = "magma", name = "% Days Open",
      direction = -1,
      labels = scales::percent,
      limits = c(0, 1)) +
    ggplot2::scale_y_continuous(
      trans = "reverse", expand = c(0, 0),
      n.breaks = (max(heatmap_o$year) - min(heatmap_o$year)) / 3) +
    ggplot2::facet_wrap(~SECT_SUBSECT, axes = "all", drop = TRUE) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      legend.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
      legend.box.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
      legend.position = "bottom"
    )

  # Heatmap of Days Closed (ZONE ALL)
  heatmap_c <- spp_closure_story |>
    dplyr::filter(VALUE == "CLOSE", ZONE_CLEAN == "ALL") |>
    dplyr::mutate(
      date_clean = as.Date(date_sequence),
      year = lubridate::year(date_clean),
      month = lubridate::month(date_clean, label = TRUE, abbr = TRUE)
    ) |>
    dplyr::group_by(SECTOR_CLEAN, SUBSECTOR_CLEAN, year, month) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    # Complete all year/month combinations
    tidyr::complete(tidyr::nesting(SECTOR_CLEAN, SUBSECTOR_CLEAN),
      year = min(
        lubridate::year(spp_closure_story$date_sequence)):max(
          lubridate::year(spp_closure_story$date_sequence)),
      # Keeps factor ordering consistent
      month = factor(month.abb, levels = month.abb, ordered = TRUE),
      fill = list(n = 0)
    ) |>
    dplyr::mutate(# Construct a dummy first-of-month date to compute total days
      # per month/year accurately
      month_date = as.Date(paste(year, match(month, month.abb), "01",
                                 sep = "-")),
      days_in_m = lubridate::days_in_month(month_date),
      # Calculate proportion of closed days (0.0 to 1.0)
      prop_closed = n / days_in_m,
      SECT_SUBSECT = paste0(SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN)
    )

  plot_heatmap_c <- ggplot2::ggplot(heatmap_c,
                                    ggplot2::aes(
                                      x = factor(month, levels = month.abb),
                                      y = year,
                                      fill = prop_closed)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(
      option = "magma",
      name = "% Days Closed",
      labels = scales::percent,
      limits = c(0, 1),
      na.value = "white",
      direction = -1) +
    ggplot2::scale_y_continuous(
      trans = "reverse",
      expand = c(0, 0),
      n.breaks = (max(heatmap_c$year) - min(heatmap_c$year)) / 3) +
    ggplot2::facet_wrap(~SECT_SUBSECT, axes = "all", drop = TRUE) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
      strip.text = ggplot2::element_text(face = "bold", size = 11),
      legend.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
      legend.box.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
      legend.position = "bottom"
    )

  # Save everything as a list
  out <- list(spp_closure_story, summ_days_open, plot_days_open,
              mtype_fr_tab, plot_heatmap_o, plot_heatmap_c)
  names(out) <- c("data", "tab_sum_days_open", "plot_days_open",
                  "tab_fr_dateranges", "heatmap_open", "heatmap_closed")
  return(out)
}
