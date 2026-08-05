#' Number of FRs published by management type and species
#'
#' @param data default=fishhist
#' @param species Filter to species of interest (common name).
#' @param region Filter to region of interest
#' @return A list
#'
#' @examples process_closures(fishhist, spp = "SNAPPER, RED", region = "SOUTH ATLANTIC")

process_closures <- function(data=fishhist, spp, region){

  # Filter data to species, region, and all management types related to closures
  mh_spp_closure <- fishhist |>
    dplyr::filter(COMMON_NAME_USE == 'AMBERJACK, GREATER',
           REGION == 'SOUTH ATLANTIC',
           MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', #'FISHING YEAR',
                                      'PROHIBITED SALE AND PURCHASE',
                                      'PROHIBITED SPECIES')) |>
    dplyr::mutate(VALUE = dplyr::case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                             MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                             TRUE ~ VALUE)) |>
    dplyr::arrange(SECTOR_USE, START_DATE2)

  unique_types <- unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

  # Process all types dynamically (long form)
  all_spp_closures <- purrr::map_dfr(unique_types, ~ expand_status(mh_spp_closure, .x))

  spp_closure_story <- all_spp_closures |>
    dplyr::select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence,
           MANAGEMENT_TYPE_USE, FR_CITATION, TIME, VALUE) |>
    dplyr::group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) |>
    # Keep ONLY the row with the most recent FR_CITATION within each group
    dplyr::slice_max(order_by = FR_CITATION, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()

  # Summarize the open and closed periods for each year
  summ_spp_closures <- spp_closure_story |>
    dplyr::mutate(YEAR = lubridate::year(date_sequence)) |>
    dplyr::group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) |>
    dplyr::arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) |>
    dplyr::mutate(change = VALUE != dplyr::lag(VALUE, default = dplyr::first(VALUE)) |
             YEAR != dplyr::lag(YEAR, default = dplyr::first(YEAR))) |>
    dplyr::mutate(group = cumsum(change)) |>
    dplyr::group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) |>
    dplyr::summarize(ndays = dplyr::n(),
              start = min(date_sequence),
              end = max(date_sequence),
              .groups = 'drop') |>
    dplyr::mutate(VALUE = dplyr::case_when(is.na(VALUE) ~ "OPEN",
                             TRUE ~ VALUE)) |>
    dplyr::arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

  # Summarize the number of days open per year
  summ_days_open <- summ_spp_closures |>
    dplyr::filter(VALUE == "OPEN") |>
    dplyr::group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) |>
    dplyr::summarize(Days_Open = sum(ndays), .groups = "drop") |>
    dplyr::arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

  # Heatmap of Days Open (ZONE ALL)
  heatmap_o <- spp_closure_story |> dplyr::filter(VALUE=='OPEN', ZONE_USE == 'ALL') |>
    dplyr::mutate(date_clean = as.Date(date_sequence),
                  year = lubridate::year(date_clean),
                  month = lubridate::month(date_clean, label = TRUE, abbr = TRUE)) |>
    dplyr::group_by(SECTOR_USE, SUBSECTOR_USE, year, month) |> dplyr::summarise(n = dplyr::n()) |> dplyr::ungroup() |>
    dplyr::mutate(month = factor(month, levels = month.abb))

  plot_heatmap_o <- ggplot2::ggplot(heatmap_o, ggplot2::aes(x = year, y = factor(month, levels = rev(month.abb)), fill = n)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "viridis", name = "Days Open", na.value = "white") +
    ggplot2::scale_x_continuous(expand = c(0, 0), n.breaks = 20) +
    ggplot2::facet_grid(SUBSECTOR_USE~SECTOR_USE) +
    ggplot2::labs(x = "Year",y = "Month") +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
                   strip.text = ggplot2::element_text(face = "bold", size = 11),
                   legend.position = "bottom" )

  # Heatmap of Days Closed
  heatmap_c <- spp_closure_story |> dplyr::filter(VALUE=='CLOSE', ZONE_USE == 'ALL') |>
    dplyr::mutate(date_clean = as.Date(date_sequence),
                  year = lubridate::year(date_clean),
                  month = lubridate::month(date_clean, label = TRUE, abbr = TRUE)) |>
    dplyr::group_by(SECTOR_USE, SUBSECTOR_USE, year, month) |> dplyr::summarise(n = dplyr::n()) |> dplyr::ungroup() |>
    dplyr::mutate(month = factor(month, levels = month.abb)) |>
    tidyr::complete(SECTOR_USE, SUBSECTOR_USE,
                    year = min(lubridate::year(spp_closure_story$date_sequence)):max(lubridate::year(spp_closure_story$date_sequence)),
                    month,
                    fill = list(n = 0))

  plot_heatmap_c <- ggplot2::ggplot(heatmap_c, ggplot2::aes(x = year, y = factor(month, levels = rev(month.abb)), fill = n)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "viridis", name = "Days Closed", na.value = "white", direction=-1) +
    ggplot2::scale_x_continuous(expand = c(0, 0), n.breaks = 20) +
    ggplot2::facet_grid(SUBSECTOR_USE~SECTOR_USE) +
    ggplot2::labs(x = "Year",y = "Month") +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
                   strip.text = ggplot2::element_text(face = "bold", size = 11),
                   legend.position = "bottom" )

  # Save everything as a list
  out <- list(spp_closure_story, summ_days_open, plot_heatmap_o, plot_heatmap_c)
  names(out) <- c("data", "sum_days_open", "heatmap_open", "heatmap_closed")
  return(out)

}



