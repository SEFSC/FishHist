#' Process closures to expand and incorporate all related mtypes for closures to know for a single day whether the fishery is open or closed
#'
#' @param data default=fishhist
#' @param species Filter to species of interest (common name).
#' @param region Filter to region of interest
#' @return A list
#'
#' @examples process_closures(fishhist, spp = "SNAPPER, RED", region = "SOUTH ATLANTIC")

process_closures <- function(data=fishhist, spp, region){

  # Filter data to species, region, and all management types related to closures
  mh_spp_closure <- data |>
    dplyr::filter(COMMON_NAME_CLEAN == spp,
           REGION == region,
           MANAGEMENT_TYPECLEAN %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                      'PROHIBITED SALE AND PURCHASE',
                                      'PROHIBITED SPECIES')) |>
    dplyr::mutate(VALUE = dplyr::case_when(MANAGEMENT_TYPECLEAN %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                             MANAGEMENT_TYPECLEAN == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                             MANAGEMENT_TYPECLEAN == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                             MANAGEMENT_TYPECLEAN == 'PROHIBITED SPECIES' ~ 'CLOSE',
                             TRUE ~ VALUE)) |>
    dplyr::arrange(SECTOR_CLEAN, STARTDATE_CLEAN)

  if(nrow(mh_spp_closure) == 0) {stop("Check filters")}

  unique_types <- unique(mh_spp_closure$MANAGEMENT_TYPECLEAN)

  # Process all types dynamically (long form)
  start = Sys.time()
  all_spp_closures <- purrr::map_dfr(unique_types, ~ expand_status(mh_spp_closure, .x))
  runtime = Sys.time() - start
  runtime

  spp_closure_story <- all_spp_closures |>
    dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, date_sequence,
           MANAGEMENT_TYPECLEAN, FR_CITATION, TIME, VALUE) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, date_sequence) |>
    # Keep ONLY the row with the most recent FR_CITATION within each group
    # IF there are two then take the CLOSE record because that overwrites OPEN
    dplyr::slice_max(order_by = tibble::tibble(
        # Primary sort: Always pick the most recent citation first
        FR_CITATION,
        # Secondary sort: Conditional tie-breaker
        is_close = dplyr::case_when(
          # For 'fishing year', prioritize CLOSE over OPEN (CLOSE = TRUE, which ranks higher than FALSE)
          MANAGEMENT_TYPECLEAN == "FISHING YEAR" ~ (VALUE == "CLOSE"),
          # For closures / other types, prioritize OPEN over CLOSE if applicable
          MANAGEMENT_TYPECLEAN == "CLOSURE" ~ (VALUE == "OPEN"),
          # Default fallback for any other management types
          TRUE ~ (VALUE == "CLOSE"))),
        n = 1,with_ties = FALSE) |>
    dplyr::ungroup()

  # Summarize the open and closed periods for each year
  summ_spp_closures <- spp_closure_story |>
    dplyr::mutate(YEAR = lubridate::year(date_sequence)) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, YEAR) |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, date_sequence) |>
    dplyr::mutate(change = VALUE != dplyr::lag(VALUE, default = dplyr::first(VALUE)) |
             YEAR != dplyr::lag(YEAR, default = dplyr::first(YEAR))) |>
    dplyr::mutate(group = cumsum(change)) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, YEAR, VALUE, group) |>
    dplyr::summarize(ndays = dplyr::n(),
              start = min(date_sequence),
              end = max(date_sequence),
              .groups = 'drop') |>
    dplyr::mutate(VALUE = dplyr::case_when(is.na(VALUE) ~ "OPEN",
                             TRUE ~ VALUE)) |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, YEAR, start)

  # Summarize the number of days open per year
  summ_days_open <- summ_spp_closures |>
    dplyr::filter(VALUE == "OPEN") |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, YEAR) |>
    dplyr::summarize(Days_Open = sum(ndays), .groups = "drop") |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, YEAR)

  # Plot number of Days Open
  plot_days_open <- ggplot2::ggplot(summ_days_open |> dplyr::mutate(SECT_SUBSECT = paste0(SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN)),
                                    ggplot2::aes(x=YEAR, y=Days_Open)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = Days_Open), vjust = -0.5, size = 2.5) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::scale_x_continuous(n.breaks = (max(summ_days_open$YEAR) - min(summ_days_open$YEAR))/3) +
    ggplot2::labs(x="", y= "Number of Days Open") +
    ggplot2::facet_wrap(~SECT_SUBSECT, ncol=1) + ggplot2::theme_bw()

  # Summarize table for start-end dates by management type and values
  mtype_fr_tab <- spp_closure_story |>
    dplyr::arrange(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, FR_CITATION, MANAGEMENT_TYPECLEAN, VALUE) |>
    # Create a gap identifier column (increments when dates are not consecutive)
    dplyr::mutate(date_gap = date_sequence > (dplyr::lag(date_sequence) + 1),
                  date_gap = ifelse(is.na(date_gap), FALSE, date_gap),
                  # cumsum() creates a running ID (e.g., 77 FR 51939 episode 1 for 9/14-9/16, episode 2 for 9/21-9/23)
                  episode_id = cumsum(date_gap)) |>
    dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, FR_CITATION, MANAGEMENT_TYPECLEAN, VALUE, episode_id) |>
    dplyr::summarise(START = min(date_sequence),
                     END = max(date_sequence), .groups = "drop") |>
    dplyr::arrange(SECTOR_CLEAN, ZONE_CLEAN, START) |>
    # For this summary table change closures - value (open) to REOPENING
    dplyr::mutate(MANAGEMENT_TYPECLEAN = dplyr::case_when(VALUE == 'OPEN' & MANAGEMENT_TYPECLEAN == 'CLOSURE' ~ 'REOPENING',
                                                          TRUE ~ MANAGEMENT_TYPECLEAN)) |> dplyr::select(-VALUE)


  # Heatmap of Days Open (ZONE ALL)
  heatmap_o <- spp_closure_story |> dplyr::filter(VALUE=='OPEN', ZONE_CLEAN == 'ALL') |>
    dplyr::mutate(date_clean = as.Date(date_sequence),
                  year = lubridate::year(date_clean),
                  month = lubridate::month(date_clean, label = TRUE, abbr = TRUE)) |>
    dplyr::group_by(SECTOR_CLEAN, SUBSECTOR_CLEAN, year, month) |> dplyr::summarise(n = dplyr::n()) |> dplyr::ungroup() |>
    dplyr::mutate(month = factor(month, levels = month.abb),
                  SECT_SUBSECT = paste0(SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN))

  plot_heatmap_o <- ggplot2::ggplot(heatmap_o, ggplot2::aes(x = year, y = factor(month, levels = rev(month.abb)), fill = n)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "magma", name = "Days Open", na.value = "white") +
    ggplot2::scale_x_continuous(expand = c(0, 0), n.breaks = (max(heatmap_o$year) - min(heatmap_o$year))/3) +
    ggplot2::facet_wrap(~SECT_SUBSECT, axes = "all", drop = TRUE) +
    ggplot2::labs(x = "",y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
                   strip.text = ggplot2::element_text(face = "bold", size = 11),
                   legend.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
                   legend.box.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
                   legend.position = "bottom" )

  # Heatmap of Days Closed (ZONE ALL)
  heatmap_c <- spp_closure_story |> dplyr::filter(VALUE=='CLOSE', ZONE_CLEAN == 'ALL') |>
    dplyr::mutate(date_clean = as.Date(date_sequence),
                  year = lubridate::year(date_clean),
                  month = lubridate::month(date_clean, label = TRUE, abbr = TRUE)) |>
    dplyr::group_by(SECTOR_CLEAN, SUBSECTOR_CLEAN, year, month) |> dplyr::summarise(n = dplyr::n()) |> dplyr::ungroup() |>
    dplyr::mutate(month = factor(month, levels = month.abb)) |>
    tidyr::complete(tidyr::nesting(SECTOR_CLEAN, SUBSECTOR_CLEAN), # return only combos that exist
                    year = min(lubridate::year(spp_closure_story$date_sequence)):max(lubridate::year(spp_closure_story$date_sequence)),
                    month,
                    fill = list(n = 0)) |>
    dplyr::mutate(SECT_SUBSECT = paste0(SECTOR_CLEAN, " - ", SUBSECTOR_CLEAN))

  plot_heatmap_c <- ggplot2::ggplot(heatmap_c, ggplot2::aes(x = year, y = factor(month, levels = rev(month.abb)), fill = n)) +
    ggplot2::geom_tile(color = "grey90", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(option = "magma", name = "Days Closed", na.value = "white", direction=-1) +
    ggplot2::scale_x_continuous(expand = c(0, 0), n.breaks = (max(heatmap_c$year) - min(heatmap_c$year))/3) +
    ggplot2::facet_wrap(~SECT_SUBSECT, axes = "all", drop = TRUE) +
    ggplot2::labs(x = "",y = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(panel.grid = ggplot2::element_line(color = "grey92", linewidth = 0.2),
                   strip.text = ggplot2::element_text(face = "bold", size = 11),
                   legend.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
                   legend.box.margin = ggplot2::margin(t = -10, r = 0, b = 0, l = 0),
                   legend.position = "bottom" )

  # Save everything as a list
  out <- list(spp_closure_story, summ_days_open, plot_days_open, mtype_fr_tab, plot_heatmap_o, plot_heatmap_c)
  names(out) <- c("data", "tab_sum_days_open", "plot_days_open", "tab_fr_dateranges", "heatmap_open", "heatmap_closed")
  return(out)

}



