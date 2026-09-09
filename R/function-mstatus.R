#' Function to expand dates based on management status
#'
#' @param x data default=fishhist
#' @param y Management Type Use
#' @return A dataframe
#'
#' @examples expand_status(fishhist, "FISHING YEAR")
#'

expand_status <- function(x, y) {

  # x = data frame
  # y = management type
  df = dplyr::filter(x, MANAGEMENT_TYPECLEAN == y,
              # only retain records that were in effect
              NEVERIMPLEMENTED == 0, REGREMOVED == 0) |>
    dplyr::group_by(CLUSTER_ID, FR_CITATION, ZONE_CLEAN) |>
    # Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time with different VALUE_TYPE or VALUE_RATE (MULTI_REG_VALUE)
    dplyr::mutate(MULTI_REG_VALUE = as.numeric(dplyr::n() > 1 & (dplyr::n_distinct(VALUE) | dplyr::n_distinct(VALUETYPE) > 1 | dplyr::n_distinct(VALUERATE) > 1)))

  if(nrow(df) == 0) {stop("no records for that management type")}

  # Still need to figure out how to run if there is no data for one mstatus
  # status may be once, seasonal, monthly recurring, weekly recurring, or daily
  mstats = unique(df$MANAGEMENT_STATUSCLEAN)

  start = Sys.time()
  if('ONCE' %in% mstats){
    # One-time event
    # df_one <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'ONCE') |>
    #   dplyr::filter(END_DATE >= STARTDATE_CLEAN) |>
    #   dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "1 day")) |>
    #   tidyr::unnest(date_sequence) |>
    #   dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
    #   dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
    #                                  date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
    #                                  TRUE ~ NA_character_)) |>
    #   dplyr::ungroup(0) |>
    #   dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
    #                 CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE, MULTI_REG_VALUE) |>
    #   dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
    #   dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
    #   # Retain only most recent FR if there are 2 records with the same date_sequence
    #   dplyr::mutate(num_records = dplyr::n(),
    #                 action_value = dplyr::if_else(num_records > 1 & MULTI_REG_VALUE != 1, "slice", "keep")) |>
    #   dplyr::filter(!(action_value == "slice")) |>
    #   dplyr::rename(CLUSTER_one = "CLUSTER_ID",
    #                 REG_ID_one = "REG_ID",
    #                 FR_CITATION_one = "FR_CITATION",
    #                 TIME_one = "TIME",
    #                 VALUE_one = "VALUE",
    #                 VALUEUNITS_one = "VALUEUNITS",
    #                 VALUETYPE_one = "VALUETYPE",
    #                 VALUERATE_one = "VALUERATE") |>
    #   dplyr::select(-num_records, -action_value)

    # 1. Row filtering
    df_one <- df[df$MANAGEMENT_STATUSCLEAN == "ONCE" &
                   !is.na(df$END_DATE) & !is.na(df$STARTDATE_CLEAN) &
                   df$END_DATE >= df$STARTDATE_CLEAN, ]

    # 2. Fast Date Sequence Expansion (Replaces map2 + unnest)
    n_days <- as.integer(df_one$END_DATE - df_one$STARTDATE_CLEAN) + 1L
    row_idx <- rep(seq_len(nrow(df_one)), times = n_days)

    # Replicate rows and calculate unnested dates vectorially
    df_one <- df_one[row_idx, ]
    day_offsets <- unlist(lapply(n_days, function(n) 0:(n - 1L)))
    df_one$date_sequence <- df_one$STARTDATE_CLEAN + day_offsets

    # 3. Vectorized TIME assignment (No grouping required!)
    df_one$TIME <- NA_character_
    is_start <- df_one$date_sequence == df_one$STARTDATE_CLEAN & !is.na(df_one$STARTTIME_CLEAN)
    is_end   <- df_one$date_sequence == df_one$END_DATE & !is.na(df_one$ENDTIME_CLEAN)

    df_one$TIME[is_start] <- df_one$STARTTIME_CLEAN[is_start]
    df_one$TIME[is_end]   <- df_one$ENDTIME_CLEAN[is_end]

    # 4. Column selection and ordering
    keep_cols <- c("FMP", "COMMON_NAME_CLEAN", "REGION", "ZONE_CLEAN",
                   "SECTOR_CLEAN", "SUBSECTOR_CLEAN", "MANAGEMENT_TYPECLEAN",
                   "date_sequence", "TIME", "CLUSTER_ID", "REG_ID",
                   "FR_CITATION", "VALUE", "VALUEUNITS", "VALUETYPE",
                   "VALUERATE", "MULTI_REG_VALUE")

    df_one <- df_one[, keep_cols]

    # Sort by date_sequence, then FR_CITATION descending
    ord <- order(df_one$date_sequence, df_one$FR_CITATION, decreasing = c(FALSE, TRUE), method = "radix")
    df_one <- df_one[ord, ]

    # 5. Fast Group Count & Slice Filtering (Replaces group_by + mutate + filter)
    grp_cols <- c("FMP", "COMMON_NAME_CLEAN", "REGION", "ZONE_CLEAN",
                  "SECTOR_CLEAN", "SUBSECTOR_CLEAN", "MANAGEMENT_TYPECLEAN", "date_sequence")

    # Generate group IDs and compute counts using fast C tabulate
    grp_key <- do.call(paste, c(df_one[grp_cols], list(sep = "\r")))
    grp_id  <- match(grp_key, unique(grp_key))
    num_records <- tabulate(grp_id)[grp_id]

    # Filter directly without building intermediate flag columns
    slice_mask <- num_records > 1 & (is.na(df_one$MULTI_REG_VALUE) | df_one$MULTI_REG_VALUE != 1)
    df_one <- df_one[!slice_mask, ]

    # 6. Column Renaming
    rename_map_one <- c(CLUSTER_ID = "CLUSTER_one",
                    REG_ID  = "REG_ID_one",
                    FR_CITATION = "FR_CITATION_one",
                    TIME = "TIME_one",
                    VALUE = "VALUE_one",
                    VALUEUNITS = "VALUEUNITS_one",
                    VALUETYPE = "VALUETYPE_one",
                    VALUERATE = "VALUERATE_one")

    old_names_one <- names(df_one)
    names(df_one) <- ifelse(old_names_one %in% names(rename_map_one),
                            rename_map_one[old_names_one],
                            old_names_one)

  } else {
    df_one <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                         SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                         date_sequence = as.Date(character()), CLUSTER_one = numeric(), REG_ID_one = numeric(),
                         FR_CITATION_one = character(), TIME_one = character(), VALUE_one = character(), VALUEUNITS_one = character(),
                         VALUETYPE_one = character(), VALUERATE_one = character())
  }
  runtime = Sys.time() - start
  runtime

  start = Sys.time()
  if('SEASONAL' %in% mstats){
    # Seasonal
    # df_seasonal <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'SEASONAL') |>
    #   dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
    #   tidyr::unnest(date_sequence) |>
    #   dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
    #   dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
    #                           date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
    #                           TRUE ~ NA_character_)) |>
    #   dplyr::ungroup() |>
    #   dplyr::mutate(STARTYEAR_expand = lubridate::year(date_sequence),
    #          END_YEAR_expand = lubridate::year(date_sequence)) |>
    #   dplyr::mutate(START_DATE_EXPAND = as.Date(paste(STARTYEAR_expand, STARTMONTH, STARTDAY_CLEAN, sep = "-")),
    #          END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, ENDMONTH_CLEAN, ENDDAY_CLEAN, sep = "-"))) |>
    #   # Create START_DATE_EXPAND2 & END_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the enxt year
    #   # Create START_DATE_EXPAND_FINAL & END_DATE_EXPAND_FINAL to choose the correct START_DATE_EXPAND and END_DATE_EXPAND for each set of circumstances
    #   dplyr::mutate(START_DATE_EXPAND2 = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ START_DATE_EXPAND - lubridate::years(1),
    #                                         TRUE ~ START_DATE_EXPAND),
    #          END_DATE_EXPAND2 = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ END_DATE_EXPAND + lubridate::years(1),
    #                                       TRUE ~ END_DATE_EXPAND),
    #          START_DATE_EXPAND_FINAL = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence <= START_DATE_EXPAND ~ START_DATE_EXPAND2,
    #                                              TRUE ~ START_DATE_EXPAND),
    #          END_DATE_EXPAND_FINAL = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence >= START_DATE_EXPAND & date_sequence >= END_DATE_EXPAND ~ END_DATE_EXPAND2,
    #                                            TRUE ~ END_DATE_EXPAND)) |>
    #   # Remove date_sequence records outside expand range
    #   dplyr::filter(date_sequence >= START_DATE_EXPAND_FINAL,
    #          END_DATE_EXPAND_FINAL >= date_sequence) |>
    #   dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
    #          CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE) |>
    #   dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
    #   dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
    #   # Retain only most recent FR if there are 2 records with the same date_sequence
    #   dplyr::slice(1) |>
    #   dplyr::rename(CLUSTER_seasonal = "CLUSTER_ID",
    #          REG_ID_seasonal = "REG_ID",
    #          FR_CITATION_seasonal = "FR_CITATION",
    #          TIME_seasonal = "TIME",
    #          VALUE_seasonal = "VALUE",
    #          VALUEUNITS_seasonal = "VALUEUNITS",
    #          VALUETYPE_seasonal = "VALUETYPE",
    #          VALUERATE_seasonal = "VALUERATE")

    # 1. Initial Filtering
    df_seasonal <- df[df$MANAGEMENT_STATUSCLEAN == "SEASONAL" &
                        !is.na(df$END_DATE) & !is.na(df$STARTDATE_CLEAN) &
                        df$END_DATE >= df$STARTDATE_CLEAN, ]

    if (nrow(df_seasonal) > 0) {

      # 2. Fast Date Expansion (Replaces map2 + unnest)
      n_days <- as.integer(df_seasonal$END_DATE - df_seasonal$STARTDATE_CLEAN) + 1L
      row_idx <- rep(seq_len(nrow(df_seasonal)), times = n_days)

      df_seasonal <- df_seasonal[row_idx, ]
      day_offsets <- unlist(lapply(n_days, function(n) 0:(n - 1L)))
      df_seasonal$date_sequence <- df_seasonal$STARTDATE_CLEAN + day_offsets

      # 3. Vectorized TIME Assignment
      df_seasonal$TIME <- NA_character_
      is_start <- df_seasonal$date_sequence == df_seasonal$STARTDATE_CLEAN & !is.na(df_seasonal$STARTTIME_CLEAN)
      is_end   <- df_seasonal$date_sequence == df_seasonal$END_DATE & !is.na(df_seasonal$ENDTIME_CLEAN)

      df_seasonal$TIME[is_start] <- df_seasonal$STARTTIME_CLEAN[is_start]
      df_seasonal$TIME[is_end]   <- df_seasonal$ENDTIME_CLEAN[is_end]

      # 4. Fast Year-by-Year Seasonal Date Computations
      # Extract current year for EACH date_sequence as integer (no string sprintf!)
      curr_year <- as.integer(format(df_seasonal$date_sequence, "%Y"))

      # Construct dates fast using paste + as.Date on vectorized inputs
      s_m <- as.integer(df_seasonal$STARTMONTH)
      s_d <- as.integer(df_seasonal$STARTDAY_CLEAN)
      e_m <- as.integer(df_seasonal$ENDMONTH_CLEAN)
      e_d <- as.integer(df_seasonal$ENDDAY_CLEAN)

      start_dates <- as.Date(paste(curr_year, s_m, s_d, sep = "-"), format = "%Y-%m-%d")
      end_dates   <- as.Date(paste(curr_year, e_m, e_d, sep = "-"), format = "%Y-%m-%d")

      # Handle year wrapping (e.g. Nov 1 to Feb 28 across year boundaries)
      is_wrapped <- start_dates > end_dates

      start_dates2 <- start_dates
      end_dates2   <- end_dates

      # Fast year shift without slow POSIXlt object allocations
      if (any(is_wrapped, na.rm = TRUE)) {
        start_dates2[is_wrapped] <- as.Date(paste(curr_year[is_wrapped] - 1L, s_m[is_wrapped], s_d[is_wrapped], sep = "-"), format = "%Y-%m-%d")
        end_dates2[is_wrapped]   <- as.Date(paste(curr_year[is_wrapped] + 1L, e_m[is_wrapped], e_d[is_wrapped], sep = "-"), format = "%Y-%m-%d")
      }

      # Compute final expanded start/end boundaries
      start_final <- start_dates
      mask_start  <- is_wrapped & (df_seasonal$date_sequence <= start_dates)
      start_final[mask_start] <- start_dates2[mask_start]

      end_final  <- end_dates
      mask_end   <- is_wrapped & (df_seasonal$date_sequence >= start_dates) & (df_seasonal$date_sequence >= end_dates)
      end_final[mask_end] <- end_dates2[mask_end]

      # 5. Filter out-of-range date records
      keep_rows <- df_seasonal$date_sequence >= start_final & df_seasonal$date_sequence <= end_final
      df_seasonal <- df_seasonal[keep_rows, ]

      # 6. Column Selection
      keep_cols <- c("FMP", "COMMON_NAME_CLEAN", "REGION", "ZONE_CLEAN",
                     "SECTOR_CLEAN", "SUBSECTOR_CLEAN", "MANAGEMENT_TYPECLEAN",
                     "date_sequence", "TIME", "CLUSTER_ID", "REG_ID",
                     "FR_CITATION", "VALUE", "VALUEUNITS", "VALUETYPE", "VALUERATE")

      df_seasonal <- df_seasonal[, keep_cols]

      # 7. Fast Radix Sorting (date_sequence ASC, FR_CITATION DESC)
      ord <- order(df_seasonal$date_sequence, df_seasonal$FR_CITATION, decreasing = c(FALSE, TRUE), method = "radix")
      df_seasonal <- df_seasonal[ord, ]

      # 8. Fast Group Deduplication
      grp_cols <- c("FMP", "COMMON_NAME_CLEAN", "REGION", "ZONE_CLEAN",
                    "SECTOR_CLEAN", "SUBSECTOR_CLEAN", "MANAGEMENT_TYPECLEAN", "date_sequence")

      grp_key <- do.call(paste, c(df_seasonal[grp_cols], list(sep = "\r")))
      df_seasonal <- df_seasonal[!duplicated(grp_key), ]

      # 9. Column Renaming
      rename_map_seasonal <- c(
        CLUSTER_ID   = "CLUSTER_seasonal",
        REG_ID       = "REG_ID_seasonal",
        FR_CITATION  = "FR_CITATION_seasonal",
        TIME         = "TIME_seasonal",
        VALUE        = "VALUE_seasonal",
        VALUEUNITS   = "VALUEUNITS_seasonal",
        VALUETYPE    = "VALUETYPE_seasonal",
        VALUERATE    = "VALUERATE_seasonal"
      )

      old_names_seasonal <- names(df_seasonal)
      names(df_seasonal) <- ifelse(old_names_seasonal %in% names(rename_map_seasonal),
                                   rename_map_seasonal[old_names_seasonal],
                                   old_names_seasonal)
    }

  } else {
    df_seasonal <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                              SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                              date_sequence = as.Date(character()), CLUSTER_seasonal = numeric(), REG_ID_seasonal = numeric(),
                              FR_CITATION_seasonal = character(), TIME_seasonal = character(), VALUE_seasonal = character(), VALUEUNITS_seasonal = character(),
                              VALUETYPE_seasonal = character(), VALUERATE_seasonal = character())
  }
  runtime = Sys.time() - start
  runtime

  start = Sys.time()
  if('MONTHLY RECURRING' %in% mstats){
    # Monthly
    df_monthly <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'MONTHLY RECURRING') |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
                              date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
                              lubridate::day(date_sequence) == STARTDAY_CLEAN ~ STARTTIME_CLEAN,
                              lubridate::day(date_sequence) == ENDDAY_CLEAN ~ ENDTIME_CLEAN,
                              TRUE ~ NA_character_)) |>
      dplyr::ungroup() |>
      # Extract year from date_sequence
      dplyr::mutate(STARTYEAR_expand = lubridate::year(date_sequence),
             END_YEAR_expand = lubridate::year(date_sequence),
             # Create STARTMONTH_expand and ENDMONTH_CLEAN_expand to eventually create the dates that are the bounds of which expansion dates should still be included
             # The conditions address cases where the monthly recurring goes from the middle of one month to the start of next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is a smaller value than the STARTDAY_CLEAN and the month of the date_sequence is not January,
             # then the STARTMONTH_expand should be one month less than the month of the date_sequence
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is a smaller value than the STARTDAY_CLEAN but the month of the date_sequence is January,
             # then the STARTMONTH_expand should be December
             # Otherwise, the STARTMONTH_EXPAND should reflect the month of the date_sequence
             STARTMONTH_expand = dplyr::case_when(ENDDAY_CLEAN < STARTDAY_CLEAN & lubridate::day(date_sequence) < STARTDAY_CLEAN & lubridate::month(date_sequence) != 1 ~ lubridate::month(date_sequence) - 1,
                                            ENDDAY_CLEAN < STARTDAY_CLEAN & lubridate::day(date_sequence) < STARTDAY_CLEAN & lubridate::month(date_sequence) == 1 ~ 12,
                                            TRUE ~ lubridate::month(date_sequence)),
             # The conditions address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is a smaller value than the STARTDAY_CLEAN and the month of the date_sequence is not January
             # then the ENDMONTH_CLEAN_expand should be the month of the date_sequence
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN value and the month of the date_sequence + 1 month is less than or equal to 12 (December),
             # then the ENDMONTH_CLEAN_expand should be the month of the date_sequence plus one month
             # If the ENDMONTH_CLEAN_expand value is smaller than the STARTDAY_CLEAN and the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN and the month of the date_sequence + 1 is equal to 13,
             # then the ENDMONTH_CLEAN_expand should be January
             # Otherwise, the month of the date_sequence should be used
             ENDMONTH_CLEAN_expand = dplyr::case_when(ENDDAY_CLEAN < STARTDAY_CLEAN & lubridate::day(date_sequence) < STARTDAY_CLEAN & lubridate::month(date_sequence) != 1 ~ lubridate::month(date_sequence),
                                          ENDDAY_CLEAN < STARTDAY_CLEAN & lubridate::day(date_sequence) >= STARTDAY_CLEAN & lubridate::month(date_sequence) + 1 <= 12 ~ lubridate::month(date_sequence) + 1,
                                          ENDDAY_CLEAN < STARTDAY_CLEAN & lubridate::day(date_sequence) >= STARTDAY_CLEAN & lubridate::month(date_sequence) + 1 == 13 ~ 1,
                                          TRUE ~ lubridate::month(date_sequence)),
             # Adjust END_YEAR_expand to address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN and the ENDMONTH_CLEAN is January & the month of the date_sequence is December, then the END_YEAR_expand should be changed to the following year
             # If the day of the date_sequence is less than the STARTDAY_CLEAN and the ENDMONTH_CLEAN is January and the month of the date_sequence is December, then use the STARTYEAR_expand
             # Otherwise, use the END_YEAR_expand
             END_YEAR_expand = dplyr::case_when(lubridate::day(date_sequence) >= STARTDAY_CLEAN & ENDMONTH_CLEAN == 1 & lubridate::month(date_sequence) == 12 ~ END_YEAR_expand + 1,
                                         lubridate::day(date_sequence) < STARTDAY_CLEAN & ENDMONTH_CLEAN == 1 & lubridate::month(date_sequence) == 12 ~ STARTYEAR_expand,
                                         TRUE ~ END_YEAR_expand)) |>
      # only include months within the start/end month range
      # when monthly recurring, the start/end month range refers to the first and last month where the recurring closure applies
      # i.e. start month = 2 and end month = 11
      # mean recurring closures start in Feb and end in Nov
      # can't use ENDMONTH_CLEAN_expand in filter for cases when the recurring closures spans 2 months (i.e. 15th -1st)
      #filter(STARTMONTH_expand >= STARTMONTH &
      #        STARTMONTH_expand <= ENDMONTH_CLEAN) |>
      dplyr::mutate(START_DATE_EXPAND = as.Date(paste(STARTYEAR_expand, STARTMONTH_expand, STARTDAY_CLEAN, sep = "-")),
             END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, ENDMONTH_CLEAN_expand, ENDDAY_CLEAN, sep = "-"))) |>
      # Remove date_sequence records outside expand range
      dplyr::filter(date_sequence == START_DATE_EXPAND |
               date_sequence == END_DATE_EXPAND |
               date_sequence > START_DATE_EXPAND & date_sequence < END_DATE_EXPAND) |>
      dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
             CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE) |>
      dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
      # Retain only most recent FR if there are 2 records with the same date_sequence
      dplyr::slice(1) |>
      dplyr::rename(CLUSTER_monthly = "CLUSTER_ID",
             REG_ID_monthly = "REG_ID",
             FR_CITATION_monthly = "FR_CITATION",
             TIME_monthly = "TIME",
             VALUE_monthly = "VALUE",
             VALUEUNITS_monthly = "VALUEUNITS",
             VALUETYPE_monthly = "VALUETYPE",
             VALUERATE_monthly = "VALUERATE")
  } else {
    df_monthly <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                             SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                             date_sequence = as.Date(character()), CLUSTER_monthly = numeric(), REG_ID_monthly = numeric(),
                             FR_CITATION_monthly = character(), TIME_monthly = character(), VALUE_monthly = character(), VALUEUNITS_monthly = character(),
                             VALUETYPE_monthly = character(), VALUERATE_monthly = character())
  }
  runtime = Sys.time() - start
  runtime

  start = Sys.time()
  if('WEEKLY RECURRING' %in% mstats){
    # Weekly
    df_weekly <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'WEEKLY RECURRING') |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      # need to get expanded days of the week to match
      dplyr::mutate(STARTDAY_CLEAN_OF_WEEK_EXPAND = dplyr::case_when(
       START_DAYOF_WEEK_CLEAN == "MONDAY" ~ "Mon",
       START_DAYOF_WEEK_CLEAN == "TUESDAY" ~ "Tue",
       START_DAYOF_WEEK_CLEAN == "WEDNESDAY" ~ "Wed",
       START_DAYOF_WEEK_CLEAN == "THURSDAY" ~ "Thu",
       START_DAYOF_WEEK_CLEAN == "FRIDAY" ~ "Fri",
       START_DAYOF_WEEK_CLEAN == "SATURDAY" ~ "Sat",
       START_DAYOF_WEEK_CLEAN == "SUNDAY" ~ "Sun",
        TRUE ~START_DAYOF_WEEK_CLEAN),
        ENDDAY_CLEAN_OF_WEEK_EXPAND = dplyr::case_when(
         END_DAYOF_WEEK_CLEAN == "MONDAY" ~ "Mon",
         END_DAYOF_WEEK_CLEAN == "TUESDAY" ~ "Tue",
         END_DAYOF_WEEK_CLEAN == "WEDNESDAY" ~ "Wed",
         END_DAYOF_WEEK_CLEAN == "THURSDAY" ~ "Thu",
         END_DAYOF_WEEK_CLEAN == "FRIDAY" ~ "Fri",
         END_DAYOF_WEEK_CLEAN == "SATURDAY" ~ "Sat",
         END_DAYOF_WEEK_CLEAN == "SUNDAY" ~ "Sun",
          TRUE ~END_DAYOF_WEEK_CLEAN)) |>
      dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
                              date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
                              lubridate::wday(date_sequence) == STARTDAY_CLEAN_OF_WEEK_EXPAND ~ STARTTIME_CLEAN,
                              lubridate::wday(date_sequence) == ENDDAY_CLEAN_OF_WEEK_EXPAND ~ ENDTIME_CLEAN,
                              TRUE ~ NA_character_)) |>
      dplyr::ungroup() |>
      dplyr::mutate(STARTYEAR_expand = lubridate::year(date_sequence),
             END_YEAR_expand = lubridate::year(date_sequence)) |>
      # function wday formats at Mon, Tue, etc.
      dplyr::mutate(Expand_day_of_week = lubridate::wday(date_sequence, label = TRUE)) |>
      # Retain on records within expand range
      dplyr::filter(Expand_day_of_week >= STARTDAY_CLEAN_OF_WEEK_EXPAND |
               Expand_day_of_week <= ENDDAY_CLEAN_OF_WEEK_EXPAND) |>
      dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
             CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE) |>
      dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
      # Retain only most recent FR if there are 2 records with the same date_sequence
      dplyr::slice(1) |>
      dplyr::rename(CLUSTER_weekly = "CLUSTER_ID",
             REG_ID_weekly = "REG_ID",
             FR_CITATION_weekly = "FR_CITATION",
             TIME_weekly = "TIME",
             VALUE_weekly = "VALUE",
             VALUEUNITS_weekly = "VALUEUNITS",
             VALUETYPE_weekly = "VALUETYPE",
             VALUERATE_weekly = "VALUERATE")
  } else {
    df_weekly <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                            SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                            date_sequence = as.Date(character()), CLUSTER_weekly = numeric(), REG_ID_weekly = numeric(),
                            FR_CITATION_weekly = character(), TIME_weekly = character(), VALUE_weekly = character(), VALUEUNITS_weekly = character(),
                            VALUETYPE_weekly = character(), VALUERATE_weekly = character())
  }
  runtime = Sys.time() - start
  runtime

  # Daily - only applies to trip limits

  # Join in Wide Form
  # First clean up duplicates
  # Then limit variables and rename
  # Then create final VALUE fields based on all status events
  start = Sys.time()
  # combine <- df_one |>
  #   dplyr::full_join(df_weekly,
  #             by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
  #   dplyr::full_join(df_seasonal,
  #             by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
  #   dplyr::full_join(df_monthly,
  #             by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
  #   # Remove records from empty dataframes
  #   dplyr::filter(!is.na(FMP)) |>
  #   # create final VALUE fields
  #   dplyr::ungroup() |>
  #   dplyr::rowwise() |>
  #   # For MANAGEMENT_TYPECLEAN == "CLOSURE": On a specific date_sequence, if the most recent FR_CITATION applies to multiple closure types (i.e., one time closure and monthly recurring)
  #   # Then maintain the more restrictive value (CLOSE)
  #   # Otherwise, the most recent FR_CITATION should be maintained with associated value fields
  #   dplyr::mutate(max_citation = pmax(FR_CITATION_one, FR_CITATION_seasonal, FR_CITATION_weekly, FR_CITATION_monthly, na.rm = TRUE),
  #          matched_values = list(c(
  #            if (isTRUE(FR_CITATION_one == max_citation)) VALUE_one else NULL,
  #            if (isTRUE(FR_CITATION_seasonal == max_citation)) VALUE_seasonal else NULL,
  #            if (isTRUE(FR_CITATION_weekly == max_citation)) VALUE_weekly else NULL,
  #            if (isTRUE(FR_CITATION_monthly == max_citation)) VALUE_monthly else NULL)),
  #          final_value = dplyr::case_when(
  #            MANAGEMENT_TYPECLEAN == "CLOSURE" & "CLOSE" %in% unlist(matched_values) ~ "CLOSE",
  #            TRUE ~ unlist(matched_values)[1]),
  #          FR_CITATION = dplyr::case_when(
  #            final_value == VALUE_one ~ FR_CITATION_one,
  #            final_value == VALUE_seasonal ~ FR_CITATION_seasonal,
  #            final_value == VALUE_weekly ~ FR_CITATION_weekly,
  #            final_value == VALUE_monthly ~ FR_CITATION_monthly,
  #            TRUE ~ max_citation),
  #          TIME = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ TIME_one,
  #                           FR_CITATION == FR_CITATION_weekly ~ TIME_weekly,
  #                           FR_CITATION == FR_CITATION_seasonal ~ TIME_seasonal,
  #                           FR_CITATION == FR_CITATION_monthly ~ TIME_monthly),
  #          VALUE = final_value,
  #          VALUEUNITS = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUEUNITS_one,
  #                                  FR_CITATION == FR_CITATION_weekly ~ VALUEUNITS_weekly,
  #                                  FR_CITATION == FR_CITATION_seasonal ~ VALUEUNITS_seasonal,
  #                                  FR_CITATION == FR_CITATION_monthly ~ VALUEUNITS_monthly),
  #          VALUETYPE = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUETYPE_one,
  #                                 FR_CITATION == FR_CITATION_weekly ~ VALUETYPE_weekly,
  #                                 FR_CITATION == FR_CITATION_seasonal ~ VALUETYPE_seasonal,
  #                                 FR_CITATION == FR_CITATION_monthly ~ VALUETYPE_monthly),
  #          VALUERATE = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUERATE_one,
  #                                 FR_CITATION == FR_CITATION_weekly ~ VALUERATE_weekly,
  #                                 FR_CITATION == FR_CITATION_seasonal ~ VALUERATE_seasonal,
  #                                 FR_CITATION == FR_CITATION_monthly ~ VALUERATE_monthly)) |>
  #   dplyr::ungroup()

  # 1. Sequential Full Joins (Replaces full_join chain)
  # by_cols <- c("FMP", "COMMON_NAME_CLEAN", "REGION", "ZONE_CLEAN",
  #              "SECTOR_CLEAN", "SUBSECTOR_CLEAN", "MANAGEMENT_TYPECLEAN", "date_sequence")
  #
  # combine <- merge(df_one, df_weekly, by = by_cols, all = TRUE)
  # combine <- merge(combine, df_seasonal, by = by_cols, all = TRUE)
  # combine <- merge(combine, df_monthly, by = by_cols, all = TRUE)

  combine <- df_one |> dplyr::ungroup() |>
      dplyr::full_join(df_weekly |> dplyr::ungroup(),
                by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
      dplyr::full_join(df_seasonal |> dplyr::ungroup(),
                by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
      dplyr::full_join(df_monthly |> dplyr::ungroup(),
                by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence))

  # Filter records with missing FMP (Replaces filter(!is.na(FMP)))
  combine <- combine[!is.na(combine$FMP), ]

  # Stop early if dataframe is empty
  if (nrow(combine) > 0) {

    # Helper function to easily grab vector or NA column if missing
    get_col <- function(df, col_name) {
      if (col_name %in% names(df)) df[[col_name]] else rep(NA, nrow(df))
    }

    # 2. Extract sources into matrices for matrix/vector operations
    sources <- c("one", "seasonal", "weekly", "monthly")

    cit_mat <- cbind(
      one      = get_col(combine, "FR_CITATION_one"),
      seasonal = get_col(combine, "FR_CITATION_seasonal"),
      weekly   = get_col(combine, "FR_CITATION_weekly"),
      monthly  = get_col(combine, "FR_CITATION_monthly")
    )

    val_mat <- cbind(
      one      = get_col(combine, "VALUE_one"),
      seasonal = get_col(combine, "VALUE_seasonal"),
      weekly   = get_col(combine, "VALUE_weekly"),
      monthly  = get_col(combine, "VALUE_monthly")
    )

    # 3. Vectorized Max Citation Calculation
    max_cit <- do.call(pmax, c(as.data.frame(cit_mat), list(na.rm = TRUE)))
    combine$max_citation <- max_cit

    # Identify matrix cells where the source citation matches max citation
    is_max <- !is.na(cit_mat) & !is.na(max_cit) & (cit_mat == max_cit)

    # Check if "CLOSE" appears in any source tied for max citation
    has_close <- rowSums(is_max & (val_mat == "CLOSE"), na.rm = TRUE) > 0

    # Determine first matched value for max citation ties
    # (Extracts the first column index that matches max citation)
    first_match_col <- max.col(is_max, ties.method = "first")
    first_match_col[rowSums(is_max) == 0] <- NA

    first_val <- val_mat[cbidx <- cbind(seq_len(nrow(combine)), first_match_col)]

    # 4. Resolve final_value
    is_closure <- !is.na(combine$MANAGEMENT_TYPECLEAN) & combine$MANAGEMENT_TYPECLEAN == "CLOSURE"
    combine$final_value <- ifelse(is_closure & has_close, "CLOSE", first_val)
    combine$VALUE <- combine$final_value

    # 5. Resolve winning FR_CITATION
    # Determine which source column supplied the final_value
    val_match <- !is.na(val_mat) & (val_mat == combine$final_value) & is_max
    win_col <- max.col(val_match, ties.method = "first")
    win_col[rowSums(val_match) == 0] <- NA

    row_idx <- seq_len(nrow(combine))
    win_cbidx <- cbind(row_idx, win_col)

    # Set FR_CITATION (Fallback to max_citation if no exact value match)
    win_cit <- cit_mat[win_cbidx]
    combine$FR_CITATION <- ifelse(!is.na(win_cit), win_cit, combine$max_citation)

    # Re-evaluate winning column indices based on final FR_CITATION match
    cit_match <- !is.na(cit_mat) & (cit_mat == combine$FR_CITATION)
    final_col <- max.col(cit_match, ties.method = "first")
    final_col[rowSums(cit_match) == 0] <- NA
    final_cbidx <- cbind(row_idx, final_col)

    # 6. Vectorized Lookup for Associated Attributes (TIME, VALUEUNITS, etc.)
    get_matched_attribute <- function(attr_base) {
      attr_mat <- cbind(
        one      = get_col(combine, paste0(attr_base, "_one")),
        seasonal = get_col(combine, paste0(attr_base, "_seasonal")),
        weekly   = get_col(combine, paste0(attr_base, "_weekly")),
        monthly  = get_col(combine, paste0(attr_base, "_monthly"))
      )
      attr_mat[final_cbidx]
    }

    combine$TIME       <- get_matched_attribute("TIME")
    combine$VALUEUNITS <- get_matched_attribute("VALUEUNITS")
    combine$VALUETYPE  <- get_matched_attribute("VALUETYPE")
    combine$VALUERATE  <- get_matched_attribute("VALUERATE")
  }

  runtime = Sys.time() - start
  runtime

  return(combine)

}
