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

  if('ONCE' %in% mstats){
    # One-time event
    df_one <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'ONCE') |>
      # this dplyr::filter should not be needed once we clean up errors in date processing
      dplyr::filter(END_DATE >= STARTDATE_CLEAN) |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "1 day")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
                                     date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
                                     TRUE ~ NA_character_)) |>
      dplyr::ungroup(0) |>
      dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
                    CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE, MULTI_REG_VALUE) |>
      dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
      # Retain only most recent FR if there are 2 records with the same date_sequence
      dplyr::mutate(num_records = dplyr::n())  |>
      dplyr::mutate(action_value = dplyr::case_when(num_records > 1 & MULTI_REG_VALUE != 1 & COMMON_NAME_CLEAN == lag(COMMON_NAME_CLEAN) ~ "slice",
                                             num_records > 1 & MULTI_REG_VALUE == 1 ~ "keep",
                                             num_records > 1 & MULTI_REG_VALUE != 1 & COMMON_NAME_CLEAN != lag(COMMON_NAME_CLEAN) ~ "keep",
                                             TRUE ~ "other")) |>
      dplyr::filter(!(action_value == "slice")) |>
      dplyr::rename(CLUSTER_one = "CLUSTER_ID",
                    REG_ID_one = "REG_ID",
                    FR_CITATION_one = "FR_CITATION",
                    TIME_one = "TIME",
                    VALUE_one = "VALUE",
                    VALUEUNITS_one = "VALUEUNITS",
                    VALUETYPE_one = "VALUETYPE",
                    VALUERATE_one = "VALUERATE") |>
      dplyr::select(-num_records, -action_value)
  } else {
    df_one <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                         SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                         date_sequence = as.Date(character()), CLUSTER_one = numeric(), REG_ID_one = numeric(),
                         FR_CITATION_one = character(), TIME_one = character(), VALUE_one = character(), VALUEUNITS_one = character(),
                         VALUETYPE_one = character(), VALUERATE_one = character())
  }

  if('SEASONAL' %in% mstats){
    # Seasonal
    df_seasonal <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'SEASONAL') |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
                              date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
                              TRUE ~ NA_character_)) |>
      dplyr::ungroup() |>
      dplyr::mutate(STARTYEAR_expand = lubridate::year(date_sequence),
             END_YEAR_expand = lubridate::year(date_sequence)) |>
      dplyr::mutate(START_DATE_EXPAND = as.Date(paste(STARTYEAR_expand, STARTMONTH, STARTDAY_CLEAN, sep = "-")),
             END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, ENDMONTH_CLEAN, ENDDAY_CLEAN, sep = "-"))) |>
      # Create START_DATE_EXPAND2 & END_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the enxt year
      # Create START_DATE_EXPAND_FINAL & END_DATE_EXPAND_FINAL to choose the correct START_DATE_EXPAND and END_DATE_EXPAND for each set of circumstances
      dplyr::mutate(START_DATE_EXPAND2 = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ START_DATE_EXPAND - lubridate::years(1),
                                            TRUE ~ START_DATE_EXPAND),
             END_DATE_EXPAND2 = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ END_DATE_EXPAND + lubridate::years(1),
                                          TRUE ~ END_DATE_EXPAND),
             START_DATE_EXPAND_FINAL = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence <= START_DATE_EXPAND ~ START_DATE_EXPAND2,
                                                 TRUE ~ START_DATE_EXPAND),
             END_DATE_EXPAND_FINAL = dplyr::case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence >= START_DATE_EXPAND & date_sequence >= END_DATE_EXPAND ~ END_DATE_EXPAND2,
                                               TRUE ~ END_DATE_EXPAND)) |>
      # Remove date_sequence records outside expand range
      dplyr::filter(date_sequence >= START_DATE_EXPAND_FINAL,
             END_DATE_EXPAND_FINAL >= date_sequence) |>
      dplyr::select(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence, TIME,
             CLUSTER_ID, REG_ID, FR_CITATION, VALUE, VALUEUNITS, VALUETYPE, VALUERATE) |>
      dplyr::arrange(date_sequence, desc(FR_CITATION)) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence) |>
      # Retain only most recent FR if there are 2 records with the same date_sequence
      dplyr::slice(1) |>
      dplyr::rename(CLUSTER_seasonal = "CLUSTER_ID",
             REG_ID_seasonal = "REG_ID",
             FR_CITATION_seasonal = "FR_CITATION",
             TIME_seasonal = "TIME",
             VALUE_seasonal = "VALUE",
             VALUEUNITS_seasonal = "VALUEUNITS",
             VALUETYPE_seasonal = "VALUETYPE",
             VALUERATE_seasonal = "VALUERATE")
  } else {
    df_seasonal <- data.frame(FMP = character(), COMMON_NAME_CLEAN = character(), REGION = character(), ZONE_CLEAN = character(),
                              SECTOR_CLEAN = character(), SUBSECTOR_CLEAN = character(), MANAGEMENT_TYPECLEAN = character(),
                              date_sequence = as.Date(character()), CLUSTER_seasonal = numeric(), REG_ID_seasonal = numeric(),
                              FR_CITATION_seasonal = character(), TIME_seasonal = character(), VALUE_seasonal = character(), VALUEUNITS_seasonal = character(),
                              VALUETYPE_seasonal = character(), VALUERATE_seasonal = character())
  }

  if('MONTHLY RECURRING' %in% mstats){
    # Monthly
    df_monthly <- dplyr::filter(df, MANAGEMENT_STATUS == 'MONTHLY RECURRING') |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      dplyr::mutate(TIME = dplyr::case_when(date_sequence == STARTDATE_CLEAN & !is.na(STARTTIME_CLEAN) ~ STARTTIME_CLEAN,
                              date_sequence == END_DATE & !is.na(ENDTIME_CLEAN) ~ ENDTIME_CLEAN,
                              day(date_sequence) == STARTDAY_CLEAN ~ STARTTIME_CLEAN,
                              day(date_sequence) == ENDDAY_CLEAN ~ ENDTIME_CLEAN,
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
             STARTMONTH_expand = dplyr::case_when(ENDDAY_CLEAN < STARTDAY_CLEAN & day(date_sequence) < STARTDAY_CLEAN & month(date_sequence) != 1 ~ month(date_sequence) - 1,
                                            ENDDAY_CLEAN < STARTDAY_CLEAN & day(date_sequence) < STARTDAY_CLEAN & month(date_sequence) == 1 ~ 12,
                                            TRUE ~ month(date_sequence)),
             # The conditions address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is a smaller value than the STARTDAY_CLEAN and the month of the date_sequence is not January
             # then the ENDMONTH_CLEAN_expand should be the month of the date_sequence
             # If the ENDDAY_CLEAN is a smaller value than the STARTDAY_CLEAN and the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN value and the month of the date_sequence + 1 month is less than or equal to 12 (December),
             # then the ENDMONTH_CLEAN_expand should be the month of the date_sequence plus one month
             # If the ENDMONTH_CLEAN_expand value is smaller than the STARTDAY_CLEAN and the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN and the month of the date_sequence + 1 is equal to 13,
             # then the ENDMONTH_CLEAN_expand should be January
             # Otherwise, the month of the date_sequence should be used
             ENDMONTH_CLEAN_expand = dplyr::case_when(ENDDAY_CLEAN < STARTDAY_CLEAN & day(date_sequence) < STARTDAY_CLEAN & month(date_sequence) != 1 ~ month(date_sequence),
                                          ENDDAY_CLEAN < STARTDAY_CLEAN & day(date_sequence) >= STARTDAY_CLEAN & month(date_sequence) + 1 <= 12 ~ month(date_sequence) + 1,
                                          ENDDAY_CLEAN < STARTDAY_CLEAN & day(date_sequence) >= STARTDAY_CLEAN & month(date_sequence) + 1 == 13 ~ 1,
                                          TRUE ~ month(date_sequence)),
             # Adjust END_YEAR_expand to address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the day of the date_sequence is greater than or equal to the STARTDAY_CLEAN and the ENDMONTH_CLEAN is January & the month of the date_sequence is December, then the END_YEAR_expand should be changed to the following year
             # If the day of the date_sequence is less than the STARTDAY_CLEAN and the ENDMONTH_CLEAN is January and the month of the date_sequence is December, then use the STARTYEAR_expand
             # Otherwise, use the END_YEAR_expand
             END_YEAR_expand = dplyr::case_when(day(date_sequence) >= STARTDAY_CLEAN & ENDMONTH_CLEAN == 1 & month(date_sequence) == 12 ~ END_YEAR_expand + 1,
                                         day(date_sequence) < STARTDAY_CLEAN & ENDMONTH_CLEAN == 1 & month(date_sequence) == 12 ~ STARTYEAR_expand,
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

  if('WEEKLY RECURRING' %in% mstats){
    # Weekly
    df_weekly <- dplyr::filter(df, MANAGEMENT_STATUSCLEAN == 'WEEKLY RECURRING') |>
      dplyr::mutate(date_sequence = purrr::map2(STARTDATE_CLEAN, END_DATE, seq, by = "days")) |>
      tidyr::unnest(date_sequence) |>
      dplyr::group_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, CLUSTER_ID) |>
      # need to get expanded days of the week to match
      dplyr::mutate(STARTDAY_CLEAN_OF_WEEK_EXPAND = dplyr::case_when(
        STARTDAY_CLEAN_OF_WEEK_USE == "MONDAY" ~ "Mon",
        STARTDAY_CLEAN_OF_WEEK_USE == "TUESDAY" ~ "Tue",
        STARTDAY_CLEAN_OF_WEEK_USE == "WEDNESDAY" ~ "Wed",
        STARTDAY_CLEAN_OF_WEEK_USE == "THURSDAY" ~ "Thu",
        STARTDAY_CLEAN_OF_WEEK_USE == "FRIDAY" ~ "Fri",
        STARTDAY_CLEAN_OF_WEEK_USE == "SATURDAY" ~ "Sat",
        STARTDAY_CLEAN_OF_WEEK_USE == "SUNDAY" ~ "Sun",
        TRUE ~ STARTDAY_CLEAN_OF_WEEK_USE),
        ENDDAY_CLEAN_OF_WEEK_EXPAND = dplyr::case_when(
          ENDDAY_CLEAN_OF_WEEK_USE == "MONDAY" ~ "Mon",
          ENDDAY_CLEAN_OF_WEEK_USE == "TUESDAY" ~ "Tue",
          ENDDAY_CLEAN_OF_WEEK_USE == "WEDNESDAY" ~ "Wed",
          ENDDAY_CLEAN_OF_WEEK_USE == "THURSDAY" ~ "Thu",
          ENDDAY_CLEAN_OF_WEEK_USE == "FRIDAY" ~ "Fri",
          ENDDAY_CLEAN_OF_WEEK_USE == "SATURDAY" ~ "Sat",
          ENDDAY_CLEAN_OF_WEEK_USE == "SUNDAY" ~ "Sun",
          TRUE ~ ENDDAY_CLEAN_OF_WEEK_USE)) |>
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

  # Daily - only applies to trip limits

  # Join in Wide Form
  # First clean up duplicates
  # Then limit variables and rename
  # Then create final VALUE fields based on all status events
  combine <- df_one |>
    dplyr::full_join(df_weekly,
              by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
    dplyr::full_join(df_seasonal,
              by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
    dplyr::full_join(df_monthly,
              by = dplyr::join_by(FMP, COMMON_NAME_CLEAN, REGION, ZONE_CLEAN, SECTOR_CLEAN, SUBSECTOR_CLEAN, MANAGEMENT_TYPECLEAN, date_sequence)) |>
    # Remove records from empty dataframes
    dplyr::filter(!is.na(FMP)) |>
    # create final VALUE fields
    dplyr::ungroup() |>
    dplyr::rowwise() |>
    # For MANAGEMENT_TYPECLEAN == "CLOSURE": On a specific date_sequence, if the most recent FR_CITATION applies to multiple closure types (i.e., one time closure and monthly recurring)
    # Then maintain the more restrictive value (CLOSE)
    # Otherwise, the most recent FR_CITATION should be maintained with associated value fields
    dplyr::mutate(max_citation = pmax(FR_CITATION_one, FR_CITATION_seasonal, FR_CITATION_weekly, FR_CITATION_monthly, na.rm = TRUE),
           matched_values = list(c(
             if (isTRUE(FR_CITATION_one == max_citation)) VALUE_one else NULL,
             if (isTRUE(FR_CITATION_seasonal == max_citation)) VALUE_seasonal else NULL,
             if (isTRUE(FR_CITATION_weekly == max_citation)) VALUE_weekly else NULL,
             if (isTRUE(FR_CITATION_monthly == max_citation)) VALUE_monthly else NULL)),
           final_value = dplyr::case_when(
             MANAGEMENT_TYPECLEAN == "CLOSURE" & "CLOSE" %in% unlist(matched_values) ~ "CLOSE",
             TRUE ~ unlist(matched_values)[1]),
           FR_CITATION = dplyr::case_when(
             final_value == VALUE_one ~ FR_CITATION_one,
             final_value == VALUE_seasonal ~ FR_CITATION_seasonal,
             final_value == VALUE_weekly ~ FR_CITATION_weekly,
             final_value == VALUE_monthly ~ FR_CITATION_monthly,
             TRUE ~ max_citation),
           TIME = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ TIME_one,
                            FR_CITATION == FR_CITATION_weekly ~ TIME_weekly,
                            FR_CITATION == FR_CITATION_seasonal ~ TIME_seasonal,
                            FR_CITATION == FR_CITATION_monthly ~ TIME_monthly),
           VALUE = final_value,
           VALUEUNITS = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUEUNITS_one,
                                   FR_CITATION == FR_CITATION_weekly ~ VALUEUNITS_weekly,
                                   FR_CITATION == FR_CITATION_seasonal ~ VALUEUNITS_seasonal,
                                   FR_CITATION == FR_CITATION_monthly ~ VALUEUNITS_monthly),
           VALUETYPE = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUETYPE_one,
                                  FR_CITATION == FR_CITATION_weekly ~ VALUETYPE_weekly,
                                  FR_CITATION == FR_CITATION_seasonal ~ VALUETYPE_seasonal,
                                  FR_CITATION == FR_CITATION_monthly ~ VALUETYPE_monthly),
           VALUERATE = dplyr::case_when(FR_CITATION == FR_CITATION_one ~ VALUERATE_one,
                                  FR_CITATION == FR_CITATION_weekly ~ VALUERATE_weekly,
                                  FR_CITATION == FR_CITATION_seasonal ~ VALUERATE_seasonal,
                                  FR_CITATION == FR_CITATION_monthly ~ VALUERATE_monthly)) |>
    dplyr::ungroup()

  return(combine)

}
