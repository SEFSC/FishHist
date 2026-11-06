library(dplyr) # mutate, left_join, filter, select, one_of, distinct, anti_join,
# group_by, arrange, summarize
library(stringr) # str_remove, str_detect
library(tidyr) # separate_rows, pivot_longer
library(lubridate) # month, day
library(here) # here
library(purrr) # map, reduce
library(tibble) # add_column

set_end_timeseries <- "2026-12-31"

mh_raw <- read.csv("data-raw/MH_DOWNLOAD_OCT_28_2026.csv",
  stringsAsFactors = FALSE,
  colClasses = c(
    "REGULATION_ID" = "numeric",
    "START_DAY" = "numeric",
    "START_MONTH" = "numeric",
    "START_YEAR" = "numeric",
    "END_DAY" = "numeric",
    "END_MONTH" = "numeric",
    "END_YEAR" = "numeric"
  ),
  # Added on 10/26/2022 because of a specific issue in how
  # Sarina & Adyan's Rstudio reads in the csv file
  # For some reason Adyan had all hyphens from species groups
  # re coded to "\x97"
  fileEncoding = "Windows-1252"
) |>
  # Reformat data frame for date formats and NAs
  # Format dates to be mm/dd/yyyy to match added data (this may not be an issue
  # in the future when pull directly from the database)
  dplyr::mutate(
    end_timeseries = as.Date(set_end_timeseries, "%Y-%m-%d"),
    EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
    INEFFECTIVE_DATE = as.Date(INEFFECTIVE_DATE, "%m/%d/%Y"),
    # Remove ="..." characters in species ITIS codes
    SPECIES_ITIS = gsub('="', "", SPECIES_ITIS),
    SPECIES_ITIS = gsub('"', "", SPECIES_ITIS)
  ) |>
  # Issue with species ITIS appearing as "\t173138"
  dplyr::mutate(
    SPECIES_ITIS = gsub("\t", "", SPECIES_ITIS),
    # Remove A or B in FR Citation (example regulation ID = 11514)
    FR_CITATION = stringr::str_remove(FR_CITATION, " [AB]"),
    # Add code for getting zone name for 3 regulations that are not
    # getting fixed in the database
    # Sarina created data bugs to update the data, but the change
    # was not reflected
    ZONE = dplyr::case_when(REGULATION_ID %in% c(11537, 11538, 11539) ~
                              "SMZ - PA - 04 RON MCMANUS MEMORIAL REEF",
                            TRUE ~ ZONE)
  )
# Replace all "blank" fields with NA for consistency
mh_raw[mh_raw == ""] <- NA

##################### Data Prep ###################

# Add "DETAILED" YES/NO field (from Google Sheets) based on MANAGEMENT_TYPE
# Read in file outlining whether a MANAGEMENT_TYPE is detailed (Y/N)
detailed_xref <- read.csv("data-raw/mtype_detailed_xref.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
) |>
  dplyr::select(-MANAGEMENT_CATEGORY)

# Translate from old ZONE names to new ZONE names
# Read in file that outlines new ZONE names for all FMPs
# These ZONEs were cleaned up for consistency
area_xref <- read.csv("data-raw/zone_name_xref.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
) |>
  # Create single variable for ZONE_USE
  dplyr::mutate(ZONE_USE = dplyr::case_when(NEW_ZONE_NAME == "" ~ ZONE,
                                            TRUE ~ NEW_ZONE_NAME)) |>
  dplyr::select(ZONE, ZONE_USE)

mh_setup <- mh_raw |>
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL'
  dplyr::mutate(SECTOR_USE = dplyr::case_when(
    MANAGEMENT_CATEGORY != "CATCH LIMITS" & SECTOR == "ALL" ~
      "RECREATIONAL,COMMERCIAL",
    TRUE ~ SECTOR)
                ) |>
  # Expand SECTOR_USE at the commas
  tidyr::separate_rows(SECTOR_USE) |>
  dplyr::left_join(detailed_xref, by = "MANAGEMENT_TYPE") |>
  # CREATE: the variables of O_COMMON_NAME, O_SPECIES_AGGREGATE,
  # O_SPECIES_GROUP to retain the original species information from the raw
  # data
  dplyr::mutate(
    O_COMMON_NAME = COMMON_NAME,
    O_SPECIES_AGGREGATE = SPECIES_AGGREGATE,
    O_SPECIES_GROUP = SPECIES_GROUP
  ) |>
  # CREATE: the variables of SPP_TYPE and SPP_NAME and transpose the species
  # information contained in the O_COMMON_NAME, O_SPECIES_AGGREGATE,
  # O_SPECIES_GROUP fields.
  # The SPP_TYPE field indicates whether the record relates to a
  # SPECIES_AGGREGATE, SPECIES_GROUP, or individual species (COMMON_NAME)
  # The SPP_NAME field indicates the specific aggregate, group, or species name
  # to which the record applies
  tidyr::pivot_longer(cols = c(COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP),
                      names_to = "SPP_TYPE", values_to = "SPP_NAME") |>
  # Remove records where SPP_NAME is null
  dplyr::filter(!is.na(SPP_NAME)) |>
  # Standardize ZONE names
  dplyr::left_join(area_xref, by = c("ZONE" = "ZONE")) |>
  # CREATE: vol and page but pulling out the volume and page number as separate
  # fields from the FR_CITATION
  dplyr::mutate(
    vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
    page = as.numeric(sub(".*FR ", "", FR_CITATION)),
    # CREATE: ADJUSTMENT variable to flag when the MANAGEMENT_TYPE
    # contains the word "ADJUSTMENT" and remove "ADJUSTMENT" from
    # the MANAGEMENT_TYPE name
    # ADJUSTMENT records are never redundant
    # Added " MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" &
    # month(INEFFECTIVE_DATE) == 12 & day(INEFFECTIVE_DATE) == 31
    # ~ 0" due to error in CLUSTER 280
    # Without this addition, a closure within CLUSTER 280 that
    # ended at the end of the calendar year was being flagged as an
    # adjustment instead of ceasing to exist.
    ADJUSTMENT = dplyr::case_when(
      stringr::str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
      MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" &
        lubridate::month(INEFFECTIVE_DATE) == 12 &
        lubridate::day(INEFFECTIVE_DATE) == 31 ~ 0,
      MANAGEMENT_TYPE == "REOPENING" & !is.na(INEFFECTIVE_DATE) ~ 1,
      TRUE ~ 0
    ),
    MANAGEMENT_TYPE_USE = dplyr::case_when(
      stringr::str_detect(
        MANAGEMENT_TYPE, "ADJUSTMENT") ~
        stringr::str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
      TRUE ~ MANAGEMENT_TYPE
    ),
    # Rename records with the MANAGEMENT_TYPE of REOPENING to the
    # MANAGEMENT_TYPE of CLOSURE and add OPEN or CLOSED to the
    # VALUE field
    # Although the MANAGEMENT_TYPE will be different from the raw
    # data (since the raw data stays true to the FR Language)
    # this will assist in processing dates to accurately capture
    # the time series of openings/closures in the fishery
    MANAGEMENT_TYPE_USE = dplyr::case_when(
      MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
      TRUE ~ MANAGEMENT_TYPE_USE
    ),
    VALUE = dplyr::case_when(
      MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
      MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
      TRUE ~ VALUE
    ),
    # CREATE: MANAGEMENT_STATUS_USE variable by transposing the
    # MANAGEMENT_STATUS values
    # Replace any NA values under MANAGEMENT_STATUS_USE as ONCE
    # because  NA values can be complicated to process
    # Both MANAGEMENT_STATUS of NA and ONCE are meant to be
    # processed the same way
    MANAGEMENT_STATUS_USE = dplyr::case_when(
      is.na(MANAGEMENT_STATUS) ~ "ONCE",
      TRUE ~ MANAGEMENT_STATUS
    ),
    # CREATE: the variable of STATUS_TYPE with the values of SIMPLE
    # or COMPLEX
    # A STATUS_TYPE of SIMPLE indicates a MANAGEMENT_STATUS_USE of
    # ONCE
    # A STATUS_TYPE of COMPLEX indicates a MANAGEMENT_STATUS_USE
    # that is RECURRING (SEASONAL, WEEKLY RECURRING, MONTHLY
    # RECURRING, DAILY)
    STATUS_TYPE = dplyr::case_when(
      MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
      MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING",
                                   "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
      TRUE ~ "COMPLEX"
    ),
    # CREATE: the variable of REG_REMOVED to indicate when a
    # regulation is "turned off"
    # A regulation is "turned off when the EFFECTIVE_DATE is equal
    # to the INEFFECTIVE_DATE
    REG_REMOVED = dplyr::case_when(
      EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
    # CREATE: the variables of GENERAL and COMPLEX to flag when a
    # regulation has a STAUS_TYPE of SIMPLE or COMPLEX,
    # respectively
    GENERAL = dplyr::case_when(
      STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
    COMPLEX = dplyr::case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
    # Format start and end day of week as ordered factors
    START_DAY_OF_WEEK = factor(START_DAY_OF_WEEK,
      levels = c(
        "MONDAY", "TUESDAY", "WEDNESDAY",
        "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"
      ),
      ordered = TRUE
    ),
    END_DAY_OF_WEEK = factor(END_DAY_OF_WEEK,
      levels = c(
        "MONDAY", "TUESDAY", "WEDNESDAY",
        "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"
      ),
      ordered = TRUE
    ),
    # CREATE: START_DATE from the START_DAY, START_MONTH, and
    # START_YEAR fields
    # The START_DATE field is only created using START_DAY,
    # START_MONTH, and START_YEAR when
    # all three start day, month, year fields are provided and the
    # management status is ONCE
    START_DATE = dplyr::case_when(
      MANAGEMENT_STATUS_USE == "ONCE" &
        !is.na(START_DAY) &
        !is.na(START_MONTH) &
        !is.na(START_YEAR) ~
        as.Date(paste0(END_MONTH, "/", END_DAY, "/", END_YEAR), "%m/%d/%Y"),
      TRUE ~ EFFECTIVE_DATE
    ),
    # Adjust the start date if its after the effective date so the
    # start date = effective date
    START_DATE = dplyr::case_when(
      START_DATE < EFFECTIVE_DATE ~ EFFECTIVE_DATE,
      TRUE ~ START_DATE
    ),
    # When the START_TIME is equal to "11:59:00 PM", the START_DATE
    # should be pushed ahead by one day since
    # the regulation will be in effect for the entirety of that day.
    START_DATE = dplyr::case_when(
      START_TIME == "11:59:00 PM" ~ START_DATE + 1,
      TRUE ~ START_DATE
    ),
    # Adjust the start day, time, and day of the week accordingly
    # so when those fields are used for recurring
    # the individual fields will be consistent
    START_DAY_USE = dplyr::case_when(
      START_TIME == "11:59:00 PM" & !is.na(START_DAY) ~ START_DAY + 1,
      TRUE ~ START_DAY
    ),
    # For start time, we remove the start time when "11:59:00 PM"
    # because a null assumes the full day
    START_TIME_USE = dplyr::case_when(START_TIME != "11:59:00 PM" ~ START_TIME),
    START_DAY_OF_WEEK_USE = dplyr::case_when(
      START_TIME == "11:59:00 PM" & !is.na(START_DAY_OF_WEEK) ~
        as.numeric(START_DAY_OF_WEEK) + 1,
      TRUE ~ as.numeric(START_DAY_OF_WEEK)
    ),

    # CREATE: END_DATE from the END_DAY, END_MONTH, and END_YEAR
    # fields
    # The END_DATE field is only created using END_DAY, END_MONTH,
    # and END_YEAR when
    # all end year, month, day fields are provided and and the
    # management status is ONCE
    END_DATE = dplyr::case_when(
      MANAGEMENT_STATUS_USE == "ONCE" &
        !is.na(END_DAY) &
        !is.na(END_MONTH) &
        !is.na(END_YEAR) ~
        as.Date(paste0(END_MONTH, "/", END_DAY, "/", END_YEAR), "%m/%d/%Y"),
      TRUE ~ INEFFECTIVE_DATE
    ),
    # Added condition on 2/2/24 because of REG_ID 762
    END_DATE = dplyr::case_when(
      MANAGEMENT_STATUS_USE == "ONCE" &
        !is.na(END_DAY) &
        !is.na(END_MONTH) &
        !is.na(END_YEAR) &
        as.Date(paste0(END_MONTH, "/", END_DAY, "/", END_YEAR), "%m/%d/%Y") >
        INEFFECTIVE_DATE ~ INEFFECTIVE_DATE,
      TRUE ~ END_DATE
    ),
    # For records with an END_TIME of "12:01:00 AM", the END_DATE
    # should be reverted to one day prior.
    # This will infer that the regulation remained in place through
    # the end of that day and not one minute into the next day.
    END_DATE = dplyr::case_when(
      END_TIME == "12:01:00 AM" ~ END_DATE - 1,
      TRUE ~ END_DATE
    ),
    # Adjust the end day, time, and day of the week accordingly
    # When end time is 12:01, use the day of end date because the
    # year of February already factored in to determine if its the
    # 28th or 29th
    END_DAY_USE = dplyr::case_when(
      END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~
        as.numeric(lubridate::day(END_DATE)),
      TRUE ~ END_DAY
    ),
    END_MONTH_USE = dplyr::case_when(
      END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~
        as.numeric(lubridate::month(END_DATE)),
      TRUE ~ END_MONTH
    ),
    END_YEAR_USE = dplyr::case_when(
      END_TIME == "12:01:00 AM" & END_MONTH == 1 & END_DAY == 1 ~ END_YEAR - 1,
      TRUE ~ END_YEAR
    ),
    # Retain end time of 12:01 only for recurring regulations where
    # the end day is the 1st
    # Otherwise remove 12:01 from end time or use the reported end
    # time
    END_TIME_USE = dplyr::case_when(
      END_TIME == "12:01:00 AM" & STATUS_TYPE == "RECURRING" &
        END_DAY == 1 ~ END_TIME,
      # Single regulation (REG_ID = 80)
      # where end time is 11:59 PM
      END_TIME != "12:01:00 AM" & END_TIME != "11:59:00 PM" ~ END_TIME
    ),
    # When the END_TIME is listed as "12:01:00 AM" and the end day
    # of the week is not missing then revert to one day prior
    # TO be consistent, still use the condition when end day does
    # not equal 1, but as of 12/30/2022 there were no end day of
    # the weeks with an end day of 1
    END_DAY_OF_WEEK_USE = dplyr::case_when(
      END_TIME == "12:01:00 AM" & END_DAY != 1 ~
        as.numeric(END_DAY_OF_WEEK) - 1,
      TRUE ~ as.numeric(END_DAY_OF_WEEK)
    ),
    # Format start and end day of week use to deal with 0 and 8 of
    # 7 level factor
    START_DAY_OF_WEEK_USE = dplyr::case_when(
      START_DAY_OF_WEEK_USE == 8 ~ 1,
      TRUE ~ START_DAY_OF_WEEK_USE
    ),
    END_DAY_OF_WEEK_USE = dplyr::case_when(
      END_DAY_OF_WEEK_USE == 0 ~ 7,
      TRUE ~ END_DAY_OF_WEEK_USE
    ),
    # Format start and end day of week use as ordered factors
    START_DAY_OF_WEEK_USE = dplyr::recode_factor(START_DAY_OF_WEEK_USE,
      `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY",
      `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"
    ),
    END_DAY_OF_WEEK_USE = dplyr::recode_factor(END_DAY_OF_WEEK_USE,
      `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY",
      `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"
    )
  )

##################### Species Expansion Fishing Year and Fishing Season #######

# Read in sp_info_use
sp_info_use <- readRDS("data-raw/MH_spp_table_20261028.RDS") |>
  dplyr::mutate(
    SPP_NAME = stringr::str_replace_all(SPP_NAME, stringr::fixed("?"), "—"))

# Species ALL expansion for FISHING YEAR and FISHING SEASON
a_sp_info_use <- sp_info_use |>
  dplyr::filter(SPP_NAME == "ALL") |>
  dplyr::rename(SPECIES = "COMMON_NAME_USE") |>
  dplyr::select(-c(FMP_GROUP_ID, SUBGRP_NAME, SPP_NAME, SCIENTIFIC_NAME))
mh_expanded_fy_fs <- mh_setup |>
  dplyr::filter(MANAGEMENT_TYPE_USE %in% c("FISHING YEAR", "FISHING SEASON"),
                SPP_NAME == "ALL") |>
  dplyr::left_join(a_sp_info_use, by = dplyr::join_by(FMP, SPP_TYPE),
                   relationship = "many-to-many") |>
  # Adjust start and end dates based on species added and removed dates
  dplyr::mutate(
    END_DATE = dplyr::case_when(
      is.na(END_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
      TRUE ~ END_DATE),
    START_DATE = dplyr::case_when(
      START_DATE < ADDED_SP_DATE ~ ADDED_SP_DATE, TRUE ~ START_DATE),
    # Remove expanded records if the species was removed before the
    # regulation start date
    REMOVE = dplyr::case_when(START_DATE > REMOVED_SP_DATE ~ 1, TRUE ~ 0)
  ) |>
  dplyr::filter(REMOVE == 0) |>
  dplyr::select(-c(REMOVE, ADDED_SP_DATE, REMOVED_SP_DATE, SPECIES_ITIS)) |>
  dplyr::rename(SPECIES_ITIS = "SPECIES_ITIS_USE")

mh_setup2 <- mh_setup |>
  # Remove Fishing year and Fishing season ALL because expanded tp species
  dplyr::filter(!(MANAGEMENT_TYPE_USE %in% c("FISHING YEAR", "FISHING SEASON")
                  & SPP_NAME == "ALL")) |>
  dplyr::bind_rows(mh_expanded_fy_fs) |>
  # Fill in SPECIES for aggregates, groups, and species
  dplyr::mutate(SPECIES = dplyr::case_when(is.na(SPECIES) ~ SPP_NAME,
                                           TRUE ~ SPECIES))

##################### Sector ID ###################

# Define matching variables for sector groupings to create unique SECTOR_IDs
sector.match <- c(
  "MANAGEMENT_TYPE_USE",
  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
  "SECTOR_USE",
  "REGION",
  "SPECIES"
)

# Suppress summarize info
options(dplyr.summarise.inform = FALSE)

# Set directory for location of existing sector_cluster files
sector_id_files <- dir(here::here("data-raw/sector_clusters"),
                       full.names = TRUE)

# Check for existing sector_id files
if (length(sector_id_files) > 0) {
  existing_sector_clusters <- sector_id_files |>
    purrr::map(read.csv) |>
    purrr::reduce(rbind)
} else {
  existing_sector_clusters <- mh_setup2 |>
    dplyr::select(dplyr::one_of(sector.match)) |>
    dplyr::filter(FMP == "") |>
    tibble::add_column(SECTOR_ID = NA)
}

# Create sector_max to establish number of existing sector groupings
sector_max <- max(c(existing_sector_clusters$SECTOR_ID, 0))

# Create new SECTOR_IDs for new files
new_sector_clusters <- mh_setup2 |>
  dplyr::select(dplyr::one_of(sector.match)) |>
  dplyr::distinct() |>
  dplyr::anti_join(existing_sector_clusters, by = sector.match) |>
  dplyr::mutate(SECTOR_ID = seq_len(dplyr::n()) + sector_max)

# Export new sector groupings into mh_sector_clusters_ CSV file
if (length(new_sector_clusters$SECTOR_ID) > 0) {
  write.csv(new_sector_clusters,
    here::here("data-raw/sector_clusters",
               paste0("mh_sector_clusters_",
                      format(Sys.Date(), "%d%b%Y"), ".csv")
               ),
    row.names = FALSE
  )
}

# Merge old and new sector groupings into unique_sector_clusters data frame
unique_sector_clusters <- rbind(existing_sector_clusters, new_sector_clusters)

# Join the unique_sector_clusters data frame to the mh_setup data frame to
# incorporate sector information in the data set
mh_sector_id <- mh_setup2 |>
  dplyr::left_join(unique_sector_clusters, by = sector.match) |>
  dplyr::mutate(SUBSECTOR_USE = SUBSECTOR)

##################### Cluster ID ###################

# Define matching variables for CLUSTER groupings
cluster.match <- c(
  "MANAGEMENT_TYPE_USE",
  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
  "SECTOR_USE", "SUBSECTOR_USE", "REGION",
  "SPECIES"
)

# Read in files with existing CLUSTER IDs
cluster_files <- dir(here::here("data-raw/clusters"), full.names = TRUE)

# Create existing_clusters data frame that includes previous CLUSTER groupings
if (length(cluster_files) > 0) {
  existing_clusters <- cluster_files |>
    purrr::map(read.csv) |>
    purrr::reduce(dplyr::bind_rows)
} else {
  existing_clusters <- mh_sector_id |>
    dplyr::select(dplyr::one_of(cluster.match)) |>
    dplyr::filter(FMP == "") |>
    tibble::add_column(CLUSTER = NA)
}

# Create clusters_max variable
clusters_max <- max(as.numeric(existing_clusters$CLUSTER), na.rm = TRUE, 0)

# Create new CLUSTER IDs for new files
# CREATE: Assign new CLUSTER_ID to any new CLUSTER groupings
new_clusters <- mh_sector_id |>
  dplyr::select(dplyr::one_of(cluster.match)) |>
  dplyr::distinct() |>
  dplyr::anti_join(existing_clusters, by = cluster.match) |>
  dplyr::mutate(CLUSTER = as.numeric(dplyr::row_number()) + clusters_max)

# Export new CLUSTERs
if (length(new_clusters$CLUSTER) > 0) {
  write.csv(new_clusters,
    here::here("data-raw/clusters",
               paste0("mh_unique_clusters_",
                      format(Sys.Date(), "%d%b%Y"), ".csv")
               ),
    row.names = FALSE
  )
}

# Join old CLUSTER groupings with new CLUSTER groupings
# Merge old and new CLUSTERS into unique_clusters data frame
unique_clusters <- rbind(existing_clusters, new_clusters)

# Join CLUSTERS to entire data set
# Results in the mh_prep data frame
mh_prep <- mh_sector_id |>
  dplyr::left_join(unique_clusters, by = cluster.match) |>
  dplyr::mutate(REG_CHANGE = 1)

# Create multi_reg related flags to prepare for subsequent date adjustments
mh_cluster_ids <- mh_prep |>
  dplyr::group_by(CLUSTER, FR_CITATION, ZONE_USE) |>
  # Create variable to indicate when there are multiple records per cluster
  # per FR_CITATION active at the same time with different VALUE_TYPE or
  # VALUE_RATE (MULTI_REG_VALUE)
  dplyr::mutate(
    MULTI_REG_VALUE = as.numeric(dplyr::n() > 1 &
                                   (dplyr::n_distinct(VALUE) |
                                      dplyr::n_distinct(VALUE_TYPE) > 1 |
                                      dplyr::n_distinct(VALUE_RATE) > 1)),
    # Create variable to indicate when there are multiple records
    # per cluster per FR_CITATION active at the same time but have
    # different START_YEARs (MULTI_REG_FORECAST)
    MULTI_REG_FORECAST = as.numeric(dplyr::n_distinct(START_YEAR) > 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(CLUSTER, FR_CITATION, MANAGEMENT_TYPE_USE,
                  MANAGEMENT_STATUS_USE) |>
  # Create variable to indicate when there are multiple records per cluster per
  # FR_CITATION active at the same time but are SEASONAL with different
  # START_MONTH and START_DAY
  dplyr::mutate(MULTI_REG_SEASONAL =
                  as.numeric(dplyr::n() > 1 &
                               dplyr::n_distinct(START_MONTH) > 1 &
                               dplyr::n_distinct(START_DAY) > 1)) |>
  dplyr::ungroup() |>
  # Create flag to indicate cases where a CLUSTER has a multi_reg related flag
  dplyr::mutate(MULTI_REG_CLUSTER =
                  as.numeric(MULTI_REG_VALUE == 1 | MULTI_REG_FORECAST == 1 |
                               MULTI_REG_SEASONAL == 1))

##################### Reversions ###################

# Create logic to adjust regulation start and end dates
# Create REVERSION records (records that occur before and after an ADJUSTMENT)
# Create variables to properly sort ADJUSTMENT records
mh_dates_detailed <- mh_cluster_ids |>
  # Remove CATCH LIMIT regulations and nondetailed records from the data frame
  # We acknowledge that we may not have all CATCH LIMIT regulations recorded in
  # the database since they are not always published in an FR notice, therefore,
  # we do not create reversion records
  dplyr::filter(MANAGEMENT_CATEGORY != "CATCH LIMITS", DETAILED == "YES") |>
  dplyr::arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) |>
  dplyr::group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) |>
  # Create the variable of GROUP_START to indicate the START_DATE of the first
  # regulation in the CLUSTER (when the CLUSTER starts) to properly create
  # REVERSION records
  # Create the variable of FIRST_REG to flag the first record within a CLUSTER
  # grouping to properly create REVERSION records
  dplyr::mutate(
    GROUP_START = min(START_DATE),
    FIRST_REG = GROUP_START == START_DATE,
    # Create the variable of LINK to connect ADJUSTMENT and
    # REVERSION records
    # The LINK variable will only be present for regulations that
    # are an ADJUSTMENT
    # The LINK variable indicates the REGULATION_ID of the
    # non-ADJUSTMENT records that occurred before an ADJUSTMENT
    # record
    # The record associated with that REGULATION_ID will go back
    # into effect once the ADJUSTMENT period has ended
    LINK = dplyr::case_when(
      ADJUSTMENT == 1 & FIRST_REG == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(REG_REMOVED, 1) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 1) != 1 ~
        dplyr::lead(REGULATION_ID, 1),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 2) != 1 ~
        dplyr::lead(REGULATION_ID, 2),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 3) != 1 ~
        dplyr::lead(REGULATION_ID, 3),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 4) != 1 ~
        dplyr::lead(REGULATION_ID, 4),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 5) != 1 ~
        dplyr::lead(REGULATION_ID, 5),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 6) != 1 ~
        dplyr::lead(REGULATION_ID, 6),
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 1) == 1 &
        dplyr::lead(FIRST_REG, 1) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 2) == 1 &
        dplyr::lead(FIRST_REG, 2) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 3) == 1 &
        dplyr::lead(FIRST_REG, 3) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 4) == 1 &
        dplyr::lead(FIRST_REG, 4) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 5) == 1 &
        dplyr::lead(FIRST_REG, 5) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 6) == 1 &
        dplyr::lead(FIRST_REG, 6) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 7) == 1 &
        dplyr::lead(FIRST_REG, 7) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 8) == 1 &
        dplyr::lead(FIRST_REG, 8) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 9) == 1 &
        dplyr::lead(FIRST_REG, 9) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 10) == 1 &
        dplyr::lead(FIRST_REG, 10) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 11) == 1 &
        dplyr::lead(FIRST_REG, 11) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 12) == 1 &
        dplyr::lead(FIRST_REG, 12) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 13) == 1 &
        dplyr::lead(FIRST_REG, 13) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 14) == 1 &
        dplyr::lead(FIRST_REG, 14) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 15) == 1 &
        dplyr::lead(FIRST_REG, 15) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 16) == 1 &
        dplyr::lead(FIRST_REG, 16) == 1 ~ 0,
      ADJUSTMENT == 1 & dplyr::lead(ADJUSTMENT, 17) == 1 &
        dplyr::lead(FIRST_REG, 17) == 1 ~ 0
    )
  )

# Explanation of LINK cases:
# 1 ADJUSTMENT with FIRST_REG (LINK = 0): The FIRST_REG in a CLUSTER is an
# ADJUSTMENT MANAGEMENT_TYPE, therefore there is no LINK to a prior regulation
# 2 ADJUSTMENT not FIRST_REG, with real reversion sometime before it
# (LINK != 0): The ADJUSTMENT record is not a FIRST_REG, therefore, it should
# revert to a prior regulation
# 3 ADJUSTMENT not FIRST_REG, with FIRST_REG ADJUSTMENT sometime before it
# (LINK = 0): The ADJUSTMENT record is not a FIRST_REG, however, the prior
# regulation is a FIRST_REG that is an ADJUSTMENT, therefore there is no
# reversion
# 4 Non-ADJUSTMENT FIRST_REG (link is null): record is a FIRST_REG but is not
# an ADJUSTMENT
# 5 Non-ADJUSTMENT not FIRST_REG (link is null): record is not a FIRST_REG or
# an ADJUSTMENT


# Insert reversion records into the time series after an ADJUSTMENT record ends
# After an ADJUSTMENT record ends, the prior non-ADJUSTMENT regulation goes
# back into effect
# A REVERSION record is a duplicate of the non-ADJUSTMENT record prior to an
# ADJUSTMENT record that goes back into effect when ADJUSTMENT ends
# ADJUSTMENT regulations that are FIRST_REG do not need a reversion since they
# either end or are overwritten and there is no regulations prior to them
# REVERSION records are only added after ADJUSTMENT records that are not
# FIRST_REG
mh_reversions <- mh_dates_detailed |>
  dplyr::ungroup() |>
  dplyr::select(LINK, REGULATION_ID, START_DATE, END_DATE) |>
  dplyr::distinct() |>
  # Filter data frame to only include ADJUSTMENT records (LINK > 0)
  dplyr::filter(abs(LINK) > 0) |>
  dplyr::group_by(LINK, REGULATION_ID) |>
  # Create the variable REVERSION to indicate when a record is a duplication of
  # the record that occurred before the prior ADJUSTMENT record
  dplyr::summarise(
    REVERSION = TRUE,
    # Create the variable START_DATE_USE to give each REVERSION
    # record a corrected start date (the day after the
    # respective ADJUSTMENT ends)
    START_DATE_USE = END_DATE + 1,
    # Create the variable SORT_DATE_USE to properly sort
    # ADJUSTMENT and REVERSION records in their order of
    # occurrence
    SORT_DATE_USE = START_DATE + 0.5
  ) |>
  # Create the variable ADJUSTMENT_ID which is the REGULATION_ID of the
  # ADJUSTMENT record prior to the newly created REVERSION record
  dplyr::rename(
    ADJUSTMENT_ID = REGULATION_ID,
    # Overwrite the REGULATION_ID with the LINK number of the
    # ADJUSTMENT record to indicate that the REVERSION record is a
    # duplication of the record prior to the ADJUSTMENT record
    REGULATION_ID = LINK
  ) |>
  # Join mh_reversions data frame to the mh_dates_detailed data frame by
  # REGULATION_ID
  dplyr::left_join(mh_dates_detailed, by = dplyr::join_by(REGULATION_ID),
                   relationship = "many-to-many") |>
  dplyr::bind_rows(mh_dates_detailed) |>
  # Adjust START_DATE_USE to accommodate REVERSION records in whole data set
  dplyr::mutate(
    START_DATE_USE = dplyr::case_when(
      is.na(START_DATE_USE) ~ START_DATE,
      TRUE ~ START_DATE_USE
    ),
    # Use sort date in cases where it is available to properly
    # sort REVERSION and ADJUSTMENT records in the proper order
    SORT_DATE_USE = dplyr::case_when(
      is.na(SORT_DATE_USE) ~ START_DATE_USE,
      TRUE ~ SORT_DATE_USE
    ),
    # Fill in REVERSION variable for records that are not REVERSIONs
    REVERSION = dplyr::case_when(REVERSION ~ TRUE, TRUE ~ FALSE)
  )


##################### Fill in End Date - Detailed Only (not Catch Limits) #####

# Create date related logic and overwrite end date for DETAILED
# MANAGEMENT_TYPEs excluding CATCH LIMITS
# Add CHANGE_DATE logic to sorted records
# Create END_DATE variable dependent on INEFFECTIVE_DATE of current record,
# START_DATE of next record, and CHANGE_DATE of current record
# Define a helper function to safely handle max() on empty vectors or all-NA
# groups
safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }
  max(x, na.rm = TRUE)
}

mh_dates1 <- mh_reversions |>
  dplyr::arrange(CLUSTER, dplyr::desc(SORT_DATE_USE), dplyr::desc(vol),
                 dplyr::desc(page)) |>
  dplyr::group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) |>
  # Create the variable of diff and diff_days to signify the length of time
  # between a regulation and its subsequent replacement (amount of days the
  # regulation is effective for)
  # The most recent regulation in each grouping gets NA for diff because no
  # regulations happens after it (it is the most recent and at the top of the list)
  dplyr::mutate(
    diff = as.numeric(dplyr::lag(START_DATE_USE) - START_DATE_USE,
                      units = "days"),
    # If a record doesn't have a START_TIME diff should be adjusted
    # by -1 (one day prior)
    # This indicates that the regulation started at start of the
    # START_DAY listed and
    # ended the day before the next regulation started (day before
    # START_DAY of subsequent regulation)
    diff_days = dplyr::case_when(
      is.na(dplyr::lag(START_TIME_USE)) ~ diff - 1,
      # When START_TIME is "12:01:00 AM",
      # diff should days should be
      # lagged by one day
      # This will infer that the
      # regulation began at the start of
      # the day, not one minute into the
      # day. This will help properly
      # calculate diff_days
      dplyr::lag(START_TIME_USE) == "12:01:00 AM" ~ diff - 1,
      # When START_TIME is "12:02:00 AM",
      # diff should days should be
      # lagged by one day
      # This will infer that the
      # regulation began at the start of
      # the day, not two minutes into
      # the day
      # This will help properly calculate
      # diff_days
      dplyr::lag(START_TIME_USE) == "12:02:00 AM" ~ diff - 1,
      TRUE ~ diff
    ),
    # Create the variable of CHANGE_DATE to indicate when a
    # regulation changed to the subsequent regulation (day before
    # START_DATE_USE of subsequent regulation)
    # When diff_days is not calculated due to there being no
    # subsequent regulation,
    # the end of the time series should be used as the CHANGE_DATE
    # This variable just indicates the time of a subsequent
    # regulation, it does not necessarily mean the regulation
    # ended then
    CHANGE_DATE = dplyr::case_when(
      is.na(diff_days) ~ end_timeseries,
      TRUE ~ START_DATE_USE + diff_days
    ),
    # When diff_days is calculated as -1, the CHANGE_DATE should be
    # lagged by one day
    # For most cases, a diff_day of -1 indicates that the regulation
    # never went into effect (except in the cases of MULTI_REG
    # regulations)
    # Therefore, the CHANGE_DATE is lagged by one day to indicate
    # that the regulation is not implemented
    # and allow the information from the prior regulation to link
    # with the subsequent regulation that is implemented.
    # Lagging the change date is different than subtracting one
    # day from the duration (since the diff_days variable is equal
    # to -1)
    CHANGE_DATE = dplyr::case_when(
      diff_days == -1 ~ dplyr::lag(CHANGE_DATE),
      TRUE ~ CHANGE_DATE
    ),
    CHANGE_DATE = dplyr::case_when(
      CHANGE_DATE > dplyr::lag(CHANGE_DATE) ~ dplyr::lag(CHANGE_DATE),
      TRUE ~ CHANGE_DATE
    ),
    # When an END_DATE is provided it should be used to signify the
    # END_DATE
    # Otherwise, the CHANGE_DATE should be used for the END_DATE
    # information
    END_DATE = dplyr::case_when(
      EFFECTIVE_DATE == INEFFECTIVE_DATE ~ START_DATE,
      !is.na(END_DATE) ~ END_DATE,
      TRUE ~ CHANGE_DATE
    ),
    # If the CHANGE_DATE is after the END_DATE and there is an
    # END_DATE provided, then the END_DATE should be used
    # If the CHANGE_DATE is after the INEFFECTIVE_DATE and an
    # INEFFECTIVE_DATE is provided, then the END_DATE should be
    # used
    # Otherwise, the CHANGE_DATE should be used as the END_DATE
    END_DATE = dplyr::case_when(
      CHANGE_DATE > END_DATE & !is.na(END_DATE) ~ END_DATE,
      CHANGE_DATE > INEFFECTIVE_DATE & !is.na(INEFFECTIVE_DATE) ~ END_DATE,
      TRUE ~ CHANGE_DATE
    )
  ) |>
  # Added 9/25/26 - fix error with end date for case where its MULTI_REG_VALUE
  dplyr::group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE,
                  FR_CITATION, REVERSION) |>
  dplyr::mutate(END_DATE = dplyr::case_when(
    MANAGEMENT_TYPE_USE == "DEFINITION" ~ end_timeseries,
    MANAGEMENT_TYPE_USE %in% c("CLOSURE", "FISHING SEASON") |
      REVERSION == TRUE | MULTI_REG_FORECAST == 1 ~ END_DATE,
    (MULTI_REG_VALUE == 1 | MULTI_REG_SEASONAL == 1) &
      !is.na(INEFFECTIVE_DATE) ~ min(END_DATE, na.rm = TRUE),
    (MULTI_REG_VALUE == 1 | MULTI_REG_SEASONAL == 1) &
      is.na(INEFFECTIVE_DATE) ~ max(END_DATE, na.rm = TRUE),
    TRUE ~ END_DATE
  )) |>
  # CREATE: the variable of NEVER_IMPLEMENTED to signify regulations that were
  # created but never went into effect
  # When the MULTI_REG_VALUE variable is flagged (1), NEVER_IMPLEMENTED should
  # not be flagged (0) meaning the regulation did go into effect
  # When the MULTI_REG_SEASONAL variable is flagged (1), NEVER_IMPLEMENTED
  # should not be flagged (0) meaning the regulation did go into effect
  # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED
  # should be flagged (1) meaning the regulation did not go into effect
  # When the START_DATE_USE is after the END_DATE, NEVER_IMPLEMENTED should be
  # flagged (1) meaning the regulation did not go into effect
  # When MULTI_REG_FORECAST is flagged (1) and the START_DATE_USE is equal to
  # the START_DATE_USE of a later FR_CITATION, then NEVER_IMPLEMENTED should be
  # flagged (1) meaning the regulation did not go into effect
  dplyr::mutate(
    NEVER_IMPLEMENTED = dplyr::case_when(
      MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" &
        START_DATE_USE == dplyr::lag(START_DATE_USE) &
        END_DATE != dplyr::lag(END_DATE) &
        VALUE != dplyr::lag(VALUE) ~ 0,
      MULTI_REG_SEASONAL == 1 ~ 0,
      START_DATE_USE > END_DATE ~ 1,
      MULTI_REG_VALUE == 1 ~ 0,
      diff_days <= -1 ~ 1,
      # Added 2/2/24 - if multi
      # reg forcast & only
      # include start year, do
      # not want to flag
      MULTI_REG_FORECAST == 1 & is.na(START_DAY_USE) & is.na(START_MONTH) &
        !is.na(START_YEAR) ~ 0,
      MULTI_REG_FORECAST == 1 &
        START_DATE_USE ==
        purrr::map_dbl(FR_CITATION, \(current_fr)
                       safe_max(START_DATE_USE[FR_CITATION > current_fr])) ~ 1,
      # Added 2/5/24 to hard code
      # the two cases of
      # WITHDRAWN/DELAYED
      # regulations
      REGULATION_ID %in% c(1355, 1589, 1356, 1463, 1341) ~ 1,
      TRUE ~ 0
    ),
    # ADJUST THE START DATE AND START TIME FOR CLUSTER 306 WHEN THE
    # REOPENING ENDS IN THE MIDDLE OF THE DAY
    START_TIME_USE = dplyr::case_when(
      dplyr::lead(!is.na(END_TIME_USE) & STATUS_TYPE == "SIMPLE" &
                    VALUE == "OPEN") &
        START_DATE_USE == dplyr::lead(END_DATE) + 1 ~
        format(as.POSIXct(dplyr::lead(END_TIME_USE), format = "%I:%M:%S %p") +
                 lubridate::as.difftime(1, units = "mins"), "%I:%M:%S %p"),
      TRUE ~ START_TIME_USE
    ),
    START_DATE_USE = dplyr::case_when(
      dplyr::lead(!is.na(END_TIME_USE) & STATUS_TYPE == "SIMPLE" &
                    VALUE == "OPEN") &
        START_DATE_USE == dplyr::lead(END_DATE) + 1 ~ dplyr::lead(END_DATE),
      TRUE ~ START_DATE_USE
    )
  ) |>
  # Added 2/2/24 - do not include records that were added because of an
  # adjustment, but not actually implemented
  dplyr::filter(!(NEVER_IMPLEMENTED == 1 & REVERSION == TRUE))

##################### Catch Limits Adjust Start Date using Fishing Year #######

# Adjust start date information for CATCH LIMIT MANAGEMENT_TYPEs
# First reclassify regulations entered as FISHING SEASON to FISHING YEAR
# Only include REG_REMOVED == 0 since we do not want to keep records that
# "turn off" the previous regulations
mh_fs <- mh_dates1 |>
  dplyr::filter(MANAGEMENT_TYPE_USE == "FISHING SEASON", REG_REMOVED == 0)

# Identify number of days within a fishing season
mh_fs_numdays <- mh_fs |>
  # Create date fields to determine season length
  dplyr::mutate(
    current_year = format(Sys.Date(), "%Y"),
    START_YEAR_USE = lubridate::year(START_DATE),
    next_end_year = START_YEAR_USE + 1,
    # CREATE: START_FY to indicate the start date of the fishing
    # year
    # When a START_YEAR_USE is not available, use the current_year
    START_FY = dplyr::case_when(
      !is.na(START_MONTH) & !is.na(START_DAY_USE) & is.na(START_YEAR_USE) ~
        as.Date(paste0(current_year, "-", START_MONTH, "-", START_DAY_USE),
                "%Y-%m-%d"),
      !is.na(START_MONTH) & !is.na(START_DAY_USE) & !is.na(START_YEAR_USE) ~
        as.Date(paste0(START_YEAR_USE, "-", START_MONTH, "-", START_DAY_USE),
                "%Y-%m-%d")
    ),
    # CREATE: END_FY to indicate the end date of the fishing year
    # When END_YEAR_USE is not available, use the next_end_year
    # if the END_MONTH_USE occurs before the START_MONTH
    # When END_YEAR_USE is not available, use the START_YEAR_USE if
    # the END_MONTH_USE occurs after the START_MONTH
    # When an END_YEAR_USE is available, use END_YEAR_USE
    # When END_MONTH_USE, END_DAY_USE, END_YEAR_USE is not available
    # and START_DAY_OF_WEEK_USE and END_DAY_OF_WEEK_USE is
    # available, use one day after the START_FY
    END_FY = dplyr::case_when(
      END_MONTH_USE < START_MONTH & !is.na(END_MONTH_USE) &
        !is.na(END_DAY_USE) & is.na(END_YEAR_USE) ~
        as.Date(paste0(next_end_year, "-", END_MONTH_USE, "-", END_DAY_USE),
                "%Y-%m-%d"),
      END_MONTH_USE >= START_MONTH & !is.na(END_MONTH_USE) &
        !is.na(END_DAY_USE) & is.na(END_YEAR_USE) ~
        as.Date(paste0(START_YEAR_USE, "-", END_MONTH_USE, "-", END_DAY_USE),
                "%Y-%m-%d"),
      !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & !is.na(END_YEAR_USE) ~
        as.Date(paste0(END_YEAR_USE, "-", END_MONTH_USE, "-", END_DAY_USE),
                "%Y-%m-%d"),
      is.na(END_MONTH_USE) & is.na(END_DAY_USE) & is.na(END_YEAR_USE) &
        !is.na(END_DAY_OF_WEEK_USE) & !is.na(START_DAY_OF_WEEK_USE) ~
        START_FY + 1
    ),
    # CREATE: DIFF to calculate the amount of time between START_FY
    # and END_FY
    # When END_MONTH_USE is after START_MONTH and END_YEAR_USE and
    # START_YEAR_USE is the same, then subtract the START_FY from
    # END_FY
    # When END_MONTH_USE is before START_MONTH and END_YEAR_USE and
    # START_YEAR_USE is the same, then subtract END_FY from
    # START_FY
    # When the END_MONTH_USE is not available, and END_FY is after
    # START_FY, then subtract START_FY from END_FY
    DIFF = dplyr::case_when(
      END_MONTH_USE >= START_MONTH ~ END_FY - START_FY,
      END_MONTH_USE < START_MONTH ~ START_FY - END_FY,
      is.na(END_MONTH_USE) & END_FY > START_FY ~ END_FY - START_FY
    ),
    DIFF = abs(DIFF)
  )

# Prepare to reclassify FISHING SEASON records as FISHING YEAR that are
# 364/365 days
fs_to_fy <- mh_fs_numdays |>
  dplyr::ungroup() |>
  dplyr::filter(DIFF %in% c(364, 365, 366)) |>
  dplyr::select(REGULATION_ID) |>
  dplyr::pull()

# Filter for only FISHING YEAR MANAGEMENT_TYPEs
# Reclassify regulations to FISHING YEAR if FISHING SEASON is 365 days
# Only include REG_REMOVED == 0 since we do not want to keep records that
# "turn off" the previous regulations
mh_fy <- mh_dates1 |>
  dplyr::ungroup() |>
  dplyr::mutate(MANAGEMENT_TYPE_USE = dplyr::case_when(
    REGULATION_ID %in% fs_to_fy ~ "FISHING YEAR",
    TRUE ~ MANAGEMENT_TYPE_USE
  )) |>
  dplyr::filter(MANAGEMENT_TYPE_USE == "FISHING YEAR", REG_REMOVED == 0) |>
  # Recode CLUSTER ID of recoded MANAGEMENT_TYPEs so the CLUSTERs match
  # These are clusters that were entered as FISHING SEASON but are actually a
  # FISHING_YEAR
  dplyr::mutate(NEW_CLUSTER = dplyr::case_when(!REGULATION_ID %in% fs_to_fy &
                                                 SPECIES != "ALL" ~ CLUSTER)
                ) |>
  dplyr::arrange(START_DATE_USE) |>
  dplyr::group_by(MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS,
                  FMP, SECTOR_USE, SUBSECTOR_USE, REGION, SPECIES) |>
  # first down and then up - this satisfies cases where first date needs a new
  # cluster
  tidyr::fill(NEW_CLUSTER, .direction = "downup") |>
  dplyr::ungroup()

# Identify clusters that have a split fishing year
mh_fy_split <- mh_fy |>
  dplyr::group_by(FR_CITATION, FMP, REGION, SPECIES, SECTOR_USE, SUBSECTOR_USE,
                  ZONE_USE, EFFECTIVE_DATE) |>
  dplyr::summarise(N = dplyr::n()) |>
  dplyr::filter(N > 1)

# WARNING fishing year should not be split -- these should be re-coded to
# fishing seasons
if ((nrow(mh_fy_split) != 0)) {
  warning("Fishing year split, please investigate. These might need to be
          re-coded to fishing season")
}

# Identify clusters that have a fishing year < 365 days
mh_fy_less365 <- mh_fy |>
  dplyr::select(FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
                MANAGEMENT_STATUS_USE, START_MONTH, START_DAY_USE,
                END_MONTH_USE, END_DAY_USE) |>
  # CHECK VARIABLE: create a date field to subtract (the year is not important
  # here)
  dplyr::mutate(
    START_FY = as.Date(paste0("2023-", START_MONTH, "-", START_DAY_USE),
                       "%Y-%m-%d"),
    END_FY = as.Date(paste0("2023-", END_MONTH_USE, "-", END_DAY_USE),
                     "%Y-%m-%d"),
    DIFF = START_FY - END_FY
  ) |>
  dplyr::filter(DIFF != 1 & DIFF != 0 & DIFF != -364)
# WARNING fishing year should be a year
if (nrow(mh_fy_less365) != 0) {
  warning("Fishing year not 365 days")
}

# CREATE: START_MONTH2 to use for determining the start month and day of the
# FISHING YEAR
mh_fy2 <- mh_fy |>
  dplyr::ungroup() |>
  dplyr::select(NEW_CLUSTER, FMP, REGION, SPECIES, SPECIES_ITIS, SECTOR_USE,
                SUBSECTOR_USE, ZONE_USE, START_DATE_USE, START_MONTH,
                START_DAY_USE) |>
  # Format start month and day as FY_MONTH and FY_DAY
  # Get month from number to month name abbreviation
  dplyr::mutate(
    START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"),
                                  "%Y-%m-%d"), "%b"),
    FY = paste0(START_DAY_USE, "-", START_MONTH2)
  )

# Consolidate into only meaningful changes
mh_fy3 <- mh_fy2 |>
  dplyr::ungroup() |>
  dplyr::group_by(NEW_CLUSTER, ZONE_USE, FY) |>
  dplyr::mutate(EFFECTIVE_DATE_FY = min(START_DATE_USE)) |>
  dplyr::select(
    NEW_CLUSTER, FMP, REGION, SPECIES, SPECIES_ITIS, SECTOR_USE,
    SUBSECTOR_USE, ZONE_USE,
    EFFECTIVE_DATE_FY, FY
  ) |>
  dplyr::distinct() |>
  # Remove error with royal red -- TEMP
  dplyr::mutate(RM = dplyr::case_when(
    REGION == "GULF OF AMERICA" & SPECIES == "SHRIMP, ROYAL RED" &
      FY == "1-Jan" ~ 1,
    TRUE ~ 0)
    ) |>
  dplyr::filter(RM == 0) |>
  dplyr::select(-RM) |>
  dplyr::group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
                  SPECIES_ITIS) |>
  dplyr::arrange(EFFECTIVE_DATE_FY) |>
  dplyr::mutate(reg_order = rank(EFFECTIVE_DATE_FY))

# Structure fishing year data to wide-form for joining to the rest of MH data
mh_fy3_w <- mh_fy3 |>
  tidyr::pivot_wider(names_from = reg_order,
                     values_from = c(EFFECTIVE_DATE_FY, FY)) |>
  dplyr::ungroup()

# Check that fishing year for a FMP/region/species is the same by sector - this
# is for when sector is ALL
# We will use the FMP/region/species FY to join to these records
chk_sect_fy <- mh_fy3_w |>
  dplyr::filter(ZONE_USE == "ALL", SUBSECTOR_USE == "ALL") |>
  dplyr::select(-c(NEW_CLUSTER, ZONE_USE, SUBSECTOR_USE, EFFECTIVE_DATE_FY_1,
                   EFFECTIVE_DATE_FY_2, EFFECTIVE_DATE_FY_3)) |>
  tidyr::pivot_wider(names_from = SECTOR_USE,
                     values_from = c(FY_1, FY_2, FY_3)) |>
  dplyr::filter(FY_1_COMMERCIAL != FY_1_RECREATIONAL)
if (nrow(chk_sect_fy) > 0) {
  warning("Fishing year is different by sector")
}

# Filter the mh_cluster_ids data frame to obtain all CATCH LIMITS records
# REG_REMOVED == 0 because if a catch limit was removed (effective ==
# ineffective), we do not want to adjust the start date of these
Catchlim <- mh_cluster_ids |>
  dplyr::filter(MANAGEMENT_CATEGORY == "CATCH LIMITS" & REG_REMOVED == 0)

# Join by all columns
mh_fy4_all <- mh_fy3_w |>
  dplyr::select(-c(NEW_CLUSTER, SPECIES)) |>
  dplyr::rename(
    EFFECTIVE_DATE_FY_1_all = "EFFECTIVE_DATE_FY_1",
    EFFECTIVE_DATE_FY_2_all = "EFFECTIVE_DATE_FY_2",
    EFFECTIVE_DATE_FY_3_all = "EFFECTIVE_DATE_FY_3",
    FY_1_all = "FY_1",
    FY_2_all = "FY_2",
    FY_3_all = "FY_3"
  )
# Join by only species and region
mh_fy4_sr <- mh_fy3_w |>
  dplyr::filter(ZONE_USE == "ALL", SUBSECTOR_USE == "ALL") |>
  dplyr::select(-c(NEW_CLUSTER, SPECIES, SECTOR_USE,
                   ZONE_USE, SUBSECTOR_USE)) |>
  # Take first row - this won't matter because both sectors
  # have the same FY - check line 635
  dplyr::group_by(FMP, REGION, SPECIES_ITIS) |>
  dplyr::slice(1) |>
  dplyr::rename(
    EFFECTIVE_DATE_FY_1_s = "EFFECTIVE_DATE_FY_1",
    EFFECTIVE_DATE_FY_2_s = "EFFECTIVE_DATE_FY_2",
    EFFECTIVE_DATE_FY_3_s = "EFFECTIVE_DATE_FY_3",
    FY_1_s = "FY_1",
    FY_2_s = "FY_2",
    FY_3_s = "FY_3"
  )

# Add FISHING YEAR information to MANAGEMENT_CATEGORY == CATCH LIMITS records
mh_combine <- Catchlim |>
  dplyr::ungroup() |>
  # Join by all columns
  dplyr::left_join(mh_fy4_all,
    by = dplyr::join_by(FMP, REGION, SPECIES_ITIS, SECTOR_USE,
                        SUBSECTOR_USE, ZONE_USE)
  ) |>
  # Join by only species and region
  dplyr::left_join(mh_fy4_sr,
    by = dplyr::join_by(FMP, REGION, SPECIES_ITIS)
  ) |>
  # Take the first non-null value
  dplyr::mutate(
    EFFECTIVE_DATE_FY_1 =
      dplyr::coalesce(EFFECTIVE_DATE_FY_1_all, EFFECTIVE_DATE_FY_1_s),
    EFFECTIVE_DATE_FY_2 =
      dplyr::coalesce(EFFECTIVE_DATE_FY_2_all, EFFECTIVE_DATE_FY_2_s),
    EFFECTIVE_DATE_FY_3 =
      dplyr::coalesce(EFFECTIVE_DATE_FY_3_all, EFFECTIVE_DATE_FY_3_s),
    FY_1 = dplyr::coalesce(FY_1_all, FY_1_s),
    FY_2 = dplyr::coalesce(FY_2_all, FY_2_s),
    FY_3 = dplyr::coalesce(FY_3_all, FY_3_s)
  ) |>
  # Remove unnecessary columns from join
  dplyr::select(-c(
    EFFECTIVE_DATE_FY_1_all, EFFECTIVE_DATE_FY_1_s, EFFECTIVE_DATE_FY_2_all,
    EFFECTIVE_DATE_FY_2_s, EFFECTIVE_DATE_FY_3_all, EFFECTIVE_DATE_FY_3_s,
    FY_1_all, FY_1_s, FY_2_all, FY_2_s, FY_3_all, FY_3_s
  ))

# Apply logic to adjust start date based on fishing year (month/day)
mh_dates_adjust <- mh_combine |>
  # First adjust to 3rd FY, then 2nd, then 1st, default original start date if
  # no FY
  dplyr::mutate( # Take start year of start date if missing
    START_YEAR = dplyr::case_when(
      is.na(START_YEAR) ~ lubridate::year(START_DATE), TRUE ~ START_YEAR),
    START_DATE_ADJ = dplyr::case_when(
      !is.na(FY_3) & START_DATE >= EFFECTIVE_DATE_FY_3 ~
        as.Date(paste0(START_YEAR, "-", FY_3), format = "%Y-%d-%b"),
      !is.na(FY_3) & START_DATE < EFFECTIVE_DATE_FY_3 ~
        as.Date(paste0(START_YEAR, "-", FY_2), format = "%Y-%d-%b"),
      is.na(FY_3) & !is.na(FY_2) &
        (START_DATE >= EFFECTIVE_DATE_FY_2 | lubridate::year(START_DATE) ==
           lubridate::year(EFFECTIVE_DATE_FY_2)) ~
        as.Date(paste0(START_YEAR, "-", FY_2), format = "%Y-%d-%b"),
      is.na(FY_3) & !is.na(FY_2) & START_DATE < EFFECTIVE_DATE_FY_2 ~
        as.Date(paste0(START_YEAR, "-", FY_1), format = "%Y-%d-%b"),
      is.na(FY_3) & is.na(FY_2) & !is.na(FY_1) &
        (START_DATE >= EFFECTIVE_DATE_FY_1 | lubridate::year(START_DATE) ==
           lubridate::year(EFFECTIVE_DATE_FY_1)) ~
        as.Date(paste0(START_YEAR, "-", FY_1), format = "%Y-%d-%b"),
      TRUE ~ START_DATE
    )
  )

# Prep to join to the rest of the MH data
mh_catchlim_prep <- mh_dates_adjust |>
  dplyr::select(-c(START_DATE, EFFECTIVE_DATE_FY_1, EFFECTIVE_DATE_FY_2,
                   EFFECTIVE_DATE_FY_3, FY_1, FY_2, FY_3)) |>
  dplyr::rename(START_DATE_USE = "START_DATE_ADJ")

# Separate CATCH LIMITS regulations where REG_REMOVED == 1
mh_removed_catchlim <- mh_cluster_ids |>
  dplyr::filter(MANAGEMENT_CATEGORY == "CATCH LIMITS") |>
  dplyr::filter(REG_REMOVED == 1) |>
  dplyr::rename(START_DATE_USE = "START_DATE")

# Separate DETAILED == NO regulations
mh_nondetailed <- mh_cluster_ids |>
  dplyr::filter(DETAILED == "NO") |>
  dplyr::rename(START_DATE_USE = "START_DATE")

# Compile all data frames
mh_dates <- mh_dates1 |>
  dplyr::bind_rows(mh_catchlim_prep) |>
  dplyr::bind_rows(mh_removed_catchlim) |>
  dplyr::bind_rows(mh_nondetailed)

##################### Species Expansion ###################

b_sp_info_use <- sp_info_use |>
  dplyr::select(-c(FMP_GROUP_ID, SUBGRP_NAME, SCIENTIFIC_NAME, SPP_TYPE))
c_sp_info_use <- sp_info_use |>
  dplyr::filter(SPP_NAME == "ALL") |>
  dplyr::select(FMP, SPECIES_ITIS_USE, SCIENTIFIC_NAME, ADDED_SP_DATE,
                REMOVED_SP_DATE) |>
  dplyr::rename(ADDED_DT = "ADDED_SP_DATE", REMOVED_DT = "REMOVED_SP_DATE")

mh_sp_expanded <- mh_dates |>
  dplyr::full_join(b_sp_info_use,
    by = dplyr::join_by(FMP, SPECIES == SPP_NAME),
    relationship = "many-to-many"
  ) |>
  # Remove SPECIES_GROUP and SPECIES_AGGREGATE that do not appear in the
  # data set or are regulated by FMPs that are not in the data set (i.e., HMS)
  dplyr::filter(!is.na(REGULATION_ID)) |>
  dplyr::mutate(
    COMMON_NAME_USE = dplyr::case_when(
      is.na(COMMON_NAME_USE) ~ SPECIES,
      TRUE ~ COMMON_NAME_USE
    ),
    SPECIES_ITIS_USE = dplyr::case_when(
      is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
      TRUE ~ SPECIES_ITIS_USE
    )
  ) |>
  dplyr::select(-c(SPECIES_ITIS)) |>
  # Get Added and Removed Dates for regulations that are species specific to
  # make sure dates are accurate
  dplyr::left_join(c_sp_info_use,
    by = dplyr::join_by(FMP, SPECIES_ITIS_USE)
  ) |>
  dplyr::mutate(
    ADDED_SP_DATE = dplyr::coalesce(ADDED_SP_DATE, ADDED_DT),
    REMOVED_SP_DATE = dplyr::coalesce(REMOVED_SP_DATE, REMOVED_DT)
  ) |>
  dplyr::select(-c(ADDED_DT, REMOVED_DT))

################# CHECK SPECIES #################
chk <- mh_sp_expanded |> dplyr::filter(is.na(ADDED_SP_DATE))

# Adjust START_DATE_USE and END_DATE information based on the ADDED_SP_DATE and
# REMOVED_SP_DATE
# Results in the mh_expanded data frame
mh_expanded <- mh_sp_expanded |>
  # CREATE: the variable of RM_SPP (remove species) to indicate that the
  # regulation does not apply to this species does not apply
  # A regulation does not apply to a particular species when:
  # The REMOVED_SP_DATE occurs prior to the START_DATE_USE of the regulation
  # The ADDED_SP_DATE occurs after the START_DATE_USE of the regulation and the
  # END_DATE of the regulation is prior the the ADDED_SP_DATE
  # The REMOVED_SP_DATE is equal to the START_DATE_USE of the regulation
  dplyr::mutate(
    RM_SPP = dplyr::case_when(
      REMOVED_SP_DATE < START_DATE_USE ~ 1,
      ADDED_SP_DATE > START_DATE_USE & END_DATE < ADDED_SP_DATE ~ 1,
      REMOVED_SP_DATE == START_DATE_USE ~ 1,
      TRUE ~ 0
    ),
    # CREATE: the variable of START_DATE2 to adjust the START_DATE_USE
    # accordingly
    # Adjust when ADDED_SP_DATE is after the START_DATE_USE of the
    # regulation and the END_DATE of the regulation is after the
    # ADDED_SP_DATE2 (or for catch limits where end date is null)
    START_DATE2 = dplyr::case_when(
      ADDED_SP_DATE > START_DATE_USE &
        (is.na(END_DATE) | END_DATE > ADDED_SP_DATE) ~ ADDED_SP_DATE,
      TRUE ~ START_DATE_USE
    ),
    # CREATE: the variable of END_DATE2 to adjust the END_DATE accordingly
    # Adjust when the REMOVED_SP_DATE is prior to the END_DATE of the
    # regulation and the REMOVED_SP_DATE is after the START_DATE_USE of
    # the regulation
    END_DATE2 = dplyr::case_when(
      MANAGEMENT_CATEGORY == "CATCH LIMITS" ~ END_DATE,
      is.na(END_DATE) & !is.na(REMOVED_SP_DATE) ~ REMOVED_SP_DATE,
      REMOVED_SP_DATE < END_DATE &
        REMOVED_SP_DATE > START_DATE_USE ~ REMOVED_SP_DATE,
      is.na(END_DATE) ~ end_timeseries,
      TRUE ~ END_DATE
    )
  ) |>
  # Clean cases where Common name is old or where using scientific name
  dplyr::mutate(COMMON_NAME_USE = dplyr::case_when(
    COMMON_NAME_USE == "Caulolatilus" ~
      "TILEFISHES",
    COMMON_NAME_USE == "JEWFISH" ~
      "GROUPER, GOLIATH",
    COMMON_NAME_USE == "Centropristis melana" ~
      "BASS, BLACK SEA",
    COMMON_NAME_USE == "TILEFISH" &  SPECIES_ITIS_USE == "168546" ~
      "TILEFISH, GOLDEN",
    COMMON_NAME_USE == "CONEY" ~
      "GROUPER, CONEY",
    COMMON_NAME_USE == "TUNNY, LITTLE" ~
      "TUNA, LITTLE TUNNY",
    COMMON_NAME_USE == "longsnout butterflyfish" ~
      "BUTTERFLYFISH, LONGSNOUT",
    COMMON_NAME_USE %in% c("Ogcocephalus", "Milleporidae") ~
      toupper(COMMON_NAME_USE),
    TRUE ~ COMMON_NAME_USE
  )) |>
  dplyr::filter(RM_SPP != 1)

mh_data_log <- mh_expanded |>
  dplyr::select(
    CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP,
    ACTION, ACTION_TYPE, AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE,
    MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, MANAGEMENT_TYPE_USE,
    MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
    JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE,
    REGION, ZONE, ZONE_USE, JURISDICTIONAL_WATERS,
    COMMON_NAME_USE, SPP_TYPE, SPECIES, SPECIES_ITIS_USE,
    ADJUSTMENT, REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE,
    START_DATE2, END_DATE2, diff_days,
    START_MONTH, START_DAY, START_DAY_USE, START_YEAR, START_TIME,
    START_TIME_USE, START_DAY_OF_WEEK, START_DAY_OF_WEEK_USE,
    END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR,
    END_YEAR_USE, END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE,
    VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
    MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_SEASONAL, MULTI_REG_CLUSTER,
    REG_REMOVED, NEVER_IMPLEMENTED, FLAG
  )


##################### Turn off Old Caribbean FMPs ###################

# Force old Caribbean FMPs to end a day before the effective date of the new
# Caribbean FMPs
# old FMPs = "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
# "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
# "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS"
# new FMPs = "FMP FOR THE EEZ AROUND PUERTO RICO", "FMP FOR THE EEZ AROUND ST.
# THOMAS AND ST. JOHN", "FMP FOR THE EEZ AROUND ST. CROIX"
mh_data_log_final <- mh_data_log |>
  dplyr::mutate(END_DATE2 = dplyr::case_when(
    FMP %in% c("SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
               "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
               "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS") &
      END_DATE2 > "2022-10-12" ~ as.Date("2022-10-12"),
    TRUE ~ END_DATE2
  ))

##################### Final Dataset ###################

fishhist <- mh_data_log_final |>
  dplyr::ungroup() |>
  dplyr::distinct() |>
  # Rename to match the processed MH dataset from ODM
  dplyr::rename(
    CLUSTER_ID = "CLUSTER",
    REG_ID = "REGULATION_ID",
    ACTIONTYPE = "ACTION_TYPE",
    AMENDMENTNUMBER = "AMENDMENT_NUMBER",
    ACCOUNTABILITYMEASURE = "ACCOUNTABILITY_MEASURE",
    MANAGEMENTCATEGORY = "MANAGEMENT_CATEGORY",
    MANAGEMENT_TYPECLEAN = "MANAGEMENT_TYPE_USE",
    MANAGEMENT_STATUSCLEAN = "MANAGEMENT_STATUS_USE",
    SECTOR_CLEAN = "SECTOR_USE",
    SUBSECTOR_CLEAN = "SUBSECTOR_USE",
    ZONE_CLEAN = "ZONE_USE",
    JURISDICTIONALWATERS = "JURISDICTIONAL_WATERS",
    SPP_NAME = "SPECIES",
    COMMON_NAME_CLEAN = "COMMON_NAME_USE",
    SPECIES_ITIS_CLEAN = "SPECIES_ITIS_USE",
    STARTDATE_CLEAN = "START_DATE2",
    END_DATE = "END_DATE2",
    STARTMONTH = "START_MONTH",
    STARTDAY_CLEAN = "START_DAY_USE",
    STARTYEAR = "START_YEAR",
    STARTTIME_CLEAN = "START_TIME_USE",
    START_DAYOF_WEEK_CLEAN = "START_DAY_OF_WEEK_USE",
    ENDDAY_CLEAN = "END_DAY_USE",
    END_DAYOF_WEEK_CLEAN = "END_DAY_OF_WEEK_USE",
    ENDMONTH_CLEAN = "END_MONTH_USE",
    ENDTIME_CLEAN = "END_TIME_USE",
    ENDYEAR_CLEAN = "END_YEAR_USE",
    NEVERIMPLEMENTED = "NEVER_IMPLEMENTED",
    REGREMOVED = "REG_REMOVED",
    VALUEUNITS = "VALUE_UNITS",
    VALUETYPE = "VALUE_TYPE",
    VALUERATE = "VALUE_RATE"
  ) |>
  # Select order and only variables in MH ODM processed dataset
  dplyr::select(
    CLUSTER_ID, REG_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION,
    ACTIONTYPE, AMENDMENTNUMBER, ACCOUNTABILITYMEASURE, MANAGEMENTCATEGORY,
    MANAGEMENT_TYPECLEAN, MANAGEMENT_STATUSCLEAN, JURISDICTION,
    SECTOR_CLEAN, SUBSECTOR_CLEAN, REGION, ZONE_CLEAN, JURISDICTIONALWATERS,
    COMMON_NAME_CLEAN, SPECIES_ITIS_CLEAN, SPP_TYPE, SPP_NAME,
    ADJUSTMENT, REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE, STARTDATE_CLEAN,
    END_DATE, STARTMONTH, STARTDAY_CLEAN,
    STARTYEAR, STARTTIME_CLEAN, START_DAYOF_WEEK_CLEAN, ENDDAY_CLEAN,
    END_DAYOF_WEEK_CLEAN, ENDMONTH_CLEAN,
    ENDTIME_CLEAN, ENDYEAR_CLEAN, NEVERIMPLEMENTED, REGREMOVED, VALUE,
    VALUEUNITS, VALUETYPE, VALUERATE, FLAG
  )

# Check that the number of starting FRs matches the number of FRs after
# processing
lost_frs <- setdiff(mh_raw$FR_CITATION, fishhist$FR_CITATION)
if (length(lost_frs) > 0) {
  warning(
    "FRs lost somewhere in processing code - ",
    paste(lost_frs, collapse = ", ")
  )
}

usethis::use_data(fishhist, overwrite = TRUE)
