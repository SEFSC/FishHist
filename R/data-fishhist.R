#' Management History data through 2026
#'
#' A processed and cleaned management history dataframe.
#'
#' @format A data frame with 45 variables:
#' \describe{
#'   \item{CLUSTER_ID}{Indicates the cluster ID number to which the regulation
#'          relates. A cluster is a group of like MANAGEMENT_TYPEs for like
#'          species/species groupings within the same FMP, JURISDICTION,
#'          JURISDICTIONAL_WATERS, REGION, SECTOR_CLEAN, and SUBSECTOR_CLEAN
#'          that overwrite one another (i.e., BAG LIMIT for Red Snapper in the
#'          Gulf of America). Regulations within the same cluster will have the
#'          same cluster ID number.}
#'   \item{REG_ID}{Unique identifier for each regulation entered into the
#'          database. Two rows can have the same REGULATION_ID if the
#'          regulation is recurring or if the regulation applies to multiple
#'          species/regions/zones/sectors/subsectors.}
#'   \item{FR_CITATION}{Federal Regulation Citation Number including the
#'          volume and page number (e.g., 49 FR 34590 was published in volume
#'          49 and begins on page 34590) for regulations published in an FR
#'          Notice. Occasionally, the first page of an FR notice is blank.
#'          In those cases, the FR Citation was recorded using the page number
#'          of the page where text related to the FR Notice began.}
#'   \item{FR_SECTION}{Part, chapter, and section in which the regulation
#'          is contained (e.g., 50 CFR 622.39 is in Part 50, Chapter 622,
#'          section 39) within an FR Notice. If the “general” chapter is
#'          listed without section information (e.g. 50 CFR 622 instead of
#'          50 CFR 622.39), the regulation was not codified (e.g., temporary
#'          regulations) or relates to an IGNORABLE MANAGEMENT_TYPE.}
#'   \item{FR_URL}{Link to the volume containing the FR notice.}
#'   \item{FMP}{Fishery Management Plan to which the regulation applies.}
#'   \item{ACTION}{Indicates the Action listed at the top of the associated
#'          FR notice.}
#'   \item{ACTIONTYPE}{Indicates the Action Type listed at the top of the
#'          associated FR notice under “Action” and after the semicolon.
#'          Loosely defined and can be left null.}
#'   \item{AMENDMENTNUMBER}{Specifies the Amendment number if “Amendment”
#'          is listed for the ACTION within the FR notice.}
#'   \item{ACCOUNTABILITYMEASURE}{A YES/NO field to indicate whether an ACL
#'          allocation was or is projected to be reached. If the resulting
#'          management measures contained in the FR Notice are intended to
#'          mitigate an overage, this field will show “YES”. This field is
#'          recorded at the FR level so all associated regulations will show
#'          "YES”.}
#'   \item{MANAGEMENTCATEGORY}{General grouping that contains more specific
#'          and related management measures (MANAGEMENT_TYPE). For example,
#'          the MANAGEMENT_CATEGORY of CATCH_LIMITS contains MANAGEMENT_TYPEs
#'          that implement catch allocations (QUOTA, ACL, ACT, etc.).}
#'   \item{MANAGEMENT_TYPECLEAN}{Specific management measure implemented,
#'          adjusted to account for “ADJUSTMENT” MANAGEMENT_TYPEs. Removes
#'          the word “ADJUSTMENT” from the MANAGEMENT_TYPE so similar
#'          MANAGEMENT_TYPEs can be grouped within the same CLUSTER to
#'          appropriately convey the history of the regulation type
#'          (i.e., BAG LIMIT and BAG LIMIT ADJUSTMENT are now in the same
#'          CLUSTER with the “ADJUSTMENT” appearing as a flag in a separate
#'          field). Renames the MANAGEMENT_TYPE of “REOPENING” to “CLOSURE”
#'          since these MANAGEMENT_TYPEs should be in the same CLUSTER to be
#'          processed together. If a regulation was a “REOPENING” the VALUE =
#'          “OPEN”. Otherwise, the MANAGEMENT_TYPE was retained.}
#'   \item{MANAGEMENT_STATUSCLEAN}{Frequency of occurrence for the regulation,
#'          adjusted to remove null values. A null management status is
#'          interpreted as “until changed.” Since end dates are filled in
#'          during processing, null management types are treated the same as
#'          management status “ONCE”. Therefore, when MANAGEMENT_STATUS is
#'          null, then MANAGEMENT_STATUS_CLEAN = “ONCE”. Otherwise, the
#'          MANAGEMENT_STATUS was retained. Includes DAILY, DELAYED, MONTHLY
#'          RECURRING, ONCE, SEASONAL, WEEKLY RECURRING, and WITHDRAWN.}
#'   \item{JURISDICTION}{Federal or State authority over the regulation.}
#'   \item{SECTOR_CLEAN}{Indicates the individual sector for which the
#'          regulation relates (Commercial or Recreational) after the
#'          expansion of the SECTOR field. If a regulation was initially
#'          entered as “ALL” for the SECTOR field during data entry, the
#'          processing expansion created two records with “COMMERCIAL” and
#'          “RECREATIONAL” under SECTOR_CLEAN.}
#'   \item{SUBSECTOR_CLEAN}{Fleet or fishing gear used in the Commercial or
#'          Recreational fisheries to which the regulation applies, expanded
#'          to indicate the specific subsector that the regulation relates
#'          to (i.e., a regulation entered as SECTOR: RECREATIONAL SUBSECTOR:
#'          ALL would be expanded into 2 records with one indicates as
#'          SECTOR_CLEAN: RECREATIONAL SUBSECTOR_CLEAN: PRIVATE and the other
#'          as SECTOR_CLEAN: RECREATIONAL SUBSECTOR_CLEAN: FOR-HIRE).}
#'   \item{REGION}{The area managed by the FMP and thus to which the regulation
#'          applies: Caribbean, Gulf of America, North Atlantic, Mid Atlantic,
#'          or South Atlantic.}
#'   \item{ZONE_CLEAN}{The specific area to which the regulation applies. The
#'          name of the ZONE matches the codified text, shapefile, or both.
#'          Coordinates are listed if applicable. If “ALL” is listed, the
#'          regulation applies to all zones/subzones in the selection REGION.
#'          Some regulations apply to a smaller spatial portion of the ZONE,
#'          unofficially referred to as a subzone in the database. A subzone is
#'          indicated by a “—” after the ZONE. Values within the ZONE field
#'          were standardized to remove abbreviations and consolidate duplicate
#'          ZONEs.}
#'   \item{JURISDICTIONALWATERS}{Federal (EEZ) or state waters in which the
#'          regulation applies.}
#'   \item{COMMON_NAME_CLEAN}{Indicates the common name for the individual
#'          species that the regulation relates to. During data entry, species
#'          information was entered using one of three fields (i.e.,
#'          COMMON_NAME, SPECIES_AGGREGATE, or SPECIES_GROUP) to indicate how
#'          the regulation was implemented within the FR notice (for a single
#'          species or a group of species). During data processing, any
#'          regulations that applied to multiple species (i.e.,
#'          SPECIES_AGGREGATE, SPECIES_GROUP, or COMMON_NAME == “ALL”) were
#'          expanded to create a row for each species within the indicated
#'          grouping. The previous three species fields were reformatted into
#'          the COMMON_NAME_CLEAN field to indicate the applicable individual
#'          species.}
#'   \item{SPECIES_ITIS_CLEAN}{Indicates the ITIS code for the individual
#'          species that the regulation relates to.}
#'   \item{SPP_TYPE}{Translated from the original species fields of COMMON_NAME,
#'          SPECIES_AGGREGATE, or SPECIES_GROUP to indicate how the species
#'          information was originally captured during data entry. Indicates
#'          the species category to which the SPECIES_NAME field relates.}
#'   \item{SPP_NAME}{Common name or species aggregate name. This allows you
#'          to trace back to whether the regulation was for an individual
#'          species or species within a grouping.}
#'   \item{ADJUSTMENT}{Flag to indicate whether the MANAGEMENT_TYPE for the
#'          regulation is an “ADJUSTMENT”. An “ADJUSTMENT” MANAGEMENT_TYPE
#'          indicates that a regulation is temporarily overwritten for a period
#'          of time, after which the previous regulation will continue to be in
#'          place. During data processing, “ADJUSTMENTs” that happen and then
#'          revert to prior regulations and re-openings that happen until a
#'          closure is re-implemented are treated the same and are flagged as
#'          an “ADJUSTMENT”. A value of 1 indicates that the MANAGEMENT_TYPE
#'          is an ADJUSTMENT, while 0 will indicate that it is not.}
#'   \item{REVERSION}{TRUE/FALSE field to indicate whether the record is a
#'          copy of a record that occurred before a given ADJUSTMENT record.
#'          Reversions are copies of records that occurred prior to an
#'          ADJUSTMENT and are reinstated following the end of the ADJUSTMENT
#'          period. A value of TRUE indicates that the record is a REVERSION.}
#'   \item{EFFECTIVE_DATE}{Date the regulation went into effect as stated in
#'          the Dates section of the FR Notice (MM/DD/YYYY). When the
#'          EFFECTIVE_DATE is equal to the INEFFECTIVE_DATE, the regulation
#'          has been removed (turned off).}
#'   \item{INEFFECTIVE_DATE}{Date the regulation was no longer in effect as
#'          stated in the Dates section of the FR Notice (MM/DD/YYYY). When the
#'          EFFECTIVE_DATE is equal to the INEFFECTIVE_DATE, the regulation has
#'          been removed (turned off).}
#'   \item{STARTDATE_CLEAN}{Adjusted START_DATE based on the circumstances of
#'          how the regulation changed throughout time. This field was adjusted
#'          based on the START_TIME of the regulation (i.e., if START_TIME ==
#'          11:59:00 PM the START_DAY was increased by one day to indicate that
#'          the next day is when the regulation went into effect) or the dates
#'          that species were removed from the FMP, SPECIES_AGGREGATE, or
#'          SPECIES_GROUP.}
#'   \item{END_DATE}{Adjusted END_DATE based on the circumstances of how the
#'          regulation changed throughout time (i.e., the dates that species
#'          were removed from the FMP, SPECIES_AGGREGATE, or SPECIES_GROUP;
#'          the date a regulation was overwritten by a subsequent regulation;
#'          the date the regulation was ineffective).}
#'   \item{STARTMONTH}{First month the regulation was in effect. If no
#'          START_DAY is listed, the regulation began at the start of the
#'          month.}
#'   \item{STARTDAY_CLEAN}{First day the regulation was in effect, adjusted for
#'          START_TIME. When the START_TIME = “11:59:00 PM” then a day was
#'          added to the START_DAY. Note that START_DAY_CLEAN may differ
#'          from the START_DATE (e.g., a seasonal regulation that was effective
#'          beginning on February 1, 2010 but only applied from June 1 through
#'          September 30 each year). If no START_TIME is listed, the regulation
#'          began at the start of the day.}
#'   \item{STARTYEAR}{The first year the regulation was in effect.}
#'   \item{STARTTIME_CLEAN}{Time the regulation began, in local time, adjusted
#'          to reflect START_DAY. When the START_TIME = “11:59:00 PM” it was
#'          removed and the START_DAY was increased by one day. Since we
#'          already adjusted the START_DATE_CLEAN, START_DAY, or
#'          START_DAY_OF_WEEK to reflect a START_TIME of “11:59:00 PM” we
#'          removed the value to avoid confusion. Otherwise, the START_TIME
#'          was retained. If the dates section of an FR notice recorded the
#'          start time as 2400 hours, the START_TIME field remained null
#'          because it is understood that the regulation began at the start of
#'          the day listed as the START_DAY.}
#'   \item{START_DAYOF_WEEK_CLEAN}{First day the regulation was in effect,
#'          adjusted for START_TIME. When START_DAY_OF_WEEK is populated and
#'          START_TIME = “11:59:00 PM”, a day was added to the
#'          START_DAY_OF_WEEK to correctly reflect the effectiveness of the
#'          regulation. If no START_TIME is listed, the regulation began at the
#'          start of the weekday. Used for DAILY or WEEKLY RECURRING
#'          MANAGEMENT_STATUS_CLEAN.}
#'   \item{ENDDAY_CLEAN}{Last day the regulation was in effect, adjusted to
#'          consider END_TIME. When END_TIME = "12:01:00 AM", the END_DAY_CLEAN
#'          is adjusted to be the day before the END_DAY listed. For
#'          regulations that were in effect “through the end of February”, the
#'          END_MONTH, END_DAY, and END_TIME fields were recorded as March 1 at
#'          12:01:00 AM. The END_DAY_CLEAN is then adjusted to February 28 or
#'          29 accordingly. If no END_TIME is listed, the regulation lasted
#'          through the end of the day.}
#'   \item{END_DAYOF_WEEK_CLEAN}{Last day the regulation was in effect,
#'          adjusted to consider END_TIME. When END_DAY_OF_WEEK is populated
#'          and END_TIME = "12:01:00 AM" then one day of the week was
#'          subtracted to adjust for the proper duration of the regulation.
#'          If no END_TIME is listed, the regulation lasted through the end of
#'          the weekday. Used for regulations that include a weekday component.}
#'   \item{ENDMONTH_CLEAN}{Last month the regulation was in effect, adjusted to
#'          consider END_TIME. When END_TIME = "12:01:00 AM" and the END_DAY
#'          was the first day of a month, the END_MONTH was subtracted by one
#'          month to adjust for the proper duration of the regulation. For
#'          regulations that were in effect “through the end of February”,
#'          the END_MONTH, END_DAY, and END_TIME fields were recorded as March
#'          1 at 12:01:00 AM. The END_MONTH_CLEAN was then adjusted to February.
#'          If no END_DAY is listed, the regulation lasted through the end of
#'          the month.}
#'   \item{ENDTIME_CLEAN}{Time the regulation ended, in local time, adjusted for
#'          END_DAY and STATUS_TYPE. When END_TIME was listed as “12:01:00 AM”
#'          and STATUS_TYPE = “RECURRING” and the END_DAY = 1 the END_TIME of
#'          “12:01:00 AM” was retained. Otherwise, END_TIME = "12:01:00 AM" or
#'          “11:59:00 PM” was removed. For recurring regulations, the END_TIME
#'          still needs to be considered and therefore is retained.}
#'   \item{ENDYEAR_CLEAN}{The last year the regulation was in effect, adjusted
#'          depending on the END_MONTH, END_DAY, and END_TIME.}
#'   \item{NEVERIMPLEMENTED}{Flag to indicate a regulation that was recorded
#'          in an FR notice but never went into effect due to a subsequent
#'          regulation being implemented and therefore overwriting the previous
#'          regulation before it was able to go into effect. A value of 1
#'          indicates that the regulation was never implemented while a value
#'          of 0 indicates that the regulation was implemented.}
#'   \item{REGREMOVED}{If the EFFECTIVE_DATE and INEFFECTIVE_DATE are the same,
#'          this means a regulation was removed and is no longer in effect. A
#'          value of 1 indicates that the regulation was removed while a value
#'          of 0 indicates that the regulation was not removed.}
#'   \item{VALUE}{Quantity or equation associated with the regulation. For the
#'          MANAGEMENT_TYPE_CLEAN of DEFINITION, the VALUE field reflects the
#'          term being defined. For the MANAGEMENT_TYPE_CLEAN of CLOSURE, the
#'          VALUE field indicates the status of the fishery (i.e., OPEN or
#'          CLOSE).}
#'   \item{VALUEUNITS}{Unit of measure on the VALUE.}
#'   \item{VALUETYPE}{Qualifier on the VALUE (e.g., total weight, gutted
#'          weight, etc.).}
#'   \item{VALUERATE}{Frequency on the VALUE.}
#'   \item{FLAG}{A YES/NO field to indicate if a regulation has extra details
#'          that were not able to be entered into the database due to
#'          constraints associated with the available fields.}
#' }
#' @docType data
#' @keywords datasets
#' @name fishhist
"fishhist"
