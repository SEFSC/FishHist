#' Gantt chart of FMPs
#'
#' Gantt chart of the first and last Federal Register (FR)
#' published within a Fishery Management Plan (FMP) within the
#' Southeast Fisheries Management History dataset.
#'
#' @param data A dataframe (default=fishhist)
#' @return A ggplot object
#'
#' @examples gantt_FMP(fishhist)
#' @export
gantt_FMP <- function(data = fishhist) {
  custom_title_case <- function(text) {
    # 1. Convert everything to title case first using stringr::str_to_title
    title_text <- stringr::str_to_title(text)

    # 2. Words to force to LOWERCASE (unless they appear at the very start)
    lower_words <- c("of", "the", "and", "in", "on",
                     "at", "for", "to", "a", "an")
    lower_pattern <- base::paste0(
      "(?<!^)\\b(",
      base::paste(stringr::str_to_title(lower_words), collapse = "|"),
      ")\\b"
    )

    title_text <- stringr::str_replace_all(
      title_text,
      stringr::regex(lower_pattern),
      function(m) stringr::str_to_lower(m)
    )

    # 3. Acronyms to force to UPPERCASE
    upper_acronyms <- c("FMP", "EEZ", "U.S.")
    upper_pattern <- base::paste0(
      "\\b(",
      base::paste(stringr::str_to_title(upper_acronyms), collapse = "|"),
      ")\\b"
    )

    title_text <- stringr::str_replace_all(
      title_text,
      stringr::regex(upper_pattern, ignore_case = TRUE),
      function(m) stringr::str_to_upper(m)
    )

    return(title_text)
  }

  data_summary <- data |>
    dplyr::group_by(FMP) |>
    dplyr::summarise(
      start_year = min(lubridate::year(STARTDATE_CLEAN)),
      end_year = max(lubridate::year(END_DATE))
    ) |>
    # Arrange by region (if FMP in multiple regions, take first row)
    dplyr::left_join(data |> dplyr::select(FMP, REGION) |>
                       dplyr::distinct(), by = dplyr::join_by(FMP)) |>
    dplyr::group_by(FMP) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(REGION, start_year) |>
    dplyr::mutate(
      # Wrap long titles
      FMP = stringr::str_wrap(custom_title_case(FMP), width = 40),
      FMP = factor(FMP, levels = unique(FMP))
    )

  plot <- ggplot2::ggplot(data_summary, ggplot2::aes(y = FMP)) +
    ggplot2::geom_segment(ggplot2::aes(x = start_year,
                                       xend = end_year,
                                       yend = FMP),
      color = "steelblue", linewidth = 6, lineend = "round"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(min(data_summary$start_year, na.rm = TRUE),
      max(data_summary$end_year, na.rm = TRUE),
      by = 5
    )) +
    ggplot2::labs(x = "Year", y = "Fishery Management Plan") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 10, face = "bold")
    )

  return(plot)
}
