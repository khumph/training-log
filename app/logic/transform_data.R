box::use(
  dplyr,
  googlesheets4,
  lubridate,
  stats[median],
  tidyr,
  tools,
)

#' @export
get_raw_data <- function(url, metric) {
  if (metric == "dur") {
    googlesheets4$read_sheet(
      googlesheets4$as_sheets_id(url),
      sheet = "log"
    )
  } else {
    process_data(
      googlesheets4$read_sheet(
        googlesheets4$as_sheets_id(url),
        sheet = "lift",
        col_types = "Dccdidddiddc"
      )
    )
  }
}

#' @export
process_data <- function(data) {
  data$pct1rm <- round(pct_1rm(data$reps, data$rpe), 2)
  data$e1rm <- round(data$weight / data$pct1rm)
  data$variation <- replace(data$variation, is.na(data$variation), "")
  data$lift_full <- paste(data$variation, data$lift)
  data$lift_full <- tools$toTitleCase(data$lift_full)
  return(data)
}

#' @export
pct_1rm <- function(reps, rpe) {
  result <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  result <- replace(result, rpe == 10 & reps == 1, 1)
  result <- replace(result, rpe == 10 & reps == 2, 0.98)
  return(result)
}

#' @export
select_data <- function(raw_data, metric, date_range) {
  if (metric == "dur") {
    dat <- raw_data |>
      dplyr$filter(!is.na(date)) |>
      dplyr$mutate(
        date = lubridate$force_tz(date, "America/Los_Angeles"),
        iso_wk = lubridate$isoweek(date),
        yr_wk = paste0(lubridate$year(date), "-", iso_wk)
      ) |>
      dplyr$group_by(yr_wk) |>
      dplyr$mutate(wk_st = min(date)) |>
      dplyr$ungroup() |>
      dplyr$filter(wk_st >= date_range[1], wk_st <= date_range[2])
  } else {
    dat <- get_metrics(raw_data, date_range) |>
      dplyr$group_by(date) |>
      dplyr$filter(param == metric)
  }
  dat
}

get_metrics <- function(data, date_range) {
  data <- dplyr$filter(
    data, .data$date >= date_range[1], .data$date <= date_range[2],
    !is.na(.data$weight) | !is.na(.data$time) | !is.na(.data$rpe)
  )
  data <- dplyr$group_by(data, date, lift_full)
  data$tonnage <- data$weight * data$reps
  data <- dplyr$summarise(
    data,
    e1rm = max(.data$e1rm),
    tonnage = sum(.data$tonnage),
    volume = sum(.data$reps),
    intensity = mean(.data$pct1rm),
    load = unique(.data$rpe * .data$time),

  )
  data <- tidyr$gather(data, param, value, -date, -lift_full)
  data <- dplyr$filter(data, !is.na(.data$value))
  return(data)
}

#' @export
add_est_weights <- function(url, date_range) {
  googlesheets4$gs4_auth(email = "khumph2@gmail.com")
  raw_data <- get_raw_data(url = url, metric = "e1rm")
  selected_data <- get_metrics(raw_data, date_range)
  dat <- dplyr$filter(raw_data, date >= Sys.Date())
  in_e1rm <- selected_data |>
    dplyr$filter(param == "e1rm", .data$lift_full %in% unique(dat$lift_full)) |>
    dplyr$group_by(lift_full) |>
    dplyr$summarise(ee1rm = median(value, na.rm = TRUE))
  out <- dplyr$left_join(dat, in_e1rm, by = "lift_full") |>
    dplyr$mutate(
      weight_p = dplyr$if_else(
        is.na(weight_p),
        (round(pct_1rm(reps_p, rpe_p) * ee1rm / 2.5) * 2.5),
        weight_p
      ),
      date = as.Date(date)
    ) |>
    dplyr$select(
      date, lift, variation, weight, reps, rpe, time, weight_p, reps_p,
      rpe_p, time_p, notes
    ) |>
    dplyr$group_by(date) |>
    dplyr$do(dplyr$add_row(., .before = 0)) |>
    dplyr$ungroup()

  dat_keep <- dplyr$filter(raw_data, date < Sys.Date())
  empty_rows <- which(is.na(raw_data$date))
  empty_rows_in_fut <-
    empty_rows >= which.max(raw_data$date[raw_data$date < Sys.Date()])
  empty_rows_in_past <- empty_rows[!empty_rows_in_fut]
  rows_to_del <- (empty_rows_in_past - seq_along(empty_rows_in_past)) + 2

  start <- nrow(dat_keep) + 1
  end <- nrow(raw_data) - (length(rows_to_del))
  bounds_to_del <- c(start, end) + 1

  ss <- googlesheets4$as_sheets_id(url, sheet = "lift")
  for (row in rows_to_del) {
    googlesheets4$range_delete(ss, sheet = "lift",
                               range = googlesheets4$cell_rows(row))
  }
  googlesheets4$range_delete(
    ss,
    sheet = "lift",
    range = googlesheets4$cell_rows(bounds_to_del)
  )
  googlesheets4$sheet_append(ss, out, sheet = "lift")
}
