box::use(
  dplyr,
  lubridate,
  tidyr,
)

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
      dplyr$mutate(wk_st = min(date), .by = "yr_wk") |>
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
    data, date >= date_range[1], date <= date_range[2],
    !is.na(weight) | !is.na(time) | !is.na(rpe)
  )
  data <- dplyr$group_by(data, date, lift_full)
  data$tonnage <- data$weight * data$reps
  data <- dplyr$summarise(
    data,
    e1rm = max(e1rm),
    tonnage = sum(tonnage),
    volume = sum(reps),
    intensity = mean(pct1rm),
    load = unique(rpe * time)
  )
  data <- tidyr$gather(data, param, value, -date, -lift_full)
  data <- dplyr$filter(data, !is.na(value))
  return(data)
}
