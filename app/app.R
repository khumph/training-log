box::use(
  dplyr,
  googlesheets4,
  htmltools,
  lubridate,
  plotly,
  shiny,
  tidyr,
  tools,
)

metric_labels <- c(
  "Estimated 1RM", "Volume", "Intensity", "Tonnage",
  "Training Load", "Duration"
)
metrics <- c("e1rm", "volume", "intensity", "tonnage", "load", "dur")
names(metrics) <- metric_labels

captions <- c(
  "Estimated 1 rep max over time.",
  paste(
    "Volume over time. Volume is the total number of repetitions (i.e.,",
    "sets times reps) performed on a given day"
  ),
  paste(
    "Average intensity over time. Averge intensity is the average load,",
    "as measured by percent of estimated 1RM, across all working sets of",
    "a given lift on a given day."
  ),
  paste(
    "Tonnage over time. Tonnage is the total weight lifted (i.e., weight",
    "times sets times reps)."
  ),
  paste(
    "Training load in aribitrary units (A.U.) over time. Arbitrary units",
    "are defined as the session RPE times the duration of the session in",
    "minutes."
  ),
  "Duration of training per week compared to plan."
)
names(captions) <- metrics

ylabs <- c(
  "Estimated 1RM",
  "Volume (repetitions)",
  "Average intensity (% of estimated 1RM)",
  "Tonnage (lbs)",
  "Training load (A.U.)",
  "Duration (hours)"
)
names(ylabs) <- metrics

pct_1rm <- function(reps, rpe) {
  result <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  result <- replace(result, rpe == 10 & reps == 1, 1)
  result <- replace(result, rpe == 10 & reps == 2, 0.98)
  return(result)
}

process_data <- function(data) {
  data$pct1rm <- round(pct_1rm(data$reps, data$rpe), 2)
  data$e1rm <- round(data$weight / data$pct1rm)
  data$variation <- replace(data$variation, is.na(data$variation), "")
  data$lift_full <- paste(data$variation, data$lift)
  data$lift_full <- tools$toTitleCase(data$lift_full)
  return(data)
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


ui <- shiny$fluidPage(
  shiny$headerPanel("Training log"),
  shiny$sidebarPanel(
    shiny$textInput(
      "url",
      paste(
        "URL to Google sheet with your training data. Must be shared publicly",
        "via link."
      ),
      "1bj9p-qcq6484bHZwfPVMoG8KcqEvjzXOoOJQxtwiTEc"
    ),
    shiny$dateRangeInput(
      "date_range", "Date range", Sys.Date() - 28, Sys.Date()
    ),
    shiny$selectInput("metric", "Metric", metrics),
    paste(
      "Calcuate estimated working weights based off your best estimated",
      "1RM over the date range specified above:"
    ),
    htmltools$br(),
    shiny$actionButton("est", "Add estimated weights"),
    htmltools$br(),
    "This estimates working weights for any blank",
    htmltools$code("weight_p"),
    "rows you have. Requires values for",
    htmltools$code("rpe_p"),
    "and",
    htmltools$code("reps_p"),
    paste(
      "in each row you want to estimate. Currently only works for the",
      "developer's google account due to limitations in the packages",
      "performing Google authentication."
    )
  ),
  shiny$mainPanel(
    shiny$textOutput("caption"),
    plotly$plotlyOutput("plot1")
  )
)

server <- function(input, output, session) {
  raw_data <- shiny$reactive({
    googlesheets4$gs4_deauth()
    if (input$metric == "dur") {
      sheet <- "log"
      googlesheets4$read_sheet(
        googlesheets4$as_sheets_id(input$url),
        sheet = sheet
      )
    } else {
      sheet <- "lift"
      process_data(
        googlesheets4$read_sheet(
          googlesheets4$as_sheets_id(input$url),
          sheet = sheet,
          col_types = "Dccdidididic"
        )
      )
    }
  })

  selected_data <- shiny$reactive({
    if (input$metric == "dur") {
      dat <- raw_data() |>
        dplyr$filter(!is.na(date)) |>
        dplyr$mutate(
          date = lubridate$force_tz(date, "America/Los_Angeles"),
          iso_wk = lubridate$isoweek(date),
          yr_wk = paste0(lubridate$year(date), "-", iso_wk)
        ) |>
        dplyr$group_by(yr_wk) |>
        dplyr$mutate(wk_st = min(date)) |>
        dplyr$ungroup() |>
        dplyr$filter(wk_st >= input$date_range[1], wk_st <= input$date_range[2])
    } else {
      dat <- get_metrics(raw_data(), input$date_range) |>
        dplyr$group_by(date) |>
        dplyr$filter(param == input$metric)
    }
    dat
  })

  output$plot1 <- plotly$renderPlotly({
    if (input$metric == "dur") {
      out <- selected_data() |>
        dplyr$group_by(wk_st) |>
        dplyr$summarise(
          value = round(sum(dur, na.rm = TRUE) / 60, 1),
          value_p = round(sum(dur_p, na.rm = TRUE) / 60, 1)
        ) |>
        plotly$plot_ly(
          x = ~wk_st, y = ~value_p, name = "Planned", type = "bar",
          alpha = 0.5, height = 800
        ) |>
        plotly$layout(
          legend = list(orientation = "h"),
          yaxis = list(title = "Total duration (hours)"),
          xaxis = list(title = "Week")
        )
      out <- out |> plotly$add_trace(x = ~wk_st, y = ~value, name = "Actual")
    } else {
      out <- selected_data() |>
        dplyr$filter(param == input$metric) |>
        dplyr$group_by(lift_full) |>
        plotly$plot_ly(
          x = ~date, y = ~value, color = ~lift_full, type = "scatter",
          mode = "lines+markers", height = 800
        ) |>
        plotly$layout(legend = list(
          title = list(text = "Lift"),
          orientation = "h"
        ))
    }
    out
  })

  output$caption <- shiny$renderText({
    captions[[input$metric]]
  })

  shiny$observeEvent(input$est, {
    email <- "khumph2@gmail.com"
    googlesheets4$gs4_auth(email = email)
    raw_data <- process_data(googlesheets4$read_sheet(
      googlesheets4$as_sheets_id(input$url),
      sheet = "lift",
      col_types = "Dccdidddiddc"
    ))
    selected_data <- get_metrics(raw_data, input$date_range)
    dat <- dplyr$filter(raw_data, date >= Sys.Date())
    in_e1rm <- selected_data |>
      dplyr$filter(param == "e1rm", lift_full %in% unique(dat$lift_full)) |>
      dplyr$group_by(lift_full) |>
      dplyr$summarise(ee1rm = min(value))
    out <- dplyr$left_join(dat, in_e1rm, by = "lift_full") |>
      dplyr$mutate(
        weight_p = dplyr$if_else(is.na(weight_p),
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

    ss <- googlesheets4$as_sheets_id(input$url, sheet = "lift")
    for (row in rows_to_del) {
      googlesheets4$range_delete(ss, sheet = "lift",
                                 range = googlesheets4$cell_rows(row))
    }
    googlesheets4$range_delete(ss,
      sheet = "lift",
      range = googlesheets4$cell_rows(bounds_to_del)
    )
    googlesheets4$sheet_append(ss, out, sheet = "lift")
  })
}

shiny$shinyApp(ui = ui, server = server)
