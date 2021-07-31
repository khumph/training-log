library(dplyr) 
library(googlesheets4)
library(shiny)
library(plotly)

options(gargle_oauth_cache = ".secrets")

last_month <- seq(Sys.Date(), length.out = 2, by = "-1 months") %>% tail(1)

metric_labels <- c("Estimated 1RM", "Volume", "Intensity", "Tonnage", "Training Load", "Duration")
metrics <- c("e1rm", "volume", "intensity", "tonnage", "load", "dur")
names(metrics) <- metric_labels

captions <- c(
  "Estimated 1 rep max over time.",
  "Volume over time. Volume is the total number of repetitions (i.e., sets times reps) performed on a given day",
  "Average intensity over time. Averge intensity is the average load, as measured by percent of estimated 1RM, across all working sets of a given lift on a given day.",
  "Tonnage over time. Tonnage is the total weight lifted (i.e., weight times sets times reps).",
  "Training load in aribitrary units (A.U.) over time. Arbitrary units are defined as the session RPE times the duration of the session in minutes.",
  "Duration of training per week compared to plan."
  )
names(captions) <- metrics

ylabs <- c(
  'Estimated 1RM',
  'Volume (repetitions)',
  'Average intensity (% of estimated 1RM)',
  "Tonnage (lbs)",
  "Training load (A.U.)",
  "Duration (hours)"
)
names(ylabs) <- metrics

pct_1rm <- function(reps, rpe) {
  x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
  x <- replace(x, rpe == 10 & reps == 1, 1)
  x <- replace(x, rpe == 10 & reps == 2, 0.98)
  return(x)
}

process_data <- function(data) {
  data$pct1rm <- round(pct_1rm(data$reps, data$rpe), 2)
  data$e1rm <- round(data$weight / data$pct1rm)
  data$variation <- replace(data$variation, is.na(data$variation), "")
  data$lift_full <- paste(data$variation, data$lift)
  data$lift_full <- tools::toTitleCase(data$lift_full)
  return(data)
}

get_metrics <- function(data, date_range) {
  data <- dplyr::filter(data, date >= date_range[1], date <= date_range[2],
                        !is.na(weight) | !is.na(time) | !is.na(rpe))
  data <- dplyr::group_by(data, date, lift_full)
  data$tonnage <- data$weight * data$reps
  data <- dplyr::summarise(
    data,
    e1rm = max(e1rm),
    tonnage = sum(tonnage),
    volume = sum(reps),
    intensity = mean(pct1rm),
    load = unique(rpe * time)
  ) 
  data <- tidyr::gather(data, param, value, -date, -lift_full)
  data <- dplyr::filter(data, !is.na(value))
  return(data)
}

ui <- fluidPage(
  headerPanel("Training log"),
  sidebarPanel(
    textInput(
      "url",
      "URL to Google sheet with your training data. Must be shared publicly via link.",
      "https://docs.google.com/spreadsheets/d/1bj9p-qcq6484bHZwfPVMoG8KcqEvjzXOoOJQxtwiTEc"
    ),
    # textInput(
    #   "email",
    #   "Email the Google sheet is associated with",
    #   "khumph2@gmail.com"
    # ),
    dateRangeInput("date_range", "Date range", Sys.Date() - 28, Sys.Date()),
    selectInput("metric", "Metric", metrics),
    # checkboxInput("overall", "Summarize over lifts"),
    # checkboxInput("ind", "Plot lifts individually"),
    paste("Calcuate estimated working weights based off your best estimated",
    "1RM over the date range specified above:"),
    br(), 
    actionButton("est", "Add estimated weights"),
    br(),
    "This estimates working weights for any blank",
    code("weight_p"),
    "rows you have. Requires values for",
    code("rpe_p"),
    "and",
    code("reps_p"),
    paste("in each row you want to estimate. Currently only works for the",
          "developer's google account due to limitations in the packages",
          "performing Google authentication.")
  ), 
  mainPanel(
    textOutput("caption"),
    plotlyOutput("plot1")
  )
)

server <- function(input, output, session) {
  
  rawData <- reactive({
    googlesheets4::gs4_deauth()
    if (input$metric == "dur") {
      sheet <-"log"
      read_sheet(as_sheets_id(input$url), sheet = sheet)
    } else {
      sheet <- "lift"
      process_data(read_sheet(as_sheets_id(input$url),
                              sheet = sheet, col_types = "Dccdidididic"))
    }
  })
  
  selectedData <- reactive({
    
    if (input$metric == "dur") {
     dat <- rawData() %>%
       filter(!is.na(date)) %>% 
       mutate(date = lubridate::force_tz(date, "America/Los_Angeles"),
              iso_wk = lubridate::isoweek(date),
              yr_wk = paste0(lubridate::year(date), "-", iso_wk)) %>% 
       group_by(yr_wk) %>% 
       mutate(wk_st = min(date)) %>% 
       ungroup() %>% 
       filter(wk_st >= input$date_range[1], wk_st <= input$date_range[2])
    } else {
      dat <- get_metrics(rawData(), input$date_range) %>% 
        dplyr::group_by(date) %>%
        dplyr::filter(param == input$metric)
    }
  
    # if (input$overall && input$metric %in% c("intensity")) {
    #   dplyr::summarise(dat, value = mean(value), param = unique(param), lift_full = "Overall")
    # } else if (input$overall && input$metric %in% c("volume", "tonnage")) {
    #   dplyr::summarise(dat, value = sum(value), param = unique(param), lift_full = "Overall")
    # } else { 
      dat 
    # }
  })
  
  output$plot1 <- renderPlotly({
    if (input$metric == "dur") {
      out <- selectedData() %>%
        dplyr::group_by(wk_st) %>% 
        dplyr::summarise(value = round(sum(dur, na.rm = T)/60, 1),
                  value_p = round(sum(dur_p, na.rm = T)/60, 1)) %>%
        plotly::plot_ly(x = ~wk_st, y = ~value_p, name = "Planned", type = "bar",
                        alpha = 0.5, height = 800) %>%
        plotly::layout(legend = list(title = list(text="Duration"),
                                     orientation = "h"))
        out <- out %>% plotly::add_trace(x = ~wk_st, y = ~value, name = "Actual",
                                         alpha = 1)
    } else {
      out <- selectedData() %>%
        dplyr::filter(param == input$metric) %>%
        dplyr::group_by(lift_full) %>%
        plotly::plot_ly(x = ~date, y = ~value, color =~lift_full, type = "scatter",
                        mode = "lines+markers", height = 800) %>%
        plotly::layout(legend = list(title = list(text="Lift"), orientation = "h"))
    }
      
    # if (input$ind) {
    #   out + facet_grid(lift_full ~ .)
    # }
    # if (input$overall && input$metric != "e1rm" || input$metric == "load") {
    #   out %>% plotly::layout(showlegend = FALSE)
    # } else {
      out
    # }
  })#, height = function() length(unique(selectedData()$lift_full)) * 150)
  
  output$caption <- renderText({
    captions[[input$metric]]
  })
  
  observeEvent(input$est, {
    email <- "khumph2@gmail.com"
    googlesheets4::gs4_auth(email = email)
    rawData <- process_data(read_sheet(as_sheets_id(input$url), col_types = "Dccdidididic"))
    selectedData <- get_metrics(rawData, input$date_range)
    dat <- rawData %>% filter(date >= Sys.Date())
    in_e1rm <- selectedData %>%
      filter(param == "e1rm", lift_full %in% unique(dat$lift_full)) %>%
      group_by(lift_full) %>%
      summarise(ee1rm = max(value))
    out <- left_join(dat, in_e1rm, by = 'lift_full') %>% 
      mutate(weight_p = if_else(is.na(weight_p),
                                (round(pct_1rm(reps_p, rpe_p) * ee1rm / 2.5) * 2.5),
                                weight_p),
             date = as.Date(date)) %>% 
      select(date, lift, variation, weight, reps, rpe, time, weight_p, reps_p,
             rpe_p, time_p, notes) %>% 
      group_by(date) %>% 
      do(add_row(., .before = 0)) %>% 
      ungroup()
    
    dat_keep <- rawData %>% filter(date < Sys.Date())
    empty_rows <- which(is.na(rawData$date))
    empty_rows_in_fut <- empty_rows >= which.max(rawData$date[rawData$date < Sys.Date()])
    empty_rows_in_past <- empty_rows[!empty_rows_in_fut]
    rows_to_del <- (empty_rows_in_past - seq_along(empty_rows_in_past)) + 2
    
    start <- nrow(dat_keep) + 1
    end <- nrow(rawData) - (length(rows_to_del))
    bounds_to_del <- c(start, end) + 1
    
    ss <- as_sheets_id(input$url)
    for (row in rows_to_del) {
      googlesheets4::range_delete(ss, range = cell_rows(row))
    }
    googlesheets4::range_delete(ss, range = cell_rows(bounds_to_del))
    googlesheets4::sheet_append(ss, out, sheet = "log")
  })
}

shinyApp(ui = ui, server = server)
