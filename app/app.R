box::use(
  dplyr,
  htmltools,
  plotly,
  shiny,
)
box::use(
  app/logic/transform_data,
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
    transform_data$get_raw_data(
      url = input$url,
      metric = input$metric
    )
  })

  selected_data <- shiny$reactive({
    transform_data$select_data(
      raw_data = raw_data(),
      metric = input$metric,
      date_range = input$date_range
    )
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
    transform_data$add_est_weights(input$url, input$date_range)
  })
}

shiny$shinyApp(ui = ui, server = server)
