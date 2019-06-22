ui <- fluidPage(
  headerPanel("Training log"),
  sidebarPanel(
    textInput(
      "url",
      "URL to google sheet with your training data. Must be shared via link:",
      "https://docs.google.com/spreadsheets/d/1bj9p-qcq6484bHZwfPVMoG8KcqEvjzXOoOJQxtwiTEc/edit#gid=1211142681"
    ),
    dateRangeInput("date_range", "Date range", Sys.Date() - 28, Sys.Date()),
    selectInput("metric", "Metric", metrics),
    checkboxInput("overall", "Summarize over lifts"),
    checkboxInput("ind", "Plot lifts individually"),
    "Calcuate and copy estimated working weights based off your best estimated 1RM over the given interval by clicking this button:",
    br(), 
    actionButton("est", "Copy weights"),
    br(),
    "This outputs estimated working weights for any blank",
    code("weight_p"),
    "rows you have. Requires values for",
    code("rpe_p"),
    "and",
    code("reps_p"),
    paste0(
      "in each row you want to estimate, and the metric in this app to be \"",
      names(metrics[metrics == 'e1rm']),
      "\""
    )
  ), 
  mainPanel(
    textOutput("caption"),
    plotOutput("plot1", height = "600px")
  )
)

server <- function(input, output) {
  
  rawData <- reactive({
    sheets_deauth()
    read_sheet(as_sheets_id(input$url)) 
  })
  
  pct_1rm <- function(reps, rpe) {
    x <- 0.725 + 0.0275 * rpe - 0.0275 * reps
    x <- replace(x, rpe == 10 & reps == 1, 1)
    x <- replace(x, rpe == 10 & reps == 2, 0.98)
  }
  
  selectedData <- reactive({
    rawData() %>%
      mutate(
        date = as.Date(date),
        pct1rm = round(pct_1rm(reps, rpe), 2),
        e1rm = round(weight / pct1rm)
      ) %>%
      filter(date >= input$date_range[1], date <= input$date_range[2],
             !is.na(weight) | !is.na(time) | !is.na(rpe)) %>%
      group_by(date, lift) %>%
      mutate(tonnage = weight * reps) %>%
      summarise(
        e1rm = max(e1rm),
        tonnage = sum(tonnage),
        volume = sum(reps),
        intensity = mean(pct1rm),
        load = unique(rpe * time)
      ) %>%
      gather(param, value, -date, -lift) %>% 
      filter(!is.na(value)) %>%
      group_by(date) %>%
      filter(param == input$metric) -> 
      dat
  
    if (input$overall && input$metric %in% c("intensity")) {
      summarise(dat, value = mean(value), param = unique(param), lift = "Overall")
    } else if (input$overall && input$metric %in% c("volume", "tonnage")) {
      summarise(dat, value = sum(value), param = unique(param), lift = "Overall")
    } else { dat }
    
  })
  
  output$plot1 <- renderPlot({
    selectedData() %>%
      filter(param == input$metric) %>%
      ggplot(aes(
        x = date,
        y = value,
        color = lift,
        linetype = lift,
        group = lift,
      )) +
      geom_point() +
      labs(
        x = "Date",
        y = ylabs[[input$metric]],
        color = "Lift",
        linetype = "Lift"
      ) +
      geom_line() +
      theme_bw() + 
      theme(legend.position = "bottom") ->
      out
      
    if (input$ind) {
      out <- out + facet_grid(lift ~ ., scales = "free")
    }
    if (input$overall && input$metric != "e1rm" || input$metric == "load") {
      out + theme(legend.position = "none")
    } else {
      out
    }
  })
  
  output$caption <- renderText({
    captions[[input$metric]]
  })
  
  observeEvent(input$est, {
    dat_raw <- rawData()
    if (any(!is.na(dat_raw$reps_p) & !is.na(dat_raw$rpe_p) & is.na(dat_raw$weight_p))) {
      in_raw <- dat_raw %>%
        select(-ee1rm) %>%
        filter(!is.na(reps_p), !is.na(rpe_p), is.na(weight_p))
      in_e1rm <- selectedData() %>%
        filter(param == "e1rm", lift %in% unique(in_raw$lift)) %>%
        group_by(lift) %>%
        summarise(ee1rm = max(value))
      full_join(in_raw, in_e1rm, by = 'lift') %>% 
        mutate(weight_p = ceiling(pct_1rm(reps_p, rpe_p) * ee1rm / 5) * 5) %>%
        pull(weight_p) %>%
        clipr::write_clip(col.names = F)
    } 
  })
}

shinyApp(ui = ui, server = server)
