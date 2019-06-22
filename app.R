ui <- fluidPage(
  headerPanel("Training log"),
  sidebarPanel(
    textInput("url", "Google sheet url", "https://docs.google.com/spreadsheets/d/1bj9p-qcq6484bHZwfPVMoG8KcqEvjzXOoOJQxtwiTEc/edit#gid=1211142681"),
    dateInput("filter_date", "Filter date", last_month),
    selectInput("metric", "Y Variable", metrics),
    checkboxInput("overall", "Summarize over lifts")
  ),
  mainPanel(
    textOutput("caption"),
    plotOutput("plot1")
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
      filter(date >= input$filter_date, !is.na(weight) | !is.na(time) | !is.na(rpe)) %>%
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
      
    if (input$overall && input$metric != "e1rm" || input$metric == "load") {
      out + theme(legend.position = "none")
    } else {
      out
    }
  })
  
  output$caption <- renderText({
    captions[[input$metric]]
  })
}

shinyApp(ui = ui, server = server)
