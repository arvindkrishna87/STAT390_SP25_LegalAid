# STAT 390 Presentation 3

# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(skimr)
library(ggthemes)
library(DT)
library(ggalt)
library(htmltools)
library(dplyr)
library(ggplot2)
library(lubridate)
library(shinydashboard)

special_intake_lines <- c(
  '13123478347', '18882652188', '13124235904', '13122296344', '13124235900',
  '13122296071', '13123478392', '13124235909', '13123478309', '13122296072',
  '13124235938', '13124312101', '13123478340', '13122296073', '18004459025',
  '18884018200', '13122296014'
)

df <- read.csv("all_calls_concat.csv") %>% 
  janitor::clean_names()

# Define UI
ui <- fluidPage(
  # Custom CSS styling
  tags$head(
    tags$style(HTML("      
      body {
        background-color: #1d285c !important;
        color: white;
      }
      .well {
        background-color: #2c3e50;
        border: none;
      }
      .box, .box-header {
        background-color: #2c3e50;
        color: white;
      }
      .value-box, .value-box .inner {
        background-color: #34495e;
        color: white;
      }
    "))
  ),
  
  # Logo
  tags$div(
    tags$img(src = "legal_aid.png", height = "80px"),
    style = "text-align:center; padding: 10px;"
  ),
  
  # App title and layout
  titlePanel("Special Intake Line Call Dashboard"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = c(".xlsx", ".xls")),
      selectInput("direction", "Call Direction", choices = c("All", unique(df$direction))),
      dateRangeInput("dateRange", "Select Date Range", start = min(df$date), end = max(df$date)),
      selectInput(
        inputId = "month",
        label = "Select Month",
        choices = NULL,  # We'll update choices dynamically in server
        selected = NULL
      )
    ),
    mainPanel(
      plotOutput("callVolumePlot"),
      plotOutput("callDurationBox")
      # tableOutput("repeatCallers")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- df
    if (input$direction != "All") {
      data <- data[data$direction == input$direction, ]
    }
    data <- data[data$date >= input$dateRange[1] & data$date <= input$dateRange[2], ]
    data
  })
  
  observe({
    df_months <- filtered_data()
    updateSelectInput(
      session,
      "selected_month",
      choices = unique(df_months$month),
      selected = unique(df_months$month)[1]
    )
  })
  
  filtered_month_data <- reactive({
    req(input$selected_month)
    filtered_data() %>%
      filter(month == input$selected_month)
  })
  
  output$callVolumePlot <- renderPlot({
    filtered <- filtered_data()

    special_calls <- filtered[filtered$called_number %in% special_intake_lines, ]

    ggplot(special_calls, aes(x = factor(called_number))) +
      geom_bar(fill = "#95d2cb") +
      labs(title = "Call Volume by Intake Line", x = "Intake Line", y = "Call Count") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#1d285c", color = NA),
            panel.background = element_rect(fill = "#1d285c"),
            text = element_text(color = "white"),
            axis.text.x = element_text(angle = 45, hjust = 1, color = "white")) # Rotated labels for readability

    # ggplot(filtered, aes(x = factor(`Called number (clean)`))) +
    #   geom_bar(fill = "skyblue") +
    #   labs(title = "Call Volume by Intake Line", x = "Intake Line", y = "Call Count") +
    #   theme_minimal() +
    #   theme(plot.background = element_rect(fill = "#1d285c", color = NA),
    #         panel.background = element_rect(fill = "#1d285c"),
    #         text = element_text(color = "white"))
  })
 
  output$callDurationBox <- renderPlot({
    filtered <- filtered_data()
    req(filtered, nrow(filtered) > 0)
    
    # Ensure duration is numeric and NA values removed
    filtered <- filtered %>%
      filter(!is.na(duration), called_number %in% special_intake_lines)
    
    filtered <- filtered %>%
      group_by(called_number) %>%
      mutate(
        Q1 = quantile(duration, 0.25),
        Q3 = quantile(duration, 0.75),
        IQR = Q3 - Q1
      ) %>%
      filter(duration >= (Q1 - 1.5 * IQR) & duration <= (Q3 + 1.5 * IQR)) %>%
      ungroup()
    
    special_calls <- filtered[filtered$called_number %in% special_intake_lines, ]
    
    ggplot(special_calls, aes(x = factor(called_number), y = duration)) +
      geom_boxplot(fill = "#ea452d") +
      labs(title = "Call Duration by Intake Line", x = "Intake Line", y = "Duration") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#1d285c", color = NA),
        panel.background = element_rect(fill = "white"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "white"),
        plot.title = element_text(color = "white"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "white")
      )
  })
  
  # output$callDurationBox <- renderPlot({
  #   filtered <- filtered_data()
  #   special_calls <- filtered[filtered$called_number %in% special_intake_lines, ]
  #   duration_summary <- filtered %>%
  #     group_by(called_number) %>%
  #     summarise(Duration = sum(duration, na.rm = TRUE))
  #   ggplot(filtered, aes(x = factor(called_number), y = duration)) +
  #     geom_boxplot(fill = "#ea452d") +
  #     labs(title = "Call Duration by Intake Line", x = "Intake Line", y = "Duration") +
  #     theme_minimal() +
  #     theme(plot.background = element_rect(fill = "#1d285c", color = NA),
  #           panel.background = element_rect(fill = "#1d285c"),
  #           text = element_text(color = "white"))
  # })
  # 
  output$repeatCallers <- renderTable({
    filtered <- filtered_month_data()
    filtered %>%
      group_by(correlation_id, called_number) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      filter(Count > 1) %>%
      arrange(desc(Count))
  })
}

# Run app
shinyApp(ui, server)

# Sample Data (replace with your actual data)
# df <- read.csv("your_data.csv")
# 

# 
# special_lines <- c("Line A", "Line B")
# 
# df <- df %>%
#   mutate(
#     Outside_Business_Hours = ifelse(Hour < 9 | Hour >= 17, TRUE, FALSE),
#     DateTime = as.POSIXct(paste(Date, paste0(Hour, ":00:00")), format = "%Y-%m-%d %H:%M:%S")
#   )
# 
# ui <- dashboardPage(
#   dashboardHeader(title = "Call Dashboard"),
#   dashboardSidebar(
#     dateRangeInput("date_range", "Select Date Range:",
#                    start = min(df$Date), end = max(df$Date)),
#     selectInput("intake_line", "Select Intake Line:",
#                 choices = unique(df$Called_number), selected = unique(df$Called_number), multiple = TRUE),
#     selectInput("direction", "Direction:", choices = c("All", unique(df$Direction))),
#     checkboxInput("outside_hours", "Only Outside Business Hours", value = FALSE)
#   ),
#   dashboardBody(
#     fluidRow(
#       valueBoxOutput("total_calls"),
#       valueBoxOutput("avg_duration"),
#       valueBoxOutput("repeat_callers")
#     ),
#     fluidRow(
#       box(plotOutput("call_volume_plot"), width = 6),
#       box(plotOutput("duration_boxplot"), width = 6)
#     ),
#     fluidRow(
#       box(dataTableOutput("repeat_table"), width = 12)
#     )
#   )
# )
# 
# server <- function(input, output) {
#   filtered_data <- reactive({
#     data <- df %>%
#       filter(Date >= input$date_range[1], Date <= input$date_range[2],
#              Called_number %in% input$intake_line)
#     if (input$direction != "All") {
#       data <- data %>% filter(Direction == input$direction)
#     }
#     if (input$outside_hours) {
#       data <- data %>% filter(Outside_Business_Hours == TRUE)
#     }
#     data
#   })
#   
#   output$total_calls <- renderValueBox({
#     valueBox(nrow(filtered_data()), "Total Calls", icon = icon("phone"), color = "blue")
#   })
#   
#   output$avg_duration <- renderValueBox({
#     avg <- round(mean(filtered_data()$Duration_min), 2)
#     valueBox(avg, "Average Duration (min)", icon = icon("clock"), color = "green")
#   })
#   
#   output$repeat_callers <- renderValueBox({
#     count <- filtered_data() %>% count(Correlation_ID) %>% filter(n > 1) %>% nrow()
#     valueBox(count, "Repeat Callers", icon = icon("redo"), color = "orange")
#   })
#   
#   output$call_volume_plot <- renderPlot({
#     filtered_data() %>%
#       group_by(Date, Called_number) %>%
#       summarise(Call_Count = n()) %>%
#       ggplot(aes(x = Date, y = Call_Count, fill = Called_number)) +
#       geom_col(position = "dodge") +
#       labs(title = "Daily Call Volume by Intake Line", x = "Date", y = "Calls")
#   })
#   
#   output$duration_boxplot <- renderPlot({
#     ggplot(filtered_data(), aes(x = Called_number, y = Duration_min, fill = Called_number)) +
#       geom_boxplot() +
#       labs(title = "Call Duration by Intake Line", x = "Intake Line", y = "Duration (min)")
#   })
#   
#   output$repeat_table <- renderDataTable({
#     filtered_data() %>%
#       group_by(Correlation_ID, Called_number) %>%
#       summarise(Call_Count = n(), Total_Duration = sum(Duration_min)) %>%
#       filter(Call_Count > 1)
#   })
# }
# 
# shinyApp(ui, server)