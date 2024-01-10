# Load necessary libraries
library(shiny)
library(ggplot2)
library(car)
library(lmtest)
library(DT)
library(corrplot)  

# Data
data <- data.frame(
  Month = month.abb[1:12],
  Visitors = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  Transactions = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  Items_Per_Transaction = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  Rating = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  Ads = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  Sales = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

# Initial multiple linear regression model
model <- lm(Sales ~ Visitors + Transactions + Items_Per_Transaction + Rating + Ads, data = data)

# Define UI
ui <- fluidPage(
  tags$style(
    HTML(
      "
      .sidebar {
        position: fixed;
        top: 0;
        left: 0;
        padding-top: 20px;
        padding-bottom: 20px;
        background-color: #f8f9fa; /* Optional: Add background color */
      }
      "
    )
  ),
  navbarPage(
    title = "Sales Prediction Dashboard",
    tabPanel("Data Exploration",
             tabsetPanel(
               tabPanel("View Data",
                        mainPanel(
                          fluidRow(
                            column(
                              width = 12,
                              h3("View Data:"),
                              DT::dataTableOutput("view_data")
                            )
                          )
                        )
               ),
               tabPanel("Statistics Descriptive",
                        mainPanel(
                          fluidRow(
                            column(
                              width = 12,
                              length = 12,
                              h3("Summary Statistics:"),
                              verbatimTextOutput("summary_statistics"),
                              h3("Correlation Matrix:"),
                              plotOutput("correlation_plot")
                            )
                          )
                        )
               )
             )
    ),
    tabPanel("Regression and Assumption",
             tabsetPanel(
               tabPanel("Multiple Linear Regression",
                        mainPanel(
                          h3("Model Summary:"),
                          verbatimTextOutput("model_summary"),
                          checkboxInput("include_interaction", "Include Interaction Term", value = FALSE)
                        )
               ),
               tabPanel("Assumption Tests",
                        mainPanel(
                          h4("Assumption Tests"),
                          verbatimTextOutput("assumption_tests")
                        )
               )
             )
    ),
    tabPanel("Sales Prediction",
             sidebarLayout(
               sidebarPanel(
                 numericInput("x1", "Number of Visitors", min = 1, max = 3000000, value = 200000),
                 numericInput("x2", "Number of Transactions", min = 1, max = 2000000, value = 10000),
                 numericInput("x3", "Average Items per Transaction", min = 1, max = 10000, value = 4.5),
                 sliderInput("x4", "Customer Rating", min = 1, max = 10, value = 8.5, step = 0.1),
                 numericInput("x5", "Number of Ads", min = 1, max = 800000, value = 30000)
               ),
               mainPanel(
                 h3("Estimated Monthly Sales Volume:"),
                 verbatimTextOutput("prediction")
               )
             )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Data Exploration
  output$view_data <- DT::renderDataTable({
    DT::datatable(data)
  })
  
  output$summary_statistics <- renderPrint({
    summary_stats <- summary(data)
    summary_stats
  })
  
  output$correlation_plot <- renderPlot({
    correlation_matrix <- cor(data[, sapply(data, is.numeric)])
    corrplot(correlation_matrix, method = "number", type = "upper", diag = FALSE)
  })
  
  # Regression and Assumption
  output$model_summary <- renderPrint({
    # Update model based on checkbox status
    if (input$include_interaction) {
      model <- lm(Sales ~ Visitors * Ads + Transactions + Items_Per_Transaction + Rating, data = data)
    } else {
      model <- lm(Sales ~ Visitors + Transactions + Items_Per_Transaction + Rating + Ads, data = data)
    }
    
    model_summary <- summary(model)
    model_summary
  })
  
  output$assumption_tests <- renderPrint({
    assumption_tests <- list(
      Autocorrelation = dwtest(model),
      Heteroskedasticity = bptest(model),
      Normality = list(Shapiro_Wilk = shapiro.test(model$residuals)),
      Multicollinearity = vif(model)
    )
    assumption_tests
  })
  
  # Sales Prediction
  output$prediction <- renderText({
    new_data <- data.frame(
      Visitors = input$x1,
      Transactions = input$x2,
      Items_Per_Transaction = input$x3,
      Rating = input$x4,
      Ads = input$x5
    )
    prediction <- predict(model, newdata = new_data)
    paste("Estimated Sales: $", round(prediction, 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
