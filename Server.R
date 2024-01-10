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
    corrplot(correlation_matrix, method = "number",type = "upper", diag = FALSE)
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