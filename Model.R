# Salary Prediction Shiny App
# Libraries
library(shiny)
library(dplyr)
library(caret)

# Load and preprocess the salary dataset
load_and_preprocess_data <- function() {
  # Assuming salary_df is your original dataset
  # In a real app, you'd load the data from a file or database
  salary_data <- salary_df %>% 
    mutate(
      experience_level = as.factor(experience_level),
      employment_type = as.factor(employment_type),
      job_title = as.factor(job_title),
      company_size = as.factor(company_size)
    )
  
  return(salary_data)
}

# Train the regression model
train_regression_model <- function(data) {
  # Set seed for reproducibility
  set.seed(123)
  
  # Split the data
  train_index <- createDataPartition(data$salary_in_usd, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  
  # Fit the multiple linear regression model
  model <- lm(salary_in_usd ~ experience_level + job_title + 
                employment_type + remote_ratio + company_size, 
              data = train_data)
  
  return(model)
}

# Calculate model performance metrics
calculate_model_metrics <- function(model) {
  # Extract performance metrics
  metrics <- list(
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    f_statistic = summary(model)$fstatistic[1],
    p_value = pf(summary(model)$fstatistic[1], 
                 summary(model)$fstatistic[2], 
                 summary(model)$fstatistic[3], 
                 lower.tail = FALSE)
  )
  
  return(metrics)
}

# Map remote_ratio categories to numerical values
remote_ratio_map <- function(category) {
  switch(category,
         "fully-remote" = 100,
         "hybrid" = 50,
         "onsite" = 0)
}

# Preprocess data and train model
salary_data <- load_and_preprocess_data()
regression_model <- train_regression_model(salary_data)

# UI
ui <- fluidPage(
  # App title
  titlePanel("Salary Prediction Model"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Experience Level Input
      selectInput("experience_level", "Experience Level",
                  choices = unique(salary_data$experience_level)),
      
      # Job Title Input
      selectInput("job_title", "Job Title", 
                  choices = unique(salary_data$job_title)),
      
      # Employment Type Input
      selectInput("employment_type", "Employment Type",
                  choices = unique(salary_data$employment_type)),
      
      # Remote Ratio Input (Changed to selectInput)
      selectInput("remote_ratio", "Remote Work Ratio",
                  choices = c("fully-remote", "hybrid", "onsite")),
      
      # Company Size Input
      selectInput("company_size", "Company Size",
                  choices = unique(salary_data$company_size)),
      
      # Predict button
      actionButton("predict_btn", "Predict Salary")
    ),
    
    # Main panel for outputs
    mainPanel(
      # Prediction output
      h3("Salary Prediction"),
      verbatimTextOutput("prediction_output"),
      
      # Model Performance Metrics
      h3("Model Performance"),
      verbatimTextOutput("model_metrics")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive prediction function
  prediction_results <- eventReactive(input$predict_btn, {
    # Create prediction input data
    pred_input <- data.frame(
      experience_level = input$experience_level,
      job_title = input$job_title,
      employment_type = input$employment_type,
      remote_ratio = remote_ratio_map(input$remote_ratio), # Map the category
      company_size = input$company_size
    )
    
    # Make prediction
    prediction <- predict(regression_model, newdata = pred_input)
    
    return(prediction)
  })
  
  # Output prediction
  output$prediction_output <- renderPrint({
    req(input$predict_btn)
    
    results <- prediction_results()
    cat("Predicted Salary:\n")
    cat("$", round(results, 2), "\n")
  })
  
  # Output model performance metrics
  output$model_metrics <- renderPrint({
    metrics <- calculate_model_metrics(regression_model)
    
    cat("Model Performance Metrics:\n")
    cat("R-squared:", round(metrics$r_squared, 4), "\n")
    cat("Adjusted R-squared:", round(metrics$adj_r_squared, 4), "\n")
    cat("F-statistic:", round(metrics$f_statistic, 4), "\n")
    cat("P-value:", format.pval(metrics$p_value), "\n")
  })
}

# Create Shiny App
shinyApp(ui = ui, server = server)