# Data Importing 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(knitr)
library(corrplot)
library(scales)
library(readxl)
library(maps)
library(ggthemes)
library(countrycode)
library(scales) 
library(glmnet)
library(gridExtra)

library(readr)
salary_df <- read_csv("C:/Users/User/Desktop/R Final Project/DataScience_salaries_2024.csv")

# View the first few rows
head(salary_df)

# Check the structure of the dataset
str(salary_df)

# Summary statistics
summary(salary_df)


# Data Cleaning 
# Check for missing values
colSums(is.na(salary_df))

# 2. Data Cleaning and Preprocessing
# Clean column names
names(salary_data) <- tolower(names(salary_data))

# Convert salary to numeric if needed
salary_data <- salary_data %>%
  mutate(
    salary_usd = as.numeric(salary_in_usd),
    work_year = as.factor(work_year),
    experience_level = factor(experience_level, 
                              levels = c('EN', 'MI', 'SE', 'EX'),
                              labels = c('Entry', 'Mid', 'Senior', 'Executive'))
  )


# Basic statistics for salary in USD
salary_stats_summary <- salary_df %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    min_salary = min(salary_in_usd),
    max_salary = max(salary_in_usd),
    sd_salary = sd(salary_in_usd)
  )

salary_stats_summary


# Visualize the data to get better understanding

# Histogram for the distribution of salaries
hist(salary_df$salary_in_usd, main='Salary Distribution', xlab='Salary in USD', breaks=30, col='blue', border='black')

# Calculate average salary by experience level

avg_salary_experience <- aggregate(salary_in_usd ~ experience_level, data = salary_df, FUN = mean)
print(avg_salary_experience)


# Create a bar plot to visualize 

barplot(avg_salary_experience$salary_in_usd,
        names.arg = avg_salary_experience$experience_level,
        main = 'Average Salary by Experience Level',
        xlab = 'Experience Level',
        ylab = 'Average Salary in USD',
        col = 'lightgreen',
        las = 1)


# Calculate average salary by job title

avg_salary_job_title <- aggregate(salary_in_usd ~ job_title, data = salary_df, FUN = mean)
print(avg_salary_job_title)


# Create a bar plot 
barplot(avg_salary_job_title$salary_in_usd,
        names.arg = avg_salary_job_title$job_title,
        main = 'Average Salary by Job Title',
        xlab = 'Job Title',
        ylab = 'Average Salary in USD',
        col = 'lightblue',
        las = 2,
        cex.names = 0.7)


column_values <- salary_df$employment_type
print(column_values)


value_counts <- table(column_values)


mode_value <- names(value_counts)[which.max(value_counts)]

mode_value

unique_names <- unique(salary_df$employment_type)
print(unique_names)

#=============================================



#======================================================

dev.off() 

# Exploratory Data Analysis (EDA)

# 2. Salary Distribution by Job Title
job_title_analysis <- salary_df %>%
  group_by(job_title) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  ) %>%
  arrange(desc(mean_salary))

job_title_analysis

# Visualization for job title analysis
ggplot(salary_df, aes(x = reorder(job_title, salary_in_usd), y = salary_in_usd)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Job Title",
    x = "Job Title",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())


# 3. Experience Level Analysis
experience_analysis <- salary_df %>%
  group_by(experience_level) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  ) %>%
  arrange(desc(mean_salary))

experience_analysis


# Visualization for experience level
ggplot(salary_df, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Experience Level",
    x = "Experience Level",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())



# 4. Remote Work Analysis
remote_analysis <- salary_df %>%
  group_by(remote_ratio) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  ) %>%
  arrange(desc(mean_salary))

remote_analysis

# Visualization for remote work
ggplot(salary_df, aes(x = as.factor(remote_ratio), y = salary_in_usd)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Remote Work Ratio",
    x = "Remote Work Ratio (%)",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())

# 5. Geographical Analysis
geo_analysis <- salary_df %>%
  group_by(company_location) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  ) %>%
  arrange(desc(mean_salary))
geo_analysis


# Visualization for geographical analysis
ggplot(salary_df, aes(x = reorder(company_location, salary_in_usd), y = salary_in_usd)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  labs(
    title = "Average Salary by Company Location",
    x = "Company Location",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())



# 6. Company Size Analysis
size_analysis <- salary_df %>%
  group_by(company_size) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  ) %>%
  arrange(desc(mean_salary))

size_analysis

# Visualization for company size
ggplot(salary_df, aes(x = company_size, y = salary_in_usd, fill = company_size)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Company Size",
    x = "Company Size",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())


# 1. Time-based Salary Trends
yearly_trends <- salary_df %>%
  group_by(work_year) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    min_salary = min(salary_in_usd),
    max_salary = max(salary_in_usd),
    n_jobs = n()
  )
yearly_trends


# Visualization of salary trends over time
ggplot(salary_df, aes(x = as.factor(work_year), y = salary_in_usd)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Salary Trends Over Time (2020-2023)",
    x = "Year",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())


# 2. Job Title Categorization and Analysis

salary_data <- salary_df %>%
  mutate(job_category = case_when(
    grepl("Data Scientist|Data Analyst", job_title) ~ "Data Analysis",
    grepl("Machine Learning|ML|AI", job_title) ~ "ML/AI",
    grepl("Manager|Head|Lead", job_title) ~ "Management",
    TRUE ~ "Other"
  ))
salary_data

# Analysis by job category
job_category_analysis <- salary_df %>%
  group_by(job_title) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  )
job_category_analysis

# 3. Salary Range Analysis
salary_data <- salary_df %>%
  mutate(salary_range = case_when(
    salary_in_usd < 40000 ~ "Low (<$40K)",
    salary_in_usd < 60000 ~ "Medium ($40K-$60K)",
    salary_in_usd < 80000 ~ "High ($60K-$80K)",
    TRUE ~ "Very High (>$80K)"
  ))

salary_data

# 4. Cross-border Analysis
cross_border <- salary_df %>%
  mutate(is_cross_border = employee_residence != company_location) %>%
  group_by(is_cross_border) %>%
  summarise(
    mean_salary = mean(salary_in_usd),
    median_salary = median(salary_in_usd),
    count = n()
  )

cross_border

# 5. Experience-Salary Ratio Analysis
# Calculate salary per year of experience (approximate)
experience_weights <- c("EN" = 2, "MI" = 5, "SE" = 8, "EX" = 12)
salary_data <- salary_df %>%
  mutate(approx_experience = experience_weights[experience_level],
         salary_per_year_exp = salary_in_usd / approx_experience)
salary_data

# salary trend overtime 

# Plot the salary trend over time
ggplot(salary_df, aes(x = work_year, y = salary_currency)) +
  geom_line(aes(group = employee_residence)) +
  labs(title = "Salary Trend over Time",
       x = "Year", y = "Salary")

# employment type and salary 

ggplot(salary_df, aes(x = employment_type, y = salary_in_usd)) + 
  geom_boxplot() + 
  labs(title = "Salary Distribution by Employment Type",
       x = "Employment Type", y = "Salary")


# 6. Regional Market Analysis
regional_comp <- salary_df %>%
  group_by(company_location) %>%
  summarise(
    avg_salary = mean(salary_in_usd),
    market_size = n(),
    unique_roles = n_distinct(job_title),
    remote_percentage = mean(remote_ratio)
  )

regional_comp

# 7. Career Progression Analysis
career_progression <- salary_df %>%
  group_by(experience_level, job_title) %>%
  summarise(
    avg_salary = mean(salary_in_usd),
    n_positions = n()
  ) %>%
  arrange(job_title, experience_level)

career_progression

# Job Category Distribution
ggplot(salary_df, aes(x = job_title, y = salary_in_usd, fill = job_title)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Job Category",
    x = "Job Category",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Experience Level vs Job Category
ggplot(salary_df, aes(x = experience_level, y = salary_in_usd, fill = job_title)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Salary by Experience Level and Job Category",
    x = "Experience Level",
    y = "Salary in USD"
  ) +
  scale_y_continuous(labels = scales::dollar_format())

# Remote Work by Region
ggplot(salary_df, aes(x = company_location, y = remote_ratio, fill = company_location)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Remote Work Ratio by Company Location",
    x = "Company Location",
    y = "Remote Work Ratio (%)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Salary Distribution by Experience Level
ggplot(salary_data, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  labs(
    title = "Salary Distribution by Experience Level",
    x = "Experience Level",
    y = "Salary (USD)"
  ) +
  theme_minimal()

# Salary by Job Title
ggplot(salary_data, aes(x = reorder(job_title, salary_in_usd), y = salary_in_usd)) +
  geom_bar(stat = "summary", fun = "mean", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Average Salary by Job Title",
    x = "Job Title",
    y = "Average Salary (USD)"
  ) +
  theme_minimal()

# checking the salary trend

# Calculate average salary by year
avg_salary_by_year <- salary_df %>%
  group_by(work_year) %>%
  summarise(average_salary = mean(salary_in_usd))

avg_salary_by_year


# Create the line plot
ggplot(avg_salary_by_year, aes(x = work_year, y = average_salary)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "Average Salary Trend (2020-2024)",
    x = "Year",
    y = "Average Salary (USD)"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = unique(avg_salary_by_year$work_year)) +
  geom_text(
    aes(label = round(average_salary, 2)), 
    vjust = -1, 
    color = "dark green"
  )


#  additional employment types for demonstration

# Box Plot
boxplot_employment <- ggplot(salary_df, aes(x = employment_type, y = salary_in_usd)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(
    title = "Salary Distribution by Employment Type",
    x = "Employment Type",
    y = "Salary (USD)"
  ) +
  theme_minimal()

boxplot_employment


# Violin Plot
violin_plot <- ggplot(salary_df, aes(x = employment_type, y = salary_in_usd)) +
  geom_violin(fill = "lightgreen", color = "darkgreen") +
  geom_boxplot(width = 0.1, fill = "white", color = "darkgreen") +
  labs(
    title = "Salary Distribution Density by Employment Type",
    x = "Employment Type",
    y = "Salary (USD)"
  ) +
  theme_minimal()


violin_plot




#============================================

# world map data 

# Convert and clean data
salary_data <- salary_data %>%
  mutate(
    salary_usd = as.numeric(salary_in_usd),
    experience_level = factor(experience_level, levels = c('EN', 'MI', 'SE', 'EX'), labels = c('Entry', 'Mid', 'Senior', 'Executive')),
    employee_residence = countrycode(employee_residence, "iso2c", "country.name")
  )


# Calculate average salary by employee residence
avg_salary_by_residence <- salary_data %>%
  group_by(employee_residence) %>%
  summarise(avg_salary = mean(salary_usd, na.rm = TRUE)) %>%
  ungroup()

# Load world map data
world_map <- map_data("world")

# Manual mappings for unmatched countries
manual_map <- data.frame(
  employee_residence = c("United States", "United Kingdom", "Czechia", "Hong Kong SAR China", "Bosnia & Herzegovina"),
  region = c("USA", "UK", "Czech Republic", "Hong Kong", "Bosnia and Herzegovina")
)

# Update avg_salary_by_residence with manual corrections
avg_salary_by_residence <- avg_salary_by_residence %>%
  left_join(manual_map, by = "employee_residence") %>%
  mutate(employee_residence = ifelse(!is.na(region), region, employee_residence)) %>%
  select(-region)

# Join with map data
map_data_merged <- world_map %>%
  left_join(avg_salary_by_residence, by = c("region" = "employee_residence"))

ggplot(map_data_merged, aes(x = long, y = lat, group = group, fill = avg_salary)) +
  geom_polygon(color = "transparent", linewidth = 0.5) +  # Removed harsh borders, subtle color separation
  scale_fill_gradientn(colors = c("#003f5c", "#7a5195", "#ef5675", "#ff764a", "#feb24c"), 
                       na.value = "lightgray", name = "Avg Salary (USD)", 
                       labels = scales::comma_format()) + 
  coord_fixed(1.3) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#a6cbe3", color = NA),  # Soft, fresh blue background
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5, color = "#2c3e50"),  # Clear, dark title
    plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#34495e"),  # Muted subtitle for better hierarchy
    plot.caption = element_text(size = 12, hjust = 1, color = "#7f8c8d"),  # Soft caption color
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 14, color = "#2c3e50"),
    legend.text = element_text(size = 12, color = "#2c3e50"),
    legend.key.width = unit(2, "cm"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(10, 20, 10, 20)  
  ) +
  labs(
    title = "Global Distribution of Data Science Salaries",
    subtitle = "Average Data Science Salary by Employee Residence (2024)",
    caption = "Source: DataScience_salaries_2024.csv"
  ) 

#===================================================

# correlation analysis

correlation_matrix <- cor(salary_data[, c("salary_in_usd", "remote_ratio")])
correlation_matrix


corrplot(correlation_matrix, method = "circle")

#===============================================
correlation_matrix_two <- cor(salary_data[, c("salary_in_usd", "salary")])

correlation_matrix_two

corrplot(correlation_matrix_two, method = "circle")

#======================================

correlation_matrix_three <- cor(salary_data[, c("salary_in_usd", "salary","remote_ratio")])

correlation_matrix_three

corrplot(correlation_matrix_three, method = "circle")


#============================================

# near regression model to predict salary_in_usd based on remote_ratio
# model_building

model <- lm(salary_in_usd ~ remote_ratio, data = salary_df)
plot(salary_df$salary_in_usd, salary_df$remote_ratio,
     xlab = "Remote Ratio", 
     ylab = "Salary in USD", 
     main = "Salary vs Remote Ratio")

# Print the model summary
summary(model)

# model_visualization

# Predicted values
salary_df$predicted_model_1 <- predict(model, newdata = salary_df)

# Plot the original data and the regression line
ggplot(salary_df, aes(x = remote_ratio, y = salary_in_usd)) +
  geom_point() +
  geom_line(aes(x = remote_ratio, y = predicted_model_1), color = "blue") +
  labs(title = "Model 1: Salary vs Remote Ratio", x = "Remote Ratio", y = "Salary in USD") +
  theme_minimal()

#=================================
# model with experience_level, job_title, company_size


model_2 <- lm(salary_in_usd ~ remote_ratio + experience_level + job_title + company_size, data = salary_df)

summary(model_2)

salary_df$predicted_model_2 <- predict(model_2, newdata = salary_df)

# Plotting: Scatter plot with the prediction line
ggplot(salary_df, aes(x = remote_ratio, y = salary_in_usd, color = experience_level)) +
  geom_point() +
  geom_line(aes(x = remote_ratio, y = predicted_model_2), color = "red") +
  labs(title = "Model 2: Salary vs Remote Ratio + Experience Level", x = "Remote Ratio", y = "Salary in USD") +
  theme_minimal()

#=============================================

# polynomial regression model

model_three <- lm(salary_in_usd ~ remote_ratio + I(remote_ratio^2), data = salary_df)

summary(model_three)

salary_df$predicted_model_3 <- predict(model_three, newdata = salary_df)

# Plotting: Scatter plot and polynomial curve
ggplot(salary_df, aes(x = remote_ratio, y = salary_in_usd)) +
  geom_point() +
  geom_line(aes(x = remote_ratio, y = predicted_model_3), color = "green") +
  labs(title = "Model 3: Polynomial Regression of Salary vs Remote Ratio", x = "Remote Ratio", y = "Salary in USD") +
  theme_minimal()

#============================================

# MODEL ON DEPENDENT VARIABLES AND INDEPENDENT VARIABLES

model_four <- lm(salary_in_usd ~ remote_ratio * experience_level, data = salary_df)

summary(model_four)
salary_df$predicted_model_4 <- predict(model_four, newdata = salary_df)

# Plotting: Scatter plot with interaction and prediction line
ggplot(salary_df, aes(x = remote_ratio, y = salary_in_usd, color = experience_level)) +
  geom_point() +
  geom_line(aes(x = remote_ratio, y = predicted_model_4), color = "purple") +
  labs(title = "Model 4: Salary vs Remote Ratio with Experience Level Interaction", x = "Remote Ratio", y = "Salary in USD") +
  theme_minimal()

#================================================
boxplot(salary_in_usd ~ salary_currency, data = salary_df,
        xlab = "Salary Currency", 
        ylab = "Salary in USD", 
        main = "Salary Distribution by Currency",
        las = 2)
#=============================================

sum(is.na(salary_df$salary_in_usd))   
sum(is.na(salary_df$salary_currency)) 


boxplot(salary_in_usd ~ salary_currency, data = salary_df,
        xlab = "Salary Currency", 
        ylab = "Salary in USD", 
        main = "Salary Distribution by Currency",
        las = 2)

#========================

salary.regression <- lm(salary_in_usd ~ salary_currency, data = salary_df)

summary(salary.regression)


#================================


salary2.regression <- lm(salary_in_usd ~ salary, data = salary_df)
summary(salary2.regression)


#======================================


unique_names <- unique(salary_df$company_location)

unique_names


unique_names <- unique(salary_df$job_title)

unique_names

mode_value <- names(unique_names)[which.max(unique_names)]

mode_value



column_values <- salary_df$job_title
print(column_values)


value_counts <- table(column_values)


mode_value <- names(value_counts)[which.max(value_counts)]

mode_value


# model including job title


model <- lm(salary_in_usd ~ experience_level + employment_type + remote_ratio + company_size + job_title, data = train_data)


# Summary of the model
model_summary <- summary(model)
print(model_summary)



# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Combine actual and predicted values for comparison
results <- data.frame(Actual = test_data$salary_in_usd, Predicted = predictions)


# Performance metrics
performance_metrics <- postResample(pred = predictions, obs = test_data$salary_in_usd)

# Print the results and performance metrics
print(head(results))
print(performance_metrics)

#====================================


# Fit a multiple linear regression model with job_title and salary_in_usd
model_job_title_salary <- lm(salary_in_usd ~ job_title, data = salary_df)

# Summary of the model
model_job_title_salary_summary <- summary(model_job_title_salary)
print(model_job_title_salary_summary)

# Testing
# Make predictions
predictions_job_title <- predict(model_job_title_salary, newdata = test_data)

# Combine actual and predicted values for comparison
results_job_title <- data.frame(Actual = test_data$salary_in_usd, Predicted = predictions_job_title)

# Performance metrics
performance_metrics_job_title <- postResample(pred = predictions_job_title, obs = test_data$salary_in_usd)

# Results and performance metrics
print(head(results_job_title))
print(performance_metrics_job_title)

# Create a scatter plot for actual vs predicted salaries (with job_title)
plot_job_title <- ggplot(results_job_title, aes(x = Actual, y = Predicted)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = 'red') + 
  labs(title = 'Actual vs Predicted Salaries', x = 'Actual Salary (USD)', y = 'Predicted Salary (USD)') + 
  theme_minimal()

print(plot_job_title)


####

# Convert categorical variables to factors
salary_data <- salary_df %>% 
  mutate(
    experience_level = as.factor(experience_level),
    employment_type = as.factor(employment_type),
    job_title = as.factor(job_title),
    company_size = as.factor(company_size)
  )

# Check the structure of the modified dataset
str(salary_data)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (80%) and testing (20%) sets
train_index <- createDataPartition(salary_data$salary_in_usd, p = 0.8, list = FALSE)
train_data <- salary_data[train_index, ]
test_data <- salary_data[-train_index, ]

# Fit a regression model based on job_title
model_job_title <- lm(salary_in_usd ~ job_title, data = train_data)

# Summary of the model
model_summary_job_title <- summary(model_job_title)
print(model_summary_job_title)

####################################################
# Testing the model
# Make predictions using the model
predictions_job_title <- predict(model_job_title, newdata = test_data)

# Combine actual and predicted values for comparison
results_job_title <- data.frame(Actual = test_data$salary_in_usd, Predicted = predictions_job_title)

# Performance metrics
performance_metrics_job_title <- postResample(pred = predictions_job_title, obs = test_data$salary_in_usd)

# Display results and performance metrics
print(head(results_job_title))
print(performance_metrics_job_title)

#####################################

# Create a scatter plot for actual vs predicted salaries
plot_job_title <- ggplot(results_job_title, aes(x = Actual, y = Predicted)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(slope = 1, intercept = 0, color = 'red') + 
  labs(title = 'Actual vs Predicted Salaries (Based on Job Title)', 
       x = 'Actual Salary (USD)', y = 'Predicted Salary (USD)') + 
  theme_minimal()

print(plot_job_title)


#==========================================


# Required Libraries
library(tidyverse)
library(caret)
library(lme4)
library(performance)

# Job Titles
job_titles <- c("Data Scientist", "BI Data Analyst", "ML Engineer", "Lead Machine Learning Engineer",
                "Data Science Manager", "Head of Machine Learning", "Research Engineer", 
                "Head of Data Science", "AI Programmer", "Machine Learning Engineer", 
                "Lead Data Scientist", "Data Engineer", "Applied Machine Learning Scientist", 
                "Lead Data Analyst", "Data Analytics Manager", "Data Integration Specialist", 
                "Principal Data Architect", "NLP Engineer", "Big Data Engineer", 
                "AI Research Engineer", "Machine Learning Software Engineer", "Data Analyst", 
                "Applied Data Scientist", "AI Scientist", "Data Analytics Lead", 
                "Business Data Analyst", "Product Data Analyst", "Computer Vision Engineer", 
                "Data Science Consultant", "AI Architect", "Analytics Engineer", 
                "Machine Learning Scientist", "Research Scientist", "Prompt Engineer", 
                "Principal Data Scientist", "Applied Scientist", "Deep Learning Engineer", 
                "Data Architect", "AI Engineer", "Data Infrastructure Engineer", 
                "Data Science", "Director of Data Science", "Data Science Tech Lead", 
                "BI Analyst", "Data Lead", "Head of Data", "Analytics Engineering Manager", 
                "Data Product Owner", "Business Intelligence", "Machine Learning Infrastructure Engineer", 
                "Research Analyst", "Managing Director Data Science", "Finance Data Analyst", 
                "Data Operations Engineer", "Robotics Software Engineer", "MLOps Engineer", 
                "Lead Data Engineer", "AI Developer", "Data Science Lead", 
                "Data Science Engineer", "Business Intelligence Manager", 
                "Business Intelligence Specialist", "Business Intelligence Engineer", 
                "Data Modeler", "AWS Data Architect", "Machine Learning Researcher", 
                "Data Product Manager", "Data Strategy Manager", "ETL Developer", 
                "Cloud Data Architect", "Computational Biologist", "AI Software Engineer", 
                "Data Strategist", "Data Manager", "Business Intelligence Analyst", 
                "Applied Machine Learning Engineer", "Data Specialist", "AI Product Manager", 
                "Data Science Director", "Software Data Engineer", "Data Operations Specialist", 
                "BI Developer", "Machine Learning Modeler", "Decision Scientist", 
                "Data Management Specialist", "Data Analyst Lead", "Marketing Data Analyst", 
                "Machine Learning Manager")


salary_data <- read.csv('C:/Users/User/Desktop/R Final Project/DataScience_salaries_2024.csv')

# Data Preprocessing
preprocess_salary_data <- function(data) {
  # Convert categorical variables to factors
  data$experience_level <- factor(data$experience_level, 
                                  levels = c("EN", "MI", "SE", "EX"),
                                  labels = c("Entry Level", "Mid Level", "Senior", "Executive"))
  
  data$employment_type <- factor(data$employment_type, 
                                 levels = c("CT", "FL", "FT", "PT"),
                                 labels = c("Contract", "Freelance", "Full-Time", "Part-Time"))
  
  data$job_title <- factor(data$job_title, levels = job_titles)
  
  data$company_size <- factor(data$company_size, 
                              levels = c("L", "M", "S"),
                              labels = c("Large", "Medium", "Small"))
  
  return(data)
}

# Prepare the data
salary_data <- preprocess_salary_data(salary_data)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(salary_data$salary, p = 0.8, list = FALSE)
train_data <- salary_data[train_index, ]
test_data <- salary_data[-train_index, ]

# Build Mixed Effects Model
# This model allows for both fixed effects (job title, experience level, etc.) 
# and random effects (to account for potential variations not explained by fixed effects)
model_with_job_titles <- lmer(
  salary ~ experience_level + employment_type + job_title + 
    remote_ratio + company_size + 
    (1 | job_title),  # Random intercept for job title
  data = train_data
)

# Model Evaluation
# Predict on test data
test_data$predicted_salary <- predict(model_with_job_titles, newdata = test_data)

# Calculate performance metrics
results <- data.frame(
  Actual = test_data$salary,
  Predicted = test_data$predicted_salary
)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

# Mean Absolute Error (MAE)
mae <- mean(abs(results$Actual - results$Predicted))

# R-squared
r_squared <- cor(results$Actual, results$Predicted)^2

# Print model summary and performance metrics
summary(model_with_job_titles)
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Save model performance results for Shiny app
save(model_with_job_titles, results, file = "salary_prediction_model.RData")

# Optional: Cross-validation for more robust performance estimation
# Create control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Perform cross-validation
cv_model <- train(
  salary ~ experience_level + employment_type + job_title + 
    remote_ratio + company_size,
  data = salary_data,
  method = "lmer",
  trControl = ctrl
)

# Print cross-validation results
print(cv_model)

# Note: This script assumes you have a CSV file with the following columns:
# - salary: Numeric salary value
# - experience_level: Factor with levels EN, MI, SE, EX
# - employment_type: Factor with levels CT, FL, FT, PT
# - job_title: Character or factor with job titles
# - remote_ratio: Numeric (0-100)
# - company_size: Factor with levels L, M, S

# Diagnostic plots
# Residuals plot
residuals_plot <- ggplot(data.frame(
  Predicted = fitted(model_with_job_titles),
  Residuals = residuals(model_with_job_titles)
)) +
  geom_point(aes(x = Predicted, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted",
       x = "Predicted Salary",
       y = "Residuals") +
  theme_minimal()

# Print or save the residuals plot
print(residuals_plot)


#==========================================


# Salary Prediction Model Script

# Required Libraries
library(tidyverse)
library(caret)
library(lme4)
library(performance)

# Job Titles
job_titles <- c("Data Scientist", "BI Data Analyst", "ML Engineer", "Lead Machine Learning Engineer",
                "Data Science Manager", "Head of Machine Learning", "Research Engineer", 
                "Head of Data Science", "AI Programmer", "Machine Learning Engineer", 
                "Lead Data Scientist", "Data Engineer", "Applied Machine Learning Scientist", 
                "Lead Data Analyst", "Data Analytics Manager", "Data Integration Specialist", 
                "Principal Data Architect", "NLP Engineer", "Big Data Engineer", 
                "AI Research Engineer", "Machine Learning Software Engineer", "Data Analyst", 
                "Applied Data Scientist", "AI Scientist", "Data Analytics Lead", 
                "Business Data Analyst", "Product Data Analyst", "Computer Vision Engineer", 
                "Data Science Consultant", "AI Architect", "Analytics Engineer", 
                "Machine Learning Scientist", "Research Scientist", "Prompt Engineer", 
                "Principal Data Scientist", "Applied Scientist", "Deep Learning Engineer", 
                "Data Architect", "AI Engineer", "Data Infrastructure Engineer", 
                "Data Science", "Director of Data Science", "Data Science Tech Lead", 
                "BI Analyst", "Data Lead", "Head of Data", "Analytics Engineering Manager", 
                "Data Product Owner", "Business Intelligence", "Machine Learning Infrastructure Engineer", 
                "Research Analyst", "Managing Director Data Science", "Finance Data Analyst", 
                "Data Operations Engineer", "Robotics Software Engineer", "MLOps Engineer", 
                "Lead Data Engineer", "AI Developer", "Data Science Lead", 
                "Data Science Engineer", "Business Intelligence Manager", 
                "Business Intelligence Specialist", "Business Intelligence Engineer", 
                "Data Modeler", "AWS Data Architect", "Machine Learning Researcher", 
                "Data Product Manager", "Data Strategy Manager", "ETL Developer", 
                "Cloud Data Architect", "Computational Biologist", "AI Software Engineer", 
                "Data Strategist", "Data Manager", "Business Intelligence Analyst", 
                "Applied Machine Learning Engineer", "Data Specialist", "AI Product Manager", 
                "Data Science Director", "Software Data Engineer", "Data Operations Specialist", 
                "BI Developer", "Machine Learning Modeler", "Decision Scientist", 
                "Data Management Specialist", "Data Analyst Lead", "Marketing Data Analyst", 
                "Machine Learning Manager")

# Load the dataset

salary_data <- read.csv("C:/Users/User/Desktop/R Final Project/DataScience_salaries_2024.csv")

# Data Preprocessing
preprocess_salary_data <- function(data) {
  # Convert categorical variables to factors
  data$experience_level <- factor(data$experience_level, 
                                  levels = c("EN", "MI", "SE", "EX"),
                                  labels = c("Entry Level", "Mid Level", "Senior", "Executive"))
  
  data$employment_type <- factor(data$employment_type, 
                                 levels = c("CT", "FL", "FT", "PT"),
                                 labels = c("Contract", "Freelance", "Full-Time", "Part-Time"))
  
  data$job_title <- factor(data$job_title, levels = job_titles)
  
  data$company_size <- factor(data$company_size, 
                              levels = c("L", "M", "S"),
                              labels = c("Large", "Medium", "Small"))
  
  return(data)
}

# Prepare the data
salary_data <- preprocess_salary_data(salary_data)

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(salary_data$salary, p = 0.8, list = FALSE)
train_data <- salary_data[train_index, ]
test_data <- salary_data[-train_index, ]

# Build Mixed Effects Model
# This model allows for both fixed effects (job title, experience level, etc.) 
# and random effects (to account for potential variations not explained by fixed effects)
model_with_job_titles <- lmer(
  salary ~ experience_level + employment_type + job_title + 
    remote_ratio + company_size + 
    (1 | job_title),  # Random intercept for job title
  data = train_data
)

# Model Evaluation
# Ensure consistent factor levels in test data
test_data$experience_level <- factor(test_data$experience_level, 
                                     levels = levels(train_data$experience_level))
test_data$employment_type <- factor(test_data$employment_type, 
                                    levels = levels(train_data$employment_type))
test_data$job_title <- factor(test_data$job_title, 
                              levels = levels(train_data$job_title))
test_data$company_size <- factor(test_data$company_size, 
                                 levels = levels(train_data$company_size))

# Predict on test data with fallback for missing levels
predict_with_fallback <- function(model, newdata) {
  # Check for any factor columns
  factor_cols <- sapply(newdata, is.factor)
  
  # For each factor column, replace unseen levels with the most frequent level
  for (col in names(newdata)[factor_cols]) {
    # Get the levels from the training data
    train_levels <- levels(model@frame[[col]])
    
    # Replace unseen levels with the most frequent level from training
    unseen_levels <- setdiff(levels(newdata[[col]]), train_levels)
    if (length(unseen_levels) > 0) {
      most_frequent_level <- names(sort(table(model@frame[[col]]), decreasing = TRUE)[1])
      warning(paste("Replacing unseen levels in", col, "with:", most_frequent_level))
      
      newdata[[col]] <- factor(
        ifelse(newdata[[col]] %in% train_levels, 
               as.character(newdata[[col]]), 
               most_frequent_level),
        levels = train_levels
      )
    }
  }
  
  # Predict with the modified data
  predict(model, newdata = newdata)
}

# Predict with fallback mechanism
test_data$predicted_salary <- predict_with_fallback(model_with_job_titles, test_data)

# Calculate performance metrics
results <- data.frame(
  Actual = test_data$salary,
  Predicted = test_data$predicted_salary
)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))

# Mean Absolute Error (MAE)
mae <- mean(abs(results$Actual - results$Predicted))

# R-squared
r_squared <- cor(results$Actual, results$Predicted)^2

# Print model summary and performance metrics
print("Model Summary:")
summary(model_with_job_titles)
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Diagnostic scatter plot of actual vs predicted
diagnostic_plot <- ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = 'red', linetype = 'dashed') +
  labs(title = 'Actual vs Predicted Salaries',
       x = 'Actual Salary',
       y = 'Predicted Salary') +
  theme_minimal()

# Print the diagnostic plot
print(diagnostic_plot)

# Save model performance results for Shiny app
save(model_with_job_titles, results, file = "salary_prediction_model.RData")

# Optional: Cross-validation for more robust performance estimation
# Create control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# Perform cross-validation
cv_model <- train(
  salary ~ experience_level + employment_type + job_title + 
    remote_ratio + company_size,
  data = salary_data,
  method = "lmer",
  trControl = ctrl
)

# Print cross-validation results
print(cv_model)

# Note: This script assumes you have a CSV file with the following columns:
# - salary: Numeric salary value
# - experience_level: Factor with levels EN, MI, SE, EX
# - employment_type: Factor with levels CT, FL, FT, PT
# - job_title: Character or factor with job titles
# - remote_ratio: Numeric (0-100)
# - company_size: Factor with levels L, M, S

# Diagnostic plots
# Residuals plot
residuals_plot <- ggplot(data.frame(
  Predicted = fitted(model_with_job_titles),
  Residuals = residuals(model_with_job_titles)
)) +
  geom_point(aes(x = Predicted, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Predicted",
       x = "Predicted Salary",
       y = "Residuals") +
  theme_minimal()

# Print or save the residuals plot
print(residuals_plot)