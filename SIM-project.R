# Read the CSV file into a data frame
library(readr)  # Load the readr package for efficient CSV reading

df <- read.csv("Applicant-details.csv")

# Display the first few rows of the data frame
head(df)

# Get the shape (number of rows and columns) of the data frame
dim(df)


# Check for missing values in each column
missing_values <- sum(is.na(df))

# Print the results in a user-friendly format
cat("Missing values in each column:\n")
print(missing_values)


summary(df)

df_original <- data.frame(df)

str(df)

#Detecting Outlines visually using box plot for Numerical columns
#Box plot

library(ggplot2) 

numerical_columns <- c("Annual_Income", "Applicant_Age", "Work_Experience", "Years_in_Current_Employment", "Years_in_Current_Residence")

# Create a grid for plots
par(mfrow = c(2, 3)) 

# Loop through columns and create box plots
for (col in numerical_columns) {
  boxplot(df[[col]], main = paste("Box plot of", col))
}

# Reset plotting parameters to defaults
par(mfrow = c(1, 1)) 

#Detecting Outlines visually using box plot for Numerical columns
#Histogram

library(ggplot2)
library(tidyr) # for pivot_longer

columns_to_check <- c("Annual_Income", "Applicant_Age", "Work_Experience", 
                      "Years_in_Current_Employment", "Years_in_Current_Residence")

# Reshape data for faceting
df_long <- df %>%
  pivot_longer(cols = all_of(columns_to_check), names_to = "Variable", values_to = "Value")

# Create faceted plot
ggplot(df_long, aes(x = Value)) +
  geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue") +
  geom_density(alpha = 0.2, fill = "blue") +
  facet_wrap(~ Variable, scales = "free") +  # Create facets for each variable
  labs(x = "", y = "Density") +  # Set labels 
  theme_minimal()

# Function to detect outliers in a given column
detect_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25)
  Q3 <- quantile(df[[column]], 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  outliers <- df[(df[[column]] < lower_bound) | (df[[column]] > upper_bound), ] 
  return(outliers)
}

# Get numeric columns 
numerical_columns <- sapply(df, is.numeric) # Identify numeric columns

# Detect outliers for each numeric column
for (col in names(df)[numerical_columns]) { # Loop over numeric column names
  outliers <- detect_outliers(df, col)
  cat(paste("Outliers in '", col, "':\n")) 
  if (nrow(outliers) > 0) { # If there are outliers
    print(outliers) # Print the outlier rows
  } else {
    cat("No outliers found.\n")
  }
  cat("\n") # Add a newline for readability
}


#Using Standarization Process to balance Results
#Standarization of boxplot

library(ggplot2)

columns_to_standardize <- c("Annual_Income", "Applicant_Age", "Work_Experience", "Years_in_Current_Employment", "Years_in_Current_Residence")

# Standardize selected columns
df[columns_to_standardize] <- scale(df[columns_to_standardize])

# Set up plotting grid (optional)
par(mfrow = c(2, 3)) 

# Loop and create box plots
for (col in columns_to_standardize) {
  boxplot(df[[col]], main = paste("Box plot of", col))
}

# Reset plotting parameters to defaults
par(mfrow = c(1, 1))


#Standarization of Histogram
library(ggplot2)
library(gridExtra) # For arranging plots

# Set up a list to store the plots
plot_list <- list()

# Loop and create plots
for (col in columns_to_check) {
  # Create a ggplot object for the histogram
  p <- ggplot(df, aes(x = .data[[col]])) + 
    geom_histogram(bins = 30, color = "black", fill = "lightblue") +
    labs(title = col, x = col, y = "Frequency") +
    theme_minimal()
  
  # Add the plot to the list
  plot_list[[col]] <- p
}

# Arrange and display plots using grid.arrange
do.call(grid.arrange, c(plot_list, ncol = 2))  # Arrange plots in 2 columns

#Seperating Numerical and non-numerical column
# Get column names (excluding Applicant_ID)
columns <- setdiff(names(df_original), "Applicant_ID")

# Identify numerical and non-numerical columns
numerical_columns <- names(df_original[columns])[sapply(df_original[columns], is.numeric)]
non_numerical_columns <- names(df_original[columns])[!sapply(df_original[columns], is.numeric)]

#Pairplot
library(GGally)
library(ggplot2)

# Sample the data if it's very large
set.seed(123)  # For reproducibility
sampled_df <- df_original[sample(nrow(df_original), min(1000, nrow(df_original))), ]

# Check if numerical_columns are indeed numerical
numerical_columns <- sapply(sampled_df, is.numeric)
numerical_columns <- names(numerical_columns[numerical_columns])

# Set up a custom ggplot theme to resemble Seaborn's whitegrid
custom_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "gray"))

# Create the pair plot using GGally::ggpairs()
pair_plot <- ggpairs(sampled_df[, numerical_columns],
                     diag = list(continuous = wrap("densityDiag", alpha = 0.7)), 
                     lower = list(continuous = wrap("points", alpha = 0.7, size = 2, shape = 16)),
                     upper = list(continuous = wrap("cor", size = 4)),
                     title = "Pair Plot of Numerical Features") +
  custom_theme  # Apply our Seaborn-like theme

ggsave("pair_plot.png", pair_plot, width = 10, height = 8)
print(pair_plot)

#Maritial status vs Loan default risk

library(ggplot2)

categorical_columns <- c("Marital_Status", "House_Ownership", "Vehicle_Ownership(car)", "Occupation", "Residence_City", "Residence_State")

# Create the count plot using ggplot2
ggplot(df_original, aes(y = Marital_Status, fill = Loan_Default_Risk)) +
  geom_bar() +  # Create the bars
  labs(title = "Marital Status vs Loan Default Risk", 
       x = "Count", y = "Marital Status") + 
  theme_bw() +   # Set the theme to be similar to Seaborn's whitegrid
  scale_fill_viridis_d() +  # Use the viridis color palette
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

#House Ownership vs Loan Default Risk
library(ggplot2)

# Create the count plot using ggplot2
ggplot(df_original, aes(y = House_Ownership, fill = Loan_Default_Risk)) +
  geom_bar() +  # Create the bars
  labs(title = "House Ownership vs Loan Default Risk", 
       x = "Count", y = "House Ownership") + 
  theme_bw() +   # Set the theme to be similar to Seaborn's whitegrid
  scale_fill_viridis_d() +  # Use the viridis color palette
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )


#Vehicle Ownership vs Loan Default Risk
library(ggplot2)
library(viridis)

# Check column names
print(names(df_original))

# Plot
ggplot(df_original, aes(y = .data[["Vehicle_Ownership.car."]], fill = Loan_Default_Risk)) +
  geom_bar() +
  labs(title = "Vehicle Ownership (car) vs Loan Default Risk",
       x = "Count", y = "Vehicle Ownership (car)") +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

#Occupation vs Loan Default Risk(top 10)
library(dplyr)
library(ggplot2)

# Get the top 10 occupations and their counts
top_10_occupations <- df_original %>%
  count(Occupation) %>%
  slice_max(order_by = n, n = 10) %>% 
  pull(Occupation)

# Filter to top 10 occupations and factorize for correct order
df_top_10_occupations <- df_original %>%
  filter(Occupation %in% top_10_occupations) %>%
  mutate(Occupation = factor(Occupation, levels = top_10_occupations))  

# Create the count plot with ggplot2
ggplot(df_top_10_occupations, aes(y = Occupation, fill = Loan_Default_Risk)) +
  geom_bar() +
  labs(title = "Occupation vs Loan Default Risk (Top 10)", 
       x = "Count", y = "Occupation") +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

#Residence vs Loan Default Risk(top 10)
library(dplyr) 
library(ggplot2)

# Get the top 10 residence states
top_10_states <- df_original %>%
  count(Residence_State) %>%
  slice_max(order_by = n, n = 10) %>%
  pull(Residence_State)

# Filter to top 10 residence states and factorize for correct order
df_top_10_states <- df_original %>%
  filter(Residence_State %in% top_10_states) %>%
  mutate(Residence_State = factor(Residence_State, levels = top_10_states))

# Create the count plot
ggplot(df_top_10_states, aes(y = Residence_State, fill = Loan_Default_Risk)) +
  geom_bar() +
  labs(title = "Residence State vs Loan Default Risk (Top 10)",
       x = "Count", y = "Residence State") +
  theme_bw() +
  scale_fill_viridis_d() +  
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16)
  )


#Violinplot
library(ggplot2)
library(gridExtra)

# Function to create violin plots
create_violin_plots <- function(df, columns) {
  # Calculate number of rows and columns
  n_cols <- 3
  n_rows <- ceiling(length(columns) / n_cols)
  
  # List to store plots
  plot_list <- list()
  
  for (i in seq_along(columns)) {
    col <- columns[i]
    
    # Create the violin plot using ggplot2
    p <- ggplot(df, aes(x = "", y = .data[[col]])) +
      geom_violin() +  # Create the violin plot
      labs(title = paste("Violin Plot for", col), x = "", y = "Density") +  # Set labels
      theme_bw()
    
    # Add the plot to the list
    plot_list[[col]] <- p
  }
  
  # Arrange and display plots using grid.arrange
  do.call(grid.arrange, c(plot_list, ncol = n_cols))  # Arrange plots in 3 columns
}

# List of columns to create violin plots for
columns_to_plot <- c("Annual_Income", "Applicant_Age", "Work_Experience", 
                     "Years_in_Current_Employment", "Years_in_Current_Residence", "Loan_Default_Risk")

# Create violin plots
create_violin_plots(df_original, columns_to_plot)

#Correaltion MAtrix To check the realtion
library(dplyr)   # For data manipulation
library(ggplot2)  # For plotting
library(forcats)  # For factor reordering

# Select relevant columns
df_corr <- df_original %>%
  select(Annual_Income, Applicant_Age, Work_Experience, Marital_Status, House_Ownership, `Vehicle_Ownership.car.`, Occupation, Residence_City, Residence_State, Years_in_Current_Employment, Years_in_Current_Residence, Loan_Default_Risk)

# Convert non-numeric columns to factor then numeric (for correlation calculation)
df_corr <- df_corr %>%
  mutate(across(where(is.character), ~ as.numeric(as.factor(.x))))

# Calculate the correlation matrix
correlation_matrix <- cor(df_corr)

# Reshape the correlation matrix to a tidy format
melted_cormat <- reshape2::melt(correlation_matrix)

# Create the heatmap using ggplot2
ggplot(melted_cormat, aes(Var1, fct_rev(Var2), fill = value)) + # Use fct_rev to reverse y-axis
  geom_tile() +
  geom_text(aes(label = format(value, digits = 2)), size = 4) +  # Add text labels with 2 decimal places
  labs(title = "Correlation Matrix", x = "", y = "") +  # Set title and empty axis labels
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1)) +  # Set color scale
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Adjust x-axis text
        axis.text.y = element_text(size = 10))                       # Adjust y-axis text

#Label Encoding and standardizing to transform the data
library(dplyr)

# Assuming your data frame is named `df_original`

categorical_columns <- c("Marital_Status", "House_Ownership", "Vehicle_Ownership.car.", "Occupation", "Residence_City", "Residence_State")

# Label encoding for categorical columns
for (col in categorical_columns) {
  df_original[[col]] <- as.numeric(as.factor(df_original[[col]])) - 1  
}

# Standardize all numerical columns
# (Make sure "Loan_Default_Risk" is numeric if you want to include it)
X_scaled <- scale(df_original)

#Splitting the data int training and testing
library(caret)     # For pre-processing and splitting
library(recipes)   # For creating preprocessing recipes

# Assuming df_original is your DataFrame and 'Loan_Default_Risk' is the target variable
X <- df_original[, !(names(df_original) %in% "Loan_Default_Risk")]  # Features (excluding target)
y <- df_original$Loan_Default_Risk                                   # Target

# --- 1. Data Preparation ---
# Assuming df_original is your DataFrame and 'Loan_Default_Risk' is the target variable
X <- df_original[, !(names(df_original) %in% "Loan_Default_Risk")]  # Features (excluding target)
y <- df_original$Loan_Default_Risk                                   # Target

set.seed(42) 
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Combine X_train and y_train for recipe
train_data <- cbind(X_train, Loan_Default_Risk = y_train)

# --- 2. Preprocessing Recipe ---
preproc_recipe <- recipe(Loan_Default_Risk ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())  # Standardize only predictors  


# --- 3. Prepare and Apply Preprocessing ---
preproc <- prep(preproc_recipe, training = train_data)
X_train_processed <- bake(preproc, new_data = X_train)
X_test_processed <- bake(preproc, new_data = X_test) 

#Modelling the data using KNN,LR and RF
library(caret)       # For modeling and metrics
library(randomForest) # For random forest
library(dplyr)       # For data manipulation
library(tidyr)       # For data reshaping
library(ggplot2)     # For plotting

# Print column names to check for the presence of 'Loan_Default_Risk'
print("Columns in original dataset:")
print(names(df_original))

# Check the structure of your original dataset
str(df_original)

# Ensure 'Loan_Default_Risk' is in your dataset before splitting
if (!"Loan_Default_Risk" %in% names(df_original)) {
  stop("The target column 'Loan_Default_Risk' is not found in the original dataset.")
}

# Assuming your preprocessing steps result in X_train_processed and X_test_processed
# Ensure that 'Loan_Default_Risk' is included in both training and testing datasets
# Let's print out the column names of X_train_processed and X_test_processed

print("Columns in X_train_processed:")
print(names(X_train_processed))

print("Columns in X_test_processed:")
print(names(X_test_processed))

# Add Loan_Default_Risk to the processed data if it was dropped (assuming y_train and y_test contain the target variable)
X_train_processed$Loan_Default_Risk <- y_train
X_test_processed$Loan_Default_Risk <- y_test

# Check again if the target column is present
if (!"Loan_Default_Risk" %in% names(X_train_processed)) {
  stop("The target column 'Loan_Default_Risk' is not found in the training dataset after processing.")
}

#Logistic Regression
# Ensure the target variable in the training data is a factor
X_train_processed$Loan_Default_Risk <- factor(X_train_processed$Loan_Default_Risk)

# Train the logistic regression model
lr_model <- train(Loan_Default_Risk ~ ., data = X_train_processed, method = "glm", family = "binomial")

# Make predictions on the test data
y_pred_lr <- predict(lr_model, X_test_processed)

# Ensure both predictions and test labels are factors with the same levels
levels_to_use <- union(levels(y_test), levels(y_pred_lr))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_lr <- factor(y_pred_lr, levels = levels_to_use)

# Compute the confusion matrix and accuracy
accuracy_lr <- confusionMatrix(y_test, y_pred_lr)$overall["Accuracy"]
log_reg_report <- confusionMatrix(y_test, y_pred_lr)

# Output the accuracy and confusion matrix
print(accuracy_lr)
print(log_reg_report)

# K-Nearest Neighbors
# Train the KNN model
# Ensure the target variable in the training data is a factor
X_train_processed$Loan_Default_Risk <- factor(X_train_processed$Loan_Default_Risk)

# Set training control with reduced number of cross-validation folds
train_control <- trainControl(method = "cv", number = 5)

# Train the KNN model with reduced neighbors and cross-validation folds
knn_model <- train(Loan_Default_Risk ~ ., 
                   data = X_train_processed, 
                   method = "knn", 
                   trControl = train_control, 
                   tuneGrid = expand.grid(k = 5))  # Setting k to a smaller number, like 5

# Make predictions on the test data
y_pred_knn <- predict(knn_model, X_test_processed)

# Ensure both predictions and test labels are factors with the same levels
levels_to_use <- union(levels(y_test), levels(y_pred_knn))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_knn <- factor(y_pred_knn, levels = levels_to_use)

# Compute the confusion matrix and accuracy
accuracy_knn <- confusionMatrix(y_test, y_pred_knn)$overall["Accuracy"]
knn_report <- confusionMatrix(y_test, y_pred_knn)

# Output the accuracy and confusion matrix
print(accuracy_knn)
print(knn_report)

# Random Forest 
#Ensure the target variable in the training data is a factor
X_train_processed$Loan_Default_Risk <- factor(X_train_processed$Loan_Default_Risk)

# Set training control with reduced number of cross-validation folds
train_control <- trainControl(method = "cv", number = 5)

# Train the Random Forest model with reduced number of trees
rf_model <- train(Loan_Default_Risk ~ ., 
                  data = X_train_processed, 
                  method = "rf", 
                  trControl = train_control, 
                  tuneGrid = expand.grid(.mtry = 2),  # Adjust the .mtry parameter if needed
                  ntree = 100)  # Reduce the number of trees to 100

# Make predictions on the test data
y_pred_rf <- predict(rf_model, X_test_processed)

# Ensure both predictions and test labels are factors with the same levels
levels_to_use <- union(levels(y_test), levels(y_pred_rf))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_rf <- factor(y_pred_rf, levels = levels_to_use)

# Compute the confusion matrix and accuracy
accuracy_rf <- confusionMatrix(y_test, y_pred_rf)$overall["Accuracy"]
rf_report <- confusionMatrix(y_test, y_pred_rf)

# Output the accuracy and confusion matrix
print(accuracy_rf)
print(rf_report)

# Create a data frame to store the metrics
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)

# Ensure the target variable in the training data is a factor
X_train_processed$Loan_Default_Risk <- factor(X_train_processed$Loan_Default_Risk)

# Logistic Regression Model
lr_model <- train(Loan_Default_Risk ~ ., data = X_train_processed, method = "glm", family = "binomial")
y_pred_lr <- predict(lr_model, X_test_processed)
levels_to_use <- union(levels(y_test), levels(y_pred_lr))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_lr <- factor(y_pred_lr, levels = levels_to_use)
accuracy_lr <- confusionMatrix(y_test, y_pred_lr)$overall["Accuracy"]
log_reg_report <- confusionMatrix(y_test, y_pred_lr)

# K-Nearest Neighbors Model
train_control <- trainControl(method = "cv", number = 5)
knn_model <- train(Loan_Default_Risk ~ ., data = X_train_processed, method = "knn", trControl = train_control, tuneGrid = expand.grid(k = 5))
y_pred_knn <- predict(knn_model, X_test_processed)
levels_to_use <- union(levels(y_test), levels(y_pred_knn))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_knn <- factor(y_pred_knn, levels = levels_to_use)
accuracy_knn <- confusionMatrix(y_test, y_pred_knn)$overall["Accuracy"]
knn_report <- confusionMatrix(y_test, y_pred_knn)

# Random Forest Model
rf_model <- train(Loan_Default_Risk ~ ., data = X_train_processed, method = "rf", trControl = train_control, tuneGrid = expand.grid(.mtry = 2), ntree = 100)
y_pred_rf <- predict(rf_model, X_test_processed)
levels_to_use <- union(levels(y_test), levels(y_pred_rf))
y_test <- factor(y_test, levels = levels_to_use)
y_pred_rf <- factor(y_pred_rf, levels = levels_to_use)
accuracy_rf <- confusionMatrix(y_test, y_pred_rf)$overall["Accuracy"]
rf_report <- confusionMatrix(y_test, y_pred_rf)

# Create a data frame to store the metrics
metrics_df <- data.frame(
  Model = c("Logistic Regression", "K-Nearest Neighbors", "Random Forest"),
  Precision = c(log_reg_report$byClass["Precision"], knn_report$byClass["Precision"], rf_report$byClass["Precision"]),
  Recall = c(log_reg_report$byClass["Recall"], knn_report$byClass["Recall"], rf_report$byClass["Recall"]),
  F1_Score = c(log_reg_report$byClass["F1"], knn_report$byClass["F1"], rf_report$byClass["F1"]),
  Accuracy = c(accuracy_lr, accuracy_knn, accuracy_rf)
)

# Reshape to long format for ggplot
metrics_df_long <- metrics_df %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

# Plot comparison
ggplot(metrics_df_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  labs(title = "Comparison of Model Metrics")



