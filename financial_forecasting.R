# Load libraries
library(readxl)
library(neuralnet)
library(forecast)

# Set the working directory to the parent directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
getwd()


# Load exchange rate data
data <- read_excel("../Data/ExchangeUSD.xlsx")
str(data)
head(data)

# Check for missing values
sum(is.na(data))

# Plot index(x), rate(y)
# plot(data$`YYYY/MM/DD`, data$`USD/EUR`, 
#      type = "l", main = "Exchange Rate Data",
#      xlab = "Date", ylab = "USD/EUR Rate")

# Extract USD/EUR exchange rates
exchange.rates <- data[, 3]





# 1. Preprocessing and Splitting Data


# Define training and testing data split
set.seed(123)
training_data <- exchange.rates[1:400, "USD/EUR"]
testing_data <- exchange.rates[401:500, "USD/EUR"]

# Normalize data (min-max scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Denormalize data (reverse min-max scaling)
denormalize <- function(x, original_data) {
  min_original <- min(original_data)
  max_original <- max(original_data)
  denormalized <- x * (max_original - min_original) + min_original
  return(denormalized)
}

normalized_training_data <- normalize(training_data)
normalized_testing_data <- normalize(testing_data)





# 2 AR Lags and I/O Matrix Creation


# Define function to create I/O matrices for a given lag
create_io_matrices <- function(data, lag) {
  input_matrix <- matrix(nrow = nrow(data) - lag, ncol = lag)
  output_matrix <- matrix(nrow = nrow(data) - lag, ncol = 1)
  
  for (i in 1:(nrow(data) - lag)) {
    input_matrix[i, ] <- data$`USD/EUR`[i:(i + lag - 1)]
    output_matrix[i, ] <- data$`USD/EUR`[i + lag]
  }
  
  # Assign column names to input_matrix
  colnames(input_matrix) <- paste0("lag", 1:lag)
  
  return(list(input = input_matrix, output = output_matrix))
}

# Experiment with different lags (e.g., 1, 2, 3, 4)
lag_options <- c(1,2,3,4)
io_matrices <- lapply(lag_options, function(lag) create_io_matrices(normalized_training_data, lag))
io_matrices_test <- lapply(lag_options, function(lag) create_io_matrices(normalized_testing_data, lag))


# 3 Building and Evaluating MLP Models


print_first_80_comparison <- function(data1, data2) {
  # Check if data structures are numeric vectors of the same length
  
  # Limit to the first 80 elements
  data1 <- data1[1:min(length(data1), 80)]
  data2 <- data2[1:min(length(data2), 80)]
  
  # Print comparison header
  cat("   ***Comparison of First 80 Values***\n")
  cat("Index | Predicated Rate      | Actual Rate\n")
  cat("------|----------------------|-------------\n")
  
  # Print values in a formatted table
  for (i in 1:length(data1)) {
    cat(sprintf("%-5d | %-20.6f | %-10.6f\n", i, data1[i], data2[i]))
  }
}


# Define function to train and evaluate an MLP model
train_and_evaluate_model <- function(input_matrix, output_matrix, 
                                     hidden_layer_structure, 
                                     activation_function, 
                                     testing_matrix, testing_data) {
  # Define formula
  formula <- as.formula(paste("output_matrix ~", paste(colnames(input_matrix), collapse = " + "), sep=""))
  
  # Define model structure
  model <- neuralnet(formula, data = cbind(input_matrix, output_matrix), 
                     hidden = hidden_layer_structure,
                     linear.output = TRUE, 
                     act.fct = activation_function)
  
  # Make predictions on testing data
  predictions <- predict(model, newdata = testing_matrix$input)
  
  denormalized_predictions <- denormalize(predictions, testing_data)
  denormalized_testing_outputs <- denormalize(testing_matrix$output, testing_data)
  
  # print_first_80_comparison(denormalized_predictions, denormalized_testing_outputs)
  
  # Calculate evaluation metrics
  rmse <- sqrt(mean((denormalized_predictions - denormalized_testing_outputs)^2))
  
  mae <- mean(abs(denormalized_predictions - denormalized_testing_outputs))
  
  mape <- mean(ifelse(denormalized_testing_outputs != 0, 
                      abs(denormalized_predictions - denormalized_testing_outputs) / denormalized_testing_outputs * 100, 0))
  
  smape <- mean(abs(denormalized_predictions - denormalized_testing_outputs) / (abs(denormalized_predictions) 
                                                                                + abs(denormalized_testing_outputs)) * 200)
  
  
  return(list(model = model, rmse = rmse, mae = mae, mape = mape, smape = smape, denormalized_predictions = denormalized_predictions))
}


hidden_layer_structure <- c(12)

# Experiment with different network structures and activation functions
models <- list()
for (i in 1:length(io_matrices)) {
  input_matrix <- io_matrices[[i]]$input  # Access input matrix for current lag
  output_matrix <- io_matrices[[i]]$output  # Access output matrix for current lag
  testing_matrix <- io_matrices_test[[i]] # Access output matrix
  for (activation in c("logistic", "tanh")) {
    # Call the function to train and evaluate the model
    model_result <- train_and_evaluate_model(input_matrix, output_matrix, 
                                             hidden_layer_structure, 
                                             activation, 
                                             testing_matrix, testing_data)
    
    # Append the model results to a list for each configuration
    models[[paste(lag_options[i], paste(hidden_layer_structure, collapse = "_"), activation, sep = "_")]] <- model_result
  }
}



# Iterate over each item in the models list and print its contents
for (model_name in names(models)) {
  cat("Model Name:", model_name, "\n")
  cat("RMSE:", models[[model_name]]$rmse, "\n")
  cat("MAE:", models[[model_name]]$mae, "\n")
  cat("MAPE:", models[[model_name]]$mape, "\n")
  cat("SMAPE:", models[[model_name]]$smape, "\n\n")
}

# Visualize model

get_model_by_name <- models[["2_12_tanh"]]

plot(get_model_by_name$model)


denormalized_testing_outputs <- denormalize(io_matrices_test[[1]]$output, testing_data)

# Plot predicted vs actual values
plot(denormalized_testing_outputs, type = "l", col = "green", xlab = "Index", ylab = "Value", main = "Predicted vs Actual Values")
lines(get_model_by_name$denormalized_predictions, col = "red")
legend("topleft", legend = c("Actual Rate", "Predicted Rate"), col = c("green", "red"), lty = 1)

