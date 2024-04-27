# Load libraries
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
plot(data$`YYYY/MM/DD`, data$`USD/EUR`, 
     type = "l", main = "Exchange Rate Data",
     xlab = "Date", ylab = "USD/EUR Rate")

# Extract USD/EUR exchange rates (assuming it's the 3rd column)
exchange.rates <- data[, 3]

# 1. Preprocessing and Splitting Data

# Define training and testing data split (400 for training, remaining for testing)
training_data <- exchange.rates[1:400, "USD/EUR"]
testing_data <- exchange.rates[401:500, "USD/EUR"]

# Normalize data (e.g., min-max scaling)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

normalized_training_data <- normalize(training_data)
normalized_testing_data <- normalize(testing_data)


# 2 Experimenting with AR Lags and I/O Matrix Creation

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
lag_options <- c(1, 2, 3, 4)
io_matrices <- lapply(lag_options, function(lag) create_io_matrices(normalized_training_data, lag))
io_matrices_test <- lapply(lag_options, function(lag) create_io_matrices(normalized_testing_data, lag))

str(io_matrices_test[[2]]$output)

# 3 Building and Evaluating MLP Models

# Define function to train and evaluate an MLP model
train_and_evaluate_model <- function(input_matrix, output_matrix, hidden_layers, nodes_per_layer, activation_function, testing_data) {
  # Define formula
  formula <- as.formula(paste("output_matrix ~", paste(colnames(input_matrix), collapse = " + "), sep=""))
  
  # Define model structure
  model <- neuralnet(formula, data = cbind(input_matrix, output_matrix), 
                     hidden = c(rep(nodes_per_layer, hidden_layers)),
                     linear.output = TRUE, 
                     act.fct = activation_function)
  print("model trained")
  
  # Make predictions on testing data
  predictions <- predict(model, newdata = testing_data$input)
  print("got predictions")
  
  # Calculate evaluation metrics
  # Calculate evaluation metrics
  rmse <- sqrt(mean((predictions - testing_data$output)^2))
  print("rmse calculated")
  mae <- mean(abs(predictions - testing_data$output))
  print("mae calculated")
  mape <- mean(abs(predictions - testing_data$output) / testing_data$output) * 100
  print("mape calculated")
  smape <- mean(abs(predictions - testing_data$output) / (abs(predictions) + abs(testing_data$output)) * 200)
  print("smape calculated")
  
  
  return(list(model = model, rmse = rmse, mae = mae, mape = mape, smape = smape))
}

# Experiment with different network structures and activation functions
models <- list()
for (lag in lag_options) {
  input_matrix <- io_matrices[[lag]]$input  # Access input matrix for current lag
  output_matrix <- io_matrices[[lag]]$output  # Access output matrix for current lag
  testing_data <- io_matrices_test[[lag]] # Access output matrix
  for (hidden_layers in c(1, 2)) {
    for (nodes in c(5, 10)) {
      for (activation in c("logistic", "tanh")) {
        # Call the function to train and evaluate the model
        model_result <- train_and_evaluate_model(input_matrix, output_matrix, hidden_layers, nodes, activation, testing_data)
        # Append the model results to a list for each configuration
        models[[paste(lag, hidden_layers, nodes, activation, sep = "_")]] <- model_result
      }
    }
  }
}



# Iterate over each item in the models list and print its contents
for (model_name in names(models)) {
  cat("Model Name:", model_name, "\n")
  cat("RMSE:", models[[model_name]]$rmse, "\n")
  cat("MAE:", models[[model_name]]$mae, "\n")
  if (is.finite(models[[model_name]]$mape)) {
    cat("MAPE:", models[[model_name]]$mape, "\n")
  } else {
    cat("MAPE: Cannot be computed (infinity)\n")
  }
  cat("SMAPE:", models[[model_name]]$smape, "\n\n")
}

print(length(models))

