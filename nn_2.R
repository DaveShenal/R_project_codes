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

# Extract USD/EUR exchange rates
exchange.rates <- data[, 3]
head(exchange.rates)

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
  colnames(output_matrix) <- "output"
  
  return(list(input = input_matrix, output = output_matrix))
}


# Experiment with different lags (e.g., 1, 2, 3, 4)
lag_options <- c(1,2,3,4)
io_matrices <- lapply(lag_options, function(lag) create_io_matrices(normalized_training_data, lag))
io_matrices_test <- lapply(lag_options, function(lag) create_io_matrices(normalized_testing_data, lag))

head(io_matrices[[1]]$input)
head(io_matrices[[1]]$output)

head(io_matrices[[2]]$input)
head(io_matrices[[2]]$output)

head(io_matrices[[3]]$input)
head(io_matrices[[3]]$output)

head(io_matrices[[4]]$input)
head(io_matrices[[4]]$output)

# 3 Building and Evaluating MLP Models

# Define function to train a neural network model
train_neural_network <- function(input_data, output_data, hidden_layers, activation_function, learning_rate) {
  # Define formula
  formula <- as.formula(paste("output ~", paste(colnames(input_data), collapse = " + "), sep=""))
  
  # Define model structure
  model <- neuralnet(formula, data = cbind(input_data, output_data), 
                     hidden = hidden_layers,
                     linear.output = TRUE, 
                     act.fct = activation_function,
                     learningrate = learning_rate,
                     algorithm = "sag")
  
  return(model)
}


# train a model
model <- train_neural_network(
  io_matrices[[1]]$input, # input matrix
  io_matrices[[1]]$output, # output matrix
  c(2,3), # node structure
  "logistic", # activation
  0.01) # learning rate



# Make predictions on testing data
predictions <- predict(model, newdata = io_matrices_test[[1]]$input)

denormalized_predictions <- denormalize(predictions, testing_data)
denormalized_testing_outputs <- denormalize(io_matrices_test[[1]]$output, testing_data)


calculate_evaluation_metrics <- function(predictions, actual) {
  rmse <- sqrt(mean((predictions - actual)^2))
  mae <- mean(abs(predictions - actual))
  mape <- mean(ifelse(actual != 0, 
                      abs(predictions - actual) / actual * 100, 
                      0))
  smape <- mean(abs(predictions - actual) / (abs(predictions) + abs(actual)) * 200)
  
  # Print metrics
  print("Evaluation Metrics:")
  print(paste("RMSE:",rmse))
  print(paste("MAE:", mae))
  print(paste("MAPE:", mape))
  print(paste("sMAPE:", smape))
  
  # Plot predicted vs actual values
  plot(actual, type = "l", col = "blue", xlab = "Index", ylab = "Value", main = "Predicted vs Actual Values")
  lines(predictions, col = "red")
  legend("topleft", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)
  
}

calculate_evaluation_metrics(denormalized_predictions, denormalized_testing_outputs)

plot(model)


