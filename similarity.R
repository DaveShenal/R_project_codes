# Check if elements are identical
all_same <- identical(training_data$`USD/EUR`, denormalized_training_data$`USD/EUR`)

# Print result
if (all_same) {
  cat("Elements are perfectly the same.\n")
} else {
  cat("Elements are not perfectly the same.\n")
}

tolerance <- 1e-6  # Adjust tolerance as needed (smaller for higher precision)
elements_similar <- all.equal(training_data$`USD/EUR`, denormalized_training_data$`USD/EUR`, tolerance = tolerance)

if (elements_similar) {
  cat("Elements are nearly identical (within tolerance).\n")
} else {
  cat("Elements might have slight differences due to rounding.\n")
}


differences <- abs(training_data$`USD/EUR` - denormalized_training_data$`USD/EUR`)
average_difference <- mean(differences)

if (average_difference < tolerance) {
  cat("Elements are very similar (average difference < ", tolerance, ").\n")
} else {
  cat("Elements might have some differences (average difference: ", average_difference, ").\n")
}