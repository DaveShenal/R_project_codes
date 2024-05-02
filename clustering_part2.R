# Load libraries
library(readxl)
library(factoextra) # For determining the number of clusters
library(NbClust)
library(cluster)   # For silhouette analysis
library(dplyr) 
library(fpc) # provides a function named calinhara that calculates the Calinski-Harabasz index.

# Set the working directory to the parent directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
getwd()

# Read the Excel file
data <- read_excel("../Data/Whitewine_v6.xlsx")
head(data)
str(data)

# Check for missing values
sum(is.na(data))

# Select the first 11 attributes
wine.data <- data[1:11]

# 1st Subtask
# (a) Preprocessing

# Outlier detection using boxplots
boxplot.results <- boxplot(wine.data, plot = TRUE)
outlier_indices <- boxplot.results$out
print(length(outlier_indices))

remove_outliers <- function(wine.data, custom_range = 1.5){
  cleaned_data <- wine.data
  
  for (col in names(cleaned_data)) {
    # Calculate quartiles
    Q1 <- quantile(cleaned_data[[col]], 0.25)
    Q3 <- quantile(cleaned_data[[col]], 0.75)
    
    # Calculate IQR
    IQR <- Q3 - Q1
    
    # Define lower and upper bounds for outlier detection
    lower_bound <- Q1 - custom_range * IQR
    upper_bound <- Q3 + custom_range * IQR
    
    # Remove outliers
    cleaned_data <- cleaned_data[(cleaned_data[[col]] >= lower_bound) & (cleaned_data[[col]] <= upper_bound), ]
  }
  
  return(cleaned_data)
}

wine.data <- remove_outliers(wine.data)
print(nrow(wine.data))

boxplot(wine.data, plot = TRUE)  # Check for outliers after removal

# Scaling
wine.data <- scale(center = TRUE, wine.data)  # Standardize data




# 2nd Subtask -----------------------------------------------------------

# Perform PCA using prcomp
pca <- prcomp(wine.data,  scale. = FALSE)

# View eigenvalues and eigenvectors
summary(pca)

# Cumulative contribution of principal components
pca.variance <- pca$sdev^2 / sum(pca$sdev^2)
cum_variance <- cumsum(pca.variance)

# Select PCs with cumulative score >= 85%
pcs_to_keep <- which(cum_variance >= 0.85)[1]
print(1:pcs_to_keep)

barplot(cum_variance, main = "Cumulative Proportion of Variance", 
        xlab = "Principal Component", ylab = "Cumulative Proportion of Variance",
        names.arg = seq_along(pca.variance))
# Add a line at y = 0.85
abline(h = 0.85, col = "red", lty = 5)


# Transformed data with selected PCs
wine.data.pca <- pca$x[, 1:pcs_to_keep]



# NBclust
set.seed(123)
clusterNo_method1_pca=NbClust(wine.data.pca,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# Elbow method
set.seed(124)
fviz_nbclust(wine.data.pca, kmeans, method = "wss") + labs(subtitle = "Elbow Method (PCA)")

# Silhouette method
set.seed(125)
fviz_nbclust(wine.data.pca, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method (PCA)")

# Gap statistic
set.seed(126)
fviz_nbclust(wine.data.pca, kmeans, method = 'gap_stat', iter.max = 20)


# Performing K-means clustring
kmeans.pca.results <- kmeans(wine.data.pca, centers = 2, nstart = 25)

# Cluster assignments, centers, etc. (similar to previous k-means section)
cluster.assignment.pca <- kmeans.pca.results$cluster
cluster.centers.pca <- kmeans.pca.results$centers
totss.pca <- kmeans.pca.results$totss
withinss.pca <- kmeans.pca.results$withinss
twss.pca <- sum(withinss.pca)
bss.pca <- totss.pca - twss.pca
bss_ratio_pca <- bss.pca / totss.pca

# Print informative message
cat("Cluster Assignments (PCA):\n")
table(cluster.assignment.pca)

cat("\nCluster Centers (PCA):\n")
print(cluster.centers.pca)

cat("\nTotal Sum of Squares (PCA):", totss.pca, "\n")
cat("Within-Cluster Sum of Squares (PCA):", twss.pca, "\n")
cat("Between-Cluster Sum of Squares (PCA):", bss.pca, "\n")
cat("Ratio of BSS to TSS (PCA):", bss_ratio_pca, "\n")

# Silhouette plot for PCA-based k-means
sil.pca <- silhouette(kmeans.pca.results$cluster, dist(wine.data.pca))
fviz_silhouette(sil.pca)


# Calinski-Harabasz Index with fpc package
ch.index <- calinhara(wine.data.pca, kmeans.pca.results$cluster)
cat("Calinski-Harabasz Index:", ch.index, "\n")

# fviz_cluster plot
fviz_cluster(kmeans.pca.results, data = wine.data.pca, 
             choose.vars = 1:2,  # first two principal components for visualization
             geom = "point")

# K to test to get Calinski-Harabasz Index
k_range <- 2:10


# Empty list to store Calinski-Harabasz values
ch_values <- list()

# Loop through the range of clusters
for (k in k_range) {
  # Perform k-means clustering with k clusters
  kmeans_model <- kmeans(wine.data.pca, centers = k, nstart = 10)
  
  # Calculate Calinski-Harabasz index using a separate function
  ch_value <- calinhara(wine.data.pca, kmeans_model$cluster)
  
  # Append the index for this k value
  ch_values[[length(ch_values) + 1]] <- ch_value
}

# Plot Calinski-Harabasz index vs number of clusters
plot(k_range, ch_values, type = "b", xlab = "Number of Clusters", ylab = "Calinski-Harabasz Index")
title("Calinski-Harabasz Index vs Number of Clusters")
