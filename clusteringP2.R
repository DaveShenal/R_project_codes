# Load libraries
library(ggplot2)
library(readxl)
library(NbClust)  # For determining the number of clusters
library(factoextra) # For determining the number of clusters
library(cluster)   # For silhouette analysis
library(dplyr) 
library(fpc) # provides a function named calinhara that calculates the Calinski-Harabasz index.

# Set the working directory to the parent directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
getwd()

# Read the Excel file
data <- read_excel("../Data/Whitewine_v6.xlsx", sheet = 1)
head(data)
str(data)

# Check for missing values
sum(is.na(data))  # Check for missing values

# Select the first 11 attributes
wine.data <- data[1:11]

# 1st Subtask
# (a) Preprocessing

# Outlier detection using boxplots
boxplot.results <- boxplot(wine.data, plot = TRUE)
outlier_indices <- boxplot.results$out
print(length(outlier_indices))

# Function to remove outliers using boxplots iteratively
remove_outliers <- function(wine.data) {
  for (col in names(wine.data)) {
    outliers <- boxplot(wine.data[[col]], plot = FALSE)$out
    wine.data <- wine.data[!wine.data[[col]] %in% outliers, ]
  }
  wine.data
}

# Function to remove outliers iteratively (can adjust the number of iterations)
remove_outliers_multiple <- function(wine.data, iterations) {
  cleaned_data <- wine.data
  for (i in 1:iterations) {
    cleaned_data <- remove_outliers(cleaned_data)
  }
  cleaned_data
}

# Filter rows that are not outliers
wine.data <- remove_outliers_multiple(wine.data, 1)

boxplot(wine.data, plot = TRUE)  # Check for outliers after removal

# Scaling
str(wine.data)
wine.data <- scale(center = TRUE, wine.data)  # Standardize data
str(wine.data)




# 2nd Subtask -----------------------------------------------------------

# Perform PCA using prcomp (recommended over princomp)
pca <- prcomp(wine.data)  # Center and scale data

# View eigenvalues and eigenvectors
summary(pca)

# Cumulative contribution of principal components
pca.variance <- pca$sdev^2 / sum(pca$sdev^2)
cum_variance <- cumsum(pca.variance)

# Select PCs with cumulative score > 85%
pcs_to_keep <- which(cum_variance >= 0.85)
print(pcs_to_keep)

# Transformed data with selected PCs
wine.data.pca <- pca$x[, pcs_to_keep]

# Discussion:
# I chose PCs that explain at least 85% of the variance in the data. 
# This helps reduce dimensionality while retaining most of the important information.

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
gap_stat_pca <- clusGap(wine.data.pca, FUN = kmeans, K.max = 10, B = 50, iter.max = 20)
print(gap_stat_pca, method = "firstmax")

# Visualize gap statistic
fviz_gap_stat(gap_stat_pca) + labs(subtitle = "Gap Statistic (PCA)")

# Discussion:
# Apply the same methods (NBclust, elbow, silhouette, gap statistic) to the PCA-transformed data 
# and analyze the results to determine the most suitable k for k-means clustering on this new dataset.

kmeans.pca.results <- kmeans(wine.data.pca, centers = 3, nstart = 25)

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

# Discussion on silhouette width and average score to assess cluster quality (similar to previous section)

# Calinski-Harabasz Index with fpc package
ch.index <- calinhara(wine.data.pca, kmeans.pca.results$cluster)
cat("Calinski-Harabasz Index:", ch.index, "\n")

# fviz_cluster plot
fviz_cluster(kmeans.pca.results, data = wine.data.pca, 
             choose.vars = 1:2,  # Choose first two principal components for visualization
             ellipse.type = "convex",  # Specify ellipse type for clusters (optional)
             show.clust.cent = TRUE)  # Show cluster centers (optional)

