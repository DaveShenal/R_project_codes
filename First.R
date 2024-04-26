# Load libraries
library(ggplot2)
library(readxl)
library(NbClust)  # to determine the number of clusters
library(factoextra)  # to determine the number of clusters
library(cluster)   # to do silhouette analysis
library(dplyr)


# Set the working directory to the parent directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
getwd()


# Read the Excel file
data <- read_excel("../Data/Whitewine_v6.xlsx", sheet = 1)
head(data)
str(data)

sum(is.na(data)) # missing values

# Select the first 11 attributes
wine.data <- data[1:11]
head(wine.data)
str(wine.data)

number_of_rows <- nrow(wine.data)
print(number_of_rows)

# 1st Subtask
# (a) Preprocessing

# Outlier detection using boxplots
boxplot.results <- boxplot(wine.data, plot = TRUE)
outlier_indices <- boxplot.results$out
print(length(outlier_indices))
print(outlier_indices)


remove_outliers <- function(wine.data) {
  for (col in names(wine.data)) {
    outliers <- boxplot(wine.data[[col]], plot=FALSE)$out
    wine.data <- wine.data[!wine.data[[col]] %in% outliers, ]
  }
  wine.data
}

remove_outliers_multiple <- function(wine.data, iterations) {
  cleaned_data <- wine.data
  
  for (i in 1:iterations) {
    cleaned_data <- remove_outliers(cleaned_data)
  }
  
  cleaned_data
}

# Filter rows that are not outliers
wine.data <- remove_outliers_multiple(wine.data,1)

print(nrow(wine.data))

boxplot(wine.data, plot = TRUE)


# Scaling ------------------------
str(wine.data)
head(wine.data)
wine.data <- scale(center = TRUE, wine.data)  # Standardize data
str(wine.data)
head(wine.data)

# (b) Determine number of clusters

# NBclust ------------------------
set.seed(123)  # Set seed for reproducibility
clusterNo_method1=NbClust(wine.data,distance="euclidean", min.nc=2,max.nc=10,method="kmeans",index="all")

# k values to ckeck
k.values <- 2:10

# Elbow method ------------------
set.seed(124)
fviz_nbclust(wine.data, kmeans, method = "wss") + labs(subtitle = "Elbow Method")


# Silhouette method -----------
# Function to compute silhouette width for given k
set.seed(125)
fviz_nbclust(wine.data, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")


# Gap statistic ----------------------------
set.seed(125)
gap_stat <- clusGap(wine.data, FUN = kmeans, K.max = 10, B = 50, iter.max = 20)
print(gap_stat, method = "firstmax")

# Visualize gap statistic
fviz_gap_stat(gap_stat) + labs(subtitle = "Gap Statistic")




# Discussion on automated methods: ------------------------------------------
# Based on the results, choose the most favored k (e.g., k=3 based on elbow method)

# (c) K-means clustering

# Perform k-means clustering with k=2
kmeans.results <- kmeans(wine.data, centers = 2, nstart = 25)

# Cluster assignments for each data point
cluster.assignment <- kmeans.results$cluster

# Access cluster centers
cluster.centers <- kmeans.results$centers

# Total sum of squares (TSS) - variance of entire data set
totss <- kmeans.results$totss

# Extract within-cluster sum of squares (WSS) for each cluster
withinss <- kmeans.results$withinss

# Calculate total within-cluster sum of squares (TWSS)
twss <- sum(withinss)

# Calculate between-cluster sum of squares (BSS)
bss <- totss - twss

# Ratio of BSS to TSS (measures cluster separation)
bss_ratio <- bss / totss

# Print informative message
cat("Cluster Assignments:\n")
table(cluster.assignment)  # Print counts for each cluster

cat("\nCluster Centers:\n")
print(cluster.centers)  # Print centers for each feature

cat("\nTotal Sum of Squares (TSS):", totss, "\n")
cat("Within-Cluster Sum of Squares (TWSS):", twss, "\n")
cat("Between-Cluster Sum of Squares (BSS):", bss, "\n")
cat("Ratio of BSS to TSS:", bss_ratio, "\n")

# Silhouette plot
sil <- silhouette(kmeans.results$cluster, dist(wine.data))
fviz_silhouette(sil)

# Discussion on silhouette plot:
# Interpret the silhouette values and average width to assess cluster quality




