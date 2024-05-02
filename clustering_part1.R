# Load libraries
library(readxl)
library(factoextra)  # to determine the number of clusters
library(cluster)   # to do silhouette analysis
library(dplyr)


# Set the working directory to the parent directory
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)
getwd()


# Read the Excel file
data <- read_excel("../Data/Whitewine_v6.xlsx")
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

boxplot(wine.data, plot = TRUE)


# Scaling ------------------------
wine.data <- scale(center = TRUE, wine.data)  # Standardize data


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
gap_stat <- clusGap(wine.data, FUN = kmeans, K.max = 10, B = 100)
print(gap_stat, method = "firstmax")

# Visualize gap statistic
fviz_gap_stat(gap_stat) + labs(subtitle = "Gap Statistic")



# (c) K-means clustering

# Perform k-means clustering with k=2
kmeans.results <- kmeans(wine.data, centers = 3, nstart = 25)

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
cat("Within Cluster Sum of Squares (TWSS):", twss, "\n")
cat("Between Cluster Sum of Squares (BSS):", bss, "\n")
cat("Ratio of BSS to TSS:", bss_ratio, "\n")

# Silhouette plot
sil <- silhouette(kmeans.results$cluster, dist(wine.data))
fviz_silhouette(sil)

fviz_cluster(kmeans.results, data = wine.data, 
             geom = "point", 
             show_labels = FALSE)


