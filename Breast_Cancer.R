install.packages("ggplot2")
install.packages("cluster")
install.packages("factoextra")
install.packages("datasets")
install.packages("dplyr")
install.packages("mlbench")
library(ggplot2)
library(cluster)
library(factoextra)
library(datasets)
library(mlbench)


# Load breast cancer dataset
data(BreastCancer)

# Assign the dataset to a variable
d <- BreastCancer

#Data Preprocessig

#Remove instances to remove missing value
d <- d[complete.cases(d), ]

#discard attributes/column with missing values
d<- na.omit(d)

#check the data types of each column
str(d)

# Remove the "Class" column from the dataset to make unsupervised
X <- subset(BreastCancer, select = -Class)


#	Feature selection
# Remove the id column as it has no correlation to the class 
X <- subset(X, select = -c(Id))

# Convert columns to numeric from factor 
X <- as.data.frame(lapply(X, as.numeric))

# Verify the new data type of columns
str(X)


#Make sure missing values are removed 
X_clean <- na.omit(X)

# Standardize the data
X_std <- scale(X_clean)

# Perform k-means clustering to find the value of K
set.seed(123)
wss <- c()
for (i in 1:10) {
  km <- kmeans(X_std, centers = i, nstart = 10)
  wss[i] <- km$tot.withinss
}

# Plot elbow curve to find the value of K
elbow_plot <- ggplot(data.frame(x=1:10, y=wss), aes(x, y)) +
  geom_point(size=3) + 
  geom_line() + 
  ggtitle("Elbow plot for Iris dataset") + 
  xlab("Number of clusters (k)") + 
  ylab("Within-cluster sum of squares") +
  theme_minimal()
print(elbow_plot)

# Perform k-means clustering with k = 1.75 ( the value of K is seen from the graph)
kmeans.model <- kmeans(X_std, centers = 1.75, nstart = 10)


# View the cluster centers
kmeans.model$centers


# convert X_std into a data frame before adding the new column as it is not in the correct form.
X_std <- as.data.frame(X_std)
X_std$cluster <- kmeans.model$cluster



# Visualize the clusters by scatterplot
ggplot(X_std, aes(x=Cell.size, y=Cell.shape, color=factor(cluster))) +
  geom_point() +
  ggtitle("K-means Clustering of  Data") +
  labs(color="Cluster")

ggplot(X_std, aes(x=Cl.thickness, y=Mitoses, color=factor(cluster))) +
  geom_point() +
  ggtitle("K-means Clustering of Data") +
  labs(color="Cluster")


#A heatmap can be used to show the intensity of the relationships between variables and how they are related to the clusters
heatmap(cor(X_std), col=cm.colors(256), Rowv=NA, Colv=NA)

