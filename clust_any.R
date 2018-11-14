
data <- read.csv("data.txt", na.strings="?")
data <- data[complete.cases(data), ]
rownames(data) <- data[, 1]
mydata<- subset(data, select = c("ERECTED","LENGTH","LANES"))

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata, 4)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)