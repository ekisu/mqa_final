
### Nonmetric MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
data <- read.csv("data.txt", na.strings="?")
data <- data[complete.cases(data), ]
rownames(data) <- data[, 1]
mydata<- subset(data, select = c("LOCATION"))
d <- dist(mydata) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7)
