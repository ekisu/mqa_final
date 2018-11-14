
data <- read.csv("data.txt", na.strings="?")
data <- data[complete.cases(data), ]
rownames(data) <- data[, 1]
X<- subset(data, select = c("LOCATION","ERECTED","LENGTH","LANES"))
hC <- hclust(dist(X))
hcd <- as.dendrogram(hC)
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.7, col = "green")
plot(hcd, xlab = "Similaridade",
     nodePar = nodePar, horiz = TRUE)
