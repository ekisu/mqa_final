library(rgl)

data <- read.csv("data.txt", na.strings="?")
dataELength <- subset(data, select = c("ERECTED", "LENGTH", "LANES"))
dataELength <- dataELength[complete.cases(dataELength), ]

plot3d(dataELength)