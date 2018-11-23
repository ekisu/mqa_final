# H_0: a distribuição das pontes é de 50% STEEL, 50% o resto
# H_1: a distribuição não é.
data <- read.csv("data.txt", na.strings="?")
dataSubset <- subset(data, select = c("MATERIAL"))
dataSubset <- dataSubset[complete.cases(dataSubset), ]

observados <- c(length(dataSubset[dataSubset == "STEEL"]), length(dataSubset[dataSubset != "STEEL"]))
p <- c(0.5, 0.5)

chisq.test(x = observados, p=p, simulate.p.value = TRUE)
