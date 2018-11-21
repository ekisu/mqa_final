significance <- function(x) {
  if (x <= 0.001) {
    "***"
  } else if (x <= 0.01) {
    "**"
  } else if (x <= 0.05) {
    "*"
  } else if (x <= 0.1) {
    "."
  } else {
    " "
  }
}

data <- read.csv("data.txt", na.strings="?")

numerical <- c("ERECTED", "LENGTH", "LANES")
nominal <- c("PURPOSE", "CLEAR.G", "T.OR.D", "MATERIAL", "SPAN", "REL.L", "TYPE")

pValuesMatrix <- matrix(0, nrow = length(nominal), ncol = length(numerical))
rownames(pValuesMatrix) <- nominal
colnames(pValuesMatrix) <- numerical

significanceMatrix <- matrix(0, nrow = length(nominal), ncol = length(numerical))
rownames(significanceMatrix) <- nominal
colnames(significanceMatrix) <- numerical

for (numVar in numerical) {
  for (nomVar in nominal) {
    form <- as.formula(paste(numVar, "~", paste0("`", nomVar, "`")))
    dataSubset <- subset(data, select = c(numVar, nomVar))
    dataSubset <- dataSubset[complete.cases(dataSubset), ]
    
    analysis <- aov(form, data = dataSubset)
    
    pVal <- summary(analysis)[[1]][["Pr(>F)"]][[1]]
    pValuesMatrix[nomVar, numVar] <- pVal
    significanceMatrix[nomVar, numVar] <- significance(pVal)
  }
}
