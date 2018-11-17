library(rgl)

data <- read.csv("data.txt", na.strings="?")
dataSubset <- subset(data, select = c("ERECTED", "LENGTH", "LANES"))
dataSubset <- dataSubset[complete.cases(dataSubset), ]

plot3d(dataSubset)

# Vamos tentar partir do modelo da regressão simples, e tentar adicionar uma outra variável (LANES).
modeloLinear <- lm(LENGTH ~ ERECTED, data = dataSubset)
modeloMultipla <- lm(LENGTH ~ ERECTED + LANES, data = dataSubset)
kable(anova(modeloLinear, modeloMultipla), caption = "Tabela ANOVA, comparando os modelos LENGTH ~ ERECTED e LENGTH ~ ERECTED + LANES")
