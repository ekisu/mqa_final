# Tentar realizar regressão simples em cima das variaveis numéricas
data <- read.csv("data.txt", na.strings="?")
dataELength <- subset(data, select = c("ERECTED", "LENGTH"))
dataELength <- dataELength[complete.cases(dataELength), ]

dataELanes <- subset(data, select = c("ERECTED", "LANES"))
dataELanes <- dataELanes[complete.cases(dataELanes), ]

dataLengthLanes <- subset(data, select = c("LENGTH", "LANES"))
dataLengthLanes <- dataLengthLanes[complete.cases(dataLengthLanes), ]

plot(dataELength$ERECTED, dataELength$LENGTH, pch = 16)
plot(dataELanes$ERECTED, dataELanes$LANES, pch = 16)
plot(dataLengthLanes$LENGTH, dataLengthLanes$LANES, pch = 16)

# Escolhemos a relação LENGTH em função de ERECTED
modeloLinear <- lm(LENGTH ~ ERECTED, data = dataELength)
summary(modeloLinear)

plot(dataELength$ERECTED, dataELength$LENGTH, pch = 16)
abline(modeloLinear)