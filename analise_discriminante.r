## Biblioteca com a função discriminante linear de Fisher
library(MASS)
## Dados (?iris apresenta informações sobre o conjunto de dados)
data <- read.csv("data.txt", na.strings="?")
dataSubset <- subset(data, select = c("MATERIAL", "ERECTED", "LENGTH", "LANES"))
dados <- dataSubset[complete.cases(dataSubset), ]
#analise discriminante

#install.packages('psych')
library(psych)
#mostra cof de colisão
pairs.panels(dados[2:4],
             gap=0,
             bg=c("red", "green", "blue")[dados$MATERIAL],
             pch=21)

#graphics.off()
par("mar")
par(mar=c(1,1,1,1))

# Data partition
set.seed(555)
ind <- sample(2, nrow(dados),
              replace=TRUE,
              prob= c(0.6, 0.4))
training <- dados[ind==1, ]
testing <- dados[ind==2, ]

# Linear discriminat analysis
library(MASS)
linear <- lda(MATERIAL~ ., data=training)
linear
attributes(linear)
linear$prior
linear$counts

#Histogram
p <- predict(linear, training) #para cada registro quando foi a previsão
# $posterior mostra a prob de cada registro para cada classe
# $x margem de erro e acerto

#mostra a separação entre os grupos
ldahist(data = p$x[, 1], g= training$MATERIAL)
ldahist(data = p$x[, 2], g=training$MATERIAL)

# Bi-Plot
#library(devtools)
#install.packages("githubinstall")
#library(githubinstall)
#install_github("fawda123/ggord")
#gh_install_packages("ggord")
library(ggord)
ggord(linear, training$MATERIAL, ylim= c(-10, 10)) #mostra escores

#Partition plot
#install.packages("klaR")
library(klaR)
partimat(MATERIAL~., data = training, method="lda")
partimat(MATERIAL~., data = training, method="qda")

#Confusion matrix and accuracy - training data
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$MATERIAL)
tab
print(paste("Quantidade prevista (%):", sum(diag(tab))/sum(tab)*100))

#Confusion matrix and accuracy - testing data
p2 <- predict(linear, testing)$class
tab2 <- table(Predicted = p2, Actual = testing$MATERIAL)
tab2
print(paste("Quantidade prevista (%):", sum(diag(tab2))/sum(tab2)*100))
