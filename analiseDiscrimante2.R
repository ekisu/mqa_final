#analise discriminante
data(iris)
str(iris)

install.packages('psych')
library(psych)
#mostra cof de colisão
pairs.panels(iris[1:4],
             gap=0,
             bg= c("red", "green", "blue")[iris$Species],
             pch=21)


# Data partition
set.seed(555)
ind <- sample(2, nrow(iris),
              replace=TRUE,
              prob= c(0.6, 0.4))
training <- iris[ind==1, ]
testing <- iris[ind==2, ]

# Linear discriminat analysis
library(MASS)
linear <- lda(Species~ ., data=training)
linear
attributes(linear)
linear$prior
linear$counts

#Histogram
p <- predict(linear, training) #para cada registro quando foi a previsão
# $posterior mostra a prob de cada registro para cada classe
# $x margem de erro e acerto

#mostra a separação entre os grupos
ldahist(data = p$x[, 1], g= training$Species)
ldahist(data = p$x[, 2], g=training$Species)

# Bi-Plot
library(devtools)
install.packages("githubinstall")
library(githubinstall)
install_github("fawda123/ggord")
gh_install_packages("ggord")
library(ggord)
ggord(linear, training$Species, ylim= c(-10, 10)) #mostra escores

#Partition plot
install.packages("klarR")
library(klarR)
partimat(Species~., data = training, method="lda")
partimat(Species~., data = training, method="qda")

#Confusion matrix and accuracy - training data
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab
sum(diag(tab))/sum(tab)

#Confusion matrix and accuracy - testing data
p2 <- predict(linear, testing)$class
tab2 <- table(Predicted = p2, Actual = testing$Species)
tab2
sum(diag(tab2))/sum(tab2)

