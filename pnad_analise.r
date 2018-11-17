library(psych)
data <- read.csv("data.txt", na.strings="?")
attach(data)

#definir variaveis
X<- subset(data, select = c("LOCATION","ERECTED","LENGTH","LANES"))
X<- X[complete.cases(X), ]

#descricoes estatisticas
summary(X)

#matriz de correlacao
cor(X)

#PCA
pca1 <- princomp(X, scores=TRUE, cor=TRUE)

#descricoes estatisticas do PCA
summary(pca1)

#Componentes Principais
loadings(pca1)

#Imprimir os autovalores
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")

#Imprimir os escores
biplot(pca1)

#Rotacao VARIMAX
pca2 <- principal(X, nfactors = 2, scores=TRUE)
summary(pca2)
biplot(pca2)


