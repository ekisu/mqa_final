library(titanic)
library(dummies)
library(caret)
data("titanic_train")
data("titanic_test")

data <- read.csv("data.txt", na.strings="?")
data <- subset(data, select = -IDENTIF)
dados <- data[complete.cases(data), ]

dados$WOOD.OU.NAO <- as.factor(ifelse(dados$MATERIAL == "WOOD", 1, 0))
# Remover previsores "perfeitos"
dados <- subset(dados, select = -MATERIAL)
dados <- subset(dados, select = -TYPE)

summary(dados)

#dividindo o dataset para treino e teste
treino <- dados[1:50,]
teste <- dados[50:nrow(dados),]
modelo <- glm(WOOD.OU.NAO ~.,family=binomial(link='logit'),data=treino, control=glm.control(maxit=50))
summary(modelo)
head(treino)

#avaliando o modelo
anova(modelo, test="Chisq")
library(pscl)
pR2(modelo)

resultado <- predict(modelo,newdata=teste,type='response')
str(resultado)
resultado <- ifelse(resultado > 0.5,1,0)
erro <- mean(resultado != teste$WOOD.OU.NAO)
print(paste('Confiabilidade',1-erro))


