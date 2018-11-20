library(titanic)
library(dummies)
library(caret)
data("titanic_train")
data("titanic_test")

colnames(titanic_train)
names(titanic_train) <- c( "Id Passageiro", "Sobreviveu", "Classe", "Nome", "Sexo","Idade", "Irmaos",
                        "Parentes","Ticket", "Tarifa","Cabine", "Embarque")

treino.raw <- titanic_train
str(treino.raw)

#tira dados que faltam
treino.raw <- subset(treino.raw, select = c(2,3,5,6,7,8,10,12))
treino.raw <- treino.raw[!(is.na(treino.raw$Idade)),]
treino.raw <- treino.raw[!is.na(treino.raw$Embarque),]
head(treino.raw)
#transforma algumas colunas em factors
treino.raw$Sobreviveu <- as.factor(treino.raw$Sobreviveu)
treino.raw$Classe <- as.factor(treino.raw$Classe)
treino.raw$Sexo <- as.factor(treino.raw$Sexo)
treino.raw$Embarque <- as.factor(treino.raw$Embarque)

#busca relacoes
xtabs(~ treino.raw$Sobreviveu + treino.raw$Sexo, data=treino.raw)
xtabs(~ treino.raw$Sobreviveu + treino.raw$Classe, data=treino.raw)
tabela1 <- na.omit(subset(treino.raw,select=c(1,2,3))) #survived, pclass, sex
table(tabela1) 

treino.raw <- dummy.data.frame(treino.raw, names=c("Classe","Sexo","Embarque"), sep="_")
head(treino.raw)

#dividindo o dataset para treino e teste
treino <- treino.raw[1:800,]
teste <- treino.raw[500:600,]
modelo <- glm(Sobreviveu ~.,family=binomial(link='logit'),data=treino)
summary(modelo)
head(treino)

#avaliando o modelo
anova(modelo, test="Chisq")
library(pscl)
pR2(modelo)


resultado <- predict(modelo,newdata=teste,type='response')
resultado <- ifelse(resultado > 0.5,1,0)
erro <- mean(resultado != teste$Sobreviveu)
print(paste('Confiabilidade',1-erro))


####################################### probit e logit ###################################

#coeficientes modelo logit
clogit <- glm(Sobreviveu ~., family=binomial (link="logit"), data=treino) 
summary(clogit)

#coeficientes modelo rpobit
cprobit <- glm(Sobreviveu ~., family=binomial (link="probit"), data=treino) 
summary(cprobit)



