#Inicializacao dos dados
X02_RTE_CEREAL_V2
dados <- X02_RTE_CEREAL_V2
summary(dados)

#Preparacao da matriz de correlacao
matriz_correlacao <- cor(dados)
print(matriz_correlacao,digits=3)

#Corgrama
library(corrplot)
corrplot(matriz_correlacao)
corrplot.mixed(matriz_correlacao, lower = "number", upper = "circle", number.cex = 0.5, tl.pos = "lt")
corrplot(matriz_correlacao, method = "square", number.cex = 0.5, addCoef.col = "black")
corrplot(matriz_correlacao, method = "circle", number.cex = 0.5, addCoef.col = "black")


#https://gomesfellipe.github.io/post/2018-01-01-analise-multivariada-em-r/an%C3%A1lise-multivariada-em-r/
#Teste de esfericidade de Bartlett

Bartlett.sphericity.test <- function(x)
{
  method <- "Teste de esfericidade de Bartlett"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) # Omitindo valores faltantes
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "X-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value,
                        method=method, data.name=data.name), class="htest"))
}
Bartlett.sphericity.test(dados)

#KMO
kmo = function(x)
{
  x = subset(x, complete.cases(x))
  r = cor(x)
  r2 = r^2 
  i = solve(r) 
  d = diag(i) 
  p2 = (-i/sqrt(outer(d, d)))^2 
  diag(r2) <- diag(p2) <- 0 
  KMO = sum(r2)/(sum(r2)+sum(p2))
  MSA = colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

kmo_data <- kmo(dados)
print(kmo_data)

#Matriz de correlacao parcial
partial.cor <- function (x)
{
  R <- cor(x)
  RI <- solve(R)
  D <- 1/sqrt(diag(RI))
  Rp <- -RI * (D %o% D)
  diag(Rp) <- 0
  rownames(Rp) <- colnames(Rp) <- colnames(x)
  Rp
}
mat_anti_imagem <- -partial.cor(dados)
mat_anti_imagem

#Método dos componentes principais com a matriz de correlação amostral
acpcor <- prcomp(dados, scale = TRUE)
summary(acpcor)

plot(1:ncol(dados), acpcor$sdev^2, type = "b", xlab = "Componente",
     ylab = "Vari�ncia", pch = 20, cex.axis = 1.3, cex.lab = 1.3)

#fatores
k <- 4
carfat <- acpcor$rotation[, 1:k] %*% diag(acpcor$sdev[1:k])
colnames(carfat) <- paste("Fator", 1:k, sep = " ")
carfat

#estimação das comunalidades e das variâncias específicas
comum <- rowSums(carfat^2)
vespec <- diag(matriz_correlacao) - comum
estimat <- cbind(comum, vespec, diag(matriz_correlacao))
rownames(estimat) <- colnames(dados)
colnames(estimat) <- c("Comunalidade", "Vari�ncia �nica", "Vari�ncia")
estimat

resid <- matriz_correlacao - (carfat %*% t(carfat) + diag(vespec))
resid

#Visando auxiliar na interpretação dos fatores, realizamos uma rotação pelo método varimax.
#A função varimax encontra-se no pacote stats.

carfatr <- varimax(carfat)
carfatr

#Plot sem rotação varimax
plot(carfat, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfat, rownames(carfat), adj = 1)

#Plot com rotação varimax
plot(carfatr$loadings, pch = 20, col = "red", xlab = "Fator 1", ylab = "Fator 2")
text(carfatr$loadings, rownames(carfat), adj = 1)


