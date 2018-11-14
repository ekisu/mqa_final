# Realizar ANOVA entre ano de criação da ponte (ERECTED) e material.
data <- read.csv("data.txt", na.strings="?")
dataSubset <- subset(data, select = c("ERECTED", "MATERIAL"))
dataSubset <- dataSubset[complete.cases(dataSubset), ]
n <- nrow(dataSubset)

xlim <- c(min(dataSubset$ERECTED), max(dataSubset$ERECTED))
stripchart(dataSubset$ERECTED, method="stack", xlim=xlim)

# Primeiro, sem nenhum fator, calcular a soma de quadrados.
mediaAno <- mean(dataSubset$ERECTED)
somaTotalQuadrados <- sum((dataSubset$ERECTED - mediaAno) ^ 2)

# Depois, levar a consideração o fator ERECTED.
dadosWood <- subset(dataSubset, MATERIAL == "WOOD")
dadosIron <- subset(dataSubset, MATERIAL == "IRON")
dadosSteel <- subset(dataSubset, MATERIAL == "STEEL")

stripchart(dadosWood$ERECTED, method="stack", xlim=xlim)
stripchart(dadosIron$ERECTED, method="stack", xlim=xlim)
stripchart(dadosSteel$ERECTED, method="stack", xlim=xlim)

mediaWood <- mean(dadosWood$ERECTED)
mediaIron <- mean(dadosIron$ERECTED)
mediaSteel <- mean(dadosSteel$ERECTED)

sqWood <- sum((dadosWood$ERECTED - mediaWood)^2)
sqIron <- sum((dadosIron$ERECTED - mediaIron)^2)
sqSteel <- sum((dadosSteel$ERECTED - mediaSteel)^2)

somaQuadradosResiduais <- sqWood + sqIron + sqSteel
somaQuadradosEntre <- somaTotalQuadrados - somaQuadradosResiduais

quadradoMedioTotal <- somaTotalQuadrados / (n - 1)
quadradoMedioEntre <- somaQuadradosEntre / 2
quadradoMedioDentro <- somaQuadradosResiduais / (n - 2 - 1)

# Montar a tabela ANOVA
tabelaAnova <- data.frame(
  liberdade = c(2, n - 2 - 1, n - 1),
  SQ = c(somaQuadradosEntre, somaQuadradosResiduais, somaTotalQuadrados),
  QM = c(quadradoMedioEntre, quadradoMedioDentro, quadradoMedioTotal),
  F = c(quadradoMedioEntre / quadradoMedioDentro, "", "")
)

print(paste("Valor de f para 2,", n - 2 - 1, ", com 95% de confiança:", qf(0.95, 2, n - 2 - 1)))
print(paste("Como este valor é menor que", quadradoMedioEntre / quadradoMedioDentro, "rejeitamos a hipótese nula."))
