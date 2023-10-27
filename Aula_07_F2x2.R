#' ---
#' title: "Exemplo 2: Esquema fatorial de tratamentos 2 X 2"
#' header-includes: \usepackage{here}
#' ---

#' # Importando os dados
#'
# Escolher o arquivo "exemplo_vieira.csv"
dados <- read.csv2(file.choose())
str(dados)
dados <- transform(dados,
                   Irrigacao = factor(Irrigacao),
                   Calagem = factor(Calagem))
dados

#' - Construindo os tratamentos
Tratamento <- dados$Calagem:dados$Irrigacao
Tratamento

dados1 <- data.frame(dados,Tratamento)
head(dados1)

#'#  Análise exploratória dos dados
require(ggplot2)

ggplot(dados1, aes(x = Tratamento, y = Peso)) +
  geom_point()

# analisando o grafico eh possivel ver que provavelmente ha interacao significativa 
ggplot(dados1, aes(x = Irrigacao, y = Peso, group = Calagem, color = Calagem)) +
  geom_point(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = mean)+
  xlab("Irrigação") +
  ylab("Peso das plantas")

ggplot(dados1, aes(x = Calagem, y = Peso, group = Irrigacao, color = Irrigacao)) +
  geom_point(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = mean)+
  xlab("Calagem") +
  ylab("Peso das plantas")

# se a interacao for significativa, fazer o desdobramento de um fator dentro do outro (Efeito simples)


#'  ## Ajuste do modelo

modelo <- lm(Peso ~ Tratamento, data = dados1)
anova(modelo)
# rejeita-se H0 ao nivel de 5% de significancia


#' Assumindo que todas as pressuposições tenham sido atendidas, temos o seguinte quadro da ANOVA.

library(ExpDes.pt)
with(dados, fat2.dic(Irrigacao, Calagem, Peso, quali = c(TRUE, TRUE),
                     fac.names = c("Irrigacao", "Calagem")))
