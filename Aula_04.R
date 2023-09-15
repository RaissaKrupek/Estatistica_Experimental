#Antes, na anova - analise e comparacao das medias, tratamentos qualitativos
#Porem o fator tbm eh quantitativo, e com isso ajustamos um modelo
#Interesse da regressao: achar um ponto otimo e predizer pontos a partir dos eixos
#Y ~ f(x) + E
#A regressao eh feita na media, entao a predicao eh em relacao a media 
#Interpolacao eh passar por cima de todos os pontos - conforme aumento o grau do polinomio, aumenta o grau de interpolacao 
#Vamos partir de um modelo considerando os tratamentos de forma qualitativo(fator) - teste geral da anova para ver se ha diferenca significativa entre os tratamentos,
#   e quando for para regressao polinomial sera quantitativo
#Na regressao nao estou interessada em comparar media, e sim ajustar uma curva
#Queremos sempre um modelo que tenha algum erro 



#' ---
#' title: "Exemplo ANOVA e Estudo de Regressão"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "09/2022"
#' ---

library(tidyverse)
#' # Entrada dos dados para análise

rm(list=ls(all=TRUE))
dados <- data.frame(dose = rep(seq(0,300,50),each=4),
                    peso = c(134.8,139.7,147.6,132.3,
                             161.7,157.7,150.3,144.7,
                             160.7,172.7,163.4,161.3,
                             169.8,168.2,160.7,161.0,
                             165.7,160.0,158.2,151.0,
                             171.8,157.3,150.4,160.4,
                             154.5,160.4,148.8,154.0))
head(dados)
str(dados)

(media <- with(dados, 
               tapply(peso, dose, mean)))
#dose 100 e 150 nos retorman o maior peso
#' # Gráfico de dispersão

library(ggplot2)
ggplot(dados,
       aes(x = dose,
           y = peso)) +
  geom_point() +
  geom_point(stat = "summary",
             fun = mean,
             color = 2) +
  xlab("dose de gesso (kg/ha)") +
  ylab("peso de 1000 sementes de feijão (g)") +
  theme_classic()

#' # 1. Ajuste do modelo (dose como variável qualitativa)
#' 
#' $H_0: \mu_{0} = \mu_{50} = \ldots = \mu_{300} = \mu$ versus $H_1:$ pelo menos um contraste de médias difere de zero.

modelo<- lm(peso ~ as.factor(dose), dados)
anova(modelo)

#' Como há evidências de um efeito significativo de doses, 
#' vamos buscar qual modelo polinomial melhor representa o 
#' peso de 1000 sementes de feijão (g) em função das 
#' doses de gesso (Kg/ha)).
#'
#'
#'
#' # 2. ANOVA e regressão polinomial
#'  
#' ## Ajuste de uma reta
#'  
#'  - Desvio de regressão linear ou falta de ajuste (as.factor(dose))
#'  
#'  $H_0: \beta_2 = \beta_3 = \beta_4 = \beta_5 = \beta_6 = 0 \mid \beta_0, \beta_1$ versus $H_1: \beta_i \neq 0 \mid \beta_0, \beta_1$ par algum $i=2,\ldots,6$.
#'  
modelo.RL <- lm(peso ~ poly(dose,1) + as.factor(dose),
                dados)
anova(modelo.RL)

#' Como há evidências para rejeitarmos a hipótese nula (Pr(>F) = 0,0004595 para as.factor(dose) < 0.05) ao nível de 5\% de significância, ajustaremos o modelo quadrático.
#' 
#' ## Ajuste de uma parábola
#'  
#'  - Desvio de regressão quadrática ou falta de ajuste (as.factor(dose))
#'  
#'  $H_0: \beta_3 = \beta_4 = \beta_5 = \beta_6 = 0 \mid \beta_0, \beta_1, \beta_2$ versus $H_1: \beta_i \neq 0 \mid \beta_0, \beta_1, \beta_2$ par algum $i=3,\ldots,6$.
#'  
modelo.RQ <- lm(peso ~ poly(dose,1) + 
                  poly(dose,2) + 
                  as.factor(dose),
                dados)
anova(modelo.RQ)

# O fator(dose) deixou de ser significativo: na zona de aceitacao
#' Como não há evidências para rejeitarmos a hipótese nula (Pr(>F) = 0,275047 para as.factor(dose)) ao nível de 5\% de significância, avaliaremos o teste de hipóteses para $\beta_2 \mid \beta_0, \beta_1$.
#' 
#' - Adição do termo quadrático ao modelo
#' 
#' $H_0: \beta_2 = 0 \mid \beta_0, \beta_1$ versus $H_1: \beta_2 \neq 0 \mid \beta_0, \beta_1$

#' Há evidências para rejeitarmos $H_0$, pois Pr(>F) para poly(dose,2) = 1,777e-05 < 0,05, ou seja, há evidências para afirmarmos que o modelo quadrático é adequado para representar o comportamento do peso de 1000 sementes de feijão (g) em função das doses de gesso (kg/ha).
#'
#'
#' - Coeficiente de determinação

(R2 <- sum(anova(modelo.RQ)$`Sum Sq`[1:2])/
    sum(anova(modelo.RQ)$`Sum Sq`[1:3]))

#'
#' # Modelo ajustado.
#' 

modelo.aj <-  lm(peso ~ dose + I(dose^2), dados)
coef(modelo.aj)
#Compilacao do modelo final, a funcao coef nos da os coeficientes da funcao 

#' # Gráfico da curva ajustada
library(ggpubr)
ggplot(dados,
       aes(x = dose,
           y = peso)) +
  geom_point() +
  geom_point(stat = "summary",
             fun = mean,
             color = 2) +
  geom_smooth(method = lm, 
              formula = y ~ poly(x, 2), 
              se = TRUE) +
  stat_regline_equation(
    aes(label = ..eq.label..),
    formula = y ~ poly(x, 2, raw = TRUE),
    label.x = 170,
    label.y = 190) +
  ylim(120,190) +
  theme_classic() +
  # colocar o R^2.
  annotate(geom = "text",
           x = 182,
           y = 185,
           label = expression(R^2 == 0.88)) +
  xlab("dose de gesso (kg/ha)") +
  ylab("peso de 1000 sementes de feijão (g)")

# O false no "se =" tira os intervalos de confianca e o true coloca

#'
#' # Usando a biblioteca ExpDes.pt
#'  

library(ExpDes.pt)
with(dados,
     dic(trat = dose,
         resp = peso,
         quali = FALSE,
         sigT = 0.05,
         sigF = 0.05))

# O teste de homogeneidade eh o teste de normalidade
