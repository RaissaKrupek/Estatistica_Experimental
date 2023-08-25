#' ---
#' title: "Exemplo análise (DIC)"
#' author: ""
#' date: "aula 03"
#' ---

#Homogeneidade eh mais importante que a normalidade
#Tratamento deve ser sempre fator
#' *Um estudo foi desenvolvido com quatro genótipos e cinco repetições, seguindo um delineamento inteiramente casualizado, tendo-se observado o peso das espigas de cada parcela (10 m$^2$)*.
#'
#' # Análise dos dados
#'
dados <- data.frame(
  Genotipo = rep(c(1:4), each = 5),
  prod = c(25, 26, 20, 23, 21,
           31, 25, 28, 27, 24,
           22, 26, 28, 25, 29,
           33, 29, 31, 34, 28))
library(tidyverse)

str(dados)

dados <- dados %>% 
  mutate(Genotipo = factor(Genotipo))

str(dados)

x11()
hist(dados$prod, freq=FALSE)
lines(density(dados$prod))
curve(dnorm(x, mean = mean(dados$prod),
            sd=sd(dados$prod)), add=TRUE, col=3)
#esta ultima ajusta a curva
boxplot(dados$prod~dados$Genotipo)
#visualmente o 4 tem maior producao 



#'
#' # ANOVA - Pressuposições
#' ## Análise dos resíduos
#' 
modelo.aov <- aov(prod ~ Genotipo, 
                  data = dados)
modelo.aov

anova(modelo.aov)


#' - Resíduos ordinários ou simples
#' $$\tilde{e}_{ij} = y_{ij} - \hat{\mu}_{i}.$$

residuals(modelo.aov)

residuo1 <- residuals(modelo.aov)
length(residuo1)

hist(residuo1, freq=FALSE)
lines(density(residuo1))
curve(dnorm(x, mean = mean(residuo1),
            sd=sd(residuo1)), add=TRUE, col=2)

#teste de hipotese para residuos de pressuposto de dist normal
shapiro.test(residuo1) #Ho: dist eh normal/ Ha: dist nao eh normal
# w eh o valor da estatistica do teste
#  valor eh maior que 5%, portanto eh normal


#'
#' - Resíduos estudentizados
#' $$d_{ij} = \frac{\tilde{e}_{ij}}{\sqrt{(1-1/J)QMRes}}.$$

(res.stud <- rstandard(modelo.aov))

hist(res.stud, freq=FALSE)
lines(density(res.stud))
curve(dnorm(x, mean = mean(res.stud),
            sd=sd(res.stud)), add=TRUE, col=2)
#nao importa a forma de calcular o residuo, o pressuposto eh que ele tenha normalidade de media 0 e variancia sigma ao quadrado
#espero que o desvio em torno da media nao seja muito grande, portanto o estudentizado eh mais indicado 

#juntando 2 graficos na mesma imagem 
par(mfrow=c(1,2))
hist(dados$prod, freq=FALSE)
lines(density(dados$prod))
curve(dnorm(x, mean = mean(dados$prod),
            sd=sd(dados$prod)), add=TRUE, col=3)
hist(residuo1, freq=FALSE)
lines(density(residuo1))
curve(dnorm(x, mean = mean(residuo1),
            sd=sd(residuo1)), add=TRUE, col=2)

#' 
#' ## Teste de Normalidade dos Erros: Hipóteses
#' 
#' $H_0: \text{os erros seguem uma distribuição normal} \quad \text{\it versus} H_1: \text{os erros não seguem uma distribuição normal}$
#' 
#' ## Teste de Homogeneidade de Variâncias: Hipóteses
#' 
#' $H_0: \text{as variâncias são homogêneas} \quad \text{\it versus} H_1: \text{as variâncias não são homogêneas}$ 
#' 
#' ## ANOVA: Hipóteses
#'  
#' $H_0: \tau_i = 0, \forall i=1,\ldots,4$ \quad {\it versus}
#' $H_1: \tau_i \neq 0, \text{ para algum } i$
#' 
#' ou
#'   
#' $H_0: \mu_1 = \mu_2 = \ldots = \mu_4 = \mu$ \quad {\it versus}
#' $H_1: \text{Pelo menos um contraste de médias difere de zero}$
#' 
#' ## Teste de comparações de pares de médias: Hipósteses
#' 
#'  $H_0: \mu_i = \mu_{i'}, i\neq i' \quad {\it versus } H_1: \mu_i \neq \mu_{i'}, i\neq i'.$
#' 
#' 
#' 
#' ## Usando a biblioteca ExpDes.pt

install.packages("ExpDes.pt", dependencies = TRUE)
library(ExpDes.pt)
#o defaut dele eho teste de Tukay

?dic

#teste t-Student
with(dados,
     dic(Genotipo,
         prod,
         mcomp = "lsd"))
# 2 e 3 nao tem diferenca significativa estatisticamente em media
# se os intervalos de confianca(box-plot) se intersectarem significa que nao ha diferenca significativa
# as letras (grupos) mostram quais se diferem; se ha a mesma letra, nao se diferem
#basta conter o 0 -> em 95% das vezes contem o 0


#teste t com a correcao de Bonferroni
with(dados,
     dic(Genotipo,
         prod,
         mcomp = "lsdb"))


#teste tukay
with(dados,
     dic(Genotipo,
         prod,
         mcomp = "tukey"))


#teste duncan
with(dados,
     dic(Genotipo,
         prod,
         mcomp = "duncan"))


library(multcomp)
levels(dados$Genotipo)
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = rbind("A-B" = c(1, -1, 0, 0),
                                           "C-B" = c(0, -1, 1, 0),
                                           "D-B" = c(0, -1, 0, 1)))))


#' # Contrastes ortogonais
# alpha = 0,05
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("A+B-C-D" = c(1, 1, -1, -1)))))
# alpha = 0,05
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("A-B" = c(1, -1, 0, 0)))))
# alpha = 0,05
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("C-D" = c(0, 0, 1, -1)))))
# conjuntamente alpha' = 1-(1-0,05)^3 = 0,1426 aprox 0,15




#' # Outro exemplo de contrastes ortogonais
#' 
#' Aqui serão testadas as hipóteses
#' 
#' $H_0: 3\mu_A -\mu_B-\mu_C-\mu_D = 0$ *versus* $H_1: 3\mu_A -\mu_B-\mu_C-\mu_D \neq 0$
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("3*A-B-C-D" = c(3, -1, -1, -1)))))

#' $H_0: \mu_B+\mu_C-2\mu_D = 0$ *versus* $H_1: \mu_B+\mu_C-2\mu_D \neq 0$
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("B+C-2*D" = c(0, 1, 1, -2)))))

#' $H_0: \mu_B-\mu_C = 0$ *versus* $H_1: \mu_B-\mu_C \neq 0$
summary(glht(modelo.aov,
             linfct = mcp(Genotipo = 
                            rbind("B-C" = c(0, 1, -1, 0)))))
