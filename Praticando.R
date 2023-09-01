#Analise dos Dados 
library(tidyverse)

str(aula2)
aula2 <- aula2 %>% 
  mutate(trat = factor(trat))

str(aula2)

hist(aula2$y, freq=FALSE)
lines(density(aula2$y))
curve(dnorm(x, mean = mean(aula2$y),
            sd=sd(aula2$y)), add=TRUE, col=3)

boxplot(aula2$y~aula2$trat)

#Analise ANOVA
modelo.aov <- aov(y ~ trar, 
                  data = aula2)
modelo.aov

anova(modelo.aov)

#Teste Tukey
with(aula2,
     dic(trar,
         y,
         mcomp = "tukey"))
