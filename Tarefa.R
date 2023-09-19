#' ---
#' title: "Exercício a ser entregue"
#' author: 
#'   - aluno(a) 1
#'   - aluno(a) 2
#'   - aluno(a) 3
#' date: "09/2022"
#' ---


#' Em um experimento de adubação em eucalipto (*Eucalyptus grandis*) 
#' conduzido em casa de vegetação, foram usadas 4 doses de K (0, 30, 60 e 90 ppm), 
#' obtendo-se as alturas, em cm, apresentadas no conjunto de dados a seguir. 
#' Considerando-se que o experimento foi conduzido segundo o delineamento inteiramente 
#' casualizado com 3 repetições, pede-se:
#' 
#' 
#' (a) Faça a análise descrita dos dados apresentando comentários
#' 
#' (b) Encontre o modelo de regressão polinomial que melhor representa a altura 
#' das plantas em função da dose de K aplicada. Justifique sua escolha apresentado 
#' as hipóteses de interesse e conclusões assumindo o nível de 5\% de significância. 
#' Apresente as estimativas dos parâmetros para tal modelo.
#' 
#' (c) Há uma dose que leva à máxima altura? Se sim, qual seria?

dados <- data.frame(
  dose = rep(seq(0, 90, 30), each = 3),
  altura = c( 80, 86, 71,
              144,151, 97,
              151,127,117,
              70, 85, 92)
)
dados

head(dados)
str(dados)

(media <- with(dados, 
               tapply(altura, dose, mean)))
#dose 30 e 60 nos retorman o maior peso

# 1. Ajuste do modelo (dose como variável qualitativa)

modelo<- lm(altura ~ as.factor(dose), dados)
anova(modelo)
#Na ANOVA houve rejeicao de H0, portanto ha efeito do tratamento

modelo.RL <- lm(altura ~ poly(dose,1) + as.factor(dose),
                dados)
anova(modelo.RL)


modelo.RQ <- lm(altura ~ poly(dose,1) + 
                  poly(dose,2) + 
                  as.factor(dose),
                dados)
anova(modelo.RQ)


modelo.RC <- lm(altura ~ poly(dose,1) + 
                  poly(dose,2) +
                  poly(dose,3) +
                  as.factor(dose),
                dados)
anova(modelo.RC)
#No modelo cubico nao rejeitamos Ho ao nivel de significancia de 5%
#Fator(dose) deixou de ser significativo

(R3 <- sum(anova(modelo.RC)$`Sum Sq`[1:2])/
    sum(anova(modelo.RC)$`Sum Sq`[1:3]))

#Modelo Ajustado
modelo.aj <-  lm(altura ~ dose + I(dose^2), dados)
coef(modelo.aj)

library(ggpubr)
ggplot(dados,
       aes(x = dose,
           y = altura)) +
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
  xlab("dose de potassio (ppm)") +
  ylab("Altura (cm)")


library(ExpDes.pt)
with(dados,
     dic(trat = dose,
         resp = altura,
         quali = FALSE,
         sigT = 0.05,
         sigF = 0.05))

