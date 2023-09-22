#'---
#'title: "Delineamento Casualizado em Blocos"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "09/2022"
#' ---

#' # Planejamento de um experimento 

sample(1:9) 
sample(1:9) 
sample(1:9) 


#' ## Utilizando a biblioteca agricolae
#library(agricolae)
#(trat <- paste("T", 1:9, sep =""))
#Casualizando <- design.rcbd(trt = trat,
#                           r = 3)

#library(agricolaeplotr)
#plot_rcdb(Casualizando,
#          factor_name = "trat")

# (Delineamento <-Casualizando$book)
# Delineamento$plots <- as.factor(rep(1:9, times = 3))
# 
# library(ggplot2)
# ggplot(Delineamento, 
#        aes(x = plots,
#            y = block,
#            fill = trt,
#            label = trt)) +
#   geom_tile(color="black") +
#   geom_text() +
#   xlab("Blocos") +
#   ylab("Parcelas")



#'
#' # Análise do experimento 
#' ## Leitura dos dados

rm(list=ls(all=TRUE)) 
# dados <- read.csv2(file.choose())
dados <- read.csv2("laranja.csv")
str(dados) 
dados <- transform(dados,
                   bloco = factor(bloco),
                   trat = factor(trat))
summary(dados) 


boxplot(dados$prod~dados$bloco)
boxplot(dados$prod~dados$trat)
#Nao espera-se que haja diferenca significativa entre as medias, pois os box-plot estao alinhados

library(ggplot2)

#' ## Análise descritiva
#' 
ggplot(dados,
       aes(x = trat,
           y = prod,
           color = trat)) +
  geom_point() +
  xlab("porta-enxertos") +
  ylab("produção")


#' Destacam-se os porta-enxertos 1 e 6, com menor produção e o porta-enxerto 8 
#' com maior produção. Os porta-enxertos 2 e 3 apresentam uma menor dispersão dos 
#' valores observados.


#' - média geral
with(dados, mean(prod))

#' - média por bloco
with(dados, tapply(prod, bloco, mean))

#' - média por tratamento
with(dados, tapply(prod, trat, mean)) 

#' ## Ajuste do modelo
#' 
#' O modelo ajustado para cada valor observado no $i$-ésimo tratamento e no $j$-ésimo bloco foi
#' $$y_{ij} = \mu + b_j + \tau_i + e_{ij},$$
#' em que $\mu$ é uma constante, $b_j$ é o efeito do $j$-ésimo bloco, $\tau_i$ é o efeito do $i$-ésimo tratamento e $e_{ij}$ é o efeito do acaso. 
#' 
#' 
modelo <- lm(prod ~ bloco + trat, 
             dados,
             contrasts = list("bloco" = contr.sum,
                              "trat" = contr.sum)) 
dummy.coef(modelo)

# #' ## Análise gráfica dos resíduos 
# 
# #' - Resíduos Studentizados
# 
res_stud<- rstandard(modelo) 
# 
# #' ### Verificando a presença de observações atípicas e a distribuição
# #' 
boxplot(res_stud) 
# 
install.packages("hnp")
library(hnp)
hnp(modelo, print.on = TRUE)
# 
# #' Aparentemente, não há observações atípicas e os erros apresentam uma 
# #' distribuição normal.
# 
# #'
# #' ### Homogeneidade de variâncias
# #' 
ggplot(dados, 
        aes(x = trat, 
            y = res_stud)) +
   geom_point() +
   geom_hline(yintercept=0, color = "red") +
   xlab("porta enxertos") +
   ylab("resíduos studentizados")
# 
# 
# 
# #' Os porta-enxertos 2 e 4 apresentam uma menor dispersão dos erros, o porta-enxerto 
# #' 7 apresenta uma maior dispersão. Para os demais a varialibidade dos resíduos 
# #' estudentizados é similar.
# 
ggplot(dados, 
        aes(x = bloco, 
            y = res_stud)) +
   geom_point() +
   geom_hline(yintercept=0, color = "red") +
   xlab("blocos") +
   ylab("resíduos studentizados")
# 
# #'
# #' ### Relação entre média e variância
# #' 
 ggplot(dados, 
        aes(x = fitted(modelo), 
            y = res_stud))+
   geom_point() +
   geom_hline(yintercept=0, color = "red") +
   xlab("valores preditos") +
   ylab("resíduos studentizados")
# 
# #' Aparentemente não há relação entre média e variância. Destaca-se o porta-enxerto 
# #' 8, com maiores valores preditos.
# 
# #'
# #' ## Teste de normalidade de Shapiro-Wilk 
# #' $H_0:$ os erros seguem uma distribuição normal *versus* $H_1:$ os erros não 
# #' seguem uma distribuição normal.
# #' 
# 
 shapiro.test(res_stud) 
# 
# #' Assumindo o nível de 5\% de significância, não há evidências para rejeitarmos 
# #' $H_0$. Logo, não podemos afirmar que os erros não seguem a distribuição normal.
# 
# #'
# #' ## Teste de homogeneidade de variâncias de Breusch-Pagan 
# #' 
# #' $H_0:$ Há homogeneidade de variâncias *versus* $H_1:$ Não há homogeneidade de 
# #' variâncias.
# #' 
 install.packages("lmtest")
 library(lmtest) 
 bptest(modelo) 
# #' Assumindo o nível de 5\% de significância, não há evidências para rejeitarmos 
# #' $H_0$. Logo, não podemos afirmar que os erros não são homogêneos.
# 
# #'
# #' ## Box-Cox 
# #' 
# 
 library(MASS) 
 with(dados, boxcox(prod ~ bloco + trat,
                    lambda = seq(-2, 3, 0.01)))
# #'
# #' Como o valor 1 para $\lambda$ pertence ao intervalo de confiança, não há 
# #' necessidade de tranformarmos os dados.
# #'
 
 

#' ## Análise da variância 
#' $H_0: \mu_1 = \mu_2 = \ldots = \mu_9$ *versus* $H_1:$ pelo menos duas médias diferem entre si. 
#'
anova(modelo) 
#' Assumindo o nível de 5\% de significância, há evidências para rejeitarmos $H_0$.
#'Sendo assim, nem todas as produções médias para os diferentes porta-enxertos são iguais.
#'havendo algum um tratamento significativo
#'  
#'  
#' ## Testes de comparações de médias de Tukey
#' $H_0: \mu_i - \mu_{i'} = 0$ *versus*  $H_1: \mu_i - \mu_{i'} \neq 0$
#'
#' - Usando as funções da biblioteca ExpDes.pt
library(ExpDes.pt) 
with(dados, 
     dbc(trat, 
         bloco, 
         prod, 
         quali = T) )
# podemos comparar o teste de tukey com o box plot de trat da nossa analise descritiva
#'
#' - Usando as funções da biblioteca multcomp (contrastes ortogonais)
#' 
library(multcomp) 

#' $H_0: -\mu_1 + 2\mu_2 + 2\mu_3 - \mu_4 - \mu_5 - \mu_6 -\mu_7 - \mu_8 + 2\mu_9 = 0$ *versus* 
#' 
#' $H_1:-\mu_1 + 2\mu_2 + 2\mu_3 - \mu_4 - \mu_5 - \mu_6 -\mu_7 - \mu_8 + 2\mu_9 \neq 0$
#' 
contr1 <-rbind("p.e. (2+3+9) vs (1+4+5+6+7+8)"
               = c(-1,2,2,-1,-1,-1,-1,-1,2)) 
summary(glht(modelo,linfct=mcp(trat=contr1))) 
#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média das médias dos porta-enxertos 2, 3 e 9 não difere da média das médias dos porta-enxertos 1, 4, 5, 6, 7 e 8.
#'
#' $H_0: 0\mu_1 + 1\mu_2 + 1\mu_3 + 0\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 - 2\mu_9 = 0$ *versus* 
#' 
#' $H_1:0\mu_1 + 1\mu_2 + 1\mu_3 + 0\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 - 2\mu_9 \neq 0$
#'
contr2 <- rbind("p.e. (2+3) vs 9"
                = c(0,1,1,0,0,0,0,0,-2)) 
summary(glht(modelo,linfct=mcp(trat=contr2))) 
#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média das médias dos porta-enxertos 2 e 3 não difere da média do porta-enxerto 9.
#'
#' $H_0: 0\mu_1 + 1\mu_2 - 1\mu_3 + 0\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 = 0$ *versus* 
#' 
#' $H_1: 0\mu_1 + 1\mu_2 - 1\mu_3 + 0\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 \neq 0$
#'
contr3 <- rbind("p.e. 2 vs 3"
                = c(0,1,-1,0,0,0,0,0,0)) 
summary(glht(modelo,linfct=mcp(trat=contr3))) 

#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média do porta-enxerto 2 não difere da média do porta-enxerto 3.
#'
#' $H_0: 1\mu_1 + 0\mu_2 + 0\mu_3 + 1\mu_4 - 1\mu_5 - 1\mu_6 + 1\mu_7 - 1\mu_8 + 0\mu_9 = 0$ *versus* 
#' 
#' $H_1: 1\mu_1 + 0\mu_2 + 0\mu_3 + 1\mu_4 - 1\mu_5 - 1\mu_6 + 1\mu_7 - 1\mu_8 + 0\mu_9 \neq 0$
#'
contr4 <- rbind("p.e. (1+4+7) vs (5+6+8)"
                = c(1,0,0,1,-1,-1,1,-1,0)) 
summary(glht(modelo,linfct=mcp(trat=contr4))) 

#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média das médias dos porta-enxertos 1, 4 e 7 não difere da média das médias dos porta-enxertos 5, 6 e 8.
#'
#' $H_0: 1\mu_1 + 0\mu_2 + 0\mu_3 + 1\mu_4 + 0\mu_5 + 0\mu_6 - 2\mu_7 + 0\mu_8 + 0\mu_9 = 0$ *versus* 
#' 
#' $H_1: 1\mu_1 + 0\mu_2 + 0\mu_3 + 1\mu_4 + 0\mu_5 + 0\mu_6 - 2\mu_7 + 0\mu_8 + 0\mu_9 \neq 0$
#'
contr5 <- rbind("p.e. (1+4) vs 7"
                = c(1,0,0,1,0,0,-2,0,0)) 
summary(glht(modelo,linfct=mcp(trat=contr5))) 

#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média das médias dos porta-enxertos 1 e 4 não difere da média do porta-enxerto 7.
#'
#' $H_0: 1\mu_1 + 0\mu_2 + 0\mu_3 - 1\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 = 0$ *versus* 
#' 
#' $H_1: 1\mu_1 + 0\mu_2 + 0\mu_3 - 1\mu_4 + 0\mu_5 + 0\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 \neq 0$
#'
contr6 <- rbind("p.e. 1 vs 4"
                = c(1,0,0,-1,0,0,0,0,0)) 
summary(glht(modelo,linfct=mcp(trat=contr6))) 

#'
#' Há evidências para rejeitarmos $H_0$. Logo, a média do porta-enxerto 1 difere da média do porta-enxerto 4. Sendo o porta-enxerto 4 o que apresenta produção média superior.
#'
#' $H_0: 0\mu_1 + 0\mu_2 + 0\mu_3 + 0\mu_4 + 1\mu_5 + 1\mu_6 + 0\mu_7 - 2\mu_8 + 0\mu_9 = 0$ *versus* 
#' 
#' $H_1: 0\mu_1 + 0\mu_2 + 0\mu_3 + 0\mu_4 + 1\mu_5 + 1\mu_6 + 0\mu_7 - 2\mu_8 + 0\mu_9 \neq 0$
#'
contr7 <- rbind("p.e. (5+6) vs 8"
                = c(0,0,0,0,1,1,0,-2,0)) 
summary(glht(modelo,linfct=mcp(trat=contr7))) 

#'
#' Há evidências para rejeitarmos $H_0$. Logo, a média das médias dos porta-enxertos 5 e 6 difere da média do porta-enxerto 8. Sendo a produção média do porta-enxerto 8 superior.
#'
#' $H_0: 0\mu_1 + 0\mu_2 + 0\mu_3 + 0\mu_4 + 1\mu_5 - 1\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 = 0$ *versus*  
#' 
#' $H_1: 0\mu_1 + 0\mu_2 + 0\mu_3 + 0\mu_4 + 1\mu_5 - 1\mu_6 + 0\mu_7 + 0\mu_8 + 0\mu_9 \neq 0$
#'
contr8 <- rbind("p.e. 5 vs 6"
                = c(0,0,0,0,1,-1,0,0,0)) 
summary(glht(modelo,linfct=mcp(trat=contr8))) 
#'
#' Não há evidências para rejeitarmos $H_0$. Logo, a média do porta-enxerto 5 não difere da média do porta-enxerto 6.
