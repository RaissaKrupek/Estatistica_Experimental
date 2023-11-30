## AULA PARCELAS SUBDIVIDIDAS

sulcos <- c(rep("simples", 20), rep("duplo", 20))
cortes <- c(rep("planta", 10), rep("soca", 10), rep("planta", 10), rep("soca", 10))
rep <- rep(1:10, 4)
cbind(sulcos,cortes)

prod <- c(92.9, 128.6, 121.7, 122.8, 118.1, 115.7, 121.4, 126.9, 118.1, 122.4, 84.5, 86.7,
           84.5, 77.0, 88.1, 82.4, 84.0, 88.8, 85.7, 78.8, 122.5, 110.0, 115.0, 125.0, 105.0,
           110.0, 115.0, 105.0, 108.5, 118.3, 84.5, 85.0, 85.5, 88.0, 86.7, 80.7, 88.3, 89.3, 94.3, 90.0)
cbind(sulcos,cortes, prod, rep)

#' ---
#' title: "Esquema de tratamentos em parcelas subdivididas"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#' date: "Novembro de 2022"
#' header-includes: \usepackage{here}
#' ---


#' # Croqui
#' 
#' Um possível croqui para um experimento com $r$ repetições e 
#' esquema de tratamentos em parcelas subdivididas, sendo o fator A casualizado 
#' às parcelas com $a$ níveis e
#' o fator B casualizado às subparcelas com $b$ níveis

#' 
#' ## DIC
#' 
library(agricolae)
a = 4
b = 4
r = 4
(FatorA = c(paste("A", 1:a, sep = "")))
(FatorB = c(paste("B", 1:b, sep = "")))
(Plano.dic <- design.split(FatorA,
                           FatorB,
                           r = r,
                           design = "crd",
                           serie = 0))

#'
#' - Croqui
#'
library(ggplot2)
ggplot(Plano.dic$book, 
       aes(x = 1, 
           y = splots, 
           label = FatorB, 
           fill = FatorA)) +
  geom_tile(color="black") +
  geom_text() +
  facet_wrap(~ plots,
             ncol = 4,
             nrow = 4) +
  xlab("parcelas") +
  ylab("subparcelas") +
  theme_minimal() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")

#' 
#' ## DBC
#' 
library(agricolae)
a = 4
b = 4
r = 4
(FatorA = c(paste("A", 1:a, sep = "")))
(FatorB = c(paste("B", 1:b, sep = "")))
(Plano.dbc <- design.split(FatorA,
                           FatorB,
                           r = r,
                           design = "rcbd",
                           serie = 0))

Plano.dbc$book
Plano.dbc$book$plots <- as.factor(
  rep(1:a, each = b, times = r))

#'
#' - Croqui
#'

Plano.dbc$book
Plano.dbc$book$block <- paste("Bloco", 
                              Plano.dbc$book$block)

library(ggplot2)
ggplot(Plano.dbc$book, 
       aes(x = plots, 
           y = splots, 
           label = FatorB, 
           fill = FatorA)) +
  geom_tile(color="black") +
  geom_text() +
  facet_grid(~ block) +
  theme_minimal() +
  xlab("parcelas") +
  ylab("subparcelas") +
  theme(legend.position = "bottom")


#' 
#' ## DQL (lembrando que r = a*b)
#' 
library(agricolae)
a = 4
b = 4
(FatorA = c(paste("A", 1:a, sep = "")))
(FatorB = c(paste("B", 1:b, sep = "")))
(Plano.dql <- design.split(FatorA,
                           FatorB,
                           design = "lsd",
                           serie = 0))


#'
#' - Croqui
#'
Plano.dql$book
Plano.dql$book$row <- paste("Linha",
                            Plano.dql$book$row)
Plano.dql$book$col <- paste("Coluna",
                            Plano.dql$book$col)

library(ggplot2)
ggplot(Plano.dql$book, 
       aes(x = 1,
           y = splots, 
           label = FatorB, 
           fill = FatorA)) +
  geom_tile(color="black") +
  geom_text() +
  facet_grid(row ~ col) +
  theme_minimal() +
  ylab("subparcelas") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom")


#' #Análise dos dados (exemplo)

#' Banzatto e Kronka (1992), apresentaram o ensaio citado por 
#' Steel e Torrie (1980), no qual são comparadas 4 variedades 
#' de aveia (A1 - Vicland 1 infectada com o fungo *Helminthosporium victoriae*, 
#' A2 - Vicland 2 não infectada, A3 - Clinton 
#' resistente a *H. victoriae* e A4 - Branch resistente a *H. victoriae*) e 
#' 4 tratamentos de sementes (B1 - Testemunha, B2 - Ceresan M, B3 - 
#' Panogen e B4 - Agros) quanto aos efeitos sobre a produção. 
#' As variedades foram distribuídas aleatoriamente nas parcelas 
#' de cada um dos quatro blocos do ensaio. Os tratamentos de sementes 
#' foram aleatoriamente distribuídos nas quatro subparcelas de cada parcela.

#'
#' ## Entrada dos dados
#' 
dados <- read.csv2(file.choose())
summary(dados)


#'
#' ## Tabelas e Gráficos
#' 

with(dados,
     tapply(prod, Variedade, mean))
ggplot(dados, aes(x = Variedade, 
                  y = prod, 
                  group = 1)) +
  geom_point(stat = 'summary', 
             fun = mean,
             color = "red")+
  geom_line(stat = 'summary', 
            fun = mean,
            color = "red")+
  xlab("variedade de aveia") +
  ylab("produção") +
  theme_bw() +
  ylim(25,80)

with(dados,
     tapply(prod, Semente, mean))
ggplot(dados, aes(x = Semente, 
                  y = prod, 
                  group = 1)) +
  geom_point(stat = 'summary', 
             fun = mean,
             color = "red")+
  geom_line(stat = 'summary', 
            fun = mean,
            color = "red")+
  xlab("tratamento de sementes") +
  ylab("produção") +
  theme_bw() +
  ylim(25,80)

with(dados,
     tapply(prod, list(Variedade, Semente), mean))
ggplot(dados, aes(x = Variedade, 
                  y = prod, 
                  group = Semente, 
                  color = Semente)) +
  geom_point(stat = 'summary', 
             fun = mean)+
  geom_line(stat = 'summary', 
            fun = mean)+
  xlab("variedade de aveia") +
  ylab("produção") +
  theme_bw() +
  ylim(25,80)

ggplot(dados, aes(x = Semente, 
                  y = prod, 
                  group = Variedade, 
                  color = Variedade)) +
  geom_point(stat = 'summary', 
             fun = mean)+
  geom_line(stat = 'summary', 
            fun = mean)+
  xlab("tratamento de sementes") +
  ylab("produção") +
  theme_bw() +
  ylim(25,80)


#'
#' ## Ajuste do modelo
#'  

modelo <- aov(prod ~ Bloco + 
                Variedade*Semente + 
                Error(Bloco:Variedade/Semente), 
              data = dados)
summary(modelo)

library(ExpDes.pt)
with(dados,
     psub2.dbc(Variedade,
               Semente,
               Bloco,
               prod,
               quali = c(TRUE, TRUE),
               fac.names = c("Variedade", "Semente")))

