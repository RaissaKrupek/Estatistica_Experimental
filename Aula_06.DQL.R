#'---
#'title: "Delineamento Quadrado Latino"
#' author: 
#'   - Clarice Garcia Borges Demétrio
#'   - Cristian Villegas
#'   - Renata Alcarde Sermarini
#'   - Taciana Villela Savian
#'date: ""
#'---

#' # Planejamento
#' 
#' A seguir são apresentados códigos para a realização do planejamento de um experimento Quadrado Latino 5$\times$ 5.
#'
#'#linha e colona sao variaveis quantitativas, mas para a analise ANOVA devem ser tambem fator 
library(agricolae)
(Variedades <- LETTERS[1:5])
(Planejamento <- design.lsd(Variedades,
                            serie=2))
Plano <- Planejamento$book

library(agricolaeplotr)
plot_latin_square(Planejamento,
                  factor_name = "Variedades")

# library(ggplot2)
# ggplot(Plano, aes(x = col, y = row, label = Variedades)) +
#   geom_tile(aes(fill = Variedades), color="black") +
#   geom_text() +
#   xlab("Colunas") +
#   ylab("Linhas")


#' Ainda, a partir de um Quadrado Latino sistemático, pode-se sortear as linhas
sample(1:5) 
#' e as Colunas
sample(1:5) 


#' # Análise dos dados
#' Considere os dados de um experimento instalado de acordo com o
#' delineamento quadrado latino, para avaliar a produção de cana-de-açúcar
#' em kg/parcela, de cinco variedades.
#' 
#' ## Importando os dados
#(dados <- read.csv2("cana1.csv"))
#str(dados)
#dados <- transform(dados,
#                   linha = factor(linha),
#                   coluna = factor(coluna))
#summary(dados) 


str(cana)

dados <- transform(cana,
             linha = factor(linha),
            coluna = factor(coluna))

#se nao converter para fator as linhas e colunas, ao inves de aparecer 4 gl, aparecera 
#1 gl, por ele trataria como uma numerica continua 

str(dados)
summary(dados) 


#' ## Descritiva
#'
library(ggplot2)
ggplot(dados, aes(x = coluna, 
                  y = linha, 
                  label = trat)) +
  geom_tile(aes(fill = prod), 
            color="black") +
  scale_fill_gradient(low="white",
                      high="maroon") +
  geom_text() +
  xlab("Colunas") +
  ylab("Linhas")

# observando a analise, espero que o trat c se diferenciem dos demais
ggplot(dados, 
       aes(x = trat, 
           y = prod)) +
  geom_point()

#' ## Ajuste do modelo DQL
#' 
modelo <- aov(prod ~ linha + coluna + trat,
              dados) 

modelo

#' ## Verificando as pressuposições 

#' Resíduos Studentizados 
(res_stud <- rstandard(modelo))
ggplot(NULL, 
       aes(x=res_stud)) +
  geom_boxplot() +
  labs(y = "Resíduos Studentizados")
#observamos simetrta dos residuos, media 0 (assim como no pressuposto)
#Residuo = Y^ - Y
#Os valores do console sao os valores dos residuos 


#' - Normalidade
#'
# qqnorm(res_stud,
#        xlab="Quantis da distribuição normal", 
#        ylab="Resíduos Studentizados"); 
# qqline(res_stud, col=2)
library(hnp)
hnp(modelo,
    print.on = TRUE)
#dados dentro do envelope significa que o modelo (DQL) representa bem

#' $H_0$: Os erros seguem uma distribuição normal *versus* 
#' $H_1:$ Os erros não seguem uma distribuição normal.
shapiro.test(res_stud)

#' Ao nível de 5\% de significância não há evidências para afirmarmos que os erros não seguem a distribuição normal.

#' - Homogeneidade de variâncias 
ggplot(dados, 
       aes(x = trat, 
           y = res_stud)) +
  geom_point() +
  xlab("Tratamentos") +
  ylab("Resíduos Studentizados")
ggplot(dados, 
       aes(x = coluna, 
           y = res_stud)) +
  geom_point() +
  xlab("Colunas") +
  ylab("Resíduos Studentizados")
ggplot(dados, 
       aes(x = linha, 
           y = res_stud)) +
  geom_point() +
  xlab("Linhas") +
  ylab("Resíduos Studentizados")
#residuos por tratamento estao bem dispersos de forma aleatoria 
#graf 2: para coluna, que tbm nao tem nenhuma tendencia 
#graf 3: para linha, que tbm nao tem nenhuma tendencia 

#' $H_0$: Há homogeneidade de variâncias *versus* 
#' $H_1:$ Não há homogeneidade de variâncias.

#Teste para homogeneidade de variancia 
library(lmtest)
bptest(modelo)
#' Ao nível de 5\% de significância não há evidências para afirmarmos que não há homogeneidade de variâncias.

#' - Relação entre média e variância

ggplot(NULL, 
       aes(x = fitted(modelo), 
           y = res_stud)) +
  geom_point() +
  xlab("Valores preditos")  +
  ylab("Resíduos Studentizados") +
  geom_hline(yintercept = 0, 
             col = "red")

library(MASS)
boxcox(modelo)
boxcox(modelo,
       lambda = seq(0, 3, 0.01))

#' Como $\lambda = 1 pertence ao intervalo de confiança e as 
#' pressuposições de normalidade dos erros e homogeneidade de 
#' variâncias foram atendidas, não há necessidade de 
#' transformação dos dados.
#' #quando tem o 1 nao preciso fazer transformacao 

#' ## ANOVA
#' $H_0$: $\mu_1 = \mu_2 = \mu_3 = \mu_4 = \mu_5$ *versus* 
#' $H_1:$ Pelo menos duas médias de tratamentos diferem entre si.
anova(modelo) 

#' Assumindo-se o nível de 5\% de significância, há evidências para rejeitarmos $H_0$...


#' ## ANOVA e comparações múltiplas pela biblioteca ExpDes.pt
install.packages("ExpDes.pt")
library(ExpDes.pt) 
with(dados, 
     dql(trat, 
         linha, 
         coluna, 
         prod, 
         quali = TRUE, 
         mcomp = "tukey", 
         sigT = 0.05, 
         sigF = 0.05))
