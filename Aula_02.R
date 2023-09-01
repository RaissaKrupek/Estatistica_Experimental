#Sorteando valores aleatórios
(vec1 <- c(1,2,3,4,5,6,7,8,9))
vec2 <- c("A","A","A","B","B","B","C","C","C")
vec3 <- c("João","Fabio","Pedro","Lucas","Gabriel","Bruno","Camila", "Roberta","Larissa")

sample(vec1)

sample(vec2)

sorteio_nome <- sample(vec3,1)
sorteio_nome 
#vetor, sorteio de apenas 1
#default do sample eh sempre sem repeticao, para repetir: replace = TRUE
#default eh o padrao de fabrica

cbind(vec1,vec3)
#associacao de 2 vetores; precisam ser do mesmo tamanho



#' ---
#' title: "Exemplo análise (DIC)"
#' author: ""
#' date: "aula 08/2023"
#' ---


#' *Um estudo foi desenvolvido com cinco genótipos e quatro repetições,*
#' *seguindo um delineamento inteiramente casualizado, tendo-se observado*
#' *o peso das espigas de cada parcela (10 m$^2$)*.

#' # Planejamento do experimento
#' 
#' ## Usando a biblioteca agricolae

# Instalando
# install.packages("agricolae", 
#                  dependencies = TRUE) para que instale tbm todos os pacotes que tiverem dependencia 

# Habilitando as funções
library(agricolae)

trt <- LETTERS[1:4]
trt
#funcao LETTERS permite construir as letras do alfabelo em letras maiusculas,e letters em letras minusculas
#os termos referen-se ao intervalo das letras do alfabeto
delineamento <- design.crd(trt,
                           r = 5,
                           serie = 0)
delineamento
#esta funcao eh do pacote agricolae
#r eh o numero da repeticao  
#serie enumera as parcelas (quando coloco 1 ele coloca uma dezena a mais, eh apenas a forma de enumerar)
#plot eh a parcela



# Graficamente
# install.packages("agricolaeplotr", 
#                  dependencies = TRUE)
library(agricolaeplotr)
plot_design_crd(delineamento,
                ncols = 4,
                nrows = 5)



#' # Análise dos dados

dados <- data.frame(
  Genotipo = rep(c(1:4), each = 5),
  prod = c(25, 26, 20, 23, 21,
           31, 25, 28, 27, 24,
           22, 26, 28, 25, 29,
           33, 29, 31, 34, 28))
dados
#quero 4 tipos de genotipos e que cada um tenha 5 repeticoes 



#install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)


#' #Verificando a estrutura dos dados
str(dados)
glimpse(dados)
#da um tratamento diferente para a variavel continua(double)
dados <- dados %>% 
  mutate(Genotipo = factor(Genotipo))
#%>% significa atribuir - neste caso transformamos o genotipo em fator (variavel qualitativa) atribuindo genotipo a fator
str(dados)



#' # Estatísticas descritivas
# #' ## Geral

(Resumo <- dados %>% 
    summarise(n = length(prod),
              Media = mean(prod),
              Var = var(prod),
              Desvio = sd(prod),
              CV = 100*sd(prod)/mean(prod)))

#' ## Agrupando os dados por nível de Genótipo, pedindo as mesmas medidas descritivas

(Resumo_Gen <- dados %>% 
    group_by(Genotipo) %>% 
    summarise(n = length(prod),
              Media = mean(prod),
              Var = var(prod),
              Desvio = sd(prod),
              CV = 100*sd(prod)/mean(prod)))

#' ## Graficamente
#' 
library(ggplot2)
ggplot(dados, 
       aes(x = Genotipo, 
           y = prod)) +
  geom_point() +
  geom_point(stat = "summary",
             fun = mean,
             color = "maroon") +
  geom_point(aes(x = Genotipo,
                 y = mean(prod)),
             color = "blue")


ggplot(dados, 
       aes(x = Genotipo, 
           y = prod)) +
  geom_boxplot() +
  geom_point(stat = "summary",
             fun = mean,
             color = "red")+
  theme_bw()


ggplot(dados, 
       aes(x = prod)) +
  geom_histogram(bins = 5)+
  theme_bw()



#' #ANOVA
#' 
#' ## Hipóteses
#'  
#' $H_0: \tau_i = 0, \forall i=1,\ldots,5$ \quad {\it versus}
#' $H_1: \tau_i \neq 0, \text{ para algum } i$
#' 
#' ou
#'   
#' $H_0: \mu_1 = \mu_2 = \ldots = \mu_5 = \mu$ \quad {\it versus}
#' $H_1: \text{Pelo menos um contraste de médias difere de zero}$
#' 
#' ## Usando a função aov()
modelo.aov <- aov(prod ~ Genotipo, 
                  data = dados)

anova(modelo.aov)
#tabela de analise de variancia
# Y(minha resposta) ~ Tratamento
#quando eh uma veriavel numerica ele vai retornar 1 grau de liberdade



#' ## F tabelado
qf(0.95, 3, 16)
#grau de liberdade 3 e 16


#' ## Nível descritivo
1-pf(7.798, 3, 16)
pf(7.798, 3, 16,lower.tail = FALSE)
#a funcao pf calcula a probab na calda superior, por isso 1 - pf (encontrando a acumulada)
#coloca ali primeiro o valor F


#para ser significativo Pvalor calculado deve ser menor que 5%
