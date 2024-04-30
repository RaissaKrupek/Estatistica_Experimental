# Criando um croqui

library(agricolaeplotr)
library(agricolae)
trt<-c(2,2) # factorial 2x2
outdesign <- design.ab(trt, r=3, serie=1,design = 'crd')
plot_design.factorial_crd(outdesign,ncols = 3,nrows = 4)


#Um experimento foi conduzido em casa de vegetação em vasos na Universidade Estadual de Londrina. 
#O trabalho tem o objetivo de avaliar a aplicação de dicloroisocianurato de sódio (DUP) em soja em 
#4 épocas de aplicação em soja inoculada ou não com Rhizobium e sua influência sobre o número de nódulos.
#O experimento foi conduzido em delineamento inteiramente casualizado com cinco repetições.
# Fatorial 4x2

NN=c(339,332,163,230,300,
     163,172,123,083,161,
     171,069,095,046,079,
     335,235,217,174,222,
     284,136,225,098,110,
     082,038,092,053,046,
     196,252,346,468,258,
     032,038,063,048,160)
(Inoculacao=rep(c("IN","NI"),e=20))

(epoca=rep(c("Plantio","V1+15","V3+15","R1+15"),e=5,2))

F1=as.factor(Inoculacao)
F2=as.factor(epoca)
Trat=paste(F1,F2)
dados=data.frame(F1,F2,resp=NN)
X="";Y="Número de nódulos"

str(dados)
hist(NN, freq=FALSE, col= "purple")

# Estatistica Descritiva 
mean(NN)
median(NN)
sd(NN)
var(NN)

library(dplyr)
(Por_Inoc <- dados %>% 
    group_by(F2) %>% 
    summarise(n = length(NN),
              Media = mean(NN),
              Var = var(NN),
              Desvio = sd(NN),
              CV = 100*sd(NN)/mean(NN)))

##Por Inoculação
MediaA = with(dados, tapply(resp, F1, mean))
VarianciaA = with(dados, tapply(resp, F1, var))
DesvioA = with(dados, tapply(resp, F1, sd))
CVA = DesvioA / MediaA * 100
Desc = cbind(MediaA, VarianciaA, DesvioA, CVA)
Desc

##Por época de aplicação
MediaB = with(dados, tapply(resp, F2, mean))
VarianciaB = with(dados, tapply(resp, F2, var))
DesvioB = with(dados, tapply(resp, F2, sd))
CVB = DesvioB / MediaB * 100
Desc = cbind(MediaB, VarianciaB, DesvioB, CVB)
Desc

# Gráficos exploratórios

## Box-Plot - Fator 1 (inoculacao)
boxplot(NN~F1, col=c("maroon","blue"))

## Box-Plot - Fator 2 (epoca de plantio)
boxplot(NN~F2, col=c("maroon","blue"))

## Juntando Fatores
par(bty='l', mai=c(1, 1, .2, .2))
par(cex=0.7)
caixas=with(dados, car::Boxplot(resp ~ F1*F2, vertical=T,las=1, col='Lightyellow',
                                xlab=X, ylab=Y))

## Gráficos de interação
library(ggplot2)
ggplot(dados, aes(x = F1, y = NN, group = F2, color = F2)) +
  geom_point(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = mean)+
  xlab("Inoculacao") +
  ylab("Numero de Nodulos")

ggplot(dados, aes(x = F2, y = NN, group = F1, color = F1)) +
  geom_point(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = mean)+
  xlab("Epoca de Plantio") +
  ylab("Numero de Nodulos")

### Analisando os graficos eh possivel ver uma possivel interacao entre os fatores
### Deve-se entao realizar o desdobramento

# Ajuste do Modelo 

## Construindo os Tratamentos
Tratamento <- dados$F1:dados$F2
Tratamento

dados <- data.frame(dados,Tratamento)
head(dados)

## Anova
modelo <- lm(NN ~ Tratamento, data = dados)
anova(modelo)
### rejeita-se H0 ao nivel de 5% de significancia, portanto ha efeito de tratamentos

library(ExpDes.pt)
with(dados, fat2.dic(F1, F2, NN, quali = c(TRUE, TRUE),
                     fac.names = c("Numero de Nodulos", "Epoca de plantio")))

# Pressuposicoes
## Normalidade dos Residuos
residuals(modelo)

residuo <- residuals(modelo)
length(residuo)

hist(residuo, freq=FALSE)
lines(density(residuo))
curve(dnorm(x, mean = mean(residuo),
            sd=sd(residuo)), add=TRUE, col=2)

shapiro.test(residuo) 

install.packages("hnp")
library(hnp)
hnp(modelo, print.on = TRUE)

## Homogeneidade de variancias para os fatores F1 e F2

bartlett.test(residuo, F1)
bartlett.test(residuo, F2)

ggplot(dados, 
       aes(x = F2, 
           y = residuo)) +
  geom_point() +
  geom_hline(yintercept=0, color = "maroon") +
  xlab("porta enxertos") +
  ylab("resíduos studentizados")

## Independência dos erros
lmtest::dwtest(modelo)


library(ggplot)
plot(residuo, las=1, pch=19, col='maroon', ylab='Resíduos brutos')
     abline(h=0)
     
# Teste de Comparacoes
fat2.dic(F1,F2,NN,
       quali = c(TRUE, TRUE),
       mcomp = "tukey",
       fac.names = c("Numero de Nodulos", "Epoca de plantio"),
       sigT = 0.05,
       sigF = 0.05)



-------------------------------------------------------------------------
library(agricolae)
trt<-c(2,2) # factorial 2x2
outdesign <-design.ab(trt, r=3, serie=1)
book<-outdesign$book
head(book,12) 

# factorial 2 x 2 x 2 with 5 replications in completely randomized design.
trt<-c(2,2,2)
outdesign<-design.ab(trt, r=5, serie=1,design="crd")
book<-outdesign$book
print(book)


dat<-gen.factorial(3,3)
dat<-gen.factorial(c(3,2,3))
dat<-gen.factorial(3,3,factors="all")
dat<-gen.factorial(3,3,varNames=c("A","B","C"))


