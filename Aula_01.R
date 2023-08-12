#criando base de dados 
#variavel 1 altura cana-de-acucar com 2 meses

CANA <- c(0.78, 0.45, 0.50, 0.56, 0.90, 0.30, 0.65, 0.47, 0.66, 0.96)
length(CANA)
CANA[6:8]

#variavel 2 variedade de cana
ESPECIE <- c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b")
length(ESPECIE)

#Calculando media, mediana, desvio-padrao, variancia
mean(CANA)
median(CANA)
sd(CANA)
var(CANA)

#Construindo histograma
hist(CANA, freq = FALSE, col="maroon")
#para dar a resposta em relacao a densidade

#ordenando os dados
sort(CANA)

#construindo box-plot
boxplot(CANA, col="maroon")

#calculo da frequencia absoluta
freq <- table(ESPECIE)

#Construindo grafico de barras
barplot(freq)

#Construindo um box-plot por especie
boxplot(CANA~ESPECIE, col=c("red","blue"))

#Criando conjunto de dados
dados_pratica1 <- data.frame(CANA, ESPECIE)
dados_pratica1

#verificando os tipos de variaveis
str(dados_pratica1)

hist(dados_pratica1$CANA, freq=FALSE)

#Carregando conjunto de dados por busca
dados_pasta <- read.csv2(file.choose())

#leitura direta da pasta projeto
dados_pasta1 <- read.csv2("aula01.csv")
dados_pasta1
str(dados_pasta1)

q()

