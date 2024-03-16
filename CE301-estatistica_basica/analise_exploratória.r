#----------------------------------------------------------------------
# CE301 - Estatística Básica
# PRÁTICAS EM R
#----------------------------------------------------------------------
# Prof. Me Lineu Alberto Cavazani de Freitas
#-----------------------------------------------------------------------
# Os comandos a seguir mostram diversas técnicas vistas em aula para
# análise exploratória univariadas de variáveis qualitativas e 
# quantitativas por meio de tabelas e gráficos.
#
# Execute os comandos, discuta o que eles fazem, comente o código e 
# busque maneiras de customizar os gráficos e tabelas
#-----------------------------------------------------------------------
rm(list = ls())
#-----------------------------------------------------------------------

dados <- read.csv("https://raw.githubusercontent.com/fernandomayer/data/master/milsa.csv")

names(dados) <- c("funcionario", "estado_civil", 
                  "instrucao", "filhos", "salario", 
                  "anos", "meses", "regiao")

head(dados)
names(dados)

#-----------------------------------------------------------------------

dados$estado_civil

table(dados$estado_civil)
tabela1 <- table(dados$estado_civil)
tabela1
sum(tabela1)

#-----------------------------------------------------------------------

prop.table(tabela1)
tabela2 <- prop.table(tabela1)
tabela2
sum(tabela2)

#-----------------------------------------------------------------------

tabela2*100
tabela3 <- tabela2*100
tabela3
sum(tabela3)

#-----------------------------------------------------------------------

plot(tabela1)
plot(tabela2)
plot(tabela3)

#-----------------------------------------------------------------------

barplot(tabela1)
barplot(tabela2)
barplot(tabela3)

#-----------------------------------------------------------------------

barplot(tabela1, horiz=T)
barplot(tabela2, horiz=T)
barplot(tabela3, horiz=T)

#-----------------------------------------------------------------------

pie(tabela1)
pie(tabela2)
pie(tabela3)

#-----------------------------------------------------------------------
tabela4 <- table(dados$estado_civil, rep(1,36))
barplot(tabela4)
barplot(tabela4, horiz = T)

#-----------------------------------------------------------------------

tabela5 <- prop.table(tabela4)
barplot(tabela4)
barplot(tabela4, horiz = T)

#-----------------------------------------------------------------------

tabela6 = data.frame(estado_civil = names(tabela1),
                     freq = as.vector(tabela1),
                     freq_r = as.vector(tabela2))

tabela6
tabela6[3,1] <- "TOTAL"
tabela6[3,2] <- sum(tabela6$freq, na.rm = T)
tabela6[3,3] <- sum(tabela6$freq_r, na.rm = T)
tabela6

names(tabela6)
names(tabela6) <- c("Estado civil",
                    "Freq. absoluta",
                    "Freq. Relativa")
tabela6

#-----------------------------------------------------------------------

dados$salario

breaks <- seq(4,24,2)

classes <- cut(dados$salario, 
               breaks = breaks, 
               include.lowest = TRUE, 
               right = FALSE)

table(classes)
tabela7 <- table(classes)
tabela7
sum(tabela7)

#-----------------------------------------------------------------------

prop.table(tabela7)
tabela8 <- prop.table(tabela7)
tabela8
sum(tabela8)

#-----------------------------------------------------------------------

tabela8*100
tabela9 <- tabela8*100
tabela9
sum(tabela9)

#-----------------------------------------------------------------------

tabela7

barplot(tabela7, space = 0)
hist(dados$salario)
hist(dados$salario, probability = T)

densidade <- density(dados$salario)
plot(densidade)


boxplot(dados$salario, horizontal = T)

acumuladas <- ecdf(dados$salario)
plot(acumuladas)

#-----------------------------------------------------------------------

tabela10 = data.frame(faixas= names(tabela7),
                     freq = as.vector(tabela7),
                     freq_r = as.vector(tabela8))

tabela10
tabela10[11,1] <- "TOTAL"
tabela10[11,2] <- sum(tabela10$freq, na.rm = T)
tabela10[11,3] <- sum(tabela10$freq_r, na.rm = T)
tabela10

names(tabela10)
names(tabela10) <- c("Faixas",
                    "Freq. absoluta",
                    "Freq. Relativa")
tabela10

