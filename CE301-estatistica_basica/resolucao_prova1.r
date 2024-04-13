
# Nome: João Vitor Basso Miranda
# GRR: 20242921

dados <- 
    data.frame(
        turma = c("A", "B", "A", "B", "A", "B", "A", "A", "B", "A"),
        sexo = c("M", "F", "F", "F", "F", "M", "M", "M", "F", "F"),
        altura = c(185, 170, 160, 167, 157, 176, 178, 180, 165, 170),
        peso = c(81, 60, 60, 58, 49, 75, 68, 85, 56, 60)
    )

#1) Quais são os tipos das variáveis coletadas? Classifique-as em qualitativa nominal, qualitativa ordinal,quantitativa discreta ou quantitativa contínua. (0,5 ponto)

#R: turma: Qualitativa Nominal, sexo: Qualitativa Nominal, altura: Quantitativa Contínua, peso: Quantitativa Contínua

#2) Considere que existia um cadastro de alunos alocados em turmas. Em cada turma os alunos foram 
# selecionados por meio de um sorteio em que todos os elementos tinham a mesma probabilidade de fazer
# parte da amostra. A amostra final foi composta pela junção das amostras tomadas em cada turma.
# Qual o nome deste método de amostragem? Este plano de amostragem corresponde a um método
# probabilístico ou não probabilístico? (0,5 ponto)

#R: turma: Amostragem estratificada, método probabilistico 

# 3) Monte uma tabela de frequências para a variável turma. Use frequências absolutas e relativas. Qual
# seria o gráfico mais adequado para representar esta tabela? (0,5 ponto)


tabela_freq <- table(dados$turma)

frequencias_relativas <- prop.table(tabela_freq)


tabela_final <- cbind(Frequencia_Absoluta = tabela_freq, Frequencia_Relativa = frequencias_relativas)

print(tabela_final)

# O gráfico mais adequado seria um gráfico de barras

# 4) Monte uma tabela de frequências para a variável peso. Use faixas de tamanho 10, partindo de 40 até
# 90. Qual é a faixa modal? (0,5 ponto)


faixas <- seq(40, 90, by = 10)


tabela_freq_peso <- cut(dados$peso, breaks = faixas, right = FALSE)
tabela_freq_peso <- table(tabela_freq_peso)


tabela_peso <- data.frame(
    Faixa_de_Peso = names(tabela_freq_peso),
    Frequencia_Absoluta = as.numeric(tabela_freq_peso)
)

print(tabela_peso)

# A faixa modal é [60,70)


# 5) Obtenha média e desvio padrão para as variáveis altura e peso. (1,0 ponto)


media_altura <- mean(dados$altura)
desvio_padrao_altura <- sd(dados$altura)


media_peso <- mean(dados$peso)
desvio_padrao_peso <- sd(dados$peso)


cat("Média da altura:", media_altura, "\n")
cat("Desvio padrão da altura:", desvio_padrao_altura, "\n\n")
cat("Média do peso:", media_peso, "\n")
cat("Desvio padrão do peso:", desvio_padrao_peso, "\n")

# 6) Altura e peso são variáveis em diferentes escalas, qual delas apresenta variabilidade maior? Utilize uma
# medida de comparação adequada. (1,0 ponto)

# Calcular o coeficiente de variação para altura
cv_altura <- (desvio_padrao_altura / media_altura) * 100

# Calcular o coeficiente de variação para peso
cv_peso <- (desvio_padrao_peso / media_peso) * 100

print(cv_altura)
print(cv_peso)

# O peso tem maior variabilidade

# 7) Com base na tabela do item (4), esboce o histograma dos pesos. O que você conclui a respeito da
# simetria? (1,0 ponto)




barplot(tabela_peso$Frequencia_Absoluta,
        names=tabela_peso$Faixa_de_Peso,
        xlab='Faixas', ylab='Frequencia',
        col='#046804',border = "black")

# O gráfico sugere que os dados estão distribuídos de equilibrada em ambos os lados da média, indicando simetria.

# 8) Obtenha as quantidades necessárias e esboce o box-plot das alturas. Coloque nos eixos os valores
# utilizados para o esboço. O que você conclui a respeito da simetria e da presença de valores atípicos?
# (1,0 ponto)

boxplot(dados$altura, 
        xlab = "Altura",
        ylab = "Valores",
        col = '#046804',
        border = "black")

# A distribuição parece simetrica e não possui valores atipicos

# 9) Monte uma tabela de dupla entrada usando frequências absolutas para sexo e turma. O que você
# conclui? (1,0 ponto)


tabela_dupla_entrada <- table(dados$sexo, dados$turma)
print(tabela_dupla_entrada)

# A turma B tem mais mulheres do que homens

# 10) Avalie os gráficos abaixo. O que você conclui? (1,0 ponto)

boxplot(altura ~ sexo, data = dados, 
        xlab = "Sexo", ylab = "Altura")

plot(peso ~ altura, data = dados,
     xlab = 'altura, ylab = peso')

# Os homens são mais altos que as mulheres em média e o que peso e altura tem correlação positiva

# 11) Obtenha uma medida de associação entre turma e sexo. O que você conclui? (1,0 ponto)

resultado_chi2 <- chisq.test(tabela_dupla_entrada)
print(resultado_chi2)

# Não tem associação

# 12) Responda de forma sucinta: (1,0 ponto)

# a)
# Na amostragem estratificada, a população é dividida em grupos homogêneos (estratos) e uma amostra é retirada de cada estrato.
# Na amostragem por conglomerados, a população é dividida em grupos heterogêneos (conglomerados) e alguns desses grupos são selecionados para compor a amostra.

# b)
# É melhor evitar o gráfico de setores devido à distorção na percepção das proporções reais entre as categorias e à dificuldade de comparação entre diferentes setores.

# c)
# Dois conjuntos com mesma média podem ter distribuições diferentes, portanto, a igualdade não pode ser afirmada apenas com base na média.
# d):

# Um ponto atípico é um valor significativamente diferente dos demais no conjunto de dados.

# e) Correlação e causalidade:
# Correlação não implica causalidade.