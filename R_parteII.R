# Carregando pacotes
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(psych)

# Lendo o arquivo CSV
dados <- read_csv("serie_historica_reduzida.csv")

# Visualizando os dados
head(dados)
str(dados)

#---------------------------------------------------------------------

# Medidas de tendência central
media <- mean(dados$producao_mil_t, na.rm = TRUE)
mediana <- median(dados$producao_mil_t, na.rm = TRUE)
moda <- as.numeric(names(sort(table(dados$producao_mil_t), decreasing = TRUE)[1]))

# Medidas de dispersão
desvio_padrao <- sd(dados$producao_mil_t, na.rm = TRUE)
variancia <- var(dados$producao_mil_t, na.rm = TRUE)
amplitude <- range(dados$producao_mil_t, na.rm = TRUE)
coef_var <- desvio_padrao / media

# Medidas separatrizes
quartis <- quantile(dados$producao_mil_t, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decis <- quantile(dados$producao_mil_t, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
percentis <- quantile(dados$producao_mil_t, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE)

# Exibindo resultados
cat("Média:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Moda:", moda, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")
cat("Coef. de variação:", coef_var, "\n")
cat("Quartis:\n")
print(quartis)

# Gráfico da distribuição
ggplot(dados, aes(x = producao_mil_t)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribuição da Produção (mil toneladas)", x = "Produção (mil t)", y = "Frequência") +
  theme_minimal()


-----------------------------------------------------------------------------------
  
  
  # Contagem de produtos
  contagem_produtos <- table(dados$produto)

# Gráfico de barras
ggplot(as.data.frame(contagem_produtos), aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência dos Produtos", x = "Produto", y = "Frequência") +
  theme_minimal() +
  coord_flip()
