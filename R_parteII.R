install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("writexl")
install.packages("psych")


# Carregando pacotes
library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(psych)

# Lendo o arquivo CSV
dados <- read_delim("serie_historica_reduzida.csv", delim = ";", locale = locale(decimal_mark = ","))  # se tiver números com vírgula
# Remove possíveis espaços e substitui vírgulas por pontos, se necessário
dados$producao_mil_t <- gsub(",", ".", dados$producao_mil_t)
dados$producao_mil_t <- as.numeric(dados$producao_mil_t)
dados$produto <- trimws(dados$produto)



# Visualizando os dados
head(dados)
str(dados)

colnames(dados)


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
head(dados$produto)
  
# Contagem de produtos
contagem_produtos <- dados %>%
  group_by(produto) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

  

# Gráfico de barras
# Gráfico de barras da contagem de produtos
ggplot(contagem_produtos, aes(x = reorder(produto, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência dos Produtos", x = "Produto", y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Agregar produção total por ano
#Gráfico de Linha 
dados %>%
  group_by(ano_agricola) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = ano_agricola, y = producao_total, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkblue") +
  labs(title = "Evolução da Produção ao Longo dos Anos",
       x = "Ano Agrícola", y = "Produção Total (mil t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



##  Gráfico de Barras por Estado
dados %>%
  group_by(uf) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(uf, -producao_total), y = producao_total)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Produção Total por Estado", x = "Estado", y = "Produção (mil t)") +
  theme_minimal()

##Gráfico de Barras por Produto

dados %>%
  group_by(produto) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(produto, -producao_total), y = producao_total)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Produção Total por Produto", x = "Produto", y = "Produção (mil t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Boxplot da Produção por Produto
ggplot(dados, aes(x = produto, y = producao_mil_t)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Distribuição da Produção por Produto", x = "Produto", y = "Produção (mil t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Heatmap: Produção por Ano e Estado
library(ggplot2)

dados %>%
  group_by(ano_agricola, uf) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = ano_agricola, y = uf, fill = producao_total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  labs(title = "Mapa de Calor da Produção por Ano e Estado", x = "Ano Agrícola", y = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
