# RenanMendes_RM563145_fase2_cap7
# ThiagoSantos_RM5633275_fase2_cap7
# ArthurRosado_RM562061_fase2_cap7
# OtavioCustodio_RM5656065_fase2_cap7
# Leandro_RM565240_fase2_cap7


# Instalando os pacotes.
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readxl")
install.packages("writexl")
install.packages("psych")

library(readr)
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(psych)

# Ler arquivo CSV - serie_historica_reduzida.
dados <- read_delim("serie_historica_reduzida.csv", delim = ";", locale = locale(decimal_mark = ","))

# removendo espaços.
dados$producao_mil_t <- gsub(",", ".", dados$producao_mil_t)
dados$producao_mil_t <- as.numeric(dados$producao_mil_t)
dados$produto <- trimws(dados$produto)


# Limpando dados.
dados <- dados %>%
  rename(ano_agricola = `ano_agricola`,
         uf = `uf`,
         produto = `produto`,
         producao_mil_t = `producao_mil_t`) %>%
  mutate(producao_mil_t = as.numeric(producao_mil_t)) %>%
  filter(!is.na(producao_mil_t))



# Visualizando a base.
head(dados)
str(dados)
colnames(dados)


#---------------------------------------------------------------------

# Calculando tendência central.
media <- mean(dados$producao_mil_t, na.rm = TRUE)
mediana <- median(dados$producao_mil_t, na.rm = TRUE)

summary_stats <- dados %>%
  summarise(
    max = max(producao_mil_t),
    min = min(producao_mil_t)
  )


# Calculando medidas de dispersão.
desvio_padrao <- sd(dados$producao_mil_t, na.rm = TRUE)
variancia <- var(dados$producao_mil_t, na.rm = TRUE)
amplitude <- range(dados$producao_mil_t, na.rm = TRUE)
coef_var <- desvio_padrao / media

# Calculando medidas separatrizes.
quartis <- quantile(dados$producao_mil_t, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decis <- quantile(dados$producao_mil_t, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
percentis <- quantile(dados$producao_mil_t, probs = seq(0.01, 0.99, 0.01), na.rm = TRUE)

cat("Média:", media, "\n")
cat("Mediana:", mediana, "\n")
cat("Desvio padrão:", desvio_padrao, "\n")
cat("Coef. de variação:", coef_var, "\n")
cat("Quartis:\n")
print(quartis)


#---------------------------------------------------------------------
# Criando váriaveis novas para análise grafica e exploratória de dados.

# criando variável ordinal (faixa de produção).
dados <- dados %>%
  mutate(categoria_producao = case_when(
    producao_mil_t < 10 ~ "Baixa",
    producao_mil_t < 50 ~ "Média",
    TRUE ~ "Alta"
  )) %>%
  mutate(categoria_producao = factor(categoria_producao,
                                     levels = c("Baixa", "Média", "Alta"),
                                     ordered = TRUE))


# criando variável discreta: ranking por produto e ano.
dados <- dados %>%
  group_by(ano_agricola, produto) %>%
  mutate(ranking_estado = dense_rank(desc(producao_mil_t))) %>%
  ungroup()


# criando variável nominal: se produção foi acima da média.
dados <- dados %>%
  group_by(produto) %>%
  mutate(media_produto = mean(producao_mil_t),
         meta_atingida = ifelse(producao_mil_t > media_produto, "Sim", "Não")) %>%
  ungroup() %>%
  select(-media_produto)

#----------------------------------------------------------------------


# Histograma da produção.
ggplot(dados, aes(x = producao_mil_t)) +
  geom_histogram(bins = 10, fill = "#009879", color = "white") +
  labs(title = "Distribuição da Produção (mil toneladas)", x = "Produção", y = "Frequência") +
  theme_minimal()

# Gráfico: categorias de produção (ordinal).
ggplot(dados, aes(x = categoria_producao)) +
  geom_bar(fill = "#007acc") +
  labs(title = "Faixas de Produção", x = "Categoria", y = "Contagem") +
  theme_minimal()



# Gráfico de barras da contagem de produtos.
contagem_produtos <- dados %>%
  group_by(produto) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

ggplot(contagem_produtos, aes(x = reorder(produto, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência dos Produtos", x = "Produto", y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##  Gráfico de Barras por Estado.
dados %>%
  group_by(uf) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(uf, -producao_total), y = producao_total)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Produção Total por Estado", x = "Estado", y = "Produção (mil t)") +
  theme_minimal()

## Gráfico de Barras por Produto.

dados %>%
  group_by(produto) %>%
  summarise(producao_total = sum(producao_mil_t, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(produto, -producao_total), y = producao_total)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Produção Total por Produto", x = "Produto", y = "Produção (mil t)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------------------------------------

# Salvando uma base nova, com insights, análises e classificações.
write_xlsx(list(
  Base_Completa = dados,
  Resumo_Producao = summary_stats
), "base_agricola_completa.xlsx")


