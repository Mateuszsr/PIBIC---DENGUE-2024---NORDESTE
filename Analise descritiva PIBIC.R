library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr) 
library(sf)
library(geobr)



dados <- read_excel(file.choose())
pop <- read.csv(file.choose())

# Filtrar os dados para manter apenas os casos com classificação 10
dadoss <- dados %>%
  filter(CLASSI_FIN == 10)
dados <- dadoss

str(dados$ID_MUNICIP)
str(pop$Código)
pop$Código <- as.character(pop$Código)


# Contar a frequência dos nomes dos municípios
municipios_duplicados <- dados_limpos %>%
  group_by(Município....) %>%  # Agrupa pelos nomes dos municípios
  summarise(frequencia = n_distinct(Código)) %>%  # Conta códigos únicos de município
  filter(frequencia > 1)  # Filtra os municípios que têm mais de um código associado

# Visualizar os municípios com o mesmo nome
print(municipios_duplicados)


table(dados$CS_SEXO)
table(dados$CS_RACA)

# ----------------------------------------------------------------------------------------------
# Extrair os 6 primeiros dígitos do código de 7 dígitos no banco de população
pop$codigo_6_digitos <- substr(pop$Código, 1, 6)
# Fazer o merge com base nos 6 dígitos
df_final <- merge(dados, pop, by.x = "ID_MUNICIP", by.y = "codigo_6_digitos", all.x = TRUE)



# Criando um novo data frame com apenas as variáveis desejadas
dados1 <- df_final %>%
  select(ID_MUNICIP, Código, SG_UF, População, Município...., DT_NOTIFIC, DT_ENCERRA, CLASSI_FIN, NU_IDADE_N, CS_SEXO, CS_RACA, CS_ESCOL_N, FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, LEUCOPENIA, LACO, DOR_RETRO, DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE, CRITERIO, EVOLUCAO)



# Remover linhas com valores nulos nas colunas de Município e População
dados_limpos <- dados1 %>%
  filter(!is.na(Município....) & !is.na(População))

# Contar o número de casos de dengue por município usando o código do município
casos_por_municipio <- dados_limpos %>%
  group_by(Código, Município...., ID_MUNICIP) %>%  
  summarise(casos_dengue = n())

# Juntar os dados de população por município
dados_completos <- casos_por_municipio %>%
  left_join(dados_limpos %>% select(Código, População) %>% distinct(), by = "Código")

# Calcular a taxa de incidência (casos por 100.000 habitantes)
dados_completos <- dados_completos %>%
  mutate(taxa_incidencia = (casos_dengue / População) * 100000)













# Distribuição Temporal de Notificações e Encerramentos -----------------------------------------

# Converter para formato de data
dados_limpos$DT_NOTIFIC <- as.Date(dados_limpos$DT_NOTIFIC)
dados_limpos$DT_ENCERRA <- as.Date(dados_limpos$DT_ENCERRA)

# Contagem por mês de notificação
dados_limpos %>%
  mutate(month_notif = floor_date(DT_NOTIFIC, "month")) %>%
  count(month_notif) %>%
  ggplot(aes(x = month_notif, y = n)) +
  geom_line() +
  labs(title = "Notificações de Dengue por Mês",
       x = "Mês", y = "Número de Notificações")






# Gráfico de série temporal de notificações por mês ------------------------------------------------
dados_limpos$DT_NOTIFIC <- as.Date(dados_limpos$DT_NOTIFIC)

# Criar a coluna com o mês de notificação
dados_por_mes <- dados_limpos %>%
  mutate(month_notif = floor_date(DT_NOTIFIC, "month")) %>%
  count(month_notif) %>%
  complete(month_notif = seq(min(month_notif), max(month_notif), by = "month"), fill = list(n = 0))


ggplot(dados_por_mes, aes(x = month_notif, y = n)) +
  geom_line(color = "blue") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +  # Exibir todos os meses
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  # Rotacionar os rótulos dos meses
  labs(title = "Notificações de Dengue por Mês",
       x = "Mês",
       y = "Número de Notificações")













# Calcular o tempo Tempo Médio para Encerramento dos Casos --------------------------------------
dados_limpos$tempo_encerramento <- as.numeric(difftime(dados_limpos$DT_ENCERRA, dados_limpos$DT_NOTIFIC, units = "days"))

#Resumo descritivo
summary(dados_limpos$tempo_encerramento)


ggplot(dados_limpos, aes(x = tempo_encerramento)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribuição do Tempo até o Encerramento dos Casos",
       x = "Dias", y = "Frequência")







# Extrair o mês e criar uma coluna de estações do ano ------------------------------------------
dados_limpos$mes_notif <- month(dados_limpos$DT_NOTIFIC)

dados_limpos$estacao <- case_when(
  dados_limpos$mes_notif %in% c(12, 1, 2) ~ "Verão",
  dados_limpos$mes_notif %in% c(3, 4, 5) ~ "Outono",
  dados_limpos$mes_notif %in% c(6, 7, 8) ~ "Inverno",
  dados_limpos$mes_notif %in% c(9, 10, 11) ~ "Primavera"
)


ggplot(dados_limpos, aes(x = estacao)) +
  geom_bar(fill = "orange") +
  labs(title = "Notificações de Dengue por Estação do Ano", 
       x = "Estação", y = "Número de Notificações")
























# -------------------------------------------------------------------------------------------------


# Criando colunas para Anos, Meses e Dias de nascimento.
dados2 <- dados1 %>%
  mutate(
    Anos = ifelse(str_starts(as.character(NU_IDADE_N), "40"), as.numeric(str_sub(as.character(NU_IDADE_N), 3, 4)), NA),
    Meses = ifelse(str_starts(as.character(NU_IDADE_N), "30"), as.numeric(str_sub(as.character(NU_IDADE_N), 3, 4)), NA),
    Dias = ifelse(str_starts(as.character(NU_IDADE_N), "20"), as.numeric(str_sub(as.character(NU_IDADE_N), 3, 4)), NA)
  )



# Substituir NAs por 0 nas colunas de Anos, Meses e Dias
dados2$Anos[is.na(dados2$Anos)] <- 0
dados2$Meses[is.na(dados2$Meses)] <- 0
dados2$Dias[is.na(dados2$Dias)] <- 0


# Calcular a idade total em anos
dados2$Idade_Total <- dados2$Anos + (dados2$Meses / 12) + (dados2$Dias / 365)




library(dplyr)

#Calcular média, mediana e quartis
estatisticas_idade <- dados2 %>%
  summarize(
    media_idade = mean(Anos, na.rm = TRUE),      # Média
    mediana_idade = median(Anos, na.rm = TRUE),  # Mediana
    quartis_idade = quantile(Anos, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)  # Quartis
  )

print(estatisticas_idade)

# Calcular faixa etária
# Definindo as categorias de faixa etária
dados2 <- dados2 %>%
  mutate(faixa_etaria = cut(Anos, 
                            breaks = c(0, 12, 18, 35, 60, Inf),  # Definir os intervalos das faixas
                            labels = c("Criança", "Adolescente", "Adulto Jovem", "Adulto", "Idoso"),
                            right = FALSE))  # Para que o intervalo seja fechado à esquerda

# Exibindo as faixas etárias
table(dados2$faixa_etaria)














## Grafico de distrubuição por Idade e Sexo --------------------------------------------------------------
library(ggplot2)
ggplot(dados2, aes(x = Idade_Total, fill = CS_SEXO)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribuição por Idade e Sexo", x = "Idade", y = "Contagem")

## Alongando os dados para criar um grafico bloxpot
dados_long <- dados2 %>%
  pivot_longer(cols = c(Anos, Meses, Dias), names_to = "Tipo_Idade", values_to = "Valor_Idade") %>%
  filter(!is.na(Valor_Idade))
## Criar o gráfico
ggplot(dados_long, aes(x = Tipo_Idade, y = Valor_Idade, fill = CS_SEXO)) +
  geom_boxplot() +
  labs(title = "Distribuição de Idade por Tipo e Gênero", x = "Tipo de Idade", y = "Valor da Idade") +
  theme_minimal()
















## Grafico de distrubuição por Idade e Raça -------------------------------------------------------------
library(ggplot2)
ggplot(dados2, aes(x = Idade_Total, fill = CS_RACA)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribuição por Idade e Raça", x = "Idade", y = "Contagem")

## Gráfico de barras para Raça
library(ggplot2)
dados2$CS_RACA <- factor(dados2$CS_RACA, 
                         levels = c(1, 2, 3, 4, 5, 9),
                         labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorado"))

ggplot(dados2, aes(x = CS_RACA, fill = CS_RACA)) +
  geom_bar() +
  labs(title = "", x = "Raça", y = "Contagem") +
  scale_fill_manual(values = c("Branca" = "#1f77b4", 
                               "Preta" = "#ff7f0e", 
                               "Amarela" = "#2ca02c", 
                               "Parda" = "#d62728", 
                               "Indígena" = "#9467bd", 
                               "Ignorado" = "#8c564b"))




















## Gráfico de barras para Escolaridade ------------------------------------------------------------------
ggplot(dados, aes(x = CS_ESCOL_N)) +
  geom_bar(fill = "forestgreen") +
  labs(title = "Distribuição por Escolaridade", x = "Escolaridade", y = "Frequência") +
  theme_minimal()






##  Criar o gráfico de barras para os 10 municípios com maior taxa de incidência --------------------------
library(dplyr)
library(ggplot2)

# Selecionar os 20 municípios com maior taxa de incidência
top10_municipios <- dados_completos %>%
  arrange(desc(taxa_incidencia)) %>%  # Ordenar em ordem decrescente pela taxa de incidência
  distinct(Código, .keep_all = TRUE) %>%  # Remover duplicatas de municípios
  head(20)


#
ggplot(top10_municipios, aes(x = reorder(Município...., -taxa_incidencia), y = taxa_incidencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 20 Municípios com Maior Taxa de Incidência de Dengue", 
       x = "Município", 
       y = "Taxa de Incidência (por 100.000 habitantes)") +
  theme_minimal()





# Definir um limite de taxa de incidência (ex: mostrar municípios com taxa > 1000 por 100.000 habitantes)
limite_incidencia <- 5000
municipios_acima_limite <- dados_completos %>%
  filter(taxa_incidencia > limite_incidencia)

# Criar o gráfico de barras para municípios com taxa de incidência acima do limite
ggplot(municipios_acima_limite, aes(x = reorder(Município...., -taxa_incidencia), y = taxa_incidencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Municípios com Taxa de Incidência Acima de 5000", 
       x = "Município", 
       y = "Taxa de Incidência (por 100.000 habitantes)") +
  theme_minimal()













# Taxa de incidencia por faixas de população --------------------------------------------------------------------
top10_municipios <- top10_municipios %>%
  mutate(faixa_populacao = cut(População, 
                               breaks = c(0, 5000, 20000, 50000, 100000, Inf),
                               labels = c("0-5K", "5K-20K", "20K-50K", "50K-100K", ">100K")))

# Criar o gráfico
ggplot(top10_municipios, aes(x = reorder(Município...., -taxa_incidencia), y = taxa_incidencia, fill = faixa_populacao)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Taxa de Incidência por Faixa de População", 
       x = "Município", 
       y = "Taxa de Incidência (por 100.000 habitantes)", 
       fill = "Faixa Populacional") +
  theme_minimal()

















# Criando grafico para frequencia de sintomas --------------------------------------------------
library(dplyr)
library(ggplot2)


# Transformar os valores 1 e 2 para "Sim" e "Não"
dados <- dados %>%
  mutate(across(c(FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, 
                  DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, 
                  LEUCOPENIA, LACO, DOR_RETRO),
                ~ ifelse(. == 1, "Sim", "Não")))

# Contar a frequência de "Sim" para cada sintoma
frequencia_sintomas <- dados %>%
  summarise(across(c(FEBRE, MIALGIA, CEFALEIA, EXANTEMA, VOMITO, NAUSEA, 
                     DOR_COSTAS, CONJUNTVIT, ARTRITE, ARTRALGIA, PETEQUIA_N, 
                     LEUCOPENIA, LACO, DOR_RETRO), 
                   ~ sum(. == "Sim"))) %>%
  pivot_longer(cols = everything(), names_to = "Sintoma", values_to = "Frequencia")

# Criar o gráfico
ggplot(frequencia_sintomas, aes(x = Sintoma, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frequência dos Sintomas de Dengue", 
       x = "Sintomas", 
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












# Criando grafico para frequencia de comorbidades ----------------------------------------------------
# Transformar os valores 1 e 2 para "Sim" e "Não" nas comorbidades ------------------------------------
dados <- dados %>%
  mutate(across(c(DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE),
                ~ ifelse(. == 1, "Sim", "Não")))

# Contar a frequência de "Sim" para cada comorbidade
frequencia_comorbidades <- dados %>%
  summarise(across(c(DIABETES, HEMATOLOG, HEPATOPAT, RENAL, HIPERTENSA, ACIDO_PEPT, AUTO_IMUNE), 
                   ~ sum(. == "Sim"))) %>%
  pivot_longer(cols = everything(), names_to = "Comorbidade", values_to = "Frequencia")

# Criar o gráfico de barras
ggplot(frequencia_comorbidades, aes(x = Comorbidade, y = Frequencia)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(title = "Frequência das Comorbidades em Pacientes com Dengue", 
       x = "Comorbidades", 
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








# Read all municipalities in the country at a given year
mun <- read_municipality(code_muni="all", year=2022)
str(mun$code_muni)
str(dados_unicos$Código)
colnames(mun)[colnames(mun) == "code_muni"] <- "Código"
# Juntar os data frames com base no código do município
dados_mapa <- dados_unicos %>%
  left_join(mun, by = "Código")  # Mude 'codigo_municipio' para o nome correto da coluna se necessário


# Criar o mapa de incidência
ggplot(dados_mapa) +
  geom_sf(aes(fill = taxa_incidencia, geometry = geom)) +  # 'taxa_incidencia' deve ser o nome da coluna no seu dados_unicos
  scale_fill_gradient(low = "yellow", high = "red", na.value = "grey80") +  # Cores do mapa
  labs(title = "Mapa de Incidência de Dengue nos Municípios do Nordeste",
       fill = "Taxa de Incidência") +
  theme_minimal()
