library(tidyverse)
# Leitura do arquivo local
cofog <- read_csv("C:/Users/Maria Gabriela/Downloads/Dados-abertos-COFOG-GC-2023.csv", 
                  locale = locale(encoding = "LATIN1"))  # Use UTF-8 se LATIN1 der problema

library(readr)

cofog <- read_delim("C:/Users/Maria Gabriela/Downloads/Dados-abertos-COFOG-GC-2023.csv", 
                    delim = ";", 
                    locale = locale(encoding = "LATIN1"),
                    trim_ws = TRUE)
glimpse(cofog)

library(dplyr)
library(ggplot2)

# 1) Definir as áreas funcionais (COFOG de primeira ordem)
areas_funcionais <- c(
  "701" = "SERVIÇOS PÚBLICOS GERAIS",
  "702" = "DEFESA",
  "703" = "ORDEM PÚBLICA",
  "704" = "ASSUNTOS ECONÔMICOS",
  "705" = "PROTEÇÃO AMBIENTAL",
  "706" = "HABITAÇÃO E SERVIÇOS COMUNITÁRIOS",
  "707" = "SAÚDE",
  "708" = "LAZER, CULTURA E RELIGIÃO",
  "709" = "EDUCAÇÃO",
  "710" = "PROTEÇÃO SOCIAL"
)

# 2) Criar coluna com código COFOG de 1ª ordem
cofog <- cofog %>%
  mutate(
    cod_cofog_nivel1 = substr(as.character(`CLASSIFICACAO COFOG`), 1, 3),
    `AREA FUNCIONAL` = recode(cod_cofog_nivel1, !!!areas_funcionais)
  )
# 2.5) Garantir que o valor esteja numérico
cofog <- cofog %>%
  mutate(`VALOR (R$)` = as.numeric(gsub(",", ".", gsub("\\.", "", `VALOR (R$)`))))

# 3) Calcular total por área funcional
ranking_areas <- cofog %>%
  group_by(`AREA FUNCIONAL`) %>%
  summarise(valor_total = sum(`VALOR (R$)`, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# Calcular o valor total de todos os gastos
total_gastos <- sum(ranking_areas$valor_total, na.rm = TRUE)


library(tidyverse)
library(plotly)

# Total geral
total_gastos <- sum(ranking_areas$valor_total)

# Criar coluna de rótulos formatados sem notação científica
ranking_areas <- ranking_areas %>%
  mutate(rotulo_valor = paste0("R$ ", 
                               format(round(valor_total), big.mark = ".", decimal.mark = ",", scientific = FALSE)))

# Distância de “nudge” (2% do máximo) para mover todos os rótulos para a direita
dist_nudge <- max(ranking_areas$valor_total) * 0.02

# Gráfico ajustado
grafico <- ggplot(ranking_areas, aes(x = reorder(`AREA FUNCIONAL`, valor_total), y = valor_total)) +
  geom_col(fill = "#2E86AB") +
  geom_text(
    aes(label = rotulo_valor),
    nudge_y = dist_nudge,   # empurra tudo para a direita
    hjust = 0,              # alinhado ao início do texto
    size = 2.3
  ) +
  coord_flip(clip = "off") +  # permite extrapolar texto
  scale_y_continuous(
    breaks = c(0, 5e11, 1e12, 1.5e12),
    labels = c("0,0e+00", "500 bilhões", "1 trilhão", "1,5 trilhão"),
    limits = c(0, 1.7e12),
    expand = c(0, 0)
  ) +
  labs(
    title = paste0(
      "Despesas por função do governo central - Base COFOG 2023\n",
      "Total dos Gastos: R$ ", 
      format(round(total_gastos / 1e12, 2), decimal.mark = ",", big.mark = "."), 
      " trilhões"
    ),
    subtitle = "Fonte: Base COFOG 2023 | Elaboração: Maria Gabriela Santos",
    x = "Área Funcional",
    y = "Total Gasto (R$)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.margin = margin(5, 40, 5, 5),     # margem direita maior
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# Tornar interativo
grafico_interativo <- ggplotly(grafico)

# Exibir
grafico_interativo
