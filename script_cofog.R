# Pacotes necessários
library(tidyverse)
library(plotly)
library(readr)
library(scales)

# Leitura do arquivo local com encoding correto
cofog <- read_delim("C:/Users/Maria Gabriela/Downloads/Dados-abertos-COFOG-GC-2023.csv", 
                    delim = ";", 
                    locale = locale(encoding = "LATIN1"),
                    trim_ws = TRUE)

# Renomear colunas para facilitar manipulação
cofog <- cofog %>%
  rename(
    cod_cofog = `CLASSIFICACAO COFOG`,
    valor_rs = `VALOR (R$)`
  )

# Dicionário de áreas funcionais
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

# Criar coluna de nível 1 COFOG e recodificar áreas
cofog <- cofog %>%
  mutate(
    cod_cofog_nivel1 = substr(as.character(cod_cofog), 1, 3),
    area_funcional = recode(cod_cofog_nivel1, !!!areas_funcionais),
    valor_rs = as.numeric(gsub(",", ".", gsub("\\.", "", valor_rs)))  # converter para numérico
  ) %>%
  filter(!is.na(valor_rs))  # Remover valores ausentes

# Agrupar e somar valores por área funcional
ranking_areas <- cofog %>%
  group_by(area_funcional) %>%
  summarise(valor_total = sum(valor_rs, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(valor_total))

# Total geral
total_gastos <- sum(ranking_areas$valor_total, na.rm = TRUE)

# Função para formatar valores curtos (rótulos das barras)
formatar_moeda_curta <- function(valor) {
  if (!is.na(valor)) {
    if (valor >= 1e12) {
      return(paste0("R$ ", format(round(valor / 1e12, 1), decimal.mark = ","), " tri"))
    } else if (valor >= 1e9) {
      return(paste0("R$ ", format(round(valor / 1e9, 1), decimal.mark = ","), " bi"))
    } else {
      return(paste0("R$ ", format(valor, big.mark = ".", decimal.mark = ",")))
    }
  } else {
    return("Valor ausente")
  }
}

# Função para tooltip completo
formatar_moeda_completa <- function(valor) {
  if (!is.na(valor)) {
    return(paste0("R$ ", format(round(valor), big.mark = ".", decimal.mark = ",")))
  } else {
    return("Valor ausente")
  }
}

# Aplicar aos rótulos
ranking_areas <- ranking_areas %>%
  mutate(
    rotulo_valor = sapply(valor_total, formatar_moeda_curta),
    tooltip_text = sapply(valor_total, formatar_moeda_completa)
  )

# Distância para deslocar rótulo
dist_nudge <- max(ranking_areas$valor_total) * 0.02

# Construção do gráfico com ggplot2
grafico <- ggplot(ranking_areas, aes(
  x = reorder(area_funcional, valor_total),
  y = valor_total,
  text = tooltip_text,
  fill = valor_total
)) +
  geom_col() +
  geom_text(
    aes(label = rotulo_valor),
    nudge_y = dist_nudge,
    hjust = 0,
    size = 2.3
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    labels = function(x) {
      sapply(x, function(v) {
        if (v == 0) {
          "R$ 0 bi"
        } else {
          formatar_moeda_curta(v)
        }
      })
    },
    expand = expansion(mult = c(0, 0.2)),
    breaks = pretty(c(0, max(ranking_areas$valor_total)), n = 6)
  ) +
  scale_fill_gradient(
    low = "#A6CEE3", high = "#1F78B4",
    name = "Total Gasto (R$)",
    labels = function(x) sapply(x, formatar_moeda_curta)
  ) +
  labs(
    title = paste0("Despesas por função do governo central - Base COFOG 2023\nTotal dos Gastos: R$ ",
                   format(round(total_gastos / 1e12, 2), decimal.mark = ",", big.mark = "."),
                   " trilhões"),
    subtitle = "Fonte: Base COFOG 2023 | Elaboração: Maria Gabriela Santos",
    x = "Área Funcional",
    y = "Total Gasto (R$)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    plot.title = element_text(size = 13, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(5, 80, 5, 5)
  )

# Tornar gráfico interativo
grafico_interativo <- ggplotly(grafico, tooltip = "text")

# Exibir
grafico_interativo

# Gerar o gráfico interativo com ggplotly
grafico_interativo <- ggplotly(grafico, tooltip = "text")

# Salvar o gráfico interativo como arquivo HTML
htmlwidgets::saveWidget(grafico_interativo, "grafico_despesas_funcionais.html")