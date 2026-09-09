library(tidyverse)

# 1. Carregar e tratar os dados
data_clean <- read.csv("tableall.csv", colClasses = "character") %>%
  mutate(across(JAN:TOTAL, ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(YEAR = as.numeric(YEAR))

# 2. Formato longo
data_long <- data_clean %>%
  select(-TOTAL) %>%
  pivot_longer(
    cols = JAN:DEC,
    names_to = "MES",
    values_to = "CASOS"
  ) %>%
  mutate(
    MES = factor(MES, levels = c("JAN", "FEV", "MAR", "APR", "MAI", "JUN", 
                                 "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
  )

# -------------------------------------------------------------
# HEATMAP DO NÚMERO ABSOLUTO DE CASOS - BRAZIL
# -------------------------------------------------------------
data_long %>%
  filter(LOCAL == "BRAZIL") %>%
  ggplot(aes(x = MES, y = factor(YEAR), fill = CASOS)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = CASOS), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(
    title = "Heatmap do Número Absoluto de Casos - BRASIL",
    subtitle = "Valores representam o total de ocorrências em cada mês e ano",
    x = "Mês",
    y = "Ano",
    fill = "Casos"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )
