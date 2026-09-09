# --- CARREGAR PACOTES NECESSÁRIOS ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, car, rstatix)

# 1. PREPARAR OS DADOS
data_clean <- read.csv("tableall.csv", colClasses = "character") %>%
  mutate(across(JAN:TOTAL, ~ as.numeric(gsub("\\.", "", .x)))) %>%
  mutate(YEAR = as.numeric(YEAR))

data_pct <- data_clean %>%
  select(-TOTAL) %>%
  pivot_longer(cols = JAN:DEC, names_to = "MES", values_to = "CASOS") %>%
  mutate(MES = factor(MES, levels = c("JAN", "FEV", "MAR", "APR", "MAI", "JUN", 
                                      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))) %>%
  group_by(LOCAL, YEAR) %>%
  mutate(
    PCT_ANO = (CASOS / sum(CASOS)) * 100,
    PROP = PCT_ANO / 100 # Proporção (0 a 1) para transformações caso necessário
  ) %>%
  ungroup()

# Selecionar região de interesse
dados_brazil <- data_pct %>% filter(LOCAL == "BRAZIL")

# 2. MODELO INICIAL E VERIFICAÇÃO DOS PRESSUPOSTOS
modelo_inicial <- aov(PCT_ANO ~ MES, data = dados_brazil)

# A. Teste de Normalidade dos Resíduos (Shapiro-Wilk)
residuos <- residuals(modelo_inicial)
shapiro_res <- shapiro.test(residuos)

# B. Teste de Homocedasticidade (Levene)
levene_res <- leveneTest(PCT_ANO ~ MES, data = dados_brazil)

# Exibir resultados das checagens
cat("=== CHECAGEM DE PRESSUPOSTOS ===\n")
cat("Shapiro-Wilk (Normalidade dos resíduos): p-valor =", round(shapiro_res$p.value, 4), "\n")
cat("Levene (Homocedasticidade): p-valor =", round(levene_res$`Pr(>F)`[1], 4), "\n\n")

# 3. EXECUÇÃO CONDICIONAL DA ANÁLISE
pressupostos_atendidos <- (shapiro_res$p.value > 0.05) && (levene_res$`Pr(>F)`[1] > 0.05)

if (pressupostos_atendidos) {
  cat("----------------------------------------------------\n")
  cat("RESULTADO: Pressupostos ATENDIDOS. Executando ANOVA e Tukey HSD.\n")
  cat("----------------------------------------------------\n\n")
  
  cat("--- TABELA ANOVA ---\n")
  print(summary(modelo_inicial))
  
  cat("\n--- PÓS-TESTE DE TUKEY HSD ---\n")
  tukey_res <- TukeyHSD(modelo_inicial)
  print(tukey_res)
  
} else {
  cat("----------------------------------------------------\n")
  cat("RESULTADO: Pressupostos VIOLADOS. Executando Kruskal-Wallis e Teste de Dunn.\n")
  cat("----------------------------------------------------\n\n")
  
  cat("--- TESTE DE KRUSKAL-WALLIS ---\n")
  kw_res <- kruskal.test(PCT_ANO ~ MES, data = dados_brazil)
  print(kw_res)
  
  cat("\n--- PÓS-TESTE DE DUNN (Ajuste: Bonferroni) ---\n")
  dunn_res <- dunn_test(PCT_ANO ~ MES, data = dados_brazil, p.adjust.method = "bonferroni")
  print(dunn_res, n = 66) # Exibe todas as comparações pares
}

# 4. GRÁFICO FINAL (Usando Bootstrap para os Intervalos de Confiança)
# Nota: mean_cl_boot é não-paramétrico e funciona independente da normalidade dos dados
ggplot(dados_brazil, aes(x = MES, y = PCT_ANO)) +
  stat_summary(fun = mean, geom = "point", size = 3, color = "#1f77b4") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, color = "#1f77b4") +
  geom_hline(yintercept = 8.33, linetype = "dashed", color = "red", linewidth = 0.8) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribuição Mensal dos Casos (Brasil)",
    subtitle = "Média com IC 95% via Bootstrap | Linha vermelha = Uniformidade (8,33%)",
    x = "Mês",
    y = "% do Total Anual",
    caption = ifelse(pressupostos_atendidos, 
                     "Análise: ANOVA + Tukey HSD", 
                     "Análise: Kruskal-Wallis + Teste de Dunn")
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold")
  )
