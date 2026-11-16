# Letícia Ruas e Lívia
# MEDIÇÕES DA TEMPERATURA DA PELE EM DOIS TRATAMENTOS PÓS-CIRÚRGICOS ANTES DA CIRURGIA EM TRÊS MOMENTOS APÓS

library(dplyr)
library(tidyr)
library(ggplot2)
library(boot)

# Carregando os dados
# O csv deve estar na mesma pasta desse documento. Para saber o caminho em que o R Studio está, use getwd() e para mudar, setwd("caminho/da/pasta")
df_temperatura <- read.csv("Temperatura.csv", sep = ";")

# Conferindo valores
summary(df_temperatura$TEMPERATURA)
df_temperatura %>%
  select(where(is.character)) %>%
  lapply(table)
df_temperatura %>% count(SUJEITO) %>% filter(n != 8)


colSums(is.na(df_temperatura))

df_temperatura %>%
  distinct(SUJEITO, GRUPO) %>%
  count(GRUPO)

df_temperatura %>%
  distinct(SUJEITO, MAO) %>%
  count(MAO)

df_temperatura %>%
  distinct(SUJEITO, DIA) %>%
  count(DIA)

df_temperatura %>%
  select(where(is.character)) %>%
  lapply(table)
  

# Considerando o momento antes da operação e somente a mão operada, a distribuição dos valores de  temperatura varia entre os dois grupos de tratamento?
df_antes_operada <- df_temperatura %>%
                      filter(MAO == "Operada" & DIA == "ANTES")

df_antes_operada %>%
  ggplot(aes(x = GRUPO, y = TEMPERATURA, fill = GRUPO)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.5) +   # mostra os pontos individuais
  labs(x = "Grupo", y = "Temperatura (°C × 10)",
       title = "Temperatura da mão operada antes da cirurgia") +
  theme_minimal() +
  theme(legend.position = "none")

# O boxplot sugere possíveis diferenças entre os grupos, que precisam ser testadas


df_antes_operada %>%
  ggplot(aes(x = TEMPERATURA, fill = GRUPO)) +
  geom_histogram(position = "identity", alpha = 0.5,
                 binwidth = 5, color = "white") +
  labs(x = "Temperatura (°C × 10)", y = "Frequência", fill = "Grupo",
       title = "Temperatura da mão operada antes da cirurgia") +
  theme_minimal()

# Optamos por não supor normalidade, então usamos métodos não paramétricos
# Os histogramas sugerem formatos diferentes de distribuição entre os grupos,
#   o que coloca em dúvida a suposição de mesma forma do Mann-Whitney
# Mann-Whitney: compara a locação (mediana), supondo mesma forma das distribuições;
#   como há muitos empates (temperatura em °C x 10), usa-se a aproximação normal
# Kolmogorov-Smirnov: compara as distribuições como um todo (forma, dispersão e
#   locação), sem exigir mesma forma
# Bootstrap: estima a distribuição amostral da diferença de medianas sem supor
#   distribuição para os dados nem mesma forma, fornecendo IC 95% (percentílico e BCa);
#   se o IC não contiver 0, rejeita-se H0 ao nível de 5%

ks.test(TEMPERATURA ~ GRUPO, data = df_antes_operada)
# Não rejeito H0: p-value = 0.1787
# Agora faremos um bootstrap para analisar se a diferença entre os grupos contém 0 (nesse caso também não rejeitamos H0)
dif_medianas <- function(dados, indices) {
  d <- dados[indices, ]
  median(d$TEMPERATURA[d$GRUPO == "Droga"]) -
    median(d$TEMPERATURA[d$GRUPO == "Placebo"])
}

set.seed(42)
res_boot <- boot(data = df_antes_operada,
                 statistic = dif_medianas,
                 R = 10000,
                 strata = factor(df_antes_operada$GRUPO))

res_boot                                         # estimativa e erro-padrão
boot.ci(res_boot, conf = 0.95, type = c("perc", "bca"))
hist(res_boot$t, main = "Distribuição bootstrap da diferença de medianas",
     xlab = "Diferença de medianas (Droga − Placebo)")
df_temperatura %>% count(SUJEITO) %>% filter(n != 8)


dif_medianas <- function(dados, indices) {
  d <- dados[indices, ]
  median(d$TEMPERATURA[d$GRUPO == "Droga"]) -
    median(d$TEMPERATURA[d$GRUPO == "Placebo"])
}

set.seed(42)

strata = factor(df_antes_operada$GRUPO)

res_boot
boot.ci(res_boot, conf = 0.95, type = c("perc", "bca"))

 
# Temperatura da mão operada ao longo dos dias

#install.packages("PMCMRplus")


# Selecionar a mão operada e organizar os quatro momentos

dias_estudo <- c("ANTES", "D1", "D3", "D5")

df_operada <- df_temperatura %>%
  filter(MAO == "Operada") %>%
  mutate(
    SUJEITO = factor(SUJEITO),
    DIA = factor(DIA, levels = dias_estudo),
    TEMP_C = TEMPERATURA / 10
  )

# TEMP_C está em graus Celsius. A coluna TEMPERATURA continua na escala original.


# Conferir o pareamento antes de aplicar o teste de Friedman

# Uma célula com 0 indica medição ausente; com 2 ou mais, repetição.
contagem_por_dia <- with(df_operada, table(SUJEITO, DIA))

if (anyNA(df_operada[c("SUJEITO", "GRUPO", "DIA", "TEMP_C")])) {
  stop("Há valores ausentes ou dias inesperados. Confira df_operada.")
}

if (any(contagem_por_dia != 1)) {
  print(contagem_por_dia[rowSums(contagem_por_dia != 1) > 0, , drop = FALSE])
  stop("Confira os pacientes acima antes de continuar a análise pareada.")
}

# Cada paciente deve pertencer a um único grupo de tratamento.
grupos_por_sujeito <- df_operada %>%
  distinct(SUJEITO, GRUPO) %>%
  count(SUJEITO, name = "n_grupos")

if (any(grupos_por_sujeito$n_grupos != 1)) {
  stop("Há paciente associado a mais de um grupo de tratamento.")
}


# Tabela descritiva

tabela_temperatura <- df_operada %>%
  group_by(GRUPO, DIA) %>%
  summarise(
    n_pacientes = n_distinct(SUJEITO),
    mediana_C = median(TEMP_C),
    Q1_C = unname(quantile(TEMP_C, 0.25)),
    Q3_C = unname(quantile(TEMP_C, 0.75)),
    .groups = "drop"
  )

print(tabela_temperatura)


# Gráfico das trajetórias individuais

grafico_evolucao <- ggplot(df_operada, aes(x = DIA, y = TEMP_C)) +
  geom_line(aes(group = SUJEITO), color = "grey55", alpha = 0.45) +
  geom_point(color = "grey45", alpha = 0.45, size = 1.3) +
  stat_summary(aes(group = 1), fun = median, geom = "line",
               color = "steelblue4", linewidth = 1.2) +
  stat_summary(fun = median, geom = "point",
               color = "steelblue4", size = 2.5) +
  facet_wrap(~ GRUPO) +
  labs(
    title = "Temperatura da mão operada ao longo do estudo",
    subtitle = "Cada linha cinza representa um paciente; a linha azul indica a mediana",
    x = "Momento da medição",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()

print(grafico_evolucao)


#  Friedman

formatar_p <- function(p) {
  ifelse(p < 0.001, "< 0,001", formatC(p, format = "f", digits = 4,
                                       decimal.mark = ","))
}

analisar_dias <- function(dados_grupo) {
  
  dados_grupo <- droplevels(dados_grupo)
  teste_global <- friedman.test(TEMP_C ~ DIA | SUJEITO, data = dados_grupo)
  
  cat("\nGrupo:", as.character(dados_grupo$GRUPO[1]), "\n")
  cat("Friedman — valor-p:", formatar_p(teste_global$p.value), "\n")
  
  comparacoes <- NULL
  
  # A decisão usa o valor-p original, antes do arredondamento.
  if (teste_global$p.value <= 0.05) {
    pos_teste <- PMCMRplus::frdAllPairsConoverTest(
      y = dados_grupo$TEMP_C,
      groups = dados_grupo$DIA,
      blocks = droplevels(dados_grupo$SUJEITO),
      p.adjust.method = "holm"
    )
    
    comparacoes <- as.data.frame(
      as.table(pos_teste$p.value),
      responseName = "p_ajustado"
    )
    names(comparacoes)[1:2] <- c("dia_1", "dia_2")
    
    comparacoes <- comparacoes %>%
      filter(!is.na(p_ajustado)) %>%
      mutate(valor_p_relatorio = formatar_p(p_ajustado))
    
    cat("Conover — comparações entre dias com ajuste de Holm:\n")
    print(select(comparacoes, dia_1, dia_2, valor_p_relatorio),
          row.names = FALSE)
  }
  
  invisible(list(friedman = teste_global, conover = comparacoes))
}

# Pacientes que receberam a droga V
resultado_droga <- analisar_dias(filter(df_operada, GRUPO == "Droga"))

# Pacientes que receberam placebo
resultado_placebo <- analisar_dias(filter(df_operada, GRUPO == "Placebo"))

set.seed(42)

res_boot <- boot::boot(
  data = df_antes_operada,
  statistic = dif_medianas,
  R = 10000,
  strata = factor(df_antes_operada$GRUPO)
)

sum(is.finite(res_boot$t))

boot::boot.ci(res_boot, conf = 0.95, type = c("perc", "bca"))
