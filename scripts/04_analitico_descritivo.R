# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 04_04_analitico_descritivo.R
# OBJETIVO: Análise descritiva e cálculo de taxas brutas de mortalidade por
#           suicídio — Brasil, 2000–2022.
#           Produz séries temporais anuais e mensais (total, masculino,
#           feminino) prontas para modelagem, além de análise exploratória
#           completa: STL, ACF/PACF, testes de sazonalidade e comparativo
#           de granularidade anual vs mensal.
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-04-04
# VERSÃO  : 1.0
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# DEPENDÊNCIAS: 03_clean_dados.R
# ENTRADA     : dados/limpos/suicidio_limpo.rds
#               dados/limpos/populacao_ibge_tratada.csv
# SAIDA       : tabelas/descriptive/
#                 tab_taxas_anuais.csv
#                 tab_taxas_mensais_total.csv
#                 tab_taxas_mensais_sexo.csv
#                 tab_taxas_faixa_etaria.csv
#                 tab_taxas_regiao.csv
#                 tab_razao_mf_anual.csv
#                 tab_stl_componentes.csv
#                 tab_testes_sazonalidade.csv
#                 tab_comparativo_granularidade.csv
#               imagens/descriptive/
#                 fig_serie_taxas_anuais.png/.pdf
#                 fig_serie_taxas_mensais.png/.pdf
#                 fig_stl_total.png/.pdf
#                 fig_stl_sexo.png/.pdf
#                 fig_acf_pacf_mensal.png/.pdf
#                 fig_acf_pacf_anual.png/.pdf
#                 fig_sazonalidade.png/.pdf
#                 fig_subseries_mensal.png/.pdf
#                 fig_taxas_regiao.png/.pdf
#                 fig_taxas_faixa.png/.pdf
#               dados/limpos/
#                 series_ts_anuais.rds    ← objetos ts + tsibble anuais
#                 series_ts_mensais.rds   ← objetos ts + tsibble mensais
# -----------------------------------------------------------------------------
# NOTAS:
#   - Fórmula da taxa: (óbitos / população exposta) × 100.000
#   - Denominador mensal: pop_anual / 12 (distribuição uniforme intra-anual)
#     Justificativa: variação intra-anual da população é desprezível para
#     taxas de mortalidade; padrão adotado na literatura epidemiológica
#   - Denominador anual: população total do ano (pop_ibge, Nível 1)
#   - Para 2007: pop_ibge usa estimativa Tab6579, não total da Contagem
#     (ver notas.qmd §2 — total da Contagem é parcial)
#   - Séries salvas em formato ts (stats) e tsibble (fable) para
#     compatibilidade com scripts de modelagem (cards #6–#8)
#   - Seed: N/A (sem aleatoriedade neste script)
# =============================================================================


# =============================================================================
# CONFIGURAÇÃO
# =============================================================================
# Edite este bloco para ajustar período, caminhos ou parâmetros analíticos.
# =============================================================================

config <- list(
  # Período de análise
  ano_inicio = 2000L,
  ano_fim    = 2022L,

  # Fator de escala para taxas (por 100 mil habitantes)
  escala_taxa = 1e5,

  # Janela da componente sazonal na decomposição STL
  # "periodic" = sazonal fixa ao longo do tempo (mais estável para n=23 anos)
  stl_s_window = "periodic",

  # Número máximo de lags nos gráficos ACF/PACF
  acf_lag_max_mensal = 36L,   # 3 anos — detecta ciclos anuais e semestrais
  acf_lag_max_anual  = 15L,   # suficiente para série com 23 pontos

  # Caminhos de entrada
  path_suicidio = here::here("dados", "limpos", "suicidio_limpo.rds"),
  path_pop      = here::here("dados", "limpos", "populacao_ibge_tratada.csv"),

  # Caminhos de saída
  dir_tabelas   = here::here("tabelas", "analitico_descritivo"),
  dir_imagens   = here::here("imagens", "analitico_descritivo"),
  path_ts_anual = here::here("dados", "limpos", "series_ts_anuais.rds"),
  path_ts_mensal= here::here("dados", "limpos", "series_ts_mensais.rds"),

  # Parâmetros de figura
  dpi        = 300L,
  fig_width  = 12,
  fig_height = 7
)


# =============================================================================
# PACOTES
# =============================================================================

library(tidyverse)    # dplyr, tidyr, ggplot2, purrr, stringr, lubridate
library(lubridate)    # ym(), year(), month()
library(here)         # here::here() — caminhos portáveis
library(glue)         # glue() — mensagens legíveis
library(logger)       # logging estruturado com fallback
library(scales)       # formatação de eixos
library(patchwork)    # composição de figuras

# Pacotes de séries temporais
library(forecast)     # stl(), Acf(), Pacf(), seasonplot(), auto.arima()
library(tsibble)      # formato moderno de séries temporais
library(feasts)       # ACF/PACF via tsibble, decomposição STL
library(seastests)    # testes formais de sazonalidade (qs, friedman, kw)


# =============================================================================
# LOGGING
# =============================================================================
# Padrão do projeto: logger com fallback para message().
# glue + parent.frame() garante interpolação correta das variáveis.
# =============================================================================

log_info_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_info(msg), error = function(e) message("[INFO] ", msg))
}

log_warn_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_warn(msg), error = function(e) message("[WARN] ", msg))
}

log_info_safe("Iniciando 04_descriptive.R | período: {config$ano_inicio}–{config$ano_fim}")


# =============================================================================
# PALETA E TEMA
# =============================================================================

# Paleta Okabe–Ito: acessível a daltônicos e reproduzível em P&B
cores_okabe <- c(
  "Total"        = "#725BA3",
  "Masculino"    = "#0072B2",
  "Feminino"     = "#D55E00",
  "Norte"        = "#56B4E9",
  "Nordeste"     = "#E69F00",
  "Sudeste"      = "#009E73",
  "Sul"          = "#0072B2",
  "Centro-Oeste" = "#CC79A7"
)

tema_anubis <- theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption  = element_text(size = 8,  color = "grey50"),
    axis.title    = element_text(size = 10),
    legend.position = "bottom",
    strip.text    = element_text(face = "bold")
  )

theme_set(tema_anubis)


# =============================================================================
# FUNÇÕES AUXILIARES
# =============================================================================

#' Salva figura em PNG e PDF no diretório de imagens do projeto.
#' @param plot Objeto ggplot.
#' @param nome Nome base do arquivo (sem extensão).
#' @param w,h Largura e altura em polegadas.
salvar_fig <- function(plot, nome,
                       w = config$fig_width, h = config$fig_height) {
  base <- file.path(config$dir_imagens, nome)
  ggsave(glue("{base}.png"), plot, width = w, height = h, dpi = config$dpi)
  ggsave(glue("{base}.pdf"), plot, width = w, height = h)
  log_info_safe("Figura salva: {nome}.png | {nome}.pdf")
}

#' Salva data.frame como CSV no diretório de tabelas do projeto.
#' @param df Data frame a salvar.
#' @param nome Nome base do arquivo (sem extensão).
salvar_tab <- function(df, nome) {
  path <- file.path(config$dir_tabelas, glue("{nome}.csv"))
  write_csv(df, path)
  log_info_safe("Tabela salva: {nome}.csv ({nrow(df)} linhas)")
}

#' Garante diretórios de saída.
criar_diretorios <- function() {
  dir.create(config$dir_tabelas, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$dir_imagens, showWarnings = FALSE, recursive = TRUE)
}


# =============================================================================
# CARREGAMENTO E VALIDAÇÃO DOS DADOS
# =============================================================================

criar_diretorios()

log_info_safe("Carregando dados...")

suicidio <- readRDS(config$path_suicidio)

# pop_ibge contém populacao_total, pop_masculino, pop_feminino por local e ano.
# Filtramos apenas "Brasil" pois as taxas nacionais são o foco deste script.
# Taxas por região serão calculadas no bloco descritivo com pop regional.
pop_ibge <- read_csv(config$path_pop, show_col_types = FALSE)

pop_brasil <- pop_ibge |>
  filter(local == "Brasil") |>
  select(ano, populacao_total, pop_masculino, pop_feminino)

pop_regioes <- pop_ibge |>
  filter(local != "Brasil") |>
  rename(regiao = local) |>
  select(regiao, ano, populacao_total, pop_masculino, pop_feminino)

# Verificações de integridade
anos_suicidio <- sort(unique(suicidio$ano))
anos_pop      <- sort(unique(pop_brasil$ano))
anos_faltando <- setdiff(anos_suicidio, anos_pop)

if (length(anos_faltando) > 0) {
  log_warn_safe(
    "Anos em suicidio_limpo sem correspondência em pop_ibge: ",
    "{paste(anos_faltando, collapse=', ')} — taxas ficarão NA nesses anos"
  )
} else {
  log_info_safe("Cobertura de anos OK: suicídio e população alinhados para {length(anos_suicidio)} anos")
}

log_info_safe(
  "Dados carregados: {nrow(suicidio)} óbitos | pop Brasil: {nrow(pop_brasil)} anos"
)


# =============================================================================
# BLOCO 1 — SÉRIES ANUAIS
# =============================================================================
# Agrega óbitos por ano (total e por sexo), junta denominadores populacionais
# e calcula taxas anuais por 100 mil habitantes.
# =============================================================================

log_info_safe("── Bloco 1: Séries anuais ──")

# --- 1.1 Contagens anuais ----------------------------------------------------

obitos_ano_total <- suicidio |>
  count(ano, name = "obitos") |>
  mutate(grupo = "Total")

obitos_ano_sexo <- suicidio |>
  count(ano, sexo_label, name = "obitos") |>
  rename(grupo = sexo_label)

obitos_ano <- bind_rows(obitos_ano_total, obitos_ano_sexo) |>
  arrange(grupo, ano)

# --- 1.2 Taxas anuais --------------------------------------------------------

# Junção com denominadores: total e por sexo
pop_long <- pop_brasil |>
  pivot_longer(
    cols      = c(populacao_total, pop_masculino, pop_feminino),
    names_to  = "var_pop",
    values_to = "populacao"
  ) |>
  mutate(
    grupo = case_when(
      var_pop == "populacao_total"  ~ "Total",
      var_pop == "pop_masculino"    ~ "Masculino",
      var_pop == "pop_feminino"     ~ "Feminino"
    )
  ) |>
  select(ano, grupo, populacao)

taxas_anuais <- obitos_ano |>
  left_join(pop_long, by = c("ano", "grupo")) |>
  mutate(taxa = (obitos / populacao) * config$escala_taxa) |>
  arrange(grupo, ano)

salvar_tab(taxas_anuais, "tab_taxas_anuais")

# Razão masculino/feminino por ano
razao_mf <- taxas_anuais |>
  filter(grupo %in% c("Masculino", "Feminino")) |>
  select(ano, grupo, taxa) |>
  pivot_wider(names_from = grupo, values_from = taxa) |>
  mutate(razao_mf = round(Masculino / Feminino, 3),
         dif_abs  = round(Masculino - Feminino, 3)) |>
  arrange(ano)

salvar_tab(razao_mf, "tab_razao_mf_anual")
log_info_safe(
  "Razão M/F — mín: {min(razao_mf$razao_mf, na.rm=TRUE)} | ",
  "máx: {max(razao_mf$razao_mf, na.rm=TRUE)}"
)

# --- 1.3 Objetos ts anuais ---------------------------------------------------

# ts() com frequency = 1 (série anual sem sazonalidade)
ts_anual_total <- ts(
  taxas_anuais |> filter(grupo == "Total") |> pull(taxa),
  start = config$ano_inicio, frequency = 1
)
ts_anual_masc <- ts(
  taxas_anuais |> filter(grupo == "Masculino") |> pull(taxa),
  start = config$ano_inicio, frequency = 1
)
ts_anual_fem <- ts(
  taxas_anuais |> filter(grupo == "Feminino") |> pull(taxa),
  start = config$ano_inicio, frequency = 1
)

# tsibble anual (formato fable/feasts)
tsibble_anual <- taxas_anuais |>
  as_tsibble(index = ano, key = grupo)

# --- 1.4 Figura: série de taxas anuais --------------------------------------

fig_anual <- ggplot(
  taxas_anuais |>
    mutate(grupo = factor(grupo, levels = c("Total","Masculino","Feminino"))),
  aes(x = ano, y = taxa, color = grupo, group = grupo)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  annotate("text", x = 2021, y = Inf, label = "COVID-19",
           vjust = 1.5, size = 3, color = "grey40") +
  scale_color_manual(
    values = cores_okabe[c("Total","Masculino","Feminino")], name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title    = "Taxa bruta de mortalidade por suicídio — Brasil, 2000–2022",
    subtitle = "Taxas por 100 mil habitantes, por sexo (série anual)",
    x = NULL, y = "Taxa por 100.000 hab.",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_anual, "fig_serie_taxas_anuais")

# --- 1.5 ACF/PACF anuais -----------------------------------------------------

# Exportamos como PNG via png() pois Acf() retorna objeto base R,
# não ggplot — não é compatível com ggsave diretamente.
path_acf_anual <- file.path(config$dir_imagens, "fig_acf_pacf_anual.png")
png(path_acf_anual,
    width = config$fig_width * config$dpi,
    height = (config$fig_height / 2) * config$dpi,
    res = config$dpi)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
Acf(ts_anual_total,
    lag.max = config$acf_lag_max_anual,
    main    = "ACF — Taxa anual (Total)")
Pacf(ts_anual_total,
     lag.max = config$acf_lag_max_anual,
     main    = "PACF — Taxa anual (Total)")
par(mfrow = c(1, 1))
dev.off()
log_info_safe("Figura salva: fig_acf_pacf_anual.png")


# =============================================================================
# BLOCO 2 — SÉRIES MENSAIS
# =============================================================================
# Agrega óbitos por ano e mês, expande para grade completa (sem lacunas),
# calcula taxas mensais usando denominador pop_anual / 12.
# =============================================================================

log_info_safe("── Bloco 2: Séries mensais ──")

# --- 2.1 Grade completa ano × mês --------------------------------------------

# Garante que todos os 276 meses existam mesmo que sem óbitos registrados
grade_mensal       <- expand_grid(ano = config$ano_inicio:config$ano_fim, mes = 1:12)
grade_mensal_sexo  <- expand_grid(
  ano       = config$ano_inicio:config$ano_fim,
  mes       = 1:12,
  sexo_label = c("Masculino", "Feminino")
)

# --- 2.2 Contagens mensais ---------------------------------------------------

obitos_mensal_total <- suicidio |>
  count(ano, mes, name = "obitos") |>
  right_join(grade_mensal, by = c("ano", "mes")) |>
  mutate(obitos = replace_na(obitos, 0L)) |>
  arrange(ano, mes)

obitos_mensal_sexo <- suicidio |>
  count(ano, mes, sexo_label, name = "obitos") |>
  right_join(grade_mensal_sexo, by = c("ano", "mes", "sexo_label")) |>
  mutate(obitos = replace_na(obitos, 0L)) |>
  arrange(sexo_label, ano, mes)

# --- 2.3 Taxas mensais -------------------------------------------------------

# Denominador: pop_anual / 12 (distribuição uniforme intra-anual)
# Esse método é padrão na literatura epidemiológica para séries mensais de
# mortalidade. A variação intra-anual da população é desprezível para
# taxas de mortalidade por suicídio.

taxas_mensais_total <- obitos_mensal_total |>
  left_join(pop_brasil |> select(ano, populacao_total),
            by = "ano") |>
  mutate(
    data        = ym(paste(ano, mes, sep = "-")),
    pop_mensal  = populacao_total / 12,
    taxa_mensal = (obitos / pop_mensal) * config$escala_taxa,
    grupo       = "Total"
  ) |>
  select(grupo, ano, mes, data, obitos, pop_mensal, taxa_mensal)

taxas_mensais_sexo <- obitos_mensal_sexo |>
  left_join(
    pop_brasil |> select(ano, pop_masculino, pop_feminino),
    by = "ano"
  ) |>
  mutate(
    data = ym(paste(ano, mes, sep = "-")),
    pop_mensal = case_when(
      sexo_label == "Masculino" ~ pop_masculino / 12,
      sexo_label == "Feminino"  ~ pop_feminino  / 12
    ),
    taxa_mensal = (obitos / pop_mensal) * config$escala_taxa,
    grupo       = sexo_label
  ) |>
  select(grupo, ano, mes, data, obitos, pop_mensal, taxa_mensal)

# Consolida total + sexo em uma única tabela
taxas_mensais <- bind_rows(taxas_mensais_total, taxas_mensais_sexo) |>
  mutate(grupo = factor(grupo, levels = c("Total","Masculino","Feminino"))) |>
  arrange(grupo, ano, mes)

salvar_tab(taxas_mensais_total, "tab_taxas_mensais_total")
salvar_tab(taxas_mensais_sexo,  "tab_taxas_mensais_sexo")

# --- 2.4 Objetos ts mensais --------------------------------------------------

ts_mensal_total <- ts(
  taxas_mensais_total$taxa_mensal,
  start = c(config$ano_inicio, 1), frequency = 12
)
ts_mensal_masc <- ts(
  taxas_mensais_sexo |> filter(grupo == "Masculino") |> pull(taxa_mensal),
  start = c(config$ano_inicio, 1), frequency = 12
)
ts_mensal_fem <- ts(
  taxas_mensais_sexo |> filter(grupo == "Feminino") |> pull(taxa_mensal),
  start = c(config$ano_inicio, 1), frequency = 12
)

# tsibble mensal (formato fable/feasts)
tsibble_mensal <- taxas_mensais |>
  mutate(data_ym = yearmonth(data)) |>
  as_tsibble(index = data_ym, key = grupo)

# --- 2.5 Figura: série mensal -----------------------------------------------

fig_mensal_total <- ggplot(
  taxas_mensais_total,
  aes(x = data, y = taxa_mensal)
) +
  geom_line(color = cores_okabe["Total"], linewidth = 0.6, alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.3, se = TRUE,
              color = "#D55E00", linewidth = 1, alpha = 0.15) +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(subtitle = "A. Total",
       x = NULL, y = "Taxa por 100.000 hab./mês")

fig_mensal_sexo <- ggplot(
  taxas_mensais_sexo |>
    mutate(grupo = factor(grupo, levels = c("Masculino","Feminino"))),
  aes(x = data, y = taxa_mensal, color = grupo)
) +
  geom_line(linewidth = 0.6, alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 1) +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  scale_color_manual(
    values = cores_okabe[c("Masculino","Feminino")], name = NULL
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(subtitle = "B. Por sexo",
       x = NULL, y = "Taxa por 100.000 hab./mês")

# Boxplot de distribuição por mês — detecta padrão sazonal visualmente
fig_boxplot_mes <- taxas_mensais_total |>
  mutate(
    mes_label = factor(
      month.abb[mes], levels = month.abb
    )
  ) |>
  ggplot(aes(x = mes_label, y = taxa_mensal)) +
  geom_boxplot(fill = "#E8F4FD", color = cores_okabe["Total"],
               outlier.size = 1.5, outlier.alpha = 0.6) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    subtitle = "C. Distribuição por mês (todos os anos)",
    x = "Mês", y = "Taxa por 100.000 hab./mês"
  )

fig_mensal <- (fig_mensal_total / fig_mensal_sexo / fig_boxplot_mes) +
  plot_annotation(
    title   = "Taxa mensal de mortalidade por suicídio — Brasil, 2000–2022",
    caption = "Fonte: SIM/DATASUS e IBGE. Elaboração própria. Linha laranja: suavização LOESS (span=0,3). Faixa cinza: período COVID-19.",
    theme   = theme(plot.title = element_text(face = "bold", size = 13))
  )

salvar_fig(fig_mensal, "fig_serie_taxas_mensais", h = 14)

# --- 2.6 Gráfico sazonal (seasonplot) ----------------------------------------

path_sazonal <- file.path(config$dir_imagens, "fig_sazonalidade.png")
png(path_sazonal,
    width  = config$fig_width * config$dpi,
    height = config$fig_height * config$dpi,
    res    = config$dpi)
seasonplot(
  ts_mensal_total,
  year.labels      = TRUE,
  year.labels.left = TRUE,
  col  = hcl.colors(config$ano_fim - config$ano_inicio + 1, palette = "Blues 3"),
  main = "Gráfico sazonal — taxa mensal de suicídio (Brasil, 2000–2022)",
  ylab = "Taxa por 100.000 hab./mês",
  xlab = "Mês"
)
dev.off()
log_info_safe("Figura salva: fig_sazonalidade.png")

# --- 2.7 Subseries por mês ---------------------------------------------------

fig_subseries <- taxas_mensais_total |>
  mutate(mes_label = factor(month.abb[mes], levels = month.abb)) |>
  ggplot(aes(x = ano, y = taxa_mensal, group = mes_label)) +
  geom_line(color = cores_okabe["Total"], linewidth = 0.5) +
  geom_smooth(method = "loess", se = FALSE,
              color = "#D55E00", linewidth = 0.9) +
  facet_wrap(~mes_label, nrow = 2) +
  scale_x_continuous(breaks = c(2000, 2010, 2022)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Subseries mensais — tendência por mês (Brasil, 2000–2022)",
    subtitle = "Cada painel = um mês; linha laranja = suavização LOESS",
    x = "Ano", y = "Taxa por 100.000 hab./mês",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_subseries, "fig_subseries_mensal", w = 14, h = 7)

# --- 2.8 ACF/PACF mensais ----------------------------------------------------

path_acf_mensal <- file.path(config$dir_imagens, "fig_acf_pacf_mensal.png")
png(path_acf_mensal,
    width  = config$fig_width * config$dpi,
    height = (config$fig_height / 2) * config$dpi,
    res    = config$dpi)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
Acf(ts_mensal_total,
    lag.max = config$acf_lag_max_mensal,
    main    = "ACF — Taxa mensal (Total)")
Pacf(ts_mensal_total,
     lag.max = config$acf_lag_max_mensal,
     main    = "PACF — Taxa mensal (Total)")
par(mfrow = c(1, 1))
dev.off()
log_info_safe("Figura salva: fig_acf_pacf_mensal.png")


# =============================================================================
# BLOCO 3 — DECOMPOSIÇÃO STL
# =============================================================================
# Aplica decomposição STL (Seasonal and Trend decomposition using Loess) na
# série mensal — total e por sexo. STL separa tendência, sazonalidade e
# resíduo de forma robusta, sem exigir estacionariedade prévia.
# Série anual não tem componente sazonal — não é decomposta com STL.
# =============================================================================

log_info_safe("── Bloco 3: Decomposição STL ──")

# --- 3.1 STL total -----------------------------------------------------------

stl_total <- stl(ts_mensal_total,
                 s.window = config$stl_s_window,
                 robust   = TRUE)  # robust = TRUE reduz influência de outliers

# Extrai componentes em tibble para exportação e visualização via ggplot
componentes_stl <- tibble(
  data      = taxas_mensais_total$data,
  original  = as.numeric(ts_mensal_total),
  tendencia = as.numeric(stl_total$time.series[, "trend"]),
  sazonal   = as.numeric(stl_total$time.series[, "seasonal"]),
  residuo   = as.numeric(stl_total$time.series[, "remainder"])
)

salvar_tab(componentes_stl, "tab_stl_componentes")

# Figura STL total via ggplot (mais controle visual que plot.stl)
fig_stl_total <- componentes_stl |>
  pivot_longer(-data, names_to = "componente", values_to = "valor") |>
  mutate(componente = factor(
    componente,
    levels = c("original","tendencia","sazonal","residuo"),
    labels = c("Original","Tendência","Sazonalidade","Resíduo")
  )) |>
  ggplot(aes(x = data, y = valor)) +
  geom_line(color = cores_okabe["Total"], linewidth = 0.6) +
  facet_wrap(~componente, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Decomposição STL — Taxa mensal de suicídio (Total) — Brasil, 2000–2022",
    subtitle = glue("s.window = '{config$stl_s_window}', robust = TRUE"),
    x = NULL, y = NULL,
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_stl_total, "fig_stl_total", h = 10)

# --- 3.2 STL por sexo --------------------------------------------------------

stl_masc <- stl(ts_mensal_masc, s.window = config$stl_s_window, robust = TRUE)
stl_fem  <- stl(ts_mensal_fem,  s.window = config$stl_s_window, robust = TRUE)

datas_mensais <- taxas_mensais_total$data

extrair_stl <- function(stl_obj, datas, grupo) {
  tibble(
    grupo     = grupo,
    data      = datas,
    tendencia = as.numeric(stl_obj$time.series[, "trend"]),
    sazonal   = as.numeric(stl_obj$time.series[, "seasonal"]),
    residuo   = as.numeric(stl_obj$time.series[, "remainder"])
  )
}

stl_sexo <- bind_rows(
  extrair_stl(stl_masc, datas_mensais, "Masculino"),
  extrair_stl(stl_fem,  datas_mensais, "Feminino")
)

# Figura: tendência STL por sexo (componente mais relevante para manuscrito)
fig_stl_tendencia_sexo <- stl_sexo |>
  mutate(grupo = factor(grupo, levels = c("Masculino","Feminino"))) |>
  ggplot(aes(x = data, y = tendencia, color = grupo)) +
  geom_line(linewidth = 0.9) +
  annotate("rect", xmin = as.Date("2020-01-01"), xmax = as.Date("2022-12-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  scale_color_manual(
    values = cores_okabe[c("Masculino","Feminino")], name = NULL
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title    = "Componente de tendência STL por sexo — Brasil, 2000–2022",
    subtitle = "Tendência extraída pela decomposição STL (s.window = 'periodic', robust = TRUE)",
    x = NULL, y = "Tendência (taxa por 100.000 hab./mês)",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_stl_tendencia_sexo, "fig_stl_sexo")


# =============================================================================
# BLOCO 4 — TESTES FORMAIS DE SAZONALIDADE
# =============================================================================
# Três testes complementares: QS (Ljung-Box sazonal), Friedman (não-paramétrico)
# e Kruskal-Wallis (por mês). Todos aplicados na série mensal total.
# =============================================================================

log_info_safe("── Bloco 4: Testes de sazonalidade ──")

# QS test: versão sazonal do teste de Ljung-Box (Maravall, 2012)
# H0: ausência de sazonalidade
qs_result       <- seastests::qs(ts_mensal_total)

# Friedman test: não-paramétrico, testa se medianas diferem entre meses
friedman_result <- seastests::fried(ts_mensal_total)

# Kruskal-Wallis: testa diferença de médias entre os 12 meses
kw_result       <- seastests::kw(ts_mensal_total)

# Consolida resultados em tabela
tab_saz <- tibble(
  Teste = c("QS (Ljung-Box sazonal)", "Friedman (fried)", "Kruskal-Wallis (kw)"),
  Estatística = c(
    round(qs_result$stat,       3),
    round(friedman_result$stat, 3),
    round(kw_result$stat,       3)
  ),
  `p-valor` = c(
    round(qs_result$Pval,       4),
    round(friedman_result$Pval, 4),
    round(kw_result$Pval,       4)
  ),
  Conclusão = c(
    ifelse(qs_result$Pval       < 0.05, "Sazonalidade detectada", "Sem sazonalidade"),
    ifelse(friedman_result$Pval < 0.05, "Sazonalidade detectada", "Sem sazonalidade"),
    ifelse(kw_result$Pval       < 0.05, "Diferença entre meses",  "Sem diferença")
  )
) 

salvar_tab(tab_saz, "tab_testes_sazonalidade")
log_info_safe("Testes de sazonalidade concluídos:")
print(tab_saz)


# =============================================================================
# BLOCO 5 — COMPARATIVO DE GRANULARIDADE ANUAL vs MENSAL
# =============================================================================
# Documenta formalmente os critérios que orientam a escolha da granularidade
# da série para modelagem (cards #6–#8). Analogia com a comparação de métodos
# de interpolação (notas.qmd §3).
# =============================================================================

log_info_safe("── Bloco 5: Comparativo anual vs mensal ──")

# --- 5.1 Métricas objetivas --------------------------------------------------

# Número de observações
n_anual  <- length(ts_anual_total)
n_mensal <- length(ts_mensal_total)

# Autocorrelação máxima (indica estrutura temporal a ser capturada)
acf_anual_max  <- max(abs(Acf(ts_anual_total,  lag.max = 10,  plot = FALSE)$acf[-1]))
acf_mensal_max <- max(abs(Acf(ts_mensal_total, lag.max = 36, plot = FALSE)$acf[-1]))

# CV (coeficiente de variação) — variabilidade relativa das séries
cv <- function(x) sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE)
cv_anual  <- cv(as.numeric(ts_anual_total))
cv_mensal <- cv(as.numeric(ts_mensal_total))

# Sazonalidade detectada nos testes formais?
saz_detectada <- any(tab_saz$`p-valor` < 0.05)

# Constrói tabela comparativa
tab_comp_gran <- tibble(
  Critério = c(
    "Número de observações (n)",
    "Autocorrelação máxima nos primeiros lags",
    "Coeficiente de variação da série",
    "Sazonalidade detectável",
    "Componente sazonal SARIMA possível",
    "Risco de overfitting (série curta)",
    "Padrão predominante na literatura de suicídio",
    "Granularidade adotada para modelagem"
  ),
  `Série anual` = c(
    as.character(n_anual),
    round(acf_anual_max, 3),
    round(cv_anual, 3),
    "Não (frequency=1)",
    "Não (sem ciclo sazonal)",
    "Alto",
    "Frequente em estudos nacionais",
    "Secundária (comparação)"
  ),
  `Série mensal` = c(
    as.character(n_mensal),
    round(acf_mensal_max, 3),
    round(cv_mensal, 3),
    ifelse(saz_detectada, "Sim (testes formais)", "Não detectada"),
    "Sim — SARIMA(p,d,q)(P,D,Q)[12]",
    "Baixo",
    "Frequente em análises de intervenção",
    "Principal"
  )
)

salvar_tab(tab_comp_gran, "tab_comparativo_granularidade")
log_info_safe("Comparativo de granularidade concluído:")
print(tab_comp_gran)


# =============================================================================
# BLOCO 6 — BLOCO DESCRITIVO: TAXAS POR FAIXA ETÁRIA E REGIÃO
# =============================================================================
# Alimenta a discussão clínica do manuscrito e o card #9.
# Denominador por faixa etária: não disponível no pop_ibge (IBGE não fornece
# desagregação simultânea por faixa, sexo e macrorregião em série anual).
# Usamos populacao_total como denominador aproximado — adequado para
# comparação de tendências relativas, não para taxas absolutas precisas.
# NOTA: para taxas por faixa etária ajustadas, será necessário Tab7358
#       (Projeções 2018) — reservado para análise futura.
# =============================================================================

log_info_safe("── Bloco 6: Taxas descritivas por faixa etária e região ──")

# --- 6.1 Taxas por faixa etária e ano ----------------------------------------

obitos_faixa <- suicidio |>
  filter(!is.na(faixa_etaria)) |>
  count(ano, faixa_etaria, name = "obitos")

# Denominador aproximado: pop_total Brasil / n_faixas (distribuição proporcional)
# LIMITAÇÃO DOCUMENTADA: denominador ideal seria pop por faixa etária e ano
taxas_faixa <- obitos_faixa |>
  left_join(pop_brasil |> select(ano, populacao_total), by = "ano") |>
  mutate(taxa_aprox = (obitos / populacao_total) * config$escala_taxa)

salvar_tab(taxas_faixa, "tab_taxas_faixa_etaria")

fig_faixa <- ggplot(
  taxas_faixa |>
    mutate(faixa_etaria = factor(
      faixa_etaria,
      levels = c("< 15 anos","15–29 anos","30–44 anos","45–59 anos","60+ anos")
    )),
  aes(x = ano, y = taxa_aprox, color = faixa_etaria, group = faixa_etaria)
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  scale_color_brewer(palette = "Dark2", name = "Faixa etária") +
  annotate("rect", xmin = 2019.5, xmax = 2022.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  labs(
    title    = "Taxa de mortalidade por suicídio por faixa etária — Brasil, 2000–2022",
    subtitle = "Denominador: população total Brasil (taxa aproximada — ver nota metodológica)",
    x = NULL, y = "Taxa por 100.000 hab.",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria.\nNota: denominador ideal (pop por faixa etária) reservado para análise com Tab7358."
  )

salvar_fig(fig_faixa, "fig_taxas_faixa", h = 6)

# --- 6.2 Taxas por região e ano ----------------------------------------------

obitos_regiao <- suicidio |>
  filter(!is.na(regiao)) |>
  count(ano, regiao, name = "obitos")

taxas_regiao <- obitos_regiao |>
  left_join(
    pop_regioes |> select(regiao, ano, populacao_total),
    by = c("regiao", "ano")
  ) |>
  mutate(taxa = (obitos / populacao_total) * config$escala_taxa) |>
  arrange(regiao, ano)

salvar_tab(taxas_regiao, "tab_taxas_regiao")

fig_regiao <- ggplot(
  taxas_regiao,
  aes(x = ano, y = taxa, color = regiao, group = regiao)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_color_manual(values = cores_okabe[unique(taxas_regiao$regiao)], name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 4)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  labs(
    title    = "Taxa de mortalidade por suicídio por macrorregião — Brasil, 2000–2022",
    subtitle = "Taxas por 100 mil habitantes, denominador: população regional (IBGE)",
    x = NULL, y = "Taxa por 100.000 hab.",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_regiao, "fig_taxas_regiao")


# =============================================================================
# BLOCO 7 — SALVAR OBJETOS PARA MODELAGEM
# =============================================================================
# Exporta objetos ts e tsibble prontos para uso nos scripts de modelagem
# (cards #6–#8). Cada lista nomeada contém as três séries (total, M, F).
# =============================================================================

log_info_safe("── Bloco 7: Salvando objetos para modelagem ──")

series_anuais <- list(
  ts      = list(total = ts_anual_total,
                 masc  = ts_anual_masc,
                 fem   = ts_anual_fem),
  tsibble = tsibble_anual,
  taxas   = taxas_anuais
)

series_mensais <- list(
  ts      = list(total = ts_mensal_total,
                 masc  = ts_mensal_masc,
                 fem   = ts_mensal_fem),
  tsibble = tsibble_mensal,
  taxas   = taxas_mensais,
  stl     = list(total = stl_total,
                 masc  = stl_masc,
                 fem   = stl_fem)
)

saveRDS(series_anuais,  config$path_ts_anual)
saveRDS(series_mensais, config$path_ts_mensal)

log_info_safe("Séries anuais salvas em:  {config$path_ts_anual}")
log_info_safe("Séries mensais salvas em: {config$path_ts_mensal}")


# =============================================================================
# RESUMO FINAL
# =============================================================================

log_info_safe("── Resumo do script 04_descriptive.R ──")
log_info_safe("Período: {config$ano_inicio}–{config$ano_fim}")
log_info_safe("Série anual  — n = {n_anual} observações")
log_info_safe("Série mensal — n = {n_mensal} observações (frequency = 12)")
log_info_safe("Sazonalidade detectada pelos testes formais: {saz_detectada}")
log_info_safe("Tabelas geradas: {length(list.files(config$dir_tabelas))} arquivos")
log_info_safe("Figuras geradas: {length(list.files(config$dir_imagens))} arquivos")
log_info_safe("04_descriptive.R concluído com sucesso.")
