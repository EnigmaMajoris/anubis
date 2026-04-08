# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 05_modelagem.R
# OBJETIVO: Ajuste e diagnóstico de modelos de séries temporais — SARIMA e ETS
#           para as séries mensais de mortalidade por suicídio no Brasil
#           (2000–2022): total, masculino e feminino.
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-04-06
# VERSÃO  : 1.0
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# DEPENDÊNCIAS: 04_analitico_descritivo.R
# ENTRADA     : dados/limpos/series_ts_mensais.rds
#               dados/limpos/series_ts_anuais.rds
# SAIDA      : tabelas/modelagem/
#                 tab_testes_estacionariedade.csv
#                 tab_metricas_modelos.csv
#                 tab_ordens_sarima.csv
#                 tab_ordens_ets.csv
#                 tab_diagnostico_residuos.csv
#               imagens/modelagem/
#                 fig_diagnostico_sarima_total.png/.pdf
#                 fig_diagnostico_sarima_masc.png/.pdf
#                 fig_diagnostico_sarima_fem.png/.pdf
#                 fig_diagnostico_ets_total.png/.pdf
#                 fig_diagnostico_ets_masc.png/.pdf
#                 fig_diagnostico_ets_fem.png/.pdf
#                 fig_fitted_vs_obs_mensal.png/.pdf
#                 fig_acf_residuos.png/.pdf
#               dados/limpos/
#                 modelos_ajustados.rds
# -----------------------------------------------------------------------------
# NOTAS METODOLÓGICAS:
#   - Série principal para modelagem: MENSAL (n=276, frequency=12)
#   - Série anual usada como comparação (n=23, frequency=1)
#   - Testes de estacionariedade: ADF (H0: raiz unitária), KPSS (H0: estacionária),
#     PP (H0: raiz unitária). Conclusão robusta exige os 3 testes concordantes.
#   - SARIMA: seleção automática via auto.arima() com ic="aicc",
#     stepwise=FALSE e approximation=FALSE (busca exaustiva).
#   - ETS: seleção automática via ets() com ic="aicc". Componentes:
#     E = erro (A=aditivo, M=multiplicativo), T = tendência, S = sazonalidade.
#   - fitdf no Ljung-Box: ajustado pelo número de parâmetros do modelo para
#     evitar inflação do tipo I. Para SARIMA(p,d,q)(P,D,Q), fitdf = p+q+P+Q.
#   - AICc preferível ao AIC para n pequeno; BIC penaliza mais a complexidade.
#   - Seed: 42 — definido para reproducibilidade do auto.arima (random start).
# =============================================================================


# =============================================================================
# CONFIGURAÇÃO
# =============================================================================

config <- list(
  # Caminhos de entrada
  path_ts_mensal  = here::here("dados", "limpos", "series_ts_mensais.rds"),
  path_ts_anual   = here::here("dados", "limpos", "series_ts_anuais.rds"),

  # Caminhos de saída
  dir_tabelas     = here::here("tabelas", "modelagem"),
  dir_imagens     = here::here("imagens", "modelagem"),
  path_modelos    = here::here("dados", "limpos", "modelos_ajustados.rds"),

  # Parâmetros auto.arima
  arima_max_p     = 3L,
  arima_max_q     = 3L,
  arima_max_P     = 2L,
  arima_max_Q     = 2L,
  arima_ic        = "aicc",

  # Lags para testes de diagnóstico
  lb_lag1         = 12L,   # 1 ciclo sazonal
  lb_lag2         = 24L,   # 2 ciclos sazonais

  # Parâmetros de figura
  dpi             = 300L,
  fig_width       = 12,
  fig_height      = 7,

  # Seed para reproducibilidade
  seed            = 42L
)

set.seed(config$seed)


# =============================================================================
# PACOTES
# =============================================================================

library(tidyverse)    # dplyr, tidyr, ggplot2, purrr, stringr
library(here)         # here::here() — caminhos portáveis
library(glue)         # glue() — mensagens legíveis
library(logger)       # logging estruturado
library(scales)       # formatação de eixos
library(patchwork)    # composição de figuras

# Pacotes de séries temporais
library(forecast)     # auto.arima(), ets(), checkresiduals(), Acf()
library(tseries)      # adf.test(), kpss.test(), pp.test()
library(urca)         # ur.df(), ur.kpss() — testes com mais controle


# =============================================================================
# LOGGING
# =============================================================================

log_info_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_info(msg), error = function(e) message("[INFO] ", msg))
}
log_warn_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_warn(msg), error = function(e) message("[WARN] ", msg))
}

log_info_safe("Iniciando 05_modelagem.R | seed: {config$seed}")


# =============================================================================
# TEMA E FUNÇÕES AUXILIARES
# =============================================================================

cores_grupos <- c("Total" = "#725BA3", "Masculino" = "#0072B2", "Feminino" = "#D55E00")

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

#' Cria diretórios de saída se não existirem.
criar_diretorios <- function() {
  dir.create(config$dir_tabelas, showWarnings = FALSE, recursive = TRUE)
  dir.create(config$dir_imagens, showWarnings = FALSE, recursive = TRUE)
}

#' Salva figura em PNG e PDF.
salvar_fig <- function(plot, nome, w = config$fig_width, h = config$fig_height) {
  base <- file.path(config$dir_imagens, nome)
  ggsave(glue("{base}.png"), plot, width = w, height = h, dpi = config$dpi)
  ggsave(glue("{base}.pdf"), plot, width = w, height = h)
  log_info_safe("Figura salva: {nome}.png | {nome}.pdf")
}

#' Salva data.frame como CSV.
salvar_tab <- function(df, nome) {
  path <- file.path(config$dir_tabelas, glue("{nome}.csv"))
  write_csv(df, path)
  log_info_safe("Tabela salva: {nome}.csv ({nrow(df)} linhas)")
}


# =============================================================================
# CARREGAMENTO DOS DADOS
# =============================================================================

criar_diretorios()

log_info_safe("Carregando séries temporais...")

series_mensais <- readRDS(config$path_ts_mensal)
series_anuais  <- readRDS(config$path_ts_anual)

# Extrai objetos ts mensais — usados em auto.arima(), ets() e testes base
ts_total <- series_mensais$ts$total
ts_masc  <- series_mensais$ts$masc
ts_fem   <- series_mensais$ts$fem

# Extrai objetos ts anuais — usados na comparação de granularidade
ts_anual_total <- series_anuais$ts$total
ts_anual_masc  <- series_anuais$ts$masc
ts_anual_fem   <- series_anuais$ts$fem

log_info_safe(
  "Séries mensais carregadas — n={length(ts_total)} obs | frequency={frequency(ts_total)}"
)
log_info_safe(
  "Séries anuais carregadas  — n={length(ts_anual_total)} obs | frequency={frequency(ts_anual_total)}"
)


# =============================================================================
# BLOCO 1 — TESTES DE ESTACIONARIEDADE
# =============================================================================
# Três testes complementares:
#   ADF  (Dickey-Fuller aumentado): H0 = raiz unitária (não estacionária)
#   KPSS (Kwiatkowski et al.):      H0 = estacionária
#   PP   (Phillips-Perron):         H0 = raiz unitária (não estacionária)
#
# Interpretação conjunta:
#   ADF p > 0,05 + KPSS p < 0,05 + PP p > 0,05 → não estacionária (diferenciação necessária)
#   ADF p < 0,05 + KPSS p > 0,05 + PP p < 0,05 → estacionária
#
# ndiffs()  → número de diferenciações não-sazonais sugeridas (d)
# nsdiffs() → número de diferenciações sazonais sugeridas (D)
# =============================================================================

log_info_safe("── Bloco 1: Testes de estacionariedade ──")

#' Aplica ADF, KPSS e PP a uma série ts e retorna tibble com resultados.
#' @param serie Objeto ts.
#' @param nome  Rótulo da série para a tabela.
#' @param granularidade "mensal" ou "anual".
#' @return tibble com uma linha por série.
testar_estacionariedade <- function(serie, nome, granularidade = "mensal") {

  # ADF com k=12 lags para série mensal; k=3 para anual
  k_adf <- if (granularidade == "mensal") 12L else 3L
  adf   <- tseries::adf.test(serie, k = k_adf)
  kpss  <- tseries::kpss.test(serie, lshort = FALSE)
  pp    <- tseries::pp.test(serie)

  n_d  <- forecast::ndiffs(serie)
  n_D  <- if (frequency(serie) > 1) forecast::nsdiffs(serie) else NA_integer_

  # Conclusão integrada (convenção: pelo menos 2 dos 3 testes concordantes)
  nao_estac <- (adf$p.value > 0.05) | (pp$p.value > 0.05) | (kpss$p.value < 0.05)
  conclusao <- if (nao_estac) "Não estacionária — diferenciação necessária" else "Estacionária"

  log_info_safe(
    "{nome} | ADF p={round(adf$p.value,4)} | KPSS p={round(kpss$p.value,4)} | ",
    "PP p={round(pp$p.value,4)} | ndiffs={n_d} | nsdiffs={n_D}"
  )

  tibble(
    serie          = nome,
    granularidade  = granularidade,
    adf_stat       = round(adf$statistic,  4),
    adf_pvalor     = round(adf$p.value,    4),
    kpss_stat      = round(kpss$statistic, 4),
    kpss_pvalor    = round(kpss$p.value,   4),
    pp_stat        = round(pp$statistic,   4),
    pp_pvalor      = round(pp$p.value,     4),
    ndiffs         = n_d,
    nsdiffs        = n_D,
    conclusao      = conclusao
  )
}

# Séries mensais
estac_total <- testar_estacionariedade(ts_total, "Total",     "mensal")
estac_masc  <- testar_estacionariedade(ts_masc,  "Masculino", "mensal")
estac_fem   <- testar_estacionariedade(ts_fem,   "Feminino",  "mensal")

# Séries anuais (comparação)
estac_anual_total <- testar_estacionariedade(ts_anual_total, "Total",     "anual")
estac_anual_masc  <- testar_estacionariedade(ts_anual_masc,  "Masculino", "anual")
estac_anual_fem   <- testar_estacionariedade(ts_anual_fem,   "Feminino",  "anual")

tab_estac <- bind_rows(
  estac_total, estac_masc, estac_fem,
  estac_anual_total, estac_anual_masc, estac_anual_fem
)

salvar_tab(tab_estac, "tab_testes_estacionariedade")
log_info_safe("Testes de estacionariedade concluídos.")
print(tab_estac |> select(serie, granularidade, adf_pvalor, kpss_pvalor, pp_pvalor,
                           ndiffs, nsdiffs, conclusao))


# =============================================================================
# BLOCO 2 — AJUSTE SARIMA
# =============================================================================
# auto.arima() com busca exaustiva (stepwise=FALSE, approximation=FALSE).
# ic="aicc" é preferível ao AIC para amostras moderadas (penaliza mais parâmetros).
# O argumento trace=TRUE imprime os modelos avaliados — útil para documentação.
#
# Limites de busca (definidos no config):
#   p, q ≤ 3 (AR e MA não-sazonais)
#   P, Q ≤ 2 (AR e MA sazonais)
# A ordem de diferenciação (d, D) é determinada automaticamente pelos testes
# internos do auto.arima (baseados em KPSS e testes sazonais).
# =============================================================================

log_info_safe("── Bloco 2: Ajuste SARIMA ──")

#' Ajusta SARIMA via auto.arima() e registra log com as ordens selecionadas.
#' @param serie Objeto ts.
#' @param nome  Rótulo da série para mensagens.
#' @return Objeto Arima ajustado.
ajustar_sarima <- function(serie, nome) {
  log_info_safe("Ajustando SARIMA — {nome} (busca exaustiva, pode demorar)...")

  modelo <- forecast::auto.arima(
    serie,
    seasonal      = TRUE,
    stepwise      = FALSE,        # busca exaustiva — mais lenta, mais confiável
    approximation = FALSE,        # ajuste exato — evita aproximações numéricas
    ic            = config$arima_ic,
    max.p         = config$arima_max_p,
    max.q         = config$arima_max_q,
    max.P         = config$arima_max_P,
    max.Q         = config$arima_max_Q,
    trace         = FALSE         # TRUE para ver todos os modelos avaliados
  )

  ord  <- arimaorder(modelo)
  sord <- if (!is.null(modelo$arma)) {
    sprintf("(%d,%d,%d)[%d]", modelo$arma[3], modelo$arma[6],
            modelo$arma[4], frequency(serie))
  } else { "N/A" }

  log_info_safe(
    "SARIMA {nome} — ordem: ({ord[1]},{ord[3]},{ord[2]})({sord}) | ",
    "AICc={round(modelo$aicc,2)} | BIC={round(modelo$bic,2)}"
  )

  return(modelo)
}

# Ajuste nas três séries mensais
sarima_total <- ajustar_sarima(ts_total, "Total")
sarima_masc  <- ajustar_sarima(ts_masc,  "Masculino")
sarima_fem   <- ajustar_sarima(ts_fem,   "Feminino")

# Ajuste nas séries anuais (comparação)
sarima_anual_total <- ajustar_sarima(ts_anual_total, "Total anual")
sarima_anual_masc  <- ajustar_sarima(ts_anual_masc,  "Masculino anual")
sarima_anual_fem   <- ajustar_sarima(ts_anual_fem,   "Feminino anual")

# Tabela de ordens SARIMA selecionadas
extrair_ordens_sarima <- function(modelo, nome, granularidade) {
  ord <- arimaorder(modelo)
  tibble(
    serie         = nome,
    granularidade = granularidade,
    p = ord["p"], d = ord["d"], q = ord["q"],
    P = ord["P"], D = ord["D"], Q = ord["Q"],
    s = frequency(modelo$x),
    aicc = round(modelo$aicc, 2),
    bic  = round(modelo$bic,  2),
    log_lik = round(modelo$loglik, 2)
  )
}

tab_ordens_sarima <- bind_rows(
  extrair_ordens_sarima(sarima_total,       "Total",          "mensal"),
  extrair_ordens_sarima(sarima_masc,        "Masculino",      "mensal"),
  extrair_ordens_sarima(sarima_fem,         "Feminino",       "mensal"),
  extrair_ordens_sarima(sarima_anual_total, "Total",          "anual"),
  extrair_ordens_sarima(sarima_anual_masc,  "Masculino",      "anual"),
  extrair_ordens_sarima(sarima_anual_fem,   "Feminino",       "anual")
)

salvar_tab(tab_ordens_sarima, "tab_ordens_sarima")
log_info_safe("Ordens SARIMA:")
print(tab_ordens_sarima)


# =============================================================================
# BLOCO 3 — AJUSTE ETS
# =============================================================================
# ETS (Error, Trend, Seasonality) é uma família de modelos de suavização
# exponencial. Componentes:
#   E (erro): A = aditivo, M = multiplicativo
#   T (tendência): N = nenhuma, A = aditiva, Ad = amortecida
#   S (sazonalidade): N = nenhuma, A = aditiva, M = multiplicativa
#
# Dada a sazonalidade aditiva detectada na STL (amplitude constante),
# ETS(A,A,A) ou ETS(A,Ad,A) são os candidatos mais prováveis.
# ets() seleciona automaticamente via AICc.
# =============================================================================

log_info_safe("── Bloco 3: Ajuste ETS ──")

#' Ajusta ETS e registra log com o componente selecionado.
#' @param serie Objeto ts.
#' @param nome  Rótulo da série para mensagens.
#' @return Objeto ets ajustado.
ajustar_ets <- function(serie, nome) {
  log_info_safe("Ajustando ETS — {nome}...")

  modelo <- forecast::ets(serie, ic = config$arima_ic)

  log_info_safe(
    "ETS {nome} — modelo: {modelo$method} | ",
    "AICc={round(modelo$aicc,2)} | BIC={round(modelo$bic,2)}"
  )

  return(modelo)
}

ets_total <- ajustar_ets(ts_total, "Total")
ets_masc  <- ajustar_ets(ts_masc,  "Masculino")
ets_fem   <- ajustar_ets(ts_fem,   "Feminino")

ets_anual_total <- ajustar_ets(ts_anual_total, "Total anual")
ets_anual_masc  <- ajustar_ets(ts_anual_masc,  "Masculino anual")
ets_anual_fem   <- ajustar_ets(ts_anual_fem,   "Feminino anual")

# Tabela de ordens ETS
extrair_ordens_ets <- function(modelo, nome, granularidade) {
  tibble(
    serie         = nome,
    granularidade = granularidade,
    metodo        = modelo$method,
    aicc          = round(modelo$aicc, 2),
    bic           = round(modelo$bic,  2),
    log_lik       = round(modelo$loglik, 2)
  )
}

tab_ordens_ets <- bind_rows(
  extrair_ordens_ets(ets_total,       "Total",     "mensal"),
  extrair_ordens_ets(ets_masc,        "Masculino", "mensal"),
  extrair_ordens_ets(ets_fem,         "Feminino",  "mensal"),
  extrair_ordens_ets(ets_anual_total, "Total",     "anual"),
  extrair_ordens_ets(ets_anual_masc,  "Masculino", "anual"),
  extrair_ordens_ets(ets_anual_fem,   "Feminino",  "anual")
)

salvar_tab(tab_ordens_ets, "tab_ordens_ets")
log_info_safe("Ordens ETS:")
print(tab_ordens_ets)


# =============================================================================
# BLOCO 4 — DIAGNÓSTICO DE RESÍDUOS
# =============================================================================
# Três verificações para cada modelo:
#   1. Ljung-Box (lag=12 e lag=24): testa autocorrelação nos resíduos.
#      H0: resíduos são ruído branco.
#      fitdf ajustado pelo número de parâmetros livres do modelo (p+q+P+Q)
#      para evitar inflação do erro tipo I.
#   2. Shapiro-Wilk: testa normalidade dos resíduos.
#      H0: resíduos são normalmente distribuídos.
#      Normalidade NÃO é necessária para previsão pontual, mas é necessária
#      para intervalos de previsão paramétricos.
#   3. Figura checkresiduals: ACF dos resíduos + histograma + série temporal.
# =============================================================================

log_info_safe("── Bloco 4: Diagnóstico de resíduos ──")

#' Executa diagnóstico completo de resíduos e salva figura.
#' @param modelo Objeto Arima ou ets.
#' @param nome   Rótulo para mensagens e nome do arquivo.
#' @param tipo   "sarima" ou "ets" — usado no nome do arquivo.
#' @return tibble com uma linha de resultados.
diagnosticar_residuos <- function(modelo, nome, tipo) {

  res <- residuals(modelo)

  # Número de parâmetros livres para ajuste do grau de liberdade no Ljung-Box
  # Para SARIMA: p+q+P+Q; para ETS: não há parâmetros AR/MA, fitdf=0
  fitdf_val <- if (inherits(modelo, "Arima")) {
    ord <- arimaorder(modelo)
    as.integer(ord["p"] + ord["q"] + ord["P"] + ord["Q"])
  } else { 0L }

  lb12 <- Box.test(res, lag = config$lb_lag1, type = "Ljung-Box", fitdf = fitdf_val)
  lb24 <- Box.test(res, lag = config$lb_lag2, type = "Ljung-Box", fitdf = fitdf_val)
  sw   <- shapiro.test(as.numeric(res))

  log_info_safe(
    "Diagnóstico {nome} | LB12 p={round(lb12$p.value,4)} | ",
    "LB24 p={round(lb24$p.value,4)} | SW p={round(sw$p.value,4)}"
  )

  # --------------------------------------------------------------------------
  # Figura de diagnóstico reconstruída com ggplot2 (3 painéis via patchwork):
  #   P1 — Série temporal dos resíduos
  #   P2 — ACF dos resíduos (até lag 36)
  #   P3 — Histograma com curva normal sobreposta
  #
  # Motivação: checkresiduals() do forecast usa layout() do base R e encerra
  # o contexto gráfico ao final do último painel, impedindo qualquer chamada
  # posterior (title, mtext) sobre o dispositivo PNG — erro "plot.new has not
  # been called yet". A reimplementação em ggplot2 + patchwork elimina esse
  # problema e mantém consistência com tema_anubis.
  # --------------------------------------------------------------------------

  res_num <- as.numeric(res)
  n_res   <- length(res_num)

  # Reconstrói índice temporal para o eixo x do painel da série
  t_index <- if (!is.null(tsp(res))) {
    time(res)
  } else {
    seq_along(res_num)
  }
  df_res <- tibble(t = as.numeric(t_index), residuo = res_num)

  # P1 — série temporal dos resíduos
  p1 <- ggplot(df_res, aes(x = t, y = residuo)) +
    geom_line(color = "grey40", linewidth = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#D55E00") +
    labs(title = "Resíduos", x = NULL, y = "Resíduo") +
    scale_x_continuous(labels = label_number(accuracy = 1))

  # P2 — ACF dos resíduos
  acf_obj  <- acf(res_num, lag.max = 36, plot = FALSE)
  ci_bound <- qnorm(0.975) / sqrt(n_res)   # banda de confiança 95%
  df_acf   <- tibble(
    lag  = as.numeric(acf_obj$lag[-1]),
    acf  = as.numeric(acf_obj$acf[-1])
  )
  p2 <- ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "grey60") +
    geom_hline(yintercept =  ci_bound, linetype = "dashed", color = "#0072B2", linewidth = 0.5) +
    geom_hline(yintercept = -ci_bound, linetype = "dashed", color = "#0072B2", linewidth = 0.5) +
    geom_segment(aes(xend = lag, yend = 0), color = "grey40", linewidth = 0.6) +
    geom_point(size = 1.2, color = "grey30") +
    scale_x_continuous(breaks = seq(0, 36, by = 6)) +
    labs(title = "ACF dos resíduos", x = "Lag", y = "ACF")

  # P3 — histograma com curva normal
  mu_res <- mean(res_num, na.rm = TRUE)
  sd_res <- sd(res_num,   na.rm = TRUE)
  p3 <- ggplot(tibble(x = res_num), aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 20, fill = "grey70", color = "white", linewidth = 0.3) +
    stat_function(
      fun  = dnorm,
      args = list(mean = mu_res, sd = sd_res),
      color = "#D55E00", linewidth = 0.8
    ) +
    labs(title = "Distribuição dos resíduos", x = "Resíduo", y = "Densidade")

  # Composição com patchwork
  fig_diag <- (p1 / (p2 | p3)) +
    plot_annotation(
      title    = glue("Diagnóstico de resíduos — {nome}"),
      subtitle = glue(
        "Ljung-Box lag 12: p={round(lb12$p.value,4)} | ",
        "lag 24: p={round(lb24$p.value,4)} | ",
        "Shapiro-Wilk: p={round(sw$p.value,4)}"
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey40")
      )
    )

  nome_fig <- glue("fig_diagnostico_{tipo}_{tolower(gsub(' ', '_', nome))}")
  salvar_fig(fig_diag, nome_fig)

  tibble(
    modelo        = nome,
    tipo          = tipo,
    fitdf         = fitdf_val,
    lb12_stat     = round(lb12$statistic, 3),
    lb12_pvalor   = round(lb12$p.value,   4),
    lb24_stat     = round(lb24$statistic, 3),
    lb24_pvalor   = round(lb24$p.value,   4),
    sw_stat       = round(sw$statistic,   4),
    sw_pvalor     = round(sw$p.value,     4),
    res_media     = round(mean(res, na.rm = TRUE), 6),
    res_dp        = round(sd(res,   na.rm = TRUE), 6),
    res_ok_lb12   = lb12$p.value > 0.05,
    res_ok_lb24   = lb24$p.value > 0.05,
    res_normal_sw = sw$p.value   > 0.05
  )
}

diag_sarima_total <- diagnosticar_residuos(sarima_total, "Total",     "sarima")
diag_sarima_masc  <- diagnosticar_residuos(sarima_masc,  "Masculino", "sarima")
diag_sarima_fem   <- diagnosticar_residuos(sarima_fem,   "Feminino",  "sarima")
diag_ets_total    <- diagnosticar_residuos(ets_total,    "Total",     "ets")
diag_ets_masc     <- diagnosticar_residuos(ets_masc,     "Masculino", "ets")
diag_ets_fem      <- diagnosticar_residuos(ets_fem,      "Feminino",  "ets")

tab_diag <- bind_rows(
  diag_sarima_total, diag_sarima_masc, diag_sarima_fem,
  diag_ets_total,    diag_ets_masc,    diag_ets_fem
)

salvar_tab(tab_diag, "tab_diagnostico_residuos")
log_info_safe("Diagnóstico de resíduos concluído:")
print(tab_diag |> select(modelo, tipo, lb12_pvalor, lb24_pvalor,
                          sw_pvalor, res_ok_lb12, res_ok_lb24, res_normal_sw))


# =============================================================================
# BLOCO 5 — COMPARAÇÃO DE MÉTRICAS (SARIMA vs ETS)
# =============================================================================
# AIC, BIC e AICc são métricas de ajuste in-sample (não de previsão).
# Servem para escolha de ordem/estrutura, NÃO como medida final de desempenho.
# A comparação de previsão out-of-sample será feita no card #8 (07_validation.R).
#
# NOTA: AIC e BIC de modelos SARIMA e ETS NÃO são diretamente comparáveis
# entre si — as famílias de modelos têm funções de verossimilhança diferentes.
# Comparamos dentro da mesma família (SARIMA total vs SARIMA masc vs fem, etc.)
# e usamos as métricas de validação do card #8 para comparação cruzada.
# =============================================================================

log_info_safe("── Bloco 5: Métricas de ajuste ──")

extrair_metricas <- function(modelo, nome, tipo, granularidade) {
  tibble(
    modelo        = glue("{tipo}_{tolower(nome)}_{granularidade}"),
    serie         = nome,
    tipo          = tipo,
    granularidade = granularidade,
    aic           = round(AIC(modelo),    2),
    bic           = round(BIC(modelo),    2),
    aicc          = round(modelo$aicc,    2),
    log_lik       = round(modelo$loglik,  2),
    sigma2        = round(modelo$sigma2,  6)
  )
}

tab_metricas <- bind_rows(
  extrair_metricas(sarima_total,       "Total",     "SARIMA", "mensal"),
  extrair_metricas(sarima_masc,        "Masculino", "SARIMA", "mensal"),
  extrair_metricas(sarima_fem,         "Feminino",  "SARIMA", "mensal"),
  extrair_metricas(ets_total,          "Total",     "ETS",    "mensal"),
  extrair_metricas(ets_masc,           "Masculino", "ETS",    "mensal"),
  extrair_metricas(ets_fem,            "Feminino",  "ETS",    "mensal"),
  extrair_metricas(sarima_anual_total, "Total",     "SARIMA", "anual"),
  extrair_metricas(sarima_anual_masc,  "Masculino", "SARIMA", "anual"),
  extrair_metricas(sarima_anual_fem,   "Feminino",  "SARIMA", "anual"),
  extrair_metricas(ets_anual_total,    "Total",     "ETS",    "anual"),
  extrair_metricas(ets_anual_masc,     "Masculino", "ETS",    "anual"),
  extrair_metricas(ets_anual_fem,      "Feminino",  "ETS",    "anual")
)

salvar_tab(tab_metricas, "tab_metricas_modelos")
log_info_safe("Métricas de ajuste (mensal):")
print(tab_metricas |> filter(granularidade == "mensal") |>
        select(modelo, aic, bic, aicc, sigma2))


# =============================================================================
# BLOCO 6 — FIGURA: AJUSTADO vs OBSERVADO
# =============================================================================
# Comparação visual dos valores ajustados (fitted) de cada modelo com a
# série observada. Útil para detectar má especificação visualmente.
# =============================================================================

log_info_safe("── Bloco 6: Figuras ajustado vs observado ──")

# Extrai fitted values e converte para tibble com data
extrair_fitted <- function(modelo, ts_obs, nome, tipo) {
  fitted_vals <- as.numeric(fitted(modelo))
  obs_vals    <- as.numeric(ts_obs)
  n           <- length(obs_vals)

  # Reconstrói vetor de datas a partir de time()
  datas <- zoo::as.Date(
    zoo::as.yearmon(time(ts_obs))
  )

  tibble(
    data     = datas,
    observado = obs_vals,
    ajustado  = fitted_vals,
    residuo   = obs_vals - fitted_vals,
    serie     = nome,
    tipo      = tipo
  )
}

fitted_df <- bind_rows(
  extrair_fitted(sarima_total, ts_total, "Total",     "SARIMA"),
  extrair_fitted(sarima_masc,  ts_masc,  "Masculino", "SARIMA"),
  extrair_fitted(sarima_fem,   ts_fem,   "Feminino",  "SARIMA"),
  extrair_fitted(ets_total,    ts_total, "Total",     "ETS"),
  extrair_fitted(ets_masc,     ts_masc,  "Masculino", "ETS"),
  extrair_fitted(ets_fem,      ts_fem,   "Feminino",  "ETS")
) |>
  mutate(
    serie = factor(serie, levels = c("Total", "Masculino", "Feminino")),
    tipo  = factor(tipo,  levels = c("SARIMA", "ETS"))
  )

fig_fitted <- ggplot(fitted_df, aes(x = data)) +
  geom_line(aes(y = observado, color = "Observado"), linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = ajustado,  color = "Ajustado"),  linewidth = 0.7, alpha = 0.9) +
  facet_grid(serie ~ tipo, scales = "free_y") +
  scale_color_manual(
    values = c("Observado" = "grey40", "Ajustado" = "#D55E00"),
    name   = NULL
  ) +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title    = "Valores ajustados vs observados — SARIMA e ETS (2000–2022)",
    subtitle = "Série mensal de taxa de mortalidade por suicídio por 100.000 hab.",
    x = NULL, y = "Taxa por 100.000 hab./mês",
    caption  = "Fonte: SIM/DATASUS e IBGE. Elaboração própria."
  )

salvar_fig(fig_fitted, "fig_fitted_vs_obs_mensal", h = 10)


# =============================================================================
# BLOCO 7 — FIGURA: ACF DOS RESÍDUOS (COMPARATIVO)
# =============================================================================
# ACF dos resíduos devem estar dentro das bandas de confiança para todos os
# lags — indica ausência de autocorrelação residual (ruído branco).
# =============================================================================

log_info_safe("── Bloco 7: ACF comparativa dos resíduos ──")

path_acf_res <- file.path(config$dir_imagens, "fig_acf_residuos.png")
png(path_acf_res,
    width  = config$fig_width  * config$dpi * 1.5,
    height = config$fig_height * config$dpi,
    res    = config$dpi)

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

Acf(residuals(sarima_total), lag.max = 36,
    main = "ACF resíduos — SARIMA Total")
Acf(residuals(sarima_masc),  lag.max = 36,
    main = "ACF resíduos — SARIMA Masculino")
Acf(residuals(sarima_fem),   lag.max = 36,
    main = "ACF resíduos — SARIMA Feminino")
Acf(residuals(ets_total),    lag.max = 36,
    main = "ACF resíduos — ETS Total")
Acf(residuals(ets_masc),     lag.max = 36,
    main = "ACF resíduos — ETS Masculino")
Acf(residuals(ets_fem),      lag.max = 36,
    main = "ACF resíduos — ETS Feminino")

par(mfrow = c(1, 1))
dev.off()
log_info_safe("Figura salva: fig_acf_residuos.png")


# =============================================================================
# BLOCO 8 — SALVAR OBJETOS PARA MODELAGEM SUBSEQUENTE
# =============================================================================
# Os objetos são salvos em lista nomeada para uso nos cards #7 (ARIMAX) e
# #8 (validação). A estrutura espelha a organização das séries_ts_mensais.rds.
# =============================================================================

log_info_safe("── Bloco 8: Salvando objetos para cards #7 e #8 ──")

modelos_ajustados <- list(
  mensal = list(
    sarima = list(total = sarima_total, masc = sarima_masc, fem = sarima_fem),
    ets    = list(total = ets_total,    masc = ets_masc,    fem = ets_fem)
  ),
  anual  = list(
    sarima = list(total = sarima_anual_total,
                  masc  = sarima_anual_masc,
                  fem   = sarima_anual_fem),
    ets    = list(total = ets_anual_total,
                  masc  = ets_anual_masc,
                  fem   = ets_anual_fem)
  ),
  # Metadados úteis para os scripts subsequentes
  meta = list(
    ts_mensais = list(total = ts_total, masc = ts_masc, fem = ts_fem),
    ts_anuais  = list(total = ts_anual_total,
                      masc  = ts_anual_masc,
                      fem   = ts_anual_fem),
    tab_metricas           = tab_metricas,
    tab_diag_residuos      = tab_diag,
    tab_ordens_sarima      = tab_ordens_sarima,
    tab_ordens_ets         = tab_ordens_ets,
    tab_estacionariedade   = tab_estac
  )
)

saveRDS(modelos_ajustados, config$path_modelos)
log_info_safe("Modelos salvos em: {config$path_modelos}")


# =============================================================================
# RESUMO FINAL
# =============================================================================

log_info_safe("── Resumo do script 05_modelagem.R ──")
log_info_safe("Seed utilizado          : {config$seed}")
log_info_safe("Série principal         : mensal (n={length(ts_total)}, freq=12)")
log_info_safe("Série de comparação     : anual  (n={length(ts_anual_total)}, freq=1)")
log_info_safe("Tabelas geradas         : {length(list.files(config$dir_tabelas))} arquivos")
log_info_safe("Figuras geradas         : {length(list.files(config$dir_imagens))} arquivos")

# Log compacto das ordens SARIMA selecionadas
log_info_safe("Ordens SARIMA (mensais):")
for (i in seq_len(nrow(tab_ordens_sarima |> filter(granularidade == "mensal")))) {
  r <- tab_ordens_sarima |> filter(granularidade == "mensal") |> slice(i)
  log_info_safe(
    "  {r$serie}: SARIMA({r$p},{r$d},{r$q})({r$P},{r$D},{r$Q})[{r$s}] | AICc={r$aicc}"
  )
}

# Log compacto das famílias ETS selecionadas
log_info_safe("Modelos ETS (mensais):")
for (i in seq_len(nrow(tab_ordens_ets |> filter(granularidade == "mensal")))) {
  r <- tab_ordens_ets |> filter(granularidade == "mensal") |> slice(i)
  log_info_safe("  {r$serie}: {r$metodo} | AICc={r$aicc}")
}

log_info_safe("05_modelagem.R concluído com sucesso.")
