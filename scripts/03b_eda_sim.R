# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 03b_eda_sim.R
# OBJETIVO: Análise exploratória e validação dos microdados SIM pós-limpeza
#           — cobertura temporal, completude, distribuição CID, variáveis
#             criadas, série bruta e flags de atenção para o manuscrito
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2025-04-03
# VERSÃO  : 1.0
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# DEPENDÊNCIAS: 03_clean_dados.R
# ENTRADA       : dados/brutos/sim_2000_2022.rds       
#                 dados/limpos/suicidio_limpo.rds       
# SAIDA         : tabelas/eda_sim/
#                 tab_cobertura_temporal.csv
#                 tab_completude_bruto.csv
#                 tab_completude_limpo.csv
#                 tab_cid_subcod.csv
#                 tab_distribuicao_vars.csv
#                 tab_cruzamento_sexo_faixa.csv
#                 tab_serie_bruta.csv
#                 tab_flags_atencao.csv
#               imagens/eda_sim/
#                 fig_cobertura_temporal.png / .pdf
#                 fig_completude.png / .pdf
#                 fig_cid_subcod.png / .pdf
#                 fig_distribuicao_vars.png / .pdf
#                 fig_serie_bruta.png / .pdf
# -----------------------------------------------------------------------------
# NOTAS:
#   - Este script NÃO calcula taxas (será tratado em outro script)
#   - Thresholds dos flags são editáveis em config
#   - Figuras seguem paleta Okabe–Ito, fonte Serif, 300 dpi
#   - Seed: N/A (sem aleatoriedade neste script)
# =============================================================================

# =============================================================================
# CONFIGURAÇÃO 
# =============================================================================

config <- list(
  ano_inicio = 2000L,
  ano_fim    = 2022L,

  # Thresholds para flags de atenção
  flag_n_minimo_ano    = 100L,   # anos com total < N geram alerta
  flag_pct_missing_max = 5.0,    # % de missing acima deste valor gera alerta
  flag_pct_ignorado_max = 2.0,   # % de "ignorado" acima deste valor gera alerta

  # Caminhos
  path_bruto       = here::here("dados", "brutos", "sim_suicidio_2000_2022.rds"),
  path_limpo       = here::here("dados", "limpos", "suicidio_limpo.rds"),
  dir_tabelas      = here::here("tabelas", "eda_sim"),
  dir_imagens      = here::here("imagens", "eda_sim"),

  # Figura
  dpi              = 300L,
  fig_width        = 10,
  fig_height        = 6,
  fonte_base       = "serif"
)

# =============================================================================
# PACOTES
# =============================================================================

library(tidyverse)
library(here)
library(glue)
library(logger)
library(scales)
library(patchwork)

# Garante diretórios de saída
dir.create(config$dir_tabelas, showWarnings = FALSE, recursive = TRUE)
dir.create(config$dir_imagens, showWarnings = FALSE, recursive = TRUE)

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

log_info_safe("Iniciando 03b_eda_sim.R")

# =============================================================================
# PALETA E TEMA
# =============================================================================

# Okabe–Ito: acessível a daltônicos e reproduzível em P&B
cores_okabe <- c(
  "Masculino"     = "#80C0E4",
  "Feminino"      = "#D672A1",
  "Total"         = "#725BA3",
  "Norte"         = "#56B4E9",
  "Nordeste"      = "#E69F00",
  "Sudeste"       = "#009E73",
  "Sul"           = "#0072B2",
  "Centro-Oeste"  = "#CC79A7"
)

tema_anubis <- theme_minimal() +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    plot.caption  = element_text(size = 8,  color = "grey50"),
    axis.title    = element_text(size = 10),
    legend.position = "bottom"
  )

theme_set(tema_anubis)

# =============================================================================
# FUNÇÕES AUXILIARES
# =============================================================================

salvar_fig <- function(plot, nome, w = config$fig_width, h = config$fig_height) {
  base <- file.path(config$dir_imagens, nome)
  ggsave(glue("{base}.png"), plot, width = w, height = h, dpi = config$dpi)
  ggsave(glue("{base}.pdf"), plot, width = w, height = h)
  log_info_safe("Figura salva: {nome}.png | {nome}.pdf")
}

salvar_tab <- function(df, nome) {
  path <- file.path(config$dir_tabelas, glue("{nome}.csv"))
  write_csv(df, path)
  log_info_safe("Tabela salva: {nome}.csv ({nrow(df)} linhas)")
}

# =============================================================================
# CARREGAMENTO
# =============================================================================

log_info_safe("Carregando dados...")

sim_bruto <- readRDS(config$path_bruto) |>
  filter(!is.na(CAUSABAS), str_detect(CAUSABAS, "^X[67]\\d|^X8[0-4]"))

sim_limpo <- readRDS(config$path_limpo)

log_info_safe("Bruto (apenas CID suicídio): {nrow(sim_bruto)} registros")
log_info_safe("Limpo: {nrow(sim_limpo)} registros")

# =============================================================================
# BLOCO 1: COBERTURA TEMPORAL
# =============================================================================

log_info_safe("── Bloco 1: Cobertura temporal ──")

tab_cobertura <- sim_limpo |>
  count(ano, sexo_label, name = "n_obitos") |>
  pivot_wider(names_from = sexo_label, values_from = n_obitos,
              values_fill = 0L) |>
  mutate(Total = Masculino + Feminino) |>
  arrange(ano)

salvar_tab(tab_cobertura, "tab_cobertura_temporal")
print(tab_cobertura)

# Série de totais com linha de tendência suavizada
tab_serie_total <- tab_cobertura |>
  select(ano, Masculino, Feminino, Total) |>
  pivot_longer(-ano, names_to = "grupo", values_to = "n_obitos")

fig_cobertura <- ggplot(
  tab_serie_total |> filter(grupo != "Total"),
  aes(x = ano, y = n_obitos, color = grupo, group = grupo)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_line(
    data = tab_serie_total |> filter(grupo == "Total"),
    aes(x = ano, y = n_obitos),
    color = cores_okabe["Total"], linewidth = 1.2, linetype = "dashed"
  ) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  annotate("text", x = 2021, y = Inf, label = "COVID-19",
           vjust = 1.5, size = 3, color = "grey40") +
  scale_color_manual(values = cores_okabe[c("Masculino", "Feminino")],
                     name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Óbitos por suicídio (CID X60–X84) — Brasil, 2000–2022",
    subtitle = "Contagem bruta por sexo (linha tracejada = total)",
    x = NULL, y = "Número de óbitos",
    caption = "Fonte: SIM/DATASUS. Elaboração própria."
  )

salvar_fig(fig_cobertura, "fig_cobertura_temporal")

# =============================================================================
# BLOCO 2: COMPLETUDE (MISSINGS E IGNORADOS) 
# =============================================================================

log_info_safe("── Bloco 2: Completude ──")

vars_interesse <- c("CAUSABAS", "DTOBITO", "SEXO", "IDADE", "CODMUNRES")

calcular_completude <- function(dados, rotulo) {
  n_total <- nrow(dados)
  dados |>
    summarise(
      across(
        all_of(vars_interesse),
        list(
          n_total   = ~n_total,
          n_valido  = ~sum(!is.na(.)),
          n_missing = ~sum(is.na(.)),
          pct_missing = ~round(100 * mean(is.na(.)), 2)
        ),
        .names = "{.col}__{.fn}"
      )
    ) |>
    pivot_longer(everything(),
                 names_to  = c("variavel", "metrica"),
                 names_sep = "__") |>
    pivot_wider(names_from = metrica, values_from = value) |>
    mutate(base = rotulo)
}

tab_comp_bruto <- calcular_completude(sim_bruto, "bruto_pos_cid")
tab_comp_limpo <- calcular_completude(sim_limpo, "limpo")

salvar_tab(tab_comp_bruto, "tab_completude_bruto")
salvar_tab(tab_comp_limpo, "tab_completude_limpo")

# Figura: comparação de % missing antes/depois
tab_comp_fig <- bind_rows(tab_comp_bruto, tab_comp_limpo) |>
  mutate(
    base = factor(base,
                  levels = c("bruto_pos_cid", "limpo"),
                  labels = c("Pós-filtro CID\n(antes limpeza)",
                             "Base limpa\n(após limpeza)"))
  )

fig_completude <- ggplot(tab_comp_fig,
                         aes(x = variavel, y = pct_missing, fill = base)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = config$flag_pct_missing_max,
             linetype = "dashed", color = "firebrick", linewidth = 0.7) +
  annotate("text", x = 0.5, y = config$flag_pct_missing_max + 0.3,
           label = glue("Limiar de alerta ({config$flag_pct_missing_max}%)"),
           hjust = 0, size = 3, color = "firebrick") +
  scale_fill_manual(values = c("#999999", "#0072B2"), name = NULL) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1)) +
  labs(
    title    = "Completude das variáveis — antes e após limpeza",
    subtitle = "% de valores ausentes por variável",
    x = NULL, y = "% missing",
    caption = "Fonte: SIM/DATASUS. Elaboração própria."
  )

salvar_fig(fig_completude, "fig_completude")

# =============================================================================
# BLOCO 3: DISTRIBUIÇÃO DOS SUBCÓDIGOS CID
# =============================================================================
log_info_safe("── Bloco 3: Distribuição CID X60–X84 ──")

# Todos os subcódigos esperados (X60–X84)
cids_esperados <- tibble(
  CAUSABAS_3 = c(
    paste0("X", 60:69),
    paste0("X", 70:79),
    paste0("X", 80:84)
  )
)

tab_cid <- sim_limpo |>
  mutate(CAUSABAS_3 = substr(CAUSABAS, 1, 3)) |>
  count(CAUSABAS_3, name = "n") |>
  right_join(cids_esperados, by = "CAUSABAS_3") |>
  replace_na(list(n = 0L)) |>
  mutate(
    pct         = round(100 * n / sum(n), 2),
    ausente     = n == 0,
    # Descrição resumida dos grupos CID
    grupo_cid = case_when(
      CAUSABAS_3 %in% paste0("X", 60:65) ~ "Intoxicação/Medicamentos",
      CAUSABAS_3 == "X66"                ~ "Solventes/Gases",
      CAUSABAS_3 %in% paste0("X", 67:69) ~ "Outros agentes químicos",
      CAUSABAS_3 %in% paste0("X", 70:74) ~ "Enforcamento/Arma de fogo",
      CAUSABAS_3 %in% paste0("X", 75:79) ~ "Outros meios mecânicos",
      CAUSABAS_3 %in% paste0("X", 80:84) ~ "Precipitação/Afogamento/Outros",
      TRUE ~ "Outro"
    )
  ) |>
  arrange(CAUSABAS_3)

salvar_tab(tab_cid, "tab_cid_subcod")
print(tab_cid |> select(CAUSABAS_3, grupo_cid, n, pct, ausente))

# Figura
fig_cid <- ggplot(tab_cid, aes(x = fct_rev(CAUSABAS_3), y = n,
                                fill = ausente)) +
  geom_col(width = 0.8) +
  geom_text(aes(label = ifelse(n == 0, "ZERO", "")),
            hjust = -0.2, size = 3, color = "firebrick") +
  scale_fill_manual(values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
                    labels = c("Com registros", "Frequência zero"),
                    name = NULL) +
  scale_x_discrete() +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  coord_flip() +
  labs(
    title    = "Frequência por subcódigo CID — Suicídio (X60–X84)",
    subtitle = "Subcódigos em laranja = frequência zero (possível subcodificação)",
    x = NULL, y = "Número de óbitos",
    caption = "Fonte: SIM/DATASUS. Elaboração própria."
  )

salvar_fig(fig_cid, "fig_cid_subcod", h = 8)

# =============================================================================
# BLOCO 4: DISTRIBUIÇÃO DAS VARIÁVEIS CRIADAS
# =============================================================================

log_info_safe("── Bloco 4: Distribuição das variáveis criadas ──")

# Frequências marginais
tab_dist <- bind_rows(
  sim_limpo |> count(categoria = sexo_label,   name = "n") |> 
    mutate(variavel = "Sexo"),
  sim_limpo |> count(categoria = faixa_etaria, name = "n") |> 
    mutate(variavel = "Faixa etária"),
  sim_limpo |> count(categoria = regiao,       name = "n") |> 
    mutate(variavel = "Região")
) |>
  group_by(variavel) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup() |>
  select(variavel, categoria, n, pct)

salvar_tab(tab_dist, "tab_distribuicao_vars")
print(tab_dist)

# Cruzamento sexo × faixa etária
tab_cruz <- sim_limpo |>
  count(sexo_label, faixa_etaria, name = "n") |>
  group_by(sexo_label) |>
  mutate(pct_dentro_sexo = round(100 * n / sum(n), 1)) |>
  ungroup()

salvar_tab(tab_cruz, "tab_cruzamento_sexo_faixa")

fig_cruz <- ggplot(tab_cruz,
                   aes(x = faixa_etaria, y = n, fill = sexo_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  geom_text(aes(label = glue("{pct_dentro_sexo}%")),
            position = position_dodge(width = 0.7),
            vjust = -0.4, size = 3) +
  scale_fill_manual(values = cores_okabe[c("Masculino", "Feminino")], name = NULL) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  labs(
    title    = "Óbitos por suicídio segundo sexo e faixa etária — Brasil, 2000–2022",
    subtitle = "% = proporção dentro de cada sexo",
    x = NULL, y = "Número de óbitos",
    caption = "Fonte: SIM/DATASUS. Elaboração própria."
  )

salvar_fig(fig_cruz, "fig_distribuicao_vars")

# =============================================================================
# BLOCO 5: SÉRIE TEMPORAL BRUTA (CONTAGEM)
# =============================================================================

log_info_safe("── Bloco 5: Série temporal bruta ──")

tab_serie <- sim_limpo |>
  count(ano, sexo_label, name = "n_obitos") |>
  bind_rows(
    sim_limpo |> count(ano, name = "n_obitos") |> mutate(sexo_label = "Total")
  ) |>
  arrange(ano, sexo_label)

salvar_tab(tab_serie, "tab_serie_bruta")

# Variação ano a ano (total)
tab_serie_yoy <- tab_serie |>
  filter(sexo_label == "Total") |>
  arrange(ano) |>
  mutate(
    var_abs = n_obitos - lag(n_obitos),
    var_pct = round(100 * (n_obitos / lag(n_obitos) - 1), 1)
  )

log_info_safe("Variação ano a ano (total):")
print(tab_serie_yoy |> select(ano, n_obitos, var_abs, var_pct))

# Figura: série + variação %
p1 <- ggplot(
  tab_serie |>
    filter(sexo_label != "Total") |>
    mutate(sexo_label = factor(sexo_label, levels = c("Masculino", "Feminino"))),
  aes(x = ano, y = n_obitos, color = sexo_label, group = sexo_label)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.8) +
  annotate("rect", xmin = 2019.5, xmax = 2022.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey30") +
  scale_color_manual(values = cores_okabe[c("Masculino", "Feminino")], name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 2)) +
  scale_y_continuous(labels = label_comma(big.mark = ".")) +
  labs(subtitle = "A. Óbitos por sexo", x = NULL, y = "N óbitos")

p2 <- ggplot(tab_serie_yoy |> filter(!is.na(var_pct)),
             aes(x = ano, y = var_pct,
                 fill = var_pct >= 0)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  scale_fill_manual(values = c("TRUE" = "#009E73", "FALSE" = "#D55E00"),
                    guide = "none") +
  scale_x_continuous(breaks = seq(2001, 2022, 2)) +
  labs(subtitle = "B. Variação % anual (total)", x = NULL, y = "Var. % a.a.")

fig_serie <- (p1 / p2) +
  plot_annotation(
    title   = "Série temporal bruta de óbitos por suicídio — Brasil, 2000–2022",
    caption = "Fonte: SIM/DATASUS. Elaboração própria.",
    theme   = theme(plot.title = element_text(face = "bold", size = 13))
  )

salvar_fig(fig_serie, "fig_serie_bruta", h = 8)

# =============================================================================
# BLOCO 6: FLAGS DE ATENÇÃO
# =============================================================================

log_info_safe("── Bloco 6: Flags de atenção ──")

flags <- list()

# Flag 1: anos com n total abaixo do threshold
anos_baixo_n <- tab_serie |>
  filter(sexo_label == "Total", n_obitos < config$flag_n_minimo_ano) |>
  mutate(flag = glue("n total = {n_obitos} (< {config$flag_n_minimo_ano})"),
         categoria = "cobertura_temporal")

flags[["anos_baixo_n"]] <- anos_baixo_n

# Flag 2: variáveis analíticas com % missing acima do limiar
vars_analiticas_limpo <- c("ano", "mes", "sexo_label", "faixa_etaria", "regiao", "CAUSABAS")

flag_missing <- sim_limpo |>
  summarise(across(all_of(vars_analiticas_limpo),
                   ~round(100 * mean(is.na(.)), 2))) |>
  pivot_longer(everything(), names_to = "variavel", values_to = "pct_missing") |>
  filter(pct_missing > config$flag_pct_missing_max) |>
  mutate(flag     = glue("missing = {pct_missing}% (> {config$flag_pct_missing_max}%)"),
         categoria = "completude",
         ano      = NA_integer_)

flags[["missing_alto"]] <- flag_missing

# Flag 3: subcódigos CID com frequência zero
flag_cid_zero <- tab_cid |>
  filter(ausente) |>
  mutate(flag     = glue("{CAUSABAS_3} sem registros (possível subcodificação)"),
         categoria = "cid_ausente",
         ano      = NA_integer_,
         variavel = "CAUSABAS") |>
  select(ano, variavel, flag, categoria)

flags[["cid_zero"]] <- flag_cid_zero

# Flag 4: regiões com % missing acima do limiar
flag_regiao <- sim_limpo |>
  summarise(pct_missing_regiao = round(100 * mean(is.na(regiao)), 2)) |>
  filter(pct_missing_regiao > config$flag_pct_missing_max) |>
  mutate(flag     = glue("regiao missing = {pct_missing_regiao}%"),
         categoria = "completude_regiao",
         ano      = NA_integer_,
         variavel = "regiao")

flags[["regiao_missing"]] <- flag_regiao

# Consolida e salva
tab_flags <- bind_rows(flags) |>
  select(categoria, ano, variavel, flag) |>
  arrange(categoria, ano)

salvar_tab(tab_flags, "tab_flags_atencao")

log_info_safe("Flags de atenção identificados: {nrow(tab_flags)}")
if (nrow(tab_flags) == 0) {
  log_info_safe("Nenhum flag disparado. Base dentro dos parâmetros esperados.")
} else {
  print(tab_flags)
}

# =============================================================================
# RESUMO FINAL
# =============================================================================

log_info_safe("── Resumo da EDA/Validação ──")

resumo <- tibble(
  item = c(
    "Registros pós-filtro CID (bruto)",
    "Registros na base limpa",
    "Perdas totais (bruto → limpo)",
    "Período coberto",
    "Flags de atenção identificados",
    "Subcódigos CID com frequência zero"
  ),
  valor = c(
    format(nrow(sim_bruto), big.mark = "."),
    format(nrow(sim_limpo), big.mark = "."),
    glue("{format(nrow(sim_bruto) - nrow(sim_limpo), big.mark = '.')} 
         ({round(100*(1 - nrow(sim_limpo)/nrow(sim_bruto)), 1)}%)"),
    glue("{min(sim_limpo$ano)}–{max(sim_limpo$ano)}"),
    as.character(nrow(tab_flags)),
    as.character(sum(tab_cid$ausente))
  )
)

print(resumo)
salvar_tab(resumo, "tab_resumo_eda")

log_info_safe("03b_eda_sim.R concluído. Outputs em tabelas/eda_sim/ e imagens/eda_sim/")
