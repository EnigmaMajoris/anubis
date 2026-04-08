# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 03_clean_dados.R
# OBJETIVO: Limpeza e tratamento dos microdados SIM — filtro CID X60–X84,
#           criação de variáveis sexo, faixa etária e região
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-04-04
# VERSÃO  : 3.0
# -----------------------------------------------------------------------------
# HISTÓRICO DE VERSÕES:
#   v1.0 — versão original (draft)
#   v2.0 — refatoração: config centralizado; funções nomeadas; here::here();
#           correção encoding IDADE (prefixo 4xx); logging com glue+parent.frame
#   v3.0 — atualização para pipeline v10.0:
#           · path_bruto atualizado para sim_suicidio_2000_2022.rds
#           · filtrar_cid_suicidio() mantida como verificação redundante
#             (o script 01 já filtra, mas manter aqui preserva rastreabilidade)
#           · Nota sobre redundância documentada no header e na função
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# DEPENDÊNCIAS: 01_download_dados.R
# LÊ          : dados/brutos/sim_suicidio_2000_2022.rds
# GERA        : dados/limpos/suicidio_limpo.rds
# -----------------------------------------------------------------------------
# NOTAS:
#   - CID X60–X84: lesões autoprovocadas intencionalmente (suicídio)
#   - O RDS de entrada já contém apenas registros X60–X84 (filtrado no script 01)
#   - O filtro CID aqui é redundante mas mantido como verificação de integridade
#   - Registros com sexo ignorado (SEXO ∉ {Masculino, Feminino}) são removidos
#   - IDADE decodificada pelo process_sim() do script 01 com prefixo 4xx (anos)
#   - Região derivada dos 2 primeiros dígitos do código municipal (CODMUNRES)
#   - Missings verificados e reportados ao final do script
#   - Seed: N/A (sem aleatoriedade neste script)
# =============================================================================


# =============================================================================
# CONFIGURAÇÃO
# =============================================================================
# Edite este bloco para adaptar período, faixas etárias ou caminhos.
# =============================================================================

config <- list(
  # Período de análise
  ano_inicio = 2000L,
  ano_fim    = 2022L,

  # Padrão regex para CIDs de suicídio (X60–X84)
  # X6x = X60–X69 | X7x = X70–X79 | X80–X84
  # NOTA: o RDS de entrada já foi filtrado por este padrão no script 01.
  #       O filtro aqui serve como verificação de integridade do pipeline.
  cid_padrao = "^X[67]\\d|^X8[0-4]",

  # Faixas etárias e seus rótulos ordenados
  faixas_breaks = c(0, 15, 30, 45, 60, Inf),
  faixas_labels = c("< 15 anos", "15–29 anos", "30–44 anos",
                    "45–59 anos", "60+ anos"),

  # Caminhos (here::here() torna absolutos e portáveis entre ambientes)
  path_bruto      = here::here("dados", "brutos", "sim_suicidio_2000_2022.rds"),
  path_limpo      = here::here("dados", "limpos", "suicidio_limpo.rds"),
  path_limpos_dir = here::here("dados", "limpos")
)


# =============================================================================
# PACOTES
# =============================================================================

library(tidyverse)   # dplyr, tidyr, stringr, forcats, lubridate
library(lubridate)   # ymd()
library(here)        # here::here() — caminhos portáveis
library(glue)        # glue() — mensagens legíveis
library(logger)      # logging com níveis INFO / WARN / ERROR


# =============================================================================
# LOGGING
# =============================================================================
# Padrão do projeto: logger com fallback para message().
# glue + parent.frame() garante interpolação correta de variáveis.
# =============================================================================

log_info_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_info(msg), error = function(e) message("[INFO] ", msg))
}

log_warn_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_warn(msg), error = function(e) message("[WARN] ", msg))
}

log_info_safe("Iniciando 03_clean_dados.R | período: {config$ano_inicio}–{config$ano_fim}")


# =============================================================================
# FUNÇÕES
# =============================================================================

#' Lê o RDS do SIM e verifica colunas mínimas necessárias.
#' @param path Caminho para o .rds (saída do script 01).
#' @return data.frame com os microdados.
carregar_sim <- function(path) {
  if (!file.exists(path)) {
    stop(glue("Arquivo não encontrado: {path}"))
  }

  dados <- readRDS(path)

  colunas_esperadas <- c("CAUSABAS", "DTOBITO", "SEXO", "IDADE", "CODMUNRES")
  ausentes <- setdiff(colunas_esperadas, names(dados))

  if (length(ausentes) > 0) {
    stop(glue(
      "Coluna(s) ausente(s) no arquivo bruto: {paste(ausentes, collapse = ', ')}"
    ))
  }

  log_info_safe("Microdados SIM carregados: {nrow(dados)} registros | {ncol(dados)} colunas")
  dados
}

#' Filtra registros com CID compatível com suicídio (X60–X84).
#' NOTA: redundante quando a entrada é sim_suicidio_2000_2022.rds
#' (já filtrado no script 01), mas mantido como verificação de integridade.
#' @param dados data.frame com coluna CAUSABAS.
#' @param padrao Expressão regular para o filtro CID.
#' @return data.frame filtrado.
filtrar_cid_suicidio <- function(dados, padrao) {
  n_antes  <- nrow(dados)

  resultado <- dados |>
    filter(!is.na(CAUSABAS), str_detect(CAUSABAS, padrao))

  n_depois <- nrow(resultado)
  pct      <- round(100 * n_depois / n_antes, 2)

  log_info_safe(
    "Verificação CID X60–X84: {n_depois} registros retidos ({pct}% da entrada)"
  )

  if (n_depois == 0) {
    stop(
      "Nenhum registro retido após filtro CID. ",
      "Verifique o padrão regex e a coluna CAUSABAS."
    )
  }

  if (n_depois < n_antes) {
    log_warn_safe(
      "{n_antes - n_depois} registro(s) removidos pelo filtro CID — ",
      "inesperado se a entrada é sim_suicidio_2000_2022.rds. Verificar pipeline."
    )
  }

  resultado
}

#' Extrai e valida ano e mês a partir de DTOBITO.
#' Remove registros com data inválida ou fora do intervalo configurado.
#' @param dados data.frame com coluna DTOBITO (formato YYYYMMDD).
#' @param ano_inicio,ano_fim Intervalo válido de anos.
#' @return data.frame com colunas data_obito, ano, mes.
criar_variaveis_tempo <- function(dados, ano_inicio, ano_fim) {
  resultado <- dados |>
    mutate(
      data_obito = ymd(DTOBITO),
      ano        = year(data_obito),
      mes        = month(data_obito)
    )

  n_data_na <- sum(is.na(resultado$data_obito))
  if (n_data_na > 0) {
    log_warn_safe("{n_data_na} registro(s) com DTOBITO inválido → removidos")
  }

  resultado <- resultado |>
    filter(!is.na(data_obito), ano >= ano_inicio, ano <= ano_fim)

  log_info_safe(
    "Variáveis de tempo criadas. ",
    "Período final: {min(resultado$ano)}–{max(resultado$ano)} | ",
    "{nrow(resultado)} registros"
  )

  resultado
}

#' Cria variável sexo_label; remove registros com sexo ignorado.
#' Mantém apenas SEXO ∈ {"Masculino", "Feminino"} — valores já decodificados
#' pelo process_sim() do script 01.
#' @param dados data.frame com coluna SEXO (character, já decodificada).
#' @return data.frame com coluna sexo_label e registros inválidos removidos.
criar_variavel_sexo <- function(dados) {
  resultado <- dados |>
    mutate(
      sexo_label = case_when(
        SEXO == "Masculino" ~ "Masculino",
        SEXO == "Feminino"  ~ "Feminino",
        TRUE                ~ NA_character_
      )
    )

  n_ignorado <- sum(is.na(resultado$sexo_label))
  if (n_ignorado > 0) {
    log_warn_safe(
      "{n_ignorado} registro(s) com sexo ignorado/inválido → removidos da análise"
    )
  }

  resultado <- filter(resultado, !is.na(sexo_label))

  log_info_safe(
    "Sexo: {sum(resultado$sexo_label == 'Masculino')} masculino | ",
    "{sum(resultado$sexo_label == 'Feminino')} feminino"
  )

  resultado
}

#' Cria variável faixa_etaria como factor ordenado.
#' Decodifica o prefixo 4xx do SIM (4xx = xx anos) antes de classificar.
#' @param dados data.frame com coluna IDADE (character ou numeric com prefixo 4xx).
#' @param breaks Vetor de cortes para cut().
#' @param labels Rótulos das faixas (length = length(breaks) - 1).
#' @return data.frame com coluna faixa_etaria.
criar_variavel_faixa_etaria <- function(dados, breaks, labels) {
  resultado <- dados |>
    mutate(
      # Decodifica prefixo 4xx: 430 → 30 anos, 401 → 1 ano
      # Valores fora do intervalo [400, 500) → NA (outros prefixos: meses, dias)
      IDADE = dplyr::if_else(
        !is.na(as.numeric(IDADE)) &
          as.numeric(IDADE) >= 400 &
          as.numeric(IDADE) < 500,
        as.numeric(IDADE) - 400,
        NA_real_
      ),
      faixa_etaria = cut(
        IDADE,
        breaks         = breaks,
        labels         = labels,
        right          = FALSE,   # intervalos fechados à esquerda: [a, b)
        include.lowest = TRUE
      )
    )

  n_faixa_na <- sum(is.na(resultado$faixa_etaria))
  if (n_faixa_na > 0) {
    log_warn_safe(
      "{n_faixa_na} registro(s) com IDADE ausente/inválida → faixa_etaria = NA"
    )
  }

  log_info_safe("Faixa etária criada. Distribuição:")
  print(table(resultado$faixa_etaria, useNA = "ifany"))

  resultado
}

#' Tabela de correspondência UF → Região (IBGE).
#' @return Tibble: uf_cod (2 dígitos, character), uf (sigla), regiao.
tabela_uf_regiao <- function() {
  tribble(
    ~uf_cod, ~uf,  ~regiao,
    "11",    "RO", "Norte",
    "12",    "AC", "Norte",
    "13",    "AM", "Norte",
    "14",    "RR", "Norte",
    "15",    "PA", "Norte",
    "16",    "AP", "Norte",
    "17",    "TO", "Norte",
    "21",    "MA", "Nordeste",
    "22",    "PI", "Nordeste",
    "23",    "CE", "Nordeste",
    "24",    "RN", "Nordeste",
    "25",    "PB", "Nordeste",
    "26",    "PE", "Nordeste",
    "27",    "AL", "Nordeste",
    "28",    "SE", "Nordeste",
    "29",    "BA", "Nordeste",
    "31",    "MG", "Sudeste",
    "32",    "ES", "Sudeste",
    "33",    "RJ", "Sudeste",
    "35",    "SP", "Sudeste",
    "41",    "PR", "Sul",
    "42",    "SC", "Sul",
    "43",    "RS", "Sul",
    "50",    "MS", "Centro-Oeste",
    "51",    "MT", "Centro-Oeste",
    "52",    "GO", "Centro-Oeste",
    "53",    "DF", "Centro-Oeste"
  )
}

#' Cria variáveis uf_cod, uf e regiao a partir de CODMUNRES.
#' @param dados data.frame com coluna CODMUNRES (6 ou 7 dígitos).
#' @param ref Tibble de referência (saída de tabela_uf_regiao()).
#' @return data.frame com colunas uf_cod, uf e regiao.
criar_variavel_regiao <- function(dados, ref) {
  resultado <- dados |>
    mutate(uf_cod = substr(as.character(CODMUNRES), 1, 2)) |>
    left_join(ref, by = "uf_cod")

  n_regiao_na <- sum(is.na(resultado$regiao))
  if (n_regiao_na > 0) {
    codigos_problematicos <- resultado |>
      filter(is.na(regiao)) |>
      count(uf_cod, sort = TRUE)
    log_warn_safe(
      "{n_regiao_na} registro(s) sem região identificada. ",
      "Códigos UF não reconhecidos:"
    )
    print(codigos_problematicos)
  }

  log_info_safe("Região criada. Distribuição:")
  print(table(resultado$regiao, useNA = "ifany"))

  resultado
}

#' Verifica e reporta missings nas variáveis analíticas principais.
#' @param dados data.frame já processado.
#' @return Tibble com n_na e pct_na por variável (impresso e retornado invisível).
checar_missings <- function(dados) {
  vars_analiticas <- c("ano", "mes", "sexo_label", "faixa_etaria",
                       "regiao", "CAUSABAS")

  missings <- dados |>
    summarise(
      across(
        all_of(vars_analiticas),
        list(n_na   = ~sum(is.na(.)),
             pct_na = ~round(100 * mean(is.na(.)), 2)),
        .names = "{.col}__{.fn}"
      )
    ) |>
    pivot_longer(
      everything(),
      names_to  = c("variavel", "metrica"),
      names_sep = "__"
    ) |>
    pivot_wider(names_from = metrica, values_from = value)

  log_info_safe("── Missings nas variáveis analíticas ──")
  print(missings)

  invisible(missings)
}

#' Salva o dataset limpo em .rds.
#' @param dados data.frame processado.
#' @param path Caminho completo de saída.
salvar_base_limpa <- function(dados, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(dados, path)
  log_info_safe(
    "Base limpa salva em: {path} | {nrow(dados)} registros | {ncol(dados)} colunas"
  )
}


# =============================================================================
# EXECUÇÃO
# =============================================================================

ref_uf_regiao <- tabela_uf_regiao()

suicidio_limpo <- carregar_sim(config$path_bruto)               |>
  filtrar_cid_suicidio(config$cid_padrao)                       |>
  criar_variaveis_tempo(config$ano_inicio, config$ano_fim)       |>
  criar_variavel_sexo()                                          |>
  criar_variavel_faixa_etaria(config$faixas_breaks,
                              config$faixas_labels)              |>
  criar_variavel_regiao(ref_uf_regiao)

checar_missings(suicidio_limpo)

salvar_base_limpa(suicidio_limpo, config$path_limpo)

log_info_safe("03_clean_dados.R concluído com sucesso.")
