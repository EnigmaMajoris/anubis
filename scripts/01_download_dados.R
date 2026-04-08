# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 01_download_dados.R
# OBJETIVO: Aquisição de todos os dados brutos do projeto — microdados do
#           SIM/DATASUS (2000–2022) e construção da base populacional IBGE
#           a partir de dados baixados manualmente do SIDRA
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-04-05
# VERSÃO  : 11.0
# -----------------------------------------------------------------------------
# HISTÓRICO DE VERSÕES:
#   v7.0  — versão original
#   v8.0  — correção do pop_feminino 2007; junção com coalesce; verificações
#            de estrutura das planilhas; if_else; verificação de colunas SIM
#   v9.0  — refatoração completa: config centralizado; funções reutilizáveis;
#            parse_number(); logging estruturado; seed documentado
#   v10.0 — tratamento de encoding latin1; filtro CID antes do bind_rows;
#            funções dentro de cada bloco; log_info_safe com parent.frame()
#   v11.0 — correção crítica em montar_serie_total():
#            · O total de 2007 da Contagem (~109M, parcial) era priorizado
#              pelo coalesce sobre a estimativa Tab6579 (~188M, completa),
#              inflando artificialmente as taxas de mortalidade de 2007.
#            · Correção: o total de 2007 é agora calculado por interpolação
#              linear entre os valores Tab6579 de 2006 e 2008, substituindo
#              o total parcial da Contagem. As proporções de sexo de 2007
#              continuam vindas da Contagem (âncora correta para esse fim).
#            · Correção alinhada com a decisão documentada em notas.qmd §2:
#              "o total de 2007 não é usado como denominador de taxas".
# -----------------------------------------------------------------------------
# NOTA SOBRE parse_number() vs as.numeric():
#   parse_number() adotado em todas as leituras populacionais porque:
#   (a) O SIDRA às vezes exporta planilhas com "-" ou "..." para dados
#       suprimidos, pontos como separador de milhar, ou vírgulas decimais.
#       as.numeric() converte tudo isso para NA silenciosamente.
#       parse_number() faz o mesmo, mas com aviso explícito.
#   (b) A Tab200 contém 265 células com "-" e "..." em grupos de UFs
#       específicas — nenhuma afeta as linhas de Total que usamos,
#       mas o risco é real nesta família de arquivos.
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# DEPENDÊNCIAS: 00_setup.R
# ENTRADA     : dados/brutos/Tabela6579_Estimativa_2000_2022.xlsx
#               dados/brutos/Tabela200_Censo2000_2010.xlsx
#               dados/brutos/Tabela9514_Censo2022.xlsx
#               dados/brutos/Tabela794_Contagem2007.xlsx
#               dados/brutos/Tabela7358_Projecao_2000_2022.xlsx  (reservado)
# SAIDA       : dados/brutos/sim_suicidio_2000_2022.rds  ← apenas CID X60–X84
#               dados/limpos/populacao_ibge_tratada.csv
#               dados/limpos/comparacao_metodos_interpolacao.csv
# -----------------------------------------------------------------------------
# COMO USAR — EXECUÇÃO PARCIAL:
#   · Só SIM:          rode os blocos SETUP e BLOCO 1
#   · Só IBGE:         rode os blocos SETUP e BLOCO 2
#   · Só interpolação: rode os blocos SETUP, BLOCO 2 e BLOCO 3
#   · Completo:        Ctrl+A → Run (roda tudo)
# =============================================================================


# =============================================================================
# SETUP — PACOTES, LOGGING E CONFIGURAÇÃO
# =============================================================================
# Execute sempre antes de qualquer bloco.
# =============================================================================

library(microdatasus)
library(tidyverse)
library(readxl)
library(zoo)
library(glue)
library(here)

# Logging: usa logger se disponível, fallback para message()
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
} else {
  log_info  <- function(...) message("[INFO]  ", glue(...))
  log_warn  <- function(...) warning("[WARN]  ", glue(...), call. = FALSE)
  log_error <- function(...) stop("[ERROR] ", glue(...), call. = FALSE)
}

# Wrappers com interpolação correta de variáveis (glue + parent.frame)
log_info_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_info(msg), error = function(e) message("[INFO] ", msg))
}
log_warn_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_warn(msg), error = function(e) message("[WARN] ", msg))
}
log_error_safe <- function(...) {
  msg <- glue::glue(..., .envir = parent.frame())
  tryCatch(log_error(msg), error = function(e) stop("[ERROR] ", msg))
}

dir.create(here("dados", "brutos"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("dados", "limpos"), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# CONFIGURAÇÃO 
# -----------------------------------------------------------------------------
# Todos os parâmetros editáveis ficam aqui.
# Alterar qualquer valor nesta lista propaga automaticamente para todo o código.
# -----------------------------------------------------------------------------

config <- list(
  # Período de análise
  anos_sim        = 2000:2022,

  # Padrão CID para suicídio (X60–X84) — usado no filtro do Bloco 1
  cid_suicidio    = "^X[67]\\d|^X8[0-4]",

  # Regiões de interesse — ordem define fator para visualizações
  locais          = c("Brasil", "Norte", "Nordeste",
                      "Sudeste", "Sul", "Centro-Oeste"),

  # Anos com levantamento censitário/contagem direto (âncoras para proporção sexo)
  anos_ancora     = c(2000L, 2007L, 2010L, 2022L),

  # Margem de tolerância do envelope demográfico (em proporção, não p.p.)
  margem_envelope = 0.005,

  # Caminhos de entrada (here::here() torna absolutos e portáveis)
  path_tab200  = here("dados", "brutos", "Tabela200_Censo2000_2010.xlsx"),
  path_tab794  = here("dados", "brutos", "Tabela794_Contagem2007.xlsx"),
  path_tab6579 = here("dados", "brutos", "Tabela6579_Estimativa_2000_2022.xlsx"),
  path_tab9514 = here("dados", "brutos", "Tabela9514_Censo2022.xlsx"),

  # Caminhos de saída
  # NOTA: sim_suicidio_2000_2022.rds contém apenas registros CID X60–X84
  #       (não o bruto completo do SIM) — decisão para contornar limite de RAM.
  #       O script 03 aplica o filtro CID novamente como verificação redundante.
  path_sim_rds = here("dados", "brutos", "sim_suicidio_2000_2022.rds"),
  path_pop_csv = here("dados", "limpos", "populacao_ibge_tratada.csv"),
  path_comp_csv= here("dados", "limpos", "comparacao_metodos_interpolacao.csv"),

  # Estrutura esperada da Tab9514 (posições 1-indexed das colunas de interesse)
  # Verificar se mudou ao re-baixar do SIDRA
  tab9514_col_total    = 2L,
  tab9514_col_homens   = 24L,
  tab9514_col_mulheres = 46L,

  # Número de linhas de cabeçalho a pular na Tab794
  tab794_skip = 7L
)

log_info_safe("Setup concluído. Seed: N/A (sem aleatoriedade neste script).")


# =============================================================================
# BLOCO 1 — DOWNLOAD SIM/DATASUS
# =============================================================================
# Pré-requisito: bloco SETUP executado.
# Saída: dados/brutos/sim_suicidio_2000_2022.rds
#
# DECISÃO DE DESIGN (v10.0):
#   O RDS gerado contém apenas registros com CID X60–X84 (suicídio),
#   não o bruto completo do SIM (~26M linhas). Motivo: o bind_rows de 23 anos
#   completos ultrapassa o limite de RAM disponível. O filtro é aplicado
#   individualmente em cada ano antes do bind, reduzindo para ~240K linhas.
#   O script 03_clean_dados.R aplica o filtro CID novamente como verificação,
#   o que é redundante mas inofensivo e mantém a rastreabilidade do pipeline.
# =============================================================================

log_info_safe("\n===== BLOCO 1: SIM/DATASUS =====\n")

# -----------------------------------------------------------------------------
# FUNÇÕES — BLOCO 1
# -----------------------------------------------------------------------------

#' Baixa, corrige encoding e processa um único ano do SIM/DATASUS.
#' Filtra apenas registros de suicídio (CID X60–X84) antes de retornar.
#' @param ano Ano a baixar (inteiro).
#' @param cid_padrao Regex para filtro CID.
#' @return data.frame filtrado com coluna ano_download, ou NULL em caso de erro.
baixar_sim_ano <- function(ano, cid_padrao = config$cid_suicidio) {
  log_info_safe("Baixando SIM — ano {ano}...")
  tryCatch({
    df <- fetch_datasus(
      year_start         = ano,
      year_end           = ano,
      uf                 = "all",
      information_system = "SIM-DO"
    )

    # Corrige encoding antes do process_sim() — necessário para anos com
    # arquivos .dbc em latin1 com bytes inválidos (identificado em 2002 e 2005).
    # iconv(..., sub = "byte") substitui bytes inválidos sem perda de dados.
    colunas_char <- names(df)[sapply(df, is.character)]
    df <- dplyr::mutate(df, dplyr::across(
      dplyr::all_of(colunas_char),
      ~iconv(., from = "latin1", to = "UTF-8", sub = "byte")
    ))

    df <- process_sim(df)

    # Filtra só suicídio ANTES de retornar — reduz memória no bind_rows final
    n_total <- nrow(df)
    df <- dplyr::filter(df,
                        !is.na(CAUSABAS),
                        stringr::str_detect(CAUSABAS, cid_padrao))

    df$ano_download <- ano
    log_info_safe("Ano {ano} OK — {nrow(df)} suicídios de {n_total} óbitos totais")
    return(df)

  }, error = function(e) {
    log_warn_safe("ERRO no ano {ano}: {e$message}")
    return(NULL)
  })
}

#' Itera sobre todos os anos, consolida em um único data.frame e
#' verifica compatibilidade de colunas e cobertura de anos.
#' @param anos Vetor de anos a baixar.
#' @return data.frame com registros de suicídio de todos os anos.
baixar_sim <- function(anos = config$anos_sim) {
  lista_anos <- lapply(anos, baixar_sim_ano)
  lista_anos <- Filter(Negate(is.null), lista_anos)

  if (length(lista_anos) == 0) {
    log_error_safe("Nenhum ano do SIM baixado com sucesso.")
  }

  anos_baixados <- sapply(lista_anos, function(df) unique(df$ano_download))
  anos_faltando <- setdiff(anos, anos_baixados)
  if (length(anos_faltando) > 0) {
    log_warn_safe(
      "Anos não baixados (retornaram NULL): {paste(anos_faltando, collapse=', ')}"
    )
  }

  # Avisa sobre diferenças de estrutura entre anos (formulário DO muda ao longo do tempo)
  colunas_por_ano <- lapply(lista_anos, names)
  if (length(unique(lapply(colunas_por_ano, sort))) > 1) {
    log_warn_safe(
      "Anos com estruturas de colunas diferentes — bind_rows preencherá NAs. ",
      "Esperado: formulário DO mudou ao longo dos anos."
    )
  } else {
    log_info_safe("Estrutura de colunas consistente entre todos os anos.")
  }

  sim <- dplyr::bind_rows(lista_anos)
  log_info_safe("Total de registros de suicídio: {nrow(sim)}")
  log_info_safe("Anos presentes: {paste(sort(unique(sim$ano_download)), collapse=', ')}")
  return(sim)
}

# -----------------------------------------------------------------------------
# EXECUÇÃO — BLOCO 1
# -----------------------------------------------------------------------------

sim_suicidio <- baixar_sim(anos = config$anos_sim)
saveRDS(sim_suicidio, config$path_sim_rds)
log_info_safe("SIM salvo em: {config$path_sim_rds} | {nrow(sim_suicidio)} registros")


# =============================================================================
# BLOCO 2 — POPULAÇÃO IBGE
# =============================================================================
# Pré-requisito: bloco SETUP executado.
# Saída: dados/limpos/populacao_ibge_tratada.csv
# =============================================================================

log_info_safe("\n===== BLOCO 2: POPULAÇÃO IBGE =====\n")

# -----------------------------------------------------------------------------
# FUNÇÕES — BLOCO 2
# -----------------------------------------------------------------------------

#' Lê a Tabela 200 (Censos 2000 e 2010).
#' Aplica parse_number() para capturar strings SIDRA sem silenciar erros.
#' @return data.frame longo: local, ano, populacao_total, pop_masculino, pop_feminino.
ler_tab200 <- function(path   = config$path_tab200,
                       locais = config$locais) {
  log_info_safe("Lendo Tabela 200 — Censos 2000 e 2010...")
  raw <- read_excel(path)
  names(raw) <- c("local", "grupo_idade",
                  "total_2000", "masc_2000", "fem_2000",
                  "total_2010", "masc_2010", "fem_2010")

  resultado <- raw |>
    filter(local %in% locais, grupo_idade == "Total") |>
    mutate(across(total_2000:fem_2010,
                  ~readr::parse_number(as.character(.x)))) |>
    select(local, total_2000, masc_2000, fem_2000,
           total_2010, masc_2010, fem_2010) |>
    pivot_longer(
      cols      = -local,
      names_to  = c(".value", "ano"),
      names_sep = "_"
    ) |>
    rename(populacao_total = total,
           pop_masculino   = masc,
           pop_feminino    = fem) |>
    mutate(ano = as.integer(ano))

  log_info_safe("Tabela 200 OK — {nrow(resultado)} registros")
  return(resultado)
}

#' Lê a Tabela 794 (Contagem 2007).
#' IMPORTANTE: pop_feminino é o valor REAL da fonte — não calculado por subtração.
#' pop_masculino + pop_feminino ≠ populacao_total em 2007 (sexo não declarado).
#' @return data.frame: local, ano, populacao_total, pop_masculino, pop_feminino.
ler_tab794 <- function(path   = config$path_tab794,
                       skip   = config$tab794_skip,
                       locais = config$locais) {
  log_info_safe("Lendo Tabela 794 — Contagem 2007...")
  raw <- read_excel(path, skip = skip)
  names(raw) <- c("local", "grupo_idade",
                  "populacao_total", "pop_masculino", "pop_feminino")

  resultado <- raw |>
    filter(local %in% locais, grupo_idade == "Total") |>
    mutate(
      ano             = 2007L,
      populacao_total = readr::parse_number(as.character(populacao_total)),
      pop_masculino   = readr::parse_number(as.character(pop_masculino)),
      pop_feminino    = readr::parse_number(as.character(pop_feminino))
    ) |>
    select(local, ano, populacao_total, pop_masculino, pop_feminino)

  dif_sem_sexo <- resultado |>
    mutate(dif = populacao_total - (pop_masculino + pop_feminino))
  log_info_safe("Tabela 794 OK — {nrow(resultado)} registros")
  log_info_safe("Diferença total-(M+F) em 2007 (pessoas c/ sexo não declarado):")
  dif_sem_sexo |>
    select(local, populacao_total, pop_masculino, pop_feminino, dif) |>
    print()

  return(resultado)
}

#' Lê a Tabela 9514 (Censo 2022).
#' Verifica rótulos das colunas antes de selecionar por posição —
#' falha com erro descritivo se o layout mudar.
#' @return data.frame: local, ano, populacao_total, pop_masculino, pop_feminino.
ler_tab9514 <- function(path         = config$path_tab9514,
                        locais       = config$locais,
                        col_total    = config$tab9514_col_total,
                        col_homens   = config$tab9514_col_homens,
                        col_mulheres = config$tab9514_col_mulheres) {
  log_info_safe("Lendo Tabela 9514 — Censo 2022...")
  raw <- read_excel(path, col_names = FALSE)

  linha_header <- as.character(raw[6, ])
  rotulos_ok <- list(
    list(pos = col_total,    esperados = c("Total", "total", "TOTAL")),
    list(pos = col_homens,   esperados = c("Homens", "homens", "Masculino")),
    list(pos = col_mulheres, esperados = c("Mulheres", "mulheres", "Feminino"))
  )
  for (chk in rotulos_ok) {
    val <- linha_header[chk$pos]
    if (!val %in% chk$esperados) {
      log_error_safe(
        "Tabela 9514: posição {chk$pos} esperava '{chk$esperados[1]}', ",
        "encontrou '{val}'. Layout mudou — ajustar config$tab9514_col_*."
      )
    }
  }
  log_info_safe(
    "Tab9514 — posições verificadas: ",
    "Total({col_total}), Homens({col_homens}), Mulheres({col_mulheres}) ✓"
  )

  resultado <- raw |>
    select(local        = all_of(col_total - 1L),
           total_2022   = all_of(col_total),
           masc_2022    = all_of(col_homens),
           fem_2022     = all_of(col_mulheres)) |>
    filter(local %in% locais) |>
    mutate(
      ano             = 2022L,
      populacao_total = readr::parse_number(as.character(total_2022)),
      pop_masculino   = readr::parse_number(as.character(masc_2022)),
      pop_feminino    = readr::parse_number(as.character(fem_2022))
    ) |>
    select(local, ano, populacao_total, pop_masculino, pop_feminino)

  log_info_safe("Tabela 9514 OK — {nrow(resultado)} registros")
  return(resultado)
}

#' Lê a Tabela 6579 (estimativas anuais 2001–2021, sem âncoras).
#' Exclui anos-âncora para que estimativas não sobrescrevam valores observados.
#' @return data.frame longo: local, ano, populacao_total.
ler_tab6579 <- function(path        = config$path_tab6579,
                        locais      = config$locais,
                        anos_ancora = config$anos_ancora) {
  log_info_safe("Lendo Tabela 6579 — estimativas anuais...")
  raw <- read_excel(path)
  names(raw)[1] <- "local"

  anos_raw <- as.character(raw[3, -1])
  anos_num  <- suppressWarnings(as.integer(anos_raw))
  if (any(is.na(anos_num))) {
    log_error_safe(
      "Tabela 6579: linha 3 contém valores não-inteiros como nomes de coluna. ",
      "Layout mudou — verificar número de linhas de cabeçalho."
    )
  }
  anos_invalidos <- anos_num[!is.na(anos_num) & (anos_num < 1990 | anos_num > 2050)]
  if (length(anos_invalidos) > 0) {
    log_warn_safe(
      "Tabela 6579: anos fora do intervalo plausível: {paste(anos_invalidos, collapse=', ')}"
    )
  }
  names(raw)[-1] <- anos_raw
  log_info_safe(
    "Tabela 6579 — anos: {paste(sort(anos_num[!is.na(anos_num)]), collapse=', ')}"
  )

  resultado <- raw |>
    filter(local %in% locais) |>
    pivot_longer(
      cols      = -local,
      names_to  = "ano",
      values_to = "populacao_total"
    ) |>
    mutate(
      ano             = as.integer(ano),
      populacao_total = readr::parse_number(as.character(populacao_total))
    ) |>
    filter(!is.na(ano), !is.na(populacao_total)) |>
    filter(!ano %in% anos_ancora) |>
    arrange(local, ano)

  log_info_safe("Tabela 6579 OK — {nrow(resultado)} registros")
  return(resultado)
}

#' Combina levantamentos censitários em base única de âncoras com proporções.
#' Verifica presença dos 4 anos para todos os locais.
#' @return data.frame: local, ano, populacao_total, pop_masculino, pop_feminino,
#'   prop_masc, prop_fem.
construir_ancoras <- function(censo_2000_2010, contagem_2007, censo_2022,
                               anos_ancora = config$anos_ancora) {
  log_info_safe("Construindo base de âncoras observadas...")

  ancora <- bind_rows(censo_2000_2010, contagem_2007, censo_2022) |>
    arrange(local, ano) |>
    mutate(
      prop_masc = pop_masculino / populacao_total,
      prop_fem  = pop_feminino  / populacao_total
    )

  check <- ancora |> count(local, name = "n_ancoras")
  if (any(check$n_ancoras < length(anos_ancora))) {
    log_warn_safe(
      "Algum local com menos de {length(anos_ancora)} âncoras — verificar!"
    )
    print(check)
  } else {
    log_info_safe(
      "Âncoras OK — {paste(anos_ancora, collapse=', ')} presentes para todos os locais."
    )
  }

  return(ancora)
}

#' Constrói série de população total 2000–2022 por local.
#'
#' Estratégia por tipo de ano:
#'   - Anos censitários (2000, 2010, 2022): total do Censo (Tab200/Tab9514).
#'   - Anos com estimativa Tab6579 (exceto 2007): valor direto da Tab6579.
#'   - Ano 2007: interpolação linear entre 2006 e 2008 (Tab6579).
#'     MOTIVO: a Contagem 2007 cobriu apenas municípios ≤ 170 mil hab.,
#'     resultando em total parcial (~109M vs ~188M reais). Usar esse valor
#'     como denominador inflaria artificialmente as taxas de mortalidade.
#'     As proporções de sexo de 2007 continuam usando a Contagem como âncora
#'     — apenas o total é substituído (ver notas.qmd §2 e §11.0).
#'
#' @return data.frame completo sem NAs: local, ano, populacao_total.
montar_serie_total <- function(ancora_completa, pop_total_6579,
                                locais = config$locais) {
  log_info_safe("Montando série de população total 2000–2022...")

  grade <- expand_grid(local = locais, ano = 2000:2022)

  # Total de 2007 por interpolação linear entre Tab6579 de 2006 e 2008.
  # A Tab6579 exclui anos-âncora, portanto 2007 não está nela diretamente.
  # A média entre 2006 e 2008 é matematicamente idêntica à interpolação linear.
  pop_2007_interp <- pop_total_6579 |>
    filter(ano %in% c(2006, 2008)) |>
    group_by(local) |>
    summarise(pop_2007 = mean(populacao_total, na.rm = TRUE), .groups = "drop")

  # Âncoras para o total: usa Tab200/Tab9514 para 2000/2010/2022,
  # mas substitui 2007 pelo valor interpolado (não pelo total da Contagem).
  ancora_total <- ancora_completa |>
    filter(ano != 2007) |>
    select(local, ano, pop_ancora = populacao_total)

  resultado <- grade |>
    left_join(ancora_total, by = c("local", "ano")) |>
    left_join(
      pop_total_6579 |> select(local, ano, pop_estimada = populacao_total),
      by = c("local", "ano")
    ) |>
    left_join(
      pop_2007_interp |> mutate(ano = 2007L),
      by = c("local", "ano")
    ) |>
    mutate(
      # Prioridade: âncora censitária > estimativa Tab6579 > interpolação 2007
      populacao_total = coalesce(pop_ancora, pop_estimada, pop_2007)
    ) |>
    select(local, ano, populacao_total) |>
    arrange(local, ano)

  faltando <- resultado |> filter(is.na(populacao_total))
  if (nrow(faltando) > 0) {
    log_warn_safe("Anos/locais sem cobertura:")
    print(faltando)
  } else {
    log_info_safe("Série total 2000–2022 completa para todos os locais.")
  }

  # Loga o valor de 2007 para conferência visual
  pop_2007_brasil <- resultado |>
    filter(local == "Brasil", ano == 2007) |>
    pull(populacao_total)
  log_info_safe(
    "Total Brasil 2007 (interpolado Tab6579): ",
    "{format(round(pop_2007_brasil), big.mark='.', scientific=FALSE)} hab. ",
    "(Contagem parcial descartada: ~108.765.037)"
  )

  return(resultado)
}

#' Calcula proporções interpoladas por dois métodos em paralelo:
#' spline cúbica (para diagnóstico) e linear por partes (método adotado).
#' @return lista: $prop_ambos (diagnóstico) e $prop_final (método linear).
interpolar_proporcoes <- function(ancora_completa,
                                   locais = config$locais) {
  log_info_safe("Interpolando proporções por sexo — spline e linear em paralelo...")

  grade <- expand_grid(local = locais, ano = 2000:2022)

  prop_base <- grade |>
    left_join(
      ancora_completa |> select(local, ano, prop_masc, prop_fem),
      by = c("local", "ano")
    ) |>
    arrange(local, ano)

  # Spline cúbica natural — avaliada para documentar overshoot, NÃO adotada
  prop_spline <- prop_base |>
    group_by(local) |>
    mutate(
      prop_masc_spline = zoo::na.spline(prop_masc, x = ano, na.rm = FALSE),
      prop_fem_spline  = zoo::na.spline(prop_fem,  x = ano, na.rm = FALSE)
    ) |>
    ungroup() |>
    select(local, ano, prop_masc_spline, prop_fem_spline)

  # Interpolação linear por partes — MÉTODO ADOTADO
  # Escolhida por 0 casos fora do envelope demográfico (ver Bloco 3 e notas.qmd)
  prop_linear <- prop_base |>
    group_by(local) |>
    mutate(
      prop_masc_linear = zoo::na.approx(prop_masc, x = ano, na.rm = FALSE),
      prop_fem_linear  = zoo::na.approx(prop_fem,  x = ano, na.rm = FALSE)
    ) |>
    ungroup() |>
    select(local, ano, prop_masc_linear, prop_fem_linear)

  prop_ambos <- prop_spline |>
    left_join(prop_linear, by = c("local", "ano"))

  if (anyNA(prop_ambos$prop_masc_spline) | anyNA(prop_ambos$prop_masc_linear)) {
    log_warn_safe("Proporções com NA após interpolação — verificar âncoras!")
  } else {
    log_info_safe("Interpolação concluída (spline e linear).")
  }

  prop_final <- prop_ambos |>
    select(local, ano,
           prop_masc = prop_masc_linear,
           prop_fem  = prop_fem_linear)

  return(list(prop_ambos = prop_ambos, prop_final = prop_final))
}

#' Aplica proporções interpoladas (lineares) sobre os totais populacionais.
#' Restaura valores reais para anos-âncora via coalesce.
#' Em 2007, pop_masculino + pop_feminino ≠ populacao_total é esperado.
#' @return data.frame: local, ano, populacao_total, pop_masculino, pop_feminino.
construir_serie_final <- function(pop_total_todos, prop_final,
                                   ancora_completa) {
  log_info_safe("Construindo série final com proporções lineares interpoladas...")

  pop <- pop_total_todos |>
  left_join(prop_final, by = c("local", "ano")) |>
  mutate(
    pop_masculino = round(populacao_total * prop_masc),
    pop_feminino  = round(populacao_total * prop_fem)
  ) |>
  # Restaura pop_masculino e pop_feminino reais para anos-âncora,
  # MAS preserva populacao_total da serie (Tab6579/Censos) — não substitui
  # pelo total da Contagem 2007, que é parcial (municípios ≤ 170 mil hab.).
  # O total de 2007 já vem correto de montar_serie_total() via Tab6579.
  left_join(
    ancora_completa |>
      filter(ano != 2007) |>   # <-- exclui 2007 do coalesce de M e F
      select(local, ano,
             pop_masc_ancora = pop_masculino,
             pop_fem_ancora  = pop_feminino),
    by = c("local", "ano")
  ) |>
  mutate(
    pop_masculino = coalesce(pop_masc_ancora, pop_masculino),
    pop_feminino  = coalesce(pop_fem_ancora,  pop_feminino)
  ) |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino)

  validacao <- pop |>
    mutate(dif = abs(populacao_total - (pop_masculino + pop_feminino)))

  # Em 2007, pop_masculino e pop_feminino vêm da interpolação linear aplicada
  # sobre o total Tab6579 (não da Contagem), então M+F ≈ total é esperado.
  # A validação abaixo só reporta problemas em outros anos.
  problemas <- validacao |> filter(ano != 2007, dif > 1)
  if (nrow(problemas) > 0) {
    log_warn_safe("Diferença M+F > 1 fora de 2007 — verificar dados!")
    print(problemas)
  } else {
    log_info_safe(
      "Consistência M+F validada para todos os anos (exceto 2007, onde dif é esperada)."
    )
  }

  return(pop)
}

# -----------------------------------------------------------------------------
# EXECUÇÃO — BLOCO 2
# -----------------------------------------------------------------------------

censo_2000_2010 <- ler_tab200(locais = config$locais)
contagem_2007   <- ler_tab794(locais = config$locais)
censo_2022      <- ler_tab9514(locais = config$locais)
pop_total_6579  <- ler_tab6579(locais = config$locais,
                                anos_ancora = config$anos_ancora)

ancora_completa <- construir_ancoras(censo_2000_2010, contagem_2007, censo_2022,
                                     anos_ancora = config$anos_ancora)

pop_total_todos <- montar_serie_total(ancora_completa, pop_total_6579,
                                      locais = config$locais)

# grade_completa mantida em memória para o script 02 (fig4 — sensibilidade 2007)
grade_completa <- expand_grid(local = config$locais, ano = 2000:2022)

interp     <- interpolar_proporcoes(ancora_completa, locais = config$locais)
prop_ambos <- interp$prop_ambos   # mantido em memória para o Bloco 3 e script 02
prop_final <- interp$prop_final

pop_final <- construir_serie_final(pop_total_todos, prop_final, ancora_completa)

log_info_safe("\nResumo — Brasil:")
pop_final |>
  filter(local == "Brasil") |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  print(n = 23)

pop_exportar <- pop_final |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  arrange(local, ano)

write_csv(pop_exportar, config$path_pop_csv)
log_info_safe("Arquivo salvo em: {config$path_pop_csv}")


# =============================================================================
# BLOCO 3 — COMPARAÇÃO DE MÉTODOS DE INTERPOLAÇÃO
# =============================================================================
# Pré-requisito: Bloco 2 executado (ancora_completa e prop_ambos em memória).
# Saída: dados/limpos/comparacao_metodos_interpolacao.csv
# =============================================================================

log_info_safe("\n===== BLOCO 3: COMPARAÇÃO DE MÉTODOS =====\n")

# -----------------------------------------------------------------------------
# FUNÇÕES — BLOCO 3
# -----------------------------------------------------------------------------

#' Deriva o envelope demográfico de plausibilidade a partir das âncoras.
#' @return data.frame: local, prop_masc_min_obs, prop_masc_max_obs,
#'   prop_masc_min, prop_masc_max.
calcular_envelope <- function(ancora_completa,
                               margem = config$margem_envelope) {
  ancora_completa |>
    group_by(local) |>
    summarise(
      prop_masc_min_obs = min(prop_masc, na.rm = TRUE),
      prop_masc_max_obs = max(prop_masc, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      prop_masc_min = prop_masc_min_obs - margem,
      prop_masc_max = prop_masc_max_obs + margem
    )
}

#' Avalia a spline cúbica contra o envelope e detecta inflexões não-monotônicas.
#' @return lista: $por_ano, $resumo, $inflexoes, $n_fora.
diagnosticar_spline <- function(prop_ambos, limites_ancora,
                                 anos_ancora = config$anos_ancora) {
  log_info_safe("Diagnóstico da spline cúbica...")

  por_ano <- prop_ambos |>
    filter(!ano %in% anos_ancora) |>
    left_join(limites_ancora, by = "local") |>
    mutate(
      fora_envelope = prop_masc_spline < prop_masc_min |
        prop_masc_spline > prop_masc_max,
      desvio_pp = case_when(
        prop_masc_spline < prop_masc_min ~
          (prop_masc_min - prop_masc_spline) * 100,
        prop_masc_spline > prop_masc_max ~
          (prop_masc_spline - prop_masc_max) * 100,
        TRUE ~ 0
      )
    )

  n_fora <- sum(por_ano$fora_envelope, na.rm = TRUE)

  resumo <- por_ano |>
    group_by(local) |>
    summarise(
      n_anos_fora   = sum(fora_envelope, na.rm = TRUE),
      desvio_max_pp = max(desvio_pp, na.rm = TRUE),
      ano_pior      = if_else(
        any(fora_envelope, na.rm = TRUE),
        ano[which.max(desvio_pp)],
        NA_integer_
      ),
      .groups = "drop"
    ) |>
    arrange(desc(n_anos_fora), desc(desvio_max_pp))

  inflexoes <- prop_ambos |>
    arrange(local, ano) |>
    group_by(local) |>
    mutate(
      delta        = prop_masc_spline - lag(prop_masc_spline),
      muda_direcao = sign(delta) != sign(lag(delta)) &
        !is.na(delta) & !is.na(lag(delta))
    ) |>
    filter(muda_direcao, !ano %in% anos_ancora) |>
    select(local, ano, prop_masc_spline, delta) |>
    ungroup()

  log_info_safe(
    "Spline — {n_fora} caso(s) fora do envelope | {nrow(inflexoes)} inflexão(ões)"
  )
  return(list(por_ano   = por_ano,
              resumo    = resumo,
              inflexoes = inflexoes,
              n_fora    = n_fora))
}

#' Verifica se a interpolação linear permanece dentro do envelope.
#' Serve como teste de sanidade e documentação formal para o manuscrito.
#' @return lista: $por_ano, $n_fora.
diagnosticar_linear <- function(prop_ambos, limites_ancora,
                                 anos_ancora = config$anos_ancora,
                                 margem      = config$margem_envelope) {
  por_ano <- prop_ambos |>
    filter(!ano %in% anos_ancora) |>
    left_join(limites_ancora, by = "local") |>
    mutate(
      fora_envelope = prop_masc_linear < prop_masc_min |
        prop_masc_linear > prop_masc_max,
      desvio_pp = case_when(
        prop_masc_linear < prop_masc_min ~
          (prop_masc_min - prop_masc_linear) * 100,
        prop_masc_linear > prop_masc_max ~
          (prop_masc_linear - prop_masc_max) * 100,
        TRUE ~ 0
      )
    )

  n_fora <- sum(por_ano$fora_envelope, na.rm = TRUE)

  if (n_fora == 0) {
    log_info_safe(
      "Linear — APROVADA: 0 casos fora do envelope (âncoras ± {margem*100} p.p.)"
    )
  } else {
    log_warn_safe("Linear — {n_fora} caso(s) fora do envelope:")
    por_ano |>
      filter(fora_envelope) |>
      select(local, ano, prop_masc_linear, prop_masc_min, prop_masc_max) |>
      print()
  }

  return(list(por_ano = por_ano, n_fora = n_fora))
}

#' Calcula a diferença absoluta entre spline e linear ano a ano.
#' Quantifica o impacto prático da escolha metodológica.
#' @return lista: $detalhe (por ano/região) e $resumo (por região).
comparar_metodos <- function(prop_ambos, anos_ancora = config$anos_ancora) {
  detalhe <- prop_ambos |>
    filter(!ano %in% anos_ancora) |>
    mutate(
      dif_pp     = (prop_masc_spline - prop_masc_linear) * 100,
      dif_abs_pp = abs(dif_pp)
    )

  resumo <- detalhe |>
    group_by(local) |>
    summarise(
      dif_media_pp = mean(dif_abs_pp, na.rm = TRUE),
      dif_max_pp   = max(dif_abs_pp,  na.rm = TRUE),
      ano_max_dif  = if_else(
        any(!is.na(dif_abs_pp)),
        ano[which.max(dif_abs_pp)],
        NA_integer_
      ),
      .groups = "drop"
    ) |>
    arrange(desc(dif_max_pp))

  return(list(detalhe = detalhe, resumo = resumo))
}

#' Monta e salva a tabela comparativa completa para o manuscrito.
#' Inclui ambos os métodos, limites do envelope, diagnóstico da spline
#' e diferença entre métodos para cada ano/região.
exportar_comparacao <- function(prop_ambos, limites_ancora,
                                 diag_spline_por_ano,
                                 anos_ancora = config$anos_ancora,
                                 path        = config$path_comp_csv) {
  tabela <- prop_ambos |>
    left_join(
      limites_ancora |> select(local, prop_masc_min, prop_masc_max),
      by = "local"
    ) |>
    left_join(
      diag_spline_por_ano |>
        select(local, ano, fora_envelope, desvio_pp) |>
        rename(spline_fora_envelope = fora_envelope,
               spline_desvio_pp     = desvio_pp),
      by = c("local", "ano")
    ) |>
    mutate(
      e_ancora       = ano %in% anos_ancora,
      dif_metodos_pp = (prop_masc_spline - prop_masc_linear) * 100
    ) |>
    select(local, ano, e_ancora,
           prop_masc_spline, prop_masc_linear,
           prop_masc_min, prop_masc_max,
           spline_fora_envelope, spline_desvio_pp,
           dif_metodos_pp) |>
    arrange(local, ano)

  write_csv(tabela, path)
  log_info_safe("Tabela comparativa salva em {path}")
  return(invisible(tabela))
}

# -----------------------------------------------------------------------------
# EXECUÇÃO — BLOCO 3
# -----------------------------------------------------------------------------

# Aliases explícitos para o script 02
anos_ancora <- config$anos_ancora
margem_pp   <- config$margem_envelope

limites_ancora <- calcular_envelope(ancora_completa, margem = margem_pp)
log_info_safe("Envelope por região (âncoras ± {margem_pp*100} p.p.):")
limites_ancora |> mutate(across(where(is.numeric), ~round(.x, 5))) |> print()

diag_spline_res <- diagnosticar_spline(prop_ambos, limites_ancora,
                                       anos_ancora = anos_ancora)

# Aliases para compatibilidade com o script 02
diag_spline         <- diag_spline_res$resumo
n_fora_spline       <- diag_spline_res$n_fora
resumo_spline       <- diag_spline_res$resumo
inflexoes_spline    <- diag_spline_res$inflexoes
diag_spline_por_ano <- diag_spline_res$por_ano

log_info_safe("Resumo spline por região:")
diag_spline |> mutate(across(where(is.double), ~round(.x, 4))) |> print()

if (nrow(diag_spline_res$inflexoes) > 0) {
  log_info_safe("Inflexões não-monotônicas da spline:")
  diag_spline_res$inflexoes |>
    mutate(across(where(is.numeric), ~round(.x, 6))) |>
    print()
}

diag_linear_res <- diagnosticar_linear(prop_ambos, limites_ancora,
                                       anos_ancora = anos_ancora,
                                       margem      = margem_pp)
n_fora_linear <- diag_linear_res$n_fora

comp_res    <- comparar_metodos(prop_ambos, anos_ancora = anos_ancora)
resumo_comp <- comp_res$resumo   # alias para script 02

log_info_safe("Diferença spline vs linear por região (p.p.):")
resumo_comp |> mutate(across(where(is.double), ~round(.x, 4))) |> print()

dif_max_global <- max(resumo_comp$dif_max_pp, na.rm = TRUE)
regiao_max_dif <- resumo_comp$local[which.max(resumo_comp$dif_max_pp)]
ano_max_dif    <- resumo_comp$ano_max_dif[which.max(resumo_comp$dif_max_pp)]

log_info_safe(paste0(
  "CONCLUSÃO: spline — {n_fora_spline} caso(s) fora do envelope | ",
  "{nrow(diag_spline_res$inflexoes)} inflexão(ões). ",
  "Linear — {n_fora_linear} caso(s). ",
  "Maior divergência: {round(dif_max_global,3)} p.p. ({regiao_max_dif}, {ano_max_dif})."
))

exportar_comparacao(
  prop_ambos          = prop_ambos,
  limites_ancora      = limites_ancora,
  diag_spline_por_ano = diag_spline_res$por_ano,
  anos_ancora         = anos_ancora,
  path                = config$path_comp_csv
)


# =============================================================================
# RESUMO FINAL
# =============================================================================

log_info_safe("\n===== RESUMO DA AQUISIÇÃO =====")
log_info_safe("SIM  — registros de suicídio (X60–X84) : {nrow(sim_suicidio)}")
log_info_safe(
  "SIM  — anos cobertos                   : {paste(sort(unique(sim_suicidio$ano_download)), collapse=', ')}"
)
log_info_safe("IBGE — registros na base                : {nrow(pop_exportar)}")
log_info_safe("IBGE — locais                           : {paste(config$locais, collapse=', ')}")
log_info_safe("IBGE — período                          : 2000–2022")
log_info_safe("IBGE — âncoras proporção sexo           : {paste(config$anos_ancora, collapse=', ')}")
log_info_safe("IBGE — total 2007                       : interpolação linear Tab6579 (2006+2008)/2")
log_info_safe("Interpolação adotada                    : linear (na.approx)")
log_info_safe("pop_feminino 2007                       : valor real da Tab794")
log_info_safe("Spline — casos fora do envelope         : {n_fora_spline}")
log_info_safe("Spline — inflexões não-monotônicas      : {nrow(diag_spline_res$inflexoes)}")
log_info_safe("Linear — casos fora do envelope         : {n_fora_linear}")
log_info_safe(
  "Maior divergência spline vs linear      : {round(dif_max_global,3)} p.p. ({regiao_max_dif}, {ano_max_dif})"
)
log_info_safe("Script 01 concluído — dados prontos para limpeza.")
