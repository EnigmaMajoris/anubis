# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 01_download_dados.R
# OBJETIVO: Aquisição de todos os dados brutos do projeto — microdados do
#           SIM/DATASUS (2000–2022) e construção da base populacional IBGE
#           a partir de dados baixados manualmente do SIDRA
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-03-29
# VERSÃO  : 9.0  ← refatorado em 2026-04-02
# -----------------------------------------------------------------------------
# HISTÓRICO DE VERSÕES:
#   v7.0 — versão original
#   v8.0 — correção do pop_feminino 2007; junção com coalesce; verificações
#           de estrutura das planilhas; if_else; verificação de colunas SIM
#   v9.0 — refatoração completa:
#           · Parâmetros centralizados em lista `config` (item 5.2)
#           · Lógica extraída em funções reutilizáveis e testáveis (item 5.1)
#           · as.numeric() → readr::parse_number() em todas as leituras de
#             dados populacionais — captura strings SIDRA como "-", "...",
#             pontos de milhar, vírgulas decimais (item 7)
#           · Logging estruturado com níveis INFO/WARN/ERROR via logger
#             (item 5.3) — fallback para message() se pacote indisponível
#           · renv já utilizado no projeto (item 6.1 — atendido em 00_setup.R)
#           · seed documentado como N/A neste script (item 6.2 — sem
#             aleatoriedade aqui; anotar quando modelagem for introduzida)
#           · Todas as correções da v8.0 mantidas
# -----------------------------------------------------------------------------
# NOTA SOBRE parse_number() vs as.numeric():
#   Neste dataset específico, os valores populacionais são integers nativos
#   do Excel — nenhum dos dois métodos produziria resultado diferente hoje.
#   Adotamos parse_number() porque:
#   (a) O SIDRA às vezes exporta planilhas com "-" ou "..." para dados
#       suprimidos, pontos como separador de milhar, ou vírgulas decimais
#       dependendo da versão/navegador de download. as.numeric() converte
#       tudo isso para NA silenciosamente. parse_number() faz o mesmo,
#       mas com aviso explícito, permitindo detectar o problema.
#   (b) A Tab200 já contém 265 células com "-" e "..." em grupos de idade
#       de UFs específicas — nenhuma afeta as linhas de Total por macrorregião
#       que usamos, mas ilustra que o risco é real nesses tipos de arquivos.
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# DEPENDÊNCIAS: 00_setup.R
# ENTRADA     : dados/brutos/Tabela6579_Estimativa_2000_2022.xlsx
#               dados/brutos/Tabela200_Censo2000_2010.xlsx
#               dados/brutos/Tabela9514_Censo2022.xlsx
#               dados/brutos/Tabela794_Contagem2007.xlsx
#               dados/brutos/Tabela7358_Projecao_2000_2022.xlsx  
# SAIDA       : dados/brutos/sim_2000_2022.rds
#               dados/limpos/populacao_ibge_tratada.csv
#               dados/limpos/comparacao_metodos_interpolacao.csv
# =============================================================================

library(microdatasus)
library(tidyverse)
library(readxl)
library(zoo)
library(glue)

# logger é opcional — se não disponível, usa message() como fallback
# Para instalar: install.packages("logger") + renv::snapshot()
if (requireNamespace("logger", quietly = TRUE)) {
  library(logger)
  log_info  <- logger::log_info
  log_warn  <- logger::log_warn
  log_error <- logger::log_error
} else {
  # Fallback simples preservando o contrato de interface
  log_info  <- function(...) message("[INFO]  ", glue(...))
  log_warn  <- function(...) warning("[WARN]  ", glue(...), call. = FALSE)
  log_error <- function(...) stop("[ERROR] ", glue(...), call. = FALSE)
}

dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/limpos", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CONFIGURAÇÃO CENTRALIZADA
# =============================================================================
# Todos os parâmetros que controlam o comportamento do script ficam aqui.
# Alterar qualquer valor nesta lista propaga automaticamente para todo o código.
# Isso facilita testes (ex: rodar só 2015:2022), auditorias e reprodutibilidade.
# =============================================================================

config <- list(
  # Período de análise
  anos_sim        = 2000:2022,

  # Regiões de interesse — ordem define fator para visualizações
  locais          = c("Brasil", "Norte", "Nordeste",
                      "Sudeste", "Sul", "Centro-Oeste"),

  # Anos com levantamento censitário/contagem direto (âncoras para proporção sexo)
  anos_ancora     = c(2000L, 2007L, 2010L, 2022L),

  # Margem de tolerância do envelope demográfico (em proporção, não p.p.)
  margem_envelope = 0.005,   # 0,5 p.p.

  # Caminhos de entrada (relativos ao raiz do projeto)
  path_tab200  = "dados/brutos/Tabela200_Censo2000_2010.xlsx",
  path_tab794  = "dados/brutos/Tabela794_Contagem2007.xlsx",
  path_tab6579 = "dados/brutos/Tabela6579_Estimativa_2000_2022.xlsx",
  path_tab9514 = "dados/brutos/Tabela9514_Censo2022.xlsx",

  # Caminhos de saída
  path_sim_rds    = "dados/brutos/sim_2000_2022.rds",
  path_pop_csv    = "dados/limpos/populacao_ibge_tratada.csv",
  path_comp_csv   = "dados/limpos/comparacao_metodos_interpolacao.csv",

  # Estrutura esperada da Tab9514 (posições 1-indexed das colunas de interesse)
  # Sempre verificar se houve mudança
  tab9514_col_total    = 2L,
  tab9514_col_homens   = 24L,
  tab9514_col_mulheres = 46L,

  # Número de linhas de cabeçalho a pular na Tab794
  tab794_skip = 7L
)

# =============================================================================
# FUNÇÕES AUXILIARES
# =============================================================================
# Cada função encapsula uma responsabilidade única, pode ser testada
# independentemente e tem contrato (entrada/saída) documentado.
# =============================================================================

# -----------------------------------------------------------------------------
# baixar_sim_ano()
# Baixa e processa um único ano do SIM/DATASUS.
# Retorna data.frame com coluna ano_download, ou NULL em caso de erro.
# -----------------------------------------------------------------------------
baixar_sim_ano <- function(ano) {
  log_info("Baixando SIM — ano {ano}...")
  tryCatch({
    df <- fetch_datasus(
      year_start         = ano,
      year_end           = ano,
      uf                 = "all",
      information_system = "SIM-DO"
    )
    df <- process_sim(df)
    df$ano_download <- ano
    return(df)
  }, error = function(e) {
    log_warn("ERRO no ano {ano}: {e$message}")
    return(NULL)
  })
}

# -----------------------------------------------------------------------------
# baixar_sim()
# Itera sobre todos os anos definidos em config, consolida em um único
# data.frame e verifica compatibilidade de colunas entre anos.
# Retorna data.frame consolidado.
# -----------------------------------------------------------------------------
baixar_sim <- function(anos = config$anos_sim) {
  lista_anos <- lapply(anos, baixar_sim_ano)
  lista_anos <- Filter(Negate(is.null), lista_anos)

  if (length(lista_anos) == 0) {
    log_error("Nenhum ano do SIM baixado com sucesso.")
  }

  # Verificar compatibilidade de colunas antes do bind_rows.
  # Anos diferentes do SIM podem ter colunas distintas por mudanças no template
  # — bind_rows preencheria as diferenças com NA silenciosamente.
  colunas_por_ano <- lapply(lista_anos, names)
  if (length(unique(lapply(colunas_por_ano, sort))) > 1) {
    anos_distintos <- sapply(lista_anos, function(df) unique(df$ano_download))
    log_warn(paste0(
      "Anos do SIM com estruturas de colunas diferentes — bind_rows vai ",
      "preencher colunas ausentes com NA. Anos afetados: ",
      paste(anos_distintos, collapse = ", ")
    ))
  } else {
    log_info("Estrutura de colunas consistente entre todos os anos do SIM.")
  }

  sim_bruto <- bind_rows(lista_anos)
  log_info("Total de registros baixados: {nrow(sim_bruto)}")
  log_info("Anos presentes: {paste(sort(unique(sim_bruto$ano_download)), collapse=', ')}")
  return(sim_bruto)
}

# -----------------------------------------------------------------------------
# ler_tab200()
# Lê a Tabela 200 (Censos 2000 e 2010) e retorna data.frame longo com
# colunas: local, ano, populacao_total, pop_masculino, pop_feminino.
# Aplica parse_number() para capturar strings SIDRA sem silenciar erros.
# -----------------------------------------------------------------------------
ler_tab200 <- function(path = config$path_tab200,
                       locais = config$locais) {
  log_info("Lendo Tabela 200 — Censos 2000 e 2010...")
  raw <- read_excel(path)
  names(raw) <- c("local", "grupo_idade",
                  "total_2000", "masc_2000", "fem_2000",
                  "total_2010", "masc_2010", "fem_2010")

  resultado <- raw |>
    filter(local %in% locais, grupo_idade == "Total") |>
    # parse_number(): converte strings como "-", "...", "1.234" sem silenciar
    mutate(across(total_2000:fem_2010, ~ readr::parse_number(as.character(.x)))) |>
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

  log_info("Tabela 200 OK — {nrow(resultado)} registros")
  return(resultado)
}

# -----------------------------------------------------------------------------
# ler_tab794()
# Lê a Tabela 794 (Contagem 2007) e retorna data.frame com
# colunas: local, ano, populacao_total, pop_masculino, pop_feminino.
# IMPORTANTE: pop_feminino é o valor REAL da fonte — não calculado por
# subtração. O total da Contagem inclui parcela com sexo não declarado,
# portanto pop_masculino + pop_feminino ≠ populacao_total em 2007.
# # -----------------------------------------------------------------------------
ler_tab794 <- function(path  = config$path_tab794,
                       skip  = config$tab794_skip,
                       locais = config$locais) {
  log_info("Lendo Tabela 794 — Contagem 2007...")
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

  # Documentar e logar a diferença esperada (sexo não declarado)
  dif_sem_sexo <- resultado |>
    mutate(dif = populacao_total - (pop_masculino + pop_feminino))
  log_info("Tabela 794 OK — {nrow(resultado)} registros")
  log_info("Diferença total-(M+F) em 2007 (pessoas c/ sexo não declarado):")
  dif_sem_sexo |>
    select(local, populacao_total, pop_masculino, pop_feminino, dif) |>
    print()

  return(resultado)
}

# -----------------------------------------------------------------------------
# ler_tab9514()
# Lê a Tabela 9514 (Censo 2022) e retorna data.frame com
# colunas: local, ano, populacao_total, pop_masculino, pop_feminino.
# Verifica explicitamente os rótulos das colunas de interesse antes de
# selecionar por posição — falha com erro descritivo se o layout mudar.
# -----------------------------------------------------------------------------
ler_tab9514 <- function(path   = config$path_tab9514,
                        locais = config$locais,
                        col_total    = config$tab9514_col_total,
                        col_homens   = config$tab9514_col_homens,
                        col_mulheres = config$tab9514_col_mulheres) {
  log_info("Lendo Tabela 9514 — Censo 2022...")
  raw <- read_excel(path, col_names = FALSE)

  # Verificação de estrutura: linha 6 (R 1-indexed) contém os rótulos de grupo
  # (Total | Homens | Mulheres). Falha com mensagem descritiva se layout mudar,
  # evitando seleção silenciosa da coluna errada.
  linha_header <- as.character(raw[6, ])
  rotulos_ok <- list(
    list(pos = col_total,    esperados = c("Total", "total", "TOTAL")),
    list(pos = col_homens,   esperados = c("Homens", "homens", "Masculino")),
    list(pos = col_mulheres, esperados = c("Mulheres", "mulheres", "Feminino"))
  )
  for (chk in rotulos_ok) {
    val <- linha_header[chk$pos]
    if (!val %in% chk$esperados) {
      log_error(paste0(
        "Tabela 9514: posição {chk$pos} esperava '{chk$esperados[1]}', ",
        "encontrou '{val}'. Layout mudou — ajustar config$tab9514_col_*."
      ))
    }
  }
  log_info("Tab9514 — posições verificadas: Total({col_total}), 
           Homens({col_homens}), Mulheres({col_mulheres}) ✓")

  resultado <- raw |>
    select(local        = all_of(col_total - 1L),  # col 1 = local (0-indexed internamente)
           total_2022   = all_of(col_total),
           masc_2022    = all_of(col_homens),
           fem_2022     = all_of(col_mulheres)) |>
    # Nota: select() com posições numéricas é mais explícito que nomes "...N"
    # porque não depende de nomes gerados automaticamente pelo readxl
    filter(local %in% locais) |>
    mutate(
      ano             = 2022L,
      populacao_total = readr::parse_number(as.character(total_2022)),
      pop_masculino   = readr::parse_number(as.character(masc_2022)),
      pop_feminino    = readr::parse_number(as.character(fem_2022))
    ) |>
    select(local, ano, populacao_total, pop_masculino, pop_feminino)

  log_info("Tabela 9514 OK — {nrow(resultado)} registros")
  return(resultado)
}

# -----------------------------------------------------------------------------
# ler_tab6579()
# Lê a Tabela 6579 (estimativas anuais 2001–2021, sem 2007) e retorna
# data.frame longo com colunas: local, ano, populacao_total.
# Exclui proativamente os anos-âncora para garantir que estimativas não
# sobrescrevam valores observados na junção posterior.
# -----------------------------------------------------------------------------
ler_tab6579 <- function(path       = config$path_tab6579,
                        locais     = config$locais,
                        anos_ancora = config$anos_ancora) {
  log_info("Lendo Tabela 6579 — estimativas anuais...")
  raw <- read_excel(path)
  names(raw)[1] <- "local"

  # Linha 3 do dataframe (R 1-indexed, com header automático na linha 1)
  # corresponde à 4ª linha da planilha, que contém os anos.
  # Verificar que a linha realmente contém inteiros no intervalo esperado
  # — falha rápido se o layout do arquivo mudar.
  anos_raw <- as.character(raw[3, -1])
  anos_num  <- suppressWarnings(as.integer(anos_raw))
  if (any(is.na(anos_num))) {
    log_error(paste0(
      "Tabela 6579: linha 3 contém valores não-inteiros como nomes de coluna. ",
      "Layout mudou — verificar número de linhas de cabeçalho."
    ))
  }
  anos_invalidos <- anos_num[!is.na(anos_num) & (anos_num < 1990 | anos_num > 2050)]
  if (length(anos_invalidos) > 0) {
    log_warn("Tabela 6579: anos fora do intervalo plausível: 
             {paste(anos_invalidos, collapse=', ')}")
  }
  names(raw)[-1] <- anos_raw
  log_info("Tabela 6579 — anos: {paste(sort(anos_num[!is.na(anos_num)]), 
           collapse=', ')}")

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
    # Excluir anos-âncora: estimativas não devem sobrescrever valores observados
    filter(!ano %in% anos_ancora) |>
    arrange(local, ano)

  log_info("Tabela 6579 OK — {nrow(resultado)} registros")
  return(resultado)
}

# -----------------------------------------------------------------------------
# construir_ancoras()
# Combina os quatro levantamentos censitários em uma única base de âncoras
# com proporções por sexo calculadas. Verifica presença dos 4 anos para
# todos os locais.
# Retorna data.frame com: local, ano, populacao_total, pop_masculino,
#   pop_feminino, prop_masc, prop_fem
# -----------------------------------------------------------------------------
construir_ancoras <- function(censo_2000_2010, contagem_2007, censo_2022,
                              anos_ancora = config$anos_ancora) {
  log_info("Construindo base de âncoras observadas...")

  ancora <- bind_rows(censo_2000_2010, contagem_2007, censo_2022) |>
    arrange(local, ano) |>
    mutate(
      prop_masc = pop_masculino / populacao_total,
      prop_fem  = pop_feminino  / populacao_total
    )

  # Verificar completude
  check <- ancora |> count(local, name = "n_ancoras")
  if (any(check$n_ancoras < length(anos_ancora))) {
    log_warn("Algum local com menos de {length(anos_ancora)} âncoras — verificar!")
    print(check)
  } else {
    log_info("Âncoras OK — {paste(anos_ancora, collapse=', ')} 
             presentes para todos os locais.")
  }

  return(ancora)
}

# -----------------------------------------------------------------------------
# montar_serie_total()
# Constrói a série de população total 2000–2022 por local, combinando
# âncoras censitárias e estimativas anuais via coalesce explícito.
# Âncoras sempre prevalecem sobre estimativas (sem dependência de ordem).
# Retorna data.frame completo: local, ano, populacao_total (sem NAs).
# -----------------------------------------------------------------------------
montar_serie_total <- function(ancora_completa, pop_total_6579,
                               locais = config$locais) {
  log_info("Montando série de população total 2000–2022...")

  grade <- expand_grid(local = locais, ano = 2000:2022)

  resultado <- grade |>
    left_join(
      ancora_completa |> select(local, ano, pop_ancora = populacao_total),
      by = c("local", "ano")
    ) |>
    left_join(
      pop_total_6579 |> select(local, ano, pop_estimada = populacao_total),
      by = c("local", "ano")
    ) |>
    mutate(populacao_total = coalesce(pop_ancora, pop_estimada)) |>
    select(local, ano, populacao_total) |>
    arrange(local, ano)

  faltando <- resultado |> filter(is.na(populacao_total))
  if (nrow(faltando) > 0) {
    log_warn("Anos/locais sem cobertura:")
    print(faltando)
  } else {
    log_info("Série total 2000–2022 completa para todos os locais.")
  }

  return(resultado)
}

# -----------------------------------------------------------------------------
# interpolar_proporcoes()
# Calcula proporções masculina/feminina interpoladas para o período completo
# usando dois métodos em paralelo (spline cúbica e linear por partes).
# Retorna lista com:
#   $prop_ambos     — data.frame com ambos os métodos (para diagnóstico)
#   $prop_final     — data.frame com método o método escolhido (spoiler: linear)
# -----------------------------------------------------------------------------
interpolar_proporcoes <- function(ancora_completa,
                                  locais = config$locais) {
  log_info("Interpolando proporções por sexo — spline e linear em paralelo...")

  grade <- expand_grid(local = locais, ano = 2000:2022)

  prop_base <- grade |>
    left_join(
      ancora_completa |> select(local, ano, prop_masc, prop_fem),
      by = c("local", "ano")
    ) |>
    arrange(local, ano)

  # Spline cúbica natural (zoo::na.spline)
  # Avaliada para documentar overshoot — NÃO adotada para a série final
  prop_spline <- prop_base |>
    group_by(local) |>
    mutate(
      prop_masc_spline = zoo::na.spline(prop_masc, x = ano, na.rm = FALSE),
      prop_fem_spline  = zoo::na.spline(prop_fem,  x = ano, na.rm = FALSE)
    ) |>
    ungroup() |>
    select(local, ano, prop_masc_spline, prop_fem_spline)

  # Interpolação linear por partes (zoo::na.approx) — MÉTODO ADOTADO
  # Escolhida por permanecer dentro do envelope demográfico das âncoras
  # em todos os anos/regiões (0 overshoot). Ver Bloco 3 e notas.qmd seção 3.
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
    log_warn("Proporções com NA após interpolação — verificar âncoras!")
  } else {
    log_info("Interpolação concluída (spline e linear).")
  }

  prop_final <- prop_ambos |>
    select(local, ano,
           prop_masc = prop_masc_linear,
           prop_fem  = prop_fem_linear)

  return(list(prop_ambos = prop_ambos, prop_final = prop_final))
}

# -----------------------------------------------------------------------------
# construir_serie_final()
# Aplica proporções interpoladas (lineares) sobre os totais populacionais.
# Para anos-âncora, os valores reais observados de pop_masculino e
# pop_feminino são restaurados via coalesce (preservando valores da fonte).
# Em 2007, pop_masculino + pop_feminino ≠ populacao_total é esperado
# (ver ler_tab794() e notas.qmd seção 2).
# Retorna data.frame: local, ano, populacao_total, pop_masculino, pop_feminino
# -----------------------------------------------------------------------------
construir_serie_final <- function(pop_total_todos, prop_final,
                                  ancora_completa) {
  log_info("Construindo série final com proporções lineares interpoladas...")

  pop <- pop_total_todos |>
    left_join(prop_final, by = c("local", "ano")) |>
    mutate(
      pop_masculino = round(populacao_total * prop_masc),
      pop_feminino  = round(populacao_total * prop_fem)
    ) |>
    # Restaurar valores reais para anos-âncora
    # (sem isso, os anos-âncora teriam pequenas diferenças de arredondamento)
    left_join(
      ancora_completa |>
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

  # Validação: dif > 1 fora de 2007 indica problema real
  # Em 2007 a diferença é esperada (sexo não declarado na Contagem)
  validacao <- pop |>
    mutate(dif = abs(populacao_total - (pop_masculino + pop_feminino)))

  problemas <- validacao |> filter(ano != 2007, dif > 1)
  if (nrow(problemas) > 0) {
    log_warn("Diferença M+F > 1 fora de 2007 — verificar dados!")
    print(problemas)
  } else {
    log_info("Consistência M+F validada para todos os anos (exceto 2007, onde dif é esperada).")
  }

  return(pop)
}

# -----------------------------------------------------------------------------
# calcular_envelope()
# Deriva o envelope demográfico de plausibilidade a partir das âncoras.
# Usado tanto no diagnóstico da spline quanto nas figuras do notas.qmd.
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# diagnosticar_spline()
# Avalia a spline cúbica contra o envelope demográfico e detecta inflexões
# não-monotônicas. Retorna lista com:
#   $por_ano     — diagnóstico ano a ano
#   $resumo      — resumo por região
#   $inflexoes   — inflexões não-monotônicas detectadas
#   $n_fora      — total de combinações ano/região fora do envelope
# -----------------------------------------------------------------------------
diagnosticar_spline <- function(prop_ambos, limites_ancora,
                                anos_ancora = config$anos_ancora) {
  log_info("Diagnóstico da spline cúbica...")

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

  log_info("Spline — {n_fora} caso(s) fora do envelope | {nrow(inflexoes)} inflexão(ões)")
  return(list(por_ano = por_ano, resumo = resumo,
              inflexoes = inflexoes, n_fora = n_fora))
}

# -----------------------------------------------------------------------------
# diagnosticar_linear()
# Verifica se a interpolação linear permanece dentro do envelope.
# Por definição matemática, a linear nunca extrapola fora das âncoras,
# então esta função serve principalmente como teste de sanidade e
# documentação formal do resultado para o manuscrito.
# Retorna lista com $por_ano e $n_fora.
# -----------------------------------------------------------------------------
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
    log_info("Linear — APROVADA: 0 casos fora do envelope 
             (âncoras ± {margem*100} p.p.)")
  } else {
    log_warn("Linear — {n_fora} caso(s) fora do envelope:")
    por_ano |>
      filter(fora_envelope) |>
      select(local, ano, prop_masc_linear, prop_masc_min, prop_masc_max) |>
      print()
  }

  return(list(por_ano = por_ano, n_fora = n_fora))
}

# -----------------------------------------------------------------------------
# comparar_metodos()
# Calcula a diferença absoluta entre spline e linear ano a ano.
# Quantifica o impacto prático da escolha metodológica.
# Retorna lista com $detalhe (por ano/região) e $resumo (por região).
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# exportar_comparacao()
# Monta e salva a tabela comparativa completa para o manuscrito.
# Inclui os dois métodos, os limites do envelope, o diagnóstico da spline
# e a diferença entre métodos para cada ano/região.
# -----------------------------------------------------------------------------
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
  log_info("Tabela comparativa salva em {path}")
  return(invisible(tabela))
}

# =============================================================================
# EXECUÇÃO PRINCIPAL
# =============================================================================
# O script é organizado como uma sequência de chamadas às funções acima.
# Cada etapa produz um objeto nomeado que é passado explicitamente para a
# próxima — sem dependências ocultas de ambiente global.
# =============================================================================

# -----------------------------------------------------------------------------
# BLOCO 1 — DOWNLOAD SIM/DATASUS
# -----------------------------------------------------------------------------
log_info("\n===== BLOCO 1: SIM/DATASUS =====\n")

sim_bruto <- baixar_sim(anos = config$anos_sim)
saveRDS(sim_bruto, config$path_sim_rds)
log_info("SIM salvo em {config$path_sim_rds}")

# -----------------------------------------------------------------------------
# BLOCO 2 — POPULAÇÃO IBGE
# -----------------------------------------------------------------------------
log_info("\n===== BLOCO 2: POPULAÇÃO IBGE =====\n")

# 2.1 Ler cada fonte censitária
censo_2000_2010 <- ler_tab200(locais = config$locais)
contagem_2007   <- ler_tab794(locais = config$locais)
censo_2022      <- ler_tab9514(locais = config$locais)
pop_total_6579  <- ler_tab6579(locais = config$locais,
                                anos_ancora = config$anos_ancora)

# 2.2 Consolidar âncoras e calcular proporções observadas
ancora_completa <- construir_ancoras(censo_2000_2010, contagem_2007, censo_2022,
                                     anos_ancora = config$anos_ancora)

# 2.3 Série de totais 2000–2022
pop_total_todos <- montar_serie_total(ancora_completa, pop_total_6579,
                                      locais = config$locais)

# 2.4 Interpolação das proporções por sexo
interp          <- interpolar_proporcoes(ancora_completa, locais = config$locais)
prop_ambos      <- interp$prop_ambos   # mantido em memória para o script 02
prop_final      <- interp$prop_final

# 2.5 Série final
pop_final <- construir_serie_final(pop_total_todos, prop_final, ancora_completa)

# Exibir resumo Brasil para conferência visual
log_info("\nResumo — Brasil:")
pop_final |>
  filter(local == "Brasil") |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  print(n = 23)

# Exportar
pop_exportar <- pop_final |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  arrange(local, ano)

write_csv(pop_exportar, config$path_pop_csv)
log_info("Arquivo salvo em {config$path_pop_csv}")

# -----------------------------------------------------------------------------
# BLOCO 3 — COMPARAÇÃO DE MÉTODOS DE INTERPOLAÇÃO
# -----------------------------------------------------------------------------
log_info("\n===== BLOCO 3: COMPARAÇÃO DE MÉTODOS =====\n")

# Objetos derivados do bloco 2 — mantidos em memória para o script 02
anos_ancora   <- config$anos_ancora   # alias explícito (script 02 usa este nome)
margem_pp     <- config$margem_envelope

limites_ancora  <- calcular_envelope(ancora_completa, margem = margem_pp)
log_info("Envelope por região (âncoras ± {margem_pp*100} p.p.):")
limites_ancora |> mutate(across(where(is.numeric), ~ round(.x, 5))) |> print()

diag_spline_res <- diagnosticar_spline(prop_ambos, limites_ancora,
                                        anos_ancora = anos_ancora)
diag_spline     <- diag_spline_res$resumo      # alias para script 02
n_fora_spline   <- diag_spline_res$n_fora

log_info("Resumo spline por região:")
diag_spline |> mutate(across(where(is.double), ~ round(.x, 4))) |> print()

if (nrow(diag_spline_res$inflexoes) > 0) {
  log_info("Inflexões não-monotônicas da spline:")
  diag_spline_res$inflexoes |>
    mutate(across(where(is.numeric), ~ round(.x, 6))) |>
    print()
}

diag_linear_res <- diagnosticar_linear(prop_ambos, limites_ancora,
                                        anos_ancora = anos_ancora,
                                        margem      = margem_pp)
n_fora_linear   <- diag_linear_res$n_fora

comp_res      <- comparar_metodos(prop_ambos, anos_ancora = anos_ancora)
resumo_comp   <- comp_res$resumo   # alias para script 02

log_info("Diferença spline vs linear por região (p.p.):")
resumo_comp |> mutate(across(where(is.double), ~ round(.x, 4))) |> print()

# Síntese para o log
dif_max_global <- max(resumo_comp$dif_max_pp, na.rm = TRUE)
regiao_max_dif <- resumo_comp$local[which.max(resumo_comp$dif_max_pp)]
ano_max_dif    <- resumo_comp$ano_max_dif[which.max(resumo_comp$dif_max_pp)]

log_info(paste0(
  "CONCLUSÃO: spline — {n_fora_spline} caso(s) fora do envelope | ", "{nrow(diag_spline_res$inflexoes)} inflexão(ões). ",
  "Linear — {n_fora_linear} caso(s). ",
  "Maior divergência: {round(dif_max_global,3)} p.p. ({regiao_max_dif}, {ano_max_dif})."
))

exportar_comparacao(
  prop_ambos         = prop_ambos,
  limites_ancora     = limites_ancora,
  diag_spline_por_ano = diag_spline_res$por_ano,
  anos_ancora        = anos_ancora,
  path               = config$path_comp_csv
)

# =============================================================================
# RESUMO FINAL
# =============================================================================
log_info("\n===== RESUMO DA AQUISIÇÃO =====")
log_info("SIM  — registros baixados          : {nrow(sim_bruto)}")
log_info("IBGE — registros na base            : {nrow(pop_exportar)}")
log_info("IBGE — locais                       : {paste(config$locais, collapse=', ')}")
log_info("IBGE — período                      : 2000–2022")
log_info("IBGE — âncoras                      : {paste(config$anos_ancora, collapse=', ')}")
log_info("Interpolação adotada                : linear (na.approx)")
log_info("pop_feminino 2007                   : valor real da Tab794")
log_info("Spline — casos fora do envelope     : {n_fora_spline}")
log_info("Spline — inflexões não-monotônicas  : {nrow(diag_spline_res$inflexoes)}")
log_info("Linear — casos fora do envelope     : {n_fora_linear}")
log_info("Maior divergência spline vs linear  : {round(dif_max_global,3)} p.p. ({regiao_max_dif}, {ano_max_dif})")
log_info("Script 01 concluído — dados prontos para limpeza.")
