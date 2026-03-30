# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 01_download_dados.R
# OBJETIVO: Aquisição de todos os dados brutos do projeto — microdados do
#           SIM/DATASUS (2000–2022) e construção da base populacional IBGE
#           a partir de dados baixados manualmente do SIDRA
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-03-29
# VERSÃO  : 7.0
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# DEPENDÊNCIAS: 00_setup.R
# ENTRADA     : dados/brutos/Tabela6579_Estimativa_2000_2022.xlsx
#               dados/brutos/Tabela200_Censo2000_2010.xlsx
#               dados/brutos/Tabela_9514_Censo2022.xlsx
#               dados/brutos/Tabela_794_Contagem2007.xlsx
#               dados/brutos/Tabela7358_Projecao_2000_2022.xlsx
# SAIDA       : dados/brutos/sim_2000_2022.rds
#               dados/limpos/populacao_ibge_tratada.csv
#               dados/limpos/comparacao_metodos_interpolacao.csv
# -----------------------------------------------------------------------------
# ESTRATEGIA POPULACIONAL:
#   Passo 1 — Âncoras observadas por região:
#               2000      : Censo (Tabela 200)
#               2007      : Contagem da População (Tabela 794)
#               2010      : Censo (Tabela 200)
#               2022      : Censo (Tabela 9514)
#   Passo 2 — Total anual 2001–2021 (exceto 2007): Tabela 6579
#   Passo 3 — Proporções por sexo nos anos-âncora (2000, 2007, 2010, 2022)
#   Passo 4 — Interpolação das proporções por dois métodos em paralelo:
#               Spline  : zoo::na.spline (cúbica natural)
#               Linear  : zoo::na.approx (interpolação linear por partes)
#   Passo 5 — Comparação dos métodos (Bloco 3): diagnóstico de overshoot
#             da spline e evidência formal da superioridade da interpolação
#             linear para fundamentar escolha metodológica no manuscrito
#   Passo 6 — Série final construída com interpolação linear (método adotado)
#   Passo 7 — Aplicar proporções lineares interpoladas na população total
# -----------------------------------------------------------------------------
# NOTAS:
#   - 2007 é âncora censitária observada (Contagem), NÃO imputado
#   - A Tabela 6579 não contém 2007 por ser operação censitária distinta
#   - A spline produz overshoot no intervalo curto 2007–2010 (3 anos) e
#     nos segmentos adjacentes, gerando proporções fora do envelope das
#     âncoras observadas — documentado no Bloco 3 para o manuscrito
#   - A interpolação linear é conservadora e permanece por definição dentro
#     do envelope das âncoras — escolha adotada para a série final
#   - A Tabela 7358 (Projeção Rev. 2018) NÃO é utilizada neste script;
#     ela serve como insumo para análise descritiva em script posterior
#   - Dados brutos SIM não versionados no GitHub (ver .gitignore)
#   - Em caso de falha em ano específico do SIM, o script continua os demais
# -----------------------------------------------------------------------------
# FONTES:
#   SIM/DATASUS  : datasus.saude.gov.br
#   SIDRA 6579   : sidra.ibge.gov.br/tabela/6579  (estimativas anuais)
#   SIDRA 200    : sidra.ibge.gov.br/tabela/200   (Censo 2000 e 2010)
#   SIDRA 9514   : sidra.ibge.gov.br/tabela/9514  (Censo 2022)
#   SIDRA 794    : sidra.ibge.gov.br/tabela/794   (Contagem 2007)
# =============================================================================

library(microdatasus)
library(tidyverse)
library(readxl)
library(zoo)
library(glue)

dir.create("dados/brutos", recursive = TRUE, showWarnings = FALSE)
dir.create("dados/limpos", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# BLOCO 1 — DOWNLOAD SIM/DATASUS
# =============================================================================
message("\n===== BLOCO 1: SIM/DATASUS =====\n")

anos <- 2000:2022

baixar_sim_ano <- function(ano) {
  message(glue("Baixando SIM — ano {ano}..."))
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
    message(glue("ERRO no ano {ano}: {e$message}"))
    return(NULL)
  })
}

lista_anos <- lapply(anos, baixar_sim_ano)
lista_anos <- Filter(Negate(is.null), lista_anos)

sim_bruto <- bind_rows(lista_anos)

message(glue("Total de registros baixados: {nrow(sim_bruto)}"))
message("Anos presentes: ",
        paste(sort(unique(sim_bruto$ano_download)), collapse = ", "))

saveRDS(sim_bruto, "dados/brutos/sim_2000_2022.rds")
message("SIM salvo em dados/brutos/sim_2000_2022.rds")

# =============================================================================
# BLOCO 2 — POPULAÇÃO IBGE
# =============================================================================
message("\n===== BLOCO 2: POPULAÇÃO IBGE =====\n")

locais_interesse <- c("Brasil", "Norte", "Nordeste",
                      "Sudeste", "Sul", "Centro-Oeste")

# -----------------------------------------------------------------------------
# 2.1 Tabela 6579 — população total anual por região (2001–2021, sem 2007)
# Formato: regiões nas linhas, anos nas colunas
# -----------------------------------------------------------------------------
message("Lendo Tabela 6579 — estimativas anuais...")

tab6579_raw <- read_excel("dados/brutos/Tabela6579_Estimativa_2000_2022.xlsx")

names(tab6579_raw)[1] <- "local"

# Linha 3 contém os anos
anos_6579 <- as.character(tab6579_raw[3, -1])
names(tab6579_raw)[-1] <- anos_6579

pop_total_6579 <- tab6579_raw |>
  filter(local %in% locais_interesse) |>
  pivot_longer(
    cols      = -local,
    names_to  = "ano",
    values_to = "populacao_total"
  ) |>
  mutate(
    ano             = as.integer(ano),
    populacao_total = as.numeric(populacao_total)
  ) |>
  filter(!is.na(ano), !is.na(populacao_total)) |>
  arrange(local, ano)

message(glue("Tabela 6579 OK — {nrow(pop_total_6579)} registros"))

# -----------------------------------------------------------------------------
# 2.2 Tabela 200 — Censos 2000 e 2010 — total e sexo por região
# Formato: col1=local, col2=grupo_idade, col3-5=2000(total/homens/mulheres),
#          col6-8=2010(total/homens/mulheres)
# -----------------------------------------------------------------------------
message("Lendo Tabela 200 — Censos 2000 e 2010...")

tab200_raw <- read_excel("dados/brutos/Tabela200_Censo2000_2010.xlsx")

names(tab200_raw) <- c("local", "grupo_idade",
                       "total_2000", "masc_2000", "fem_2000",
                       "total_2010", "masc_2010", "fem_2010")

censo_2000_2010 <- tab200_raw |>
  filter(
    local       %in% locais_interesse,
    grupo_idade == "Total"
  ) |>
  mutate(across(total_2000:fem_2010, as.numeric)) |>
  select(local, total_2000, masc_2000, fem_2000,
         total_2010, masc_2010, fem_2010) |>
  pivot_longer(
    cols      = -local,
    names_to  = c(".value", "ano"),
    names_sep = "_"
  ) |>
  rename(
    populacao_total = total,
    pop_masculino   = masc,
    pop_feminino    = fem
  ) |>
  mutate(ano = as.integer(ano))

message(glue("Tabela 200 OK — {nrow(censo_2000_2010)} registros"))

# -----------------------------------------------------------------------------
# 2.3 Tabela 9514 — Censo 2022 — total e sexo por região
# Formato: col1=local, col2=Total geral, col24=Homens Total, col46=Mulheres Total
# -----------------------------------------------------------------------------
message("Lendo Tabela 9514 — Censo 2022...")

tab9514_raw <- read_excel("dados/brutos/Tabela9514_Censo2022.xlsx")

names(tab9514_raw)[1] <- "local"

censo_2022 <- tab9514_raw |>
  select(local,
         total_2022 = ...2,
         masc_2022  = ...24,
         fem_2022   = ...46) |>
  filter(local %in% locais_interesse) |>
  mutate(
    ano             = 2022L,
    populacao_total = as.numeric(total_2022),
    pop_masculino   = as.numeric(masc_2022),
    pop_feminino    = as.numeric(fem_2022)
  ) |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino)

message(glue("Tabela 9514 OK — {nrow(censo_2022)} registros"))

# -----------------------------------------------------------------------------
# 2.4 Tabela 794 — Contagem da População 2007 — total e sexo por região
# Formato esperado: col1=local, colunas de total geral, homens e mulheres
# NOTA: 2007 é âncora OBSERVADA — não será imputado por spline
# -----------------------------------------------------------------------------
message("Lendo Tabela 794 — Contagem 2007...")

tab794_raw <- read_excel(
  "dados/brutos/Tabela794_Contagem2007.xlsx",
  skip = 7    # pula 7 linhas de cabeçalho do SIDRA (título, variável, filtros)
)

names(tab794_raw) <- c("local", "grupo_idade",
                       "populacao_total", "pop_masculino", "pop_feminino")

contagem_2007 <- tab794_raw |>
  filter(
    local       %in% locais_interesse,
    grupo_idade == "Total"
  ) |>
  mutate(
    ano             = 2007L,
    populacao_total = as.numeric(populacao_total),
    pop_masculino   = as.numeric(pop_masculino),
    pop_feminino    = as.numeric(pop_feminino)
  ) |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino)

message(glue("Tabela 794 OK — {nrow(contagem_2007)} registros"))

# -----------------------------------------------------------------------------
# 2.5 Base censitária completa (2000, 2007, 2010, 2022) com proporções
# Quatro âncoras observadas — nenhuma imputação por spline nos totais
# -----------------------------------------------------------------------------
message("Construindo base de âncoras observadas...")

ancora_completa <- bind_rows(censo_2000_2010, contagem_2007, censo_2022) |>
  arrange(local, ano) |>
  mutate(
    prop_masc = pop_masculino / populacao_total,
    prop_fem  = pop_feminino  / populacao_total
  )

# Verificar se os 4 anos-âncora estão presentes para todos os locais
ancora_check <- ancora_completa |>
  count(local, name = "n_ancoras")

anos_ancora_esperados <- 4L

if (any(ancora_check$n_ancoras < anos_ancora_esperados)) {
  warning("Algum local com âncoras faltando — verificar!")
  print(ancora_check)
} else {
  message("Âncoras OK — 2000, 2007, 2010 e 2022 presentes para todos os locais")
}

# -----------------------------------------------------------------------------
# 2.6 Grade completa local x ano (2000–2022)
# -----------------------------------------------------------------------------
grade_completa <- expand_grid(
  local = locais_interesse,
  ano   = 2000:2022
)

# -----------------------------------------------------------------------------
# 2.7 Montar população total 2000–2022 por local
#   2000       : Censo (Tabela 200)
#   2001–2006  : Estimativas (Tabela 6579)
#   2007       : Contagem (Tabela 794) — âncora observada
#   2008–2021  : Estimativas (Tabela 6579)
#   2022       : Censo (Tabela 9514)
# Não há anos sem cobertura — spline não é necessário para os totais
# -----------------------------------------------------------------------------
message("Montando série de população total 2000–2022...")

pop_total_todos <- grade_completa |>
  left_join(
    bind_rows(
      ancora_completa |> select(local, ano, populacao_total),
      pop_total_6579
    ) |>
      distinct(local, ano, .keep_all = TRUE),
    by = c("local", "ano")
  ) |>
  arrange(local, ano)

# Verificar se não há NAs remanescentes
anos_faltando <- pop_total_todos |>
  filter(is.na(populacao_total))

if (nrow(anos_faltando) > 0) {
  warning("Anos/locais sem cobertura na série total:")
  print(anos_faltando)
} else {
  message("Série total 2000–2022 completa para todos os locais — OK!")
}

# -----------------------------------------------------------------------------
# 2.8 Interpolação das proporções por sexo — dois métodos em paralelo
# Spline e linear calculados simultaneamente sobre as mesmas âncoras.
# O Bloco 3 compara os métodos; a série final usa interpolação linear.
# -----------------------------------------------------------------------------
message("Interpolando proporções por sexo — spline e linear em paralelo...")

prop_base <- grade_completa |>
  left_join(
    ancora_completa |> select(local, ano, prop_masc, prop_fem),
    by = c("local", "ano")
  ) |>
  arrange(local, ano)

# Spline cúbica natural
prop_spline <- prop_base |>
  group_by(local) |>
  mutate(
    prop_masc_spline = zoo::na.spline(prop_masc, x = ano, na.rm = FALSE),
    prop_fem_spline  = zoo::na.spline(prop_fem,  x = ano, na.rm = FALSE)
  ) |>
  ungroup() |>
  select(local, ano, prop_masc_spline, prop_fem_spline)

# Interpolação linear por partes
prop_linear <- prop_base |>
  group_by(local) |>
  mutate(
    prop_masc_linear = zoo::na.approx(prop_masc, x = ano, na.rm = FALSE),
    prop_fem_linear  = zoo::na.approx(prop_fem,  x = ano, na.rm = FALSE)
  ) |>
  ungroup() |>
  select(local, ano, prop_masc_linear, prop_fem_linear)

# Base unificada com ambos os métodos
prop_ambos <- prop_spline |>
  left_join(prop_linear, by = c("local", "ano"))

if (anyNA(prop_ambos$prop_masc_spline) | anyNA(prop_ambos$prop_masc_linear)) {
  warning("Proporções com NA após interpolação — verificar âncoras!")
} else {
  message("Interpolação (spline e linear) concluída — OK!")
}

# Série final: interpolação linear (método adotado após comparação no Bloco 3)
prop_interpolada <- prop_ambos |>
  select(local, ano,
         prop_masc = prop_masc_linear,
         prop_fem  = prop_fem_linear)

# -----------------------------------------------------------------------------
# 2.9 Aplicar proporções interpoladas (lineares) na população total
# -----------------------------------------------------------------------------
message("Aplicando proporções na população total...")

pop_final <- pop_total_todos |>
  left_join(prop_interpolada, by = c("local", "ano")) |>
  mutate(
    pop_masculino = round(populacao_total * prop_masc),
    pop_feminino  = populacao_total - pop_masculino  
  )

# -----------------------------------------------------------------------------
# 2.10 Validação de consistência M+F
# -----------------------------------------------------------------------------
pop_final <- pop_final |>
  mutate(
    soma_sexos = pop_masculino + pop_feminino,
    dif        = abs(populacao_total - soma_sexos)
  )

if (max(pop_final$dif, na.rm = TRUE) > 1) {
  warning("Diferença entre total e soma M+F — verificar dados!")
} else {
  message("Consistência M+F validada — OK!")
}

# Exibir resumo Brasil para conferência visual
message("\nResumo — Brasil:")
pop_final |>
  filter(local == "Brasil") |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  print(n = 23)

# -----------------------------------------------------------------------------
# 2.11 Exportação da série final
# -----------------------------------------------------------------------------
pop_exportar <- pop_final |>
  select(local, ano, populacao_total, pop_masculino, pop_feminino) |>
  arrange(local, ano)

write_csv(pop_exportar, "dados/limpos/populacao_ibge_tratada.csv")
message("Arquivo salvo em dados/limpos/populacao_ibge_tratada.csv")

# =============================================================================
# BLOCO 3 — COMPARAÇÃO DE MÉTODOS DE INTERPOLAÇÃO
# =============================================================================
# Objetivo: documentar formalmente por que a interpolação linear foi adotada
# em detrimento da spline cúbica, gerando evidência para o manuscrito.
#
# Estrutura:
#   3.1 — Definir envelope demográfico das âncoras (limites de plausibilidade)
#   3.2 — Diagnóstico da spline: overshoot e inflexões não-monotônicas
#   3.3 — Diagnóstico da linear: verificação dentro do envelope
#   3.4 — Comparação direta spline vs linear (diferença entre métodos)
#   3.5 — Síntese qualitativa e justificativa formal da escolha
#   3.6 — Exportação da tabela comparativa para o manuscrito
# =============================================================================
message("\n===== BLOCO 3: COMPARAÇÃO DE MÉTODOS DE INTERPOLAÇÃO =====\n")

anos_ancora <- c(2000L, 2007L, 2010L, 2022L)
margem_pp   <- 0.005   # 0,5 p.p. de tolerância sobre o envelope das âncoras

# -----------------------------------------------------------------------------
# 3.1 Envelope demográfico — limites derivados das âncoras observadas
# -----------------------------------------------------------------------------
message("Derivando envelope demográfico das âncoras...")

limites_ancora <- ancora_completa |>
  group_by(local) |>
  summarise(
    prop_masc_min_obs = min(prop_masc, na.rm = TRUE),
    prop_masc_max_obs = max(prop_masc, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    prop_masc_min = prop_masc_min_obs - margem_pp,
    prop_masc_max = prop_masc_max_obs + margem_pp
  )

message("Envelope por região (âncoras ± 0,5 p.p.):")
limites_ancora |>
  mutate(across(where(is.numeric), ~ round(.x, 5))) |>
  print()

# -----------------------------------------------------------------------------
# 3.2 Diagnóstico da spline — overshoot e inflexões
# -----------------------------------------------------------------------------
message("\n--- Diagnóstico: Spline cúbica ---")

anos_interp <- setdiff(2000:2022, anos_ancora)

diag_spline <- prop_ambos |>
  filter(!ano %in% anos_ancora) |>
  left_join(limites_ancora, by = "local") |>
  mutate(
    fora_envelope = prop_masc_spline < prop_masc_min |
      prop_masc_spline > prop_masc_max,
    desvio_pp     = case_when(
      prop_masc_spline < prop_masc_min ~
        (prop_masc_min - prop_masc_spline) * 100,
      prop_masc_spline > prop_masc_max ~
        (prop_masc_spline - prop_masc_max) * 100,
      TRUE ~ 0
    )
  )

n_fora_spline <- sum(diag_spline$fora_envelope, na.rm = TRUE)

resumo_spline <- diag_spline |>
  group_by(local) |>
  summarise(
    n_anos_fora  = sum(fora_envelope, na.rm = TRUE),
    desvio_max_pp = max(desvio_pp, na.rm = TRUE),
    ano_pior      = ifelse(
      any(fora_envelope, na.rm = TRUE),
      ano[which.max(desvio_pp)],
      NA_integer_
    ),
    .groups = "drop"
  ) |>
  arrange(desc(n_anos_fora), desc(desvio_max_pp))

message(glue("\nSpline — total de anos/região fora do envelope: {n_fora_spline}"))
message("Resumo por região:")
resumo_spline |>
  mutate(across(where(is.double), ~ round(.x, 4))) |>
  print()

# Inflexões não-monotônicas da spline
inflexoes_spline <- prop_ambos |>
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

message(glue("\nSpline — inflexões não-monotônicas detectadas: ",
             "{nrow(inflexoes_spline)}"))
if (nrow(inflexoes_spline) > 0) {
  inflexoes_spline |>
    mutate(across(where(is.numeric), ~ round(.x, 6))) |>
    print()
}

# -----------------------------------------------------------------------------
# 3.3 Diagnóstico da linear — verificação dentro do envelope
# A interpolação linear entre âncoras é monotônica por definição em cada
# segmento — não pode extrapolrar fora do envelope das próprias âncoras
# -----------------------------------------------------------------------------
message("\n--- Diagnóstico: Interpolação linear ---")

diag_linear <- prop_ambos |>
  filter(!ano %in% anos_ancora) |>
  left_join(limites_ancora, by = "local") |>
  mutate(
    fora_envelope = prop_masc_linear < prop_masc_min |
      prop_masc_linear > prop_masc_max,
    desvio_pp     = case_when(
      prop_masc_linear < prop_masc_min ~
        (prop_masc_min - prop_masc_linear) * 100,
      prop_masc_linear > prop_masc_max ~
        (prop_masc_linear - prop_masc_max) * 100,
      TRUE ~ 0
    )
  )

n_fora_linear <- sum(diag_linear$fora_envelope, na.rm = TRUE)

if (n_fora_linear == 0) {
  message(glue(
    "Linear — APROVADA: nenhum ano/região fora do envelope ",
    "(âncoras ± {margem_pp * 100} p.p.)"
  ))
} else {
  warning(glue("Linear — {n_fora_linear} caso(s) fora do envelope — verificar!"))
  diag_linear |>
    filter(fora_envelope) |>
    select(local, ano, prop_masc_linear, prop_masc_min, prop_masc_max) |>
    print()
}

# -----------------------------------------------------------------------------
# 3.4 Comparação direta: diferença entre os métodos ano a ano
# Quantifica o impacto prático da escolha metodológica na série final
# -----------------------------------------------------------------------------
message("\n--- Comparação direta: spline vs linear ---")

comparacao <- prop_ambos |>
  filter(!ano %in% anos_ancora) |>
  mutate(
    dif_pp        = (prop_masc_spline - prop_masc_linear) * 100,
    dif_abs_pp    = abs(dif_pp)
  )

resumo_comp <- comparacao |>
  group_by(local) |>
  summarise(
    dif_media_pp = mean(dif_abs_pp, na.rm = TRUE),
    dif_max_pp   = max(dif_abs_pp,  na.rm = TRUE),
    ano_max_dif  = ano[which.max(dif_abs_pp)],
    .groups      = "drop"
  ) |>
  arrange(desc(dif_max_pp))

message("\nDiferença spline vs linear por região (p.p. na proporção masculina):")
resumo_comp |>
  mutate(across(where(is.double), ~ round(.x, 4))) |>
  print()

# -----------------------------------------------------------------------------
# 3.5 Síntese qualitativa — justificativa formal para o manuscrito
# -----------------------------------------------------------------------------
message("\n--- Síntese e justificativa metodológica ---\n")

message(glue(
  "Spline cúbica : {n_fora_spline} caso(s) fora do envelope | ",
  "{nrow(inflexoes_spline)} inflexão(ões) não-monotônica(s)"
))
message(glue(
  "Linear        : {n_fora_linear} caso(s) fora do envelope | ",
  "0 inflexões por definição"
))

dif_max_global <- max(resumo_comp$dif_max_pp, na.rm = TRUE)
regiao_max_dif <- resumo_comp$local[which.max(resumo_comp$dif_max_pp)]
ano_max_dif    <- resumo_comp$ano_max_dif[which.max(resumo_comp$dif_max_pp)]

message(glue(
  "Maior divergência entre métodos: {round(dif_max_global, 3)} p.p. ",
  "({regiao_max_dif}, {ano_max_dif})"
))

if (n_fora_spline > 0 & n_fora_linear == 0) {
  message(paste0(
    "\nCONCLUSÃO: A interpolação spline cúbica produziu overshoot em ",
    n_fora_spline, " combinação(ões) ano/região, com proporções masculinas ",
    "fora do envelope demográfico definido pelos valores observados nas ",
    "quatro âncoras censitárias (2000, 2007, 2010, 2022) acrescido de ",
    margem_pp * 100, " p.p. de tolerância. O fenômeno foi agravado pela ",
    "proximidade temporal entre a Contagem 2007 e o Censo 2010 (intervalo ",
    "de 3 anos), que induziu curvatura excessiva da spline. A interpolação ",
    "linear por partes permaneceu integralmente dentro do envelope em todos ",
    "os anos e regiões, sendo adotada como método final."
  ))
}

# -----------------------------------------------------------------------------
# 3.6 Exportação da tabela comparativa — insumo direto para o manuscrito
# -----------------------------------------------------------------------------
comparacao_exportar <- prop_ambos |>
  left_join(limites_ancora |> select(local, prop_masc_min, prop_masc_max),
            by = "local") |>
  left_join(
    diag_spline |> select(local, ano, fora_envelope, desvio_pp) |>
      rename(spline_fora_envelope = fora_envelope,
             spline_desvio_pp     = desvio_pp),
    by = c("local", "ano")
  ) |>
  mutate(
    e_ancora      = ano %in% anos_ancora,
    dif_metodos_pp = (prop_masc_spline - prop_masc_linear) * 100
  ) |>
  select(
    local, ano, e_ancora,
    prop_masc_spline, prop_masc_linear,
    prop_masc_min, prop_masc_max,
    spline_fora_envelope, spline_desvio_pp,
    dif_metodos_pp
  ) |>
  arrange(local, ano)

write_csv(comparacao_exportar,
          "dados/limpos/comparacao_metodos_interpolacao.csv")
message("\nTabela comparativa salva em dados/limpos/comparacao_metodos_interpolacao.csv")

# =============================================================================
# RESUMO FINAL
# =============================================================================
message("\n===== RESUMO DA AQUISIÇÃO =====")
message(glue("SIM  — registros baixados          : {nrow(sim_bruto)}"))
message(glue("IBGE — registros na base            : {nrow(pop_exportar)}"))
message(glue("IBGE — locais                       : {paste(locais_interesse,
             collapse = ', ')}"))
message(glue("IBGE — período                      : 2000–2022"))
message(glue("IBGE — âncoras observadas           : 2000 (Censo), 2007 (Contagem),",
             " 2010 (Censo), 2022 (Censo)"))
message(glue("Interpolação adotada                : linear (na.approx)"))
message(glue("Spline — casos fora do envelope     : {n_fora_spline}"))
message(glue("Spline — inflexões não-monotônicas  : {nrow(inflexoes_spline)}"))
message(glue("Linear — casos fora do envelope     : {n_fora_linear}"))
message(glue("Maior divergência spline vs linear  : {round(dif_max_global, 3)} p.p.",
             " ({regiao_max_dif}, {ano_max_dif})"))
message("Script 01 concluído — dados prontos para limpeza.")