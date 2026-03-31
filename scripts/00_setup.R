# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 00_setup.R
# OBJETIVO: Configuração inicial e carregamento do ambiente do projeto
# -----------------------------------------------------------------------------
# AUTORA  : Majory Melo
# DATA    : 2026-03-30
# VERSÃO  : 5.0
# -----------------------------------------------------------------------------
# REPOSITÓRIO: github.com/EnigmaMajoris/anubis
# -----------------------------------------------------------------------------
# NOTAS:
#   - setup_inicial() → rodar apenas na primeira vez ou em máquina nova
#   - iniciar_sessao() → chamada automaticamente pelo .Rprofile ao abrir o projeto
#   - Este projeto usa renv para reprodutibilidade. Não instale pacotes fora do
#     fluxo descrito nos roteiros abaixo.
# =============================================================================

# =============================================================================
# 📋 ROTEIROS DE USO — LEIA ANTES DE MEXER NO AMBIENTE
# =============================================================================
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │  ROTEIRO A — PRIMEIRA VEZ / MÁQUINA NOVA                               │
# ├─────────────────────────────────────────────────────────────────────────┤
# │  Pré-requisito: ter clonado o repositório (git clone ...)               │
# │                                                                         │
# │  1. Abra o projeto (.Rproj) no RStudio                                  │
# │     → O renv será ativado automaticamente pelo .Rprofile                │
# │                                                                         │
# │  2. No console, rode:                                                   │
# │       source("scripts/00_setup.R")                                      │
# │       setup_inicial()                                                   │
# │                                                                         │
# │     Isso vai:                                                           │
# │       ✔ Restaurar todos os pacotes via renv::restore()                  │
# │       ✔ Instalar pacotes ausentes (CRAN + GitHub)                       │
# │       ✔ Criar a estrutura de diretórios do projeto                      │
# │       ✔ Criar arquivos .gitkeep nas pastas vazias                       │
# │                                                                         │
# │  3. Reinicie o R (Ctrl+Shift+F10) e reabra o projeto                   │
# │     → iniciar_sessao() rodará automaticamente                           │
# └─────────────────────────────────────────────────────────────────────────┘
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │  ROTEIRO B — USO DIÁRIO (SESSÕES NORMAIS)                               │
# ├─────────────────────────────────────────────────────────────────────────┤
# │  Basta abrir o projeto (.Rproj). O .Rprofile chama iniciar_sessao()     │
# │  automaticamente, que:                                                  │
# │    ✔ Ativa o renv                                                       │
# │    ✔ Carrega todos os pacotes do projeto                                │
# │    ✔ Aplica as configurações globais                                    │
# └─────────────────────────────────────────────────────────────────────────┘
#
# ┌─────────────────────────────────────────────────────────────────────────┐
# │  ROTEIRO C — ADICIONANDO NOVOS PACOTES                                  │
# ├─────────────────────────────────────────────────────────────────────────┤
# │  1. Adicione o pacote na lista correta (pacotes_cran ou pacotes_github) │
# │     neste arquivo, na seção correspondente ao seu grupo temático        │
# │                                                                         │
# │  2. No console, rode:                                                   │
# │       install.packages("nome_do_pacote")   # ou remotes::install_github │
# │                                                                         │
# │  3. Atualize o lockfile:                                                │
# │       renv::snapshot()                                                  │
# │                                                                         │
# │  4. Faça commit dos dois arquivos alterados:                            │
# │       - scripts/00_setup.R   (lista atualizada)                         │
# │       - renv.lock            (lockfile atualizado)                      │
# │                                                                         │
# │  ⚠ Nunca rode install.packages() sem depois rodar renv::snapshot()      │
# │    O lockfile desatualizado quebra a reprodutibilidade do projeto.      │
# └─────────────────────────────────────────────────────────────────────────┘

# =============================================================================
# 🔹 BLOCO 1 — PACOTES (FONTE ÚNICA DA VERDADE)
# =============================================================================

pacotes_cran <- c(
  # =========================================================
  # 🔹 CORE (NÃO MEXER)
  # =========================================================
  "tidyverse",
  "here",

  # =========================================================
  # 🔹 IMPORTAÇÃO DE DADOS
  # =========================================================
  "httr",
  "readr",
  "readxl",
  "jsonlite",

  # =========================================================
  # 🔹 WRANGLING / LIMPEZA
  # =========================================================
  "dplyr",
  "tidyr",
  "lubridate",
  "stringr",
  "forcats",
  "janitor",

  # =========================================================
  # 🔹 VISUALIZAÇÃO
  # =========================================================
  "ggplot2",
  "ggthemes",
  "patchwork",
  "scales",
  "viridis",
  "RColorBrewer",

  # =========================================================
  # 🔹 MODELAGEM / SÉRIES TEMPORAIS
  # =========================================================
  "forecast",
  "tseries",
  "zoo",
  "xts",
  "fpp3",
  "feasts",
  "fable",
  "tsibble",
  "seastests",
  "urca",
  "strucchange",
  "lmtest",
  "sandwich",
  "Metrics",
  "modelr",

  # =========================================================
  # 🔹 OUTPUT / RELATÓRIOS
  # =========================================================
  "knitr",
  "rmarkdown",
  "kableExtra",
  "gt",
  "flextable"
)

# =========================================================
# 🔹 INSTALAÇÃO REMOTA
# =========================================================

pacotes_github <- c(
  "rfsaldanha/microdatasus"
)

# =============================================================================
# 🔹 BLOCO 2 — FUNÇÕES AUXILIARES
# =============================================================================

instalar_cran <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste0("  → Instalando CRAN: ", pkg))
    install.packages(pkg)
  }
}

instalar_github <- function(repo) {
  pkg <- basename(repo)

  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    message(paste0("  → Instalando GitHub: ", repo))
    remotes::install_github(repo)
  }
}

carregar_pacotes <- function(pacotes) {
  invisible(lapply(pacotes, library, character.only = TRUE))
}

# =============================================================================
# 🔹 BLOCO 3 — SETUP INICIAL (PRIMEIRA VEZ / MÁQUINA NOVA)
# =============================================================================

setup_inicial <- function() {

  message("🔧 Executando setup inicial do projeto ANUBIS...")

  # -----------------------------------------------------------
  # 1. Ativa o renv (se ainda não estiver ativo)
  # -----------------------------------------------------------
  if (!requireNamespace("renv", quietly = TRUE)) {
    message("  → Instalando renv...")
    install.packages("renv")
  }

  # -----------------------------------------------------------
  # 2. Restaura o ambiente a partir do lockfile
  #    (garante que todos usem as mesmas versões)
  # -----------------------------------------------------------
  message("  → Restaurando pacotes via renv::restore()...")
  renv::restore()

  # -----------------------------------------------------------
  # 3. Instala pacotes ausentes que não estejam no lockfile
  #    (fallback de segurança)
  # -----------------------------------------------------------
  message("  → Verificando pacotes CRAN...")
  invisible(lapply(pacotes_cran, instalar_cran))

  message("  → Verificando pacotes GitHub...")
  invisible(lapply(pacotes_github, instalar_github))

  # -----------------------------------------------------------
  # 4. Estrutura de diretórios
  # -----------------------------------------------------------
  dirs <- c(
    "scripts",
    "dados/brutos",
    "dados/limpos",
    "imagens",
    "tabelas",
    "manuscrito"
  )

  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

  # Cria .gitkeep para versionar pastas vazias
  gitkeeps <- file.path(dirs, ".gitkeep")
  invisible(lapply(gitkeeps, function(x) {
    if (!file.exists(x)) file.create(x)
  }))

  message("")
  message("✔ Setup inicial concluído!")
  message("  ➜ Reinicie o R (Ctrl+Shift+F10) e reabra o projeto.")
}

# =============================================================================
# 🔹 BLOCO 4 — ROTINA DE INICIALIZAÇÃO (SEMPRE AO ABRIR)
# =============================================================================

iniciar_sessao <- function() {

  message("🚀 Iniciando sessão do projeto ANUBIS...")

  # -----------------------------------------------------------
  # 1. Ativa o renv (garante isolamento do ambiente)
  # -----------------------------------------------------------
  if (requireNamespace("renv", quietly = TRUE)) {
    renv::activate()
  } else {
    warning("⚠ renv não encontrado. Rode setup_inicial() para configurar o ambiente.")
  }

  # -----------------------------------------------------------
  # 2. Carrega todos os pacotes do projeto
  # -----------------------------------------------------------
  todos_os_pacotes <- c(pacotes_cran, basename(pacotes_github))
  carregar_pacotes(todos_os_pacotes)

  # -----------------------------------------------------------
  # 3. Configurações globais
  # -----------------------------------------------------------
  options(stringsAsFactors = FALSE)

  message("✔ Ambiente carregado com sucesso!")
}

# =============================================================================
# 🔹 BLOCO 5 — EXECUÇÃO AUTOMÁTICA CONTROLADA
# =============================================================================
# Este bloco roda iniciar_sessao() quando o script é executado diretamente
# (ex: via .Rprofile). Não executa quando o script é apenas carregado com
# source() dentro de outro script.

if (sys.nframe() == 0) {
  iniciar_sessao()
}
