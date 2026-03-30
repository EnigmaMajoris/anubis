# =============================================================================
# .Rprofile — PROJETO ANUBIS
# Executado automaticamente pelo R ao abrir o projeto
# -----------------------------------------------------------------------------
# ATENÇÃO: A ordem das chamadas aqui é crítica. Não reorganize os blocos.
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Ativa o renv PRIMEIRO — isola a biblioteca do projeto
#    Deve ser sempre a primeira instrução executada
# -----------------------------------------------------------------------------
source("renv/activate.R")

# -----------------------------------------------------------------------------
# 2. Preferência por binários (evita compilação desnecessária no Windows/Mac)
# -----------------------------------------------------------------------------
options(pkgType = "binary")

# -----------------------------------------------------------------------------
# 3. Carrega o ambiente do projeto via 00_setup.R
#    A função iniciar_sessao() é chamada internamente pelo script
#    (via bloco `if (sys.nframe() == 0)`) — não chamar novamente aqui
# -----------------------------------------------------------------------------
if (file.exists("scripts/00_setup.R")) {
  source("scripts/00_setup.R")
}