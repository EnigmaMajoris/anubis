# =============================================================================
# PROJETO : ANUBIS
# SCRIPT  : 02_visualizacoes_metodologicas.R
# OBJETIVO: Evidências visuais e tabulares para o manuscrito
# AUTORA  : Majory Melo
# DATA    : 2026-03-29
# VERSÃO  : 3.0 
#
# NORMAS TIPOGRÁFICAS APLICADAS (ABNT NBR):
#   • Fonte do corpo do texto  : Times New Roman, 12 pt
#   • Títulos de figuras/tabelas: Times New Roman, bold, 10 pt  ← legenda ABNT
#   • Subtítulos e notas de rodapé: Times New Roman, 10 pt
#   • Legendas de eixos        : 9 pt
#   • Ticks numéricos          : 8 pt
#   • Paleta de cores          : aprovada para reprodução em P&B e colorido
#     
#     
# ESTRUTURA DE DIRETÓRIOS (caminhos relativos ao raiz do projeto):
#   <raiz>/
#   ├── imagens/   ← figuras PNG + PDF
#   └── tabelas/   ← tabelas CSV
#
# USO: defina `dir_projeto` antes de executar, ou execute o script a partir
# do diretório raiz do projeto (recomendado via RStudio Project / here::here).
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)
library(glue)
library(zoo)
library(here)          # gestão de caminhos relativos ao raiz do projeto

# =============================================================================
# 1. DIRETÓRIOS DE SAÍDA  (caminhos relativos — independentes de máquina)
# =============================================================================
# here::here() resolve o raiz do projeto automaticamente (.Rproj / .here).
# Caso não use RStudio Project, crie um arquivo vazio chamado ".here" na raiz.

dir_imagens <- here("imagens")
dir_tabelas <- here("tabelas")

# Garante que os diretórios existam antes de salvar
dir.create(dir_imagens, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tabelas, showWarnings = FALSE, recursive = TRUE)

# Resolução padrão para impressão 
dpi_padrao <- 300

# =============================================================================
# 2. PALETA EDITORIAL 
#    Esquema Okabe–Ito (2008): acessível a daltônicos, distinto em P&B.
#    Referência: Okabe M, Ito K. Color Universal Design (CUD). 2008.
#    https://jfly.uni-koeln.de/color/
#
#    ATENÇÃO: Caso queira alterar, substitua apenas os valores hex abaixo. 
#    O restante do script permanece.
# =============================================================================
cores <- list(
  spline   = "#E69F00",   # laranja dourado  — linha spline cúbica
  linear   = "#0072B2",   # azul cobalto     — interpolação linear
  ancora   = "#009E73",   # verde esmeralda  — pontos âncora censitários
  fora     = "#D55E00",   # vermelho telha   — extrapolação fora do envelope
  dentro   = "#56B4E9",   # azul céu         — dentro do envelope
  envelope = "#999999",   # cinza médio      — ribbon do envelope
  neutro   = "#F0E442"    # amarelo pálido   — destaque neutro 
)

# =============================================================================
# 3. TEMA EDITORIAL — ABNT NBR
#    Times New Roman é a fonte exigida nos templates Word/LaTeX.
#    Em sistemas sem a fonte instalada, o R substitui por uma serif similar;
#    para garantir exatidão use: extrafont::loadfonts() após instalar a fonte.
# =============================================================================

fonte_base  <- "Times New Roman"   # altere para "serif" se não instalada
tamanho_base <- 10                 # pt — base para escalamento relativo

tema_artigo <- theme_bw(base_size = tamanho_base, base_family = fonte_base) +
  theme(
    # ------ Títulos e subtítulos (ABNT: negrito, fonte ≥ 10 pt) ------
    plot.title    = element_text(
      face   = "bold",
      size   = tamanho_base,        # 10 pt bold
      hjust  = 0,
      family = fonte_base),
    plot.subtitle = element_text(
      size   = tamanho_base - 1,    # 9 pt
      color  = "grey30",
      hjust  = 0,
      family = fonte_base),
    plot.caption  = element_text(
      size   = tamanho_base - 2,    # 8 pt — nota de fonte
      color  = "grey40",
      hjust  = 0,
      family = fonte_base),
    
    # ------ Eixos ------
    axis.title    = element_text(
      size   = tamanho_base - 1,    # 9 pt
      face   = "plain",
      family = fonte_base),
    axis.text     = element_text(
      size   = tamanho_base - 2,    # 8 pt
      family = fonte_base),
    
    # ------ Facetas ------
    strip.background = element_rect(fill = "grey95", color = "grey60"),
    strip.text       = element_text(
      face   = "bold",
      size   = tamanho_base - 1, # 9 pt
      family = fonte_base),
    
    # ------ Legenda ------
    legend.position  = "bottom",
    legend.title     = element_text(
      face   = "bold",
      size   = tamanho_base - 1, # 9 pt
      family = fonte_base),
    legend.text      = element_text(
      size   = tamanho_base - 2, # 8 pt
      family = fonte_base),
    legend.key.size  = unit(0.45, "cm"),
    
    # ------ Grade ------
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey88", linewidth = 0.3)
  )

message("===== SCRIPT 02 — ARTIGO =====")

# =============================================================================
# 4. PREPARAÇÃO DOS DADOS (comum a todas as figuras)
# =============================================================================

dados_fig1 <- prop_ambos |>
  left_join(
    limites_ancora |> select(local, prop_masc_min, prop_masc_max),
    by = "local"
  ) |>
  mutate(
    ancora = ano %in% anos_ancora,
    local  = factor(local, levels = c("Brasil", "Norte", "Nordeste",
                                      "Sudeste", "Sul", "Centro-Oeste"))
  )

# =============================================================================
# FIGURA 1 — Spline vs. Interpolação linear
# Legenda ABNT: título ACIMA da figura, nota de fonte ABAIXO.
# =============================================================================

fig1 <- ggplot(dados_fig1, aes(x = ano)) +
  geom_ribbon(
    aes(ymin = prop_masc_min, ymax = prop_masc_max),
    fill  = cores$envelope,
    alpha = 0.20
  ) +
  geom_line(
    aes(y = prop_masc_linear, color = "Interpolação linear"),
    linewidth = 0.8
  ) +
  geom_line(
    aes(y = prop_masc_spline, color = "Spline cúbica"),
    linetype  = "dashed",
    linewidth = 0.7
  ) +
  facet_wrap(~ local, ncol = 3, scales = "free_y") +
  scale_color_manual(
    name   = "Método",
    values = c(
      "Interpolação linear" = cores$linear,
      "Spline cúbica"       = cores$spline
    )
  ) +
  scale_x_continuous(breaks = c(2000, 2007, 2010, 2022)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title    = "Figura 1. Proporção masculina interpolada por método — Brasil e macrorregiões, 2000–2022",
    subtitle = "Envelope sombreado definido pelas âncoras censitárias ± 0,5 p.p.",
    x        = "Ano",
    y        = "Proporção masculina (%)",
    caption  = "Fonte: IBGE (Censos 2000, 2010, 2022; Contagem da População 2007)."
  ) +
  tema_artigo

ggsave(
  filename = file.path(dir_imagens, "fig01_proporcoes_spline_vs_linear.png"),
  plot     = fig1,
  width    = 16,     # cm — compatível com largura de coluna dupla A4
  height   = 11,
  units    = "cm",
  dpi      = dpi_padrao,
  bg       = "white"
)
ggsave(
  filename = file.path(dir_imagens, "fig01_proporcoes_spline_vs_linear.pdf"),
  plot     = fig1,
  width    = 16,
  height   = 11,
  units    = "cm",
  device   = cairo_pdf
)

# =============================================================================
# FIGURA 2 — Overshoot da spline cúbica
# =============================================================================

fig2 <- ggplot(diag_spline, aes(x = ano, y = desvio_pp)) +
  geom_col(aes(fill = fora_envelope), width = 0.7) +
  scale_fill_manual(
    name   = "Situação",
    values = c("TRUE" = cores$fora, "FALSE" = cores$dentro),
    labels = c("TRUE" = "Fora do envelope", "FALSE" = "Dentro do envelope")
  ) +
  facet_wrap(~ local, ncol = 3) +
  scale_x_continuous(breaks = c(2000, 2007, 2010, 2022)) +
  scale_y_continuous(labels = label_number(suffix = " p.p.", accuracy = 0.01)) +
  labs(
    title   = "Figura 2. Desvio da spline cúbica em relação ao envelope demográfico",
    subtitle = "Valores positivos indicam extrapolação além dos limites censitários",
    x       = "Ano",
    y       = "Desvio (p.p.)",
    caption = "Fonte: elaboração própria com base nas âncoras censitárias do IBGE."
  ) +
  tema_artigo

ggsave(file.path(dir_imagens, "fig02_overshoot_spline.png"),
       fig2, width = 16, height = 11, units = "cm",
       dpi = dpi_padrao, bg = "white")
ggsave(file.path(dir_imagens, "fig02_overshoot_spline.pdf"),
       fig2, width = 16, height = 11, units = "cm", device= cairo_pdf)

# =============================================================================
# FIGURA 3 — Diferença absoluta entre métodos
# =============================================================================

dados_fig3 <- prop_ambos |>
  filter(!ano %in% anos_ancora) |>
  mutate(dif = abs((prop_masc_spline - prop_masc_linear) * 100))

fig3 <- ggplot(dados_fig3, aes(x = ano, y = dif)) +
  geom_line(color = cores$spline, linewidth = 0.8) +
  geom_point(color = cores$spline, size = 1.5) +
  facet_wrap(~ local, ncol = 3) +
  scale_x_continuous(breaks = c(2001, 2005, 2009, 2013, 2017, 2021)) +
  scale_y_continuous(labels = label_number(suffix = " p.p.", accuracy = 0.01)) +
  labs(
    title    = "Figura 3. Diferença absoluta entre spline cúbica e interpolação linear",
    subtitle = "Erro potencial caso a spline cúbica fosse adotada nos anos intercensitários",
    x        = "Ano",
    y        = "Diferença absoluta (p.p.)",
    caption  = "Fonte: elaboração própria. Anos âncora excluídos (desvio definicionalmente nulo)."
  ) +
  tema_artigo

ggsave(file.path(dir_imagens, "fig03_diferenca_absoluta_metodos.png"),
       fig3, width = 16, height = 11, units = "cm",
       dpi = dpi_padrao, bg = "white")
ggsave(file.path(dir_imagens, "fig03_diferenca_absoluta_metodos.pdf"),
       fig3, width = 16, height = 11, units = "cm", device = cairo_pdf)

# =============================================================================
# FIGURA 4 — Sensibilidade à inclusão da Contagem 2007
# =============================================================================

# Série sem 2007: interpolação direta entre 2000 e 2010

prop_sem_2007 <- grade_completa |>
  left_join(
    ancora_completa |> filter(ano != 2007),
    by = c("local", "ano")
  ) |>
  group_by(local) |>
  mutate(prop_sem2007 = zoo::na.approx(prop_masc, x = ano, na.rm = FALSE)) |>
  ungroup()

# Série completa (com 2007) já em prop_ambos
dados_fig4 <- prop_ambos |>
  left_join(prop_sem_2007 |> select(local, ano, prop_sem2007),
            by = c("local", "ano")) |>
  pivot_longer(
    cols      = c(prop_masc_linear, prop_sem2007),
    names_to  = "serie",
    values_to = "prop"
  ) |>
  mutate(
    serie = recode(serie,
                   prop_masc_linear = "Com Contagem 2007",
                   prop_sem2007     = "Sem Contagem 2007"
    )
  )

fig4 <- ggplot(dados_fig4, aes(x = ano, y = prop, color = serie,
                               linetype = serie)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ local, ncol = 3) +
  scale_color_manual(
    name   = "Série",
    values = c("Com Contagem 2007" = cores$linear,
               "Sem Contagem 2007" = cores$fora)
  ) +
  scale_linetype_manual(
    name   = "Série",
    values = c("Com Contagem 2007" = "solid",
               "Sem Contagem 2007" = "dashed")
  ) +
  scale_x_continuous(breaks = c(2000, 2007, 2010, 2022)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title    = "Figura 4. Sensibilidade da interpolação à inclusão da Contagem 2007",
    subtitle = "Comparação entre séries com e sem a âncora de 2007",
    x        = "Ano",
    y        = "Proporção masculina (%)",
    caption  = "Fonte: IBGE (Censos 2000, 2010, 2022; Contagem da População 2007)."
  ) +
  tema_artigo

ggsave(file.path(dir_imagens, "fig04_sensibilidade_2007.png"),
       fig4, width = 16, height = 11, units = "cm",
       dpi = dpi_padrao, bg = "white")
ggsave(file.path(dir_imagens, "fig04_sensibilidade_2007.pdf"),
       fig4, width = 16, height = 11, units = "cm", device = cairo_pdf)

# =============================================================================
# FIGURA 5 — Âncoras censitárias observadas
# =============================================================================

fig5 <- ggplot(ancora_completa, aes(x = ano, y = prop_masc)) +
  geom_line(color = "grey65", linetype = "dashed", linewidth = 0.6) +
  geom_point(color = cores$ancora, size = 2.5, shape = 19) +
  facet_wrap(~ local, ncol = 3) +
  scale_x_continuous(breaks = c(2000, 2007, 2010, 2022)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  labs(
    title    = "Figura 5. Proporção masculina observada nas âncoras censitárias",
    subtitle = "Pontos: valores diretamente observados nos levantamentos do IBGE",
    x        = "Ano",
    y        = "Proporção masculina (%)",
    caption  = "Fonte: IBGE (Censos 2000, 2010, 2022; Contagem da População 2007)."
  ) +
  tema_artigo

ggsave(file.path(dir_imagens, "fig05_ancoras_censitarias.png"),
       fig5, width = 16, height = 11, units = "cm",
       dpi = dpi_padrao, bg = "white")
ggsave(file.path(dir_imagens, "fig05_ancoras_censitarias.pdf"),
       fig5, width = 16, height = 11, units = "cm", device = cairo_pdf)

# =============================================================================
# 5. EXPORTAÇÃO DAS TABELAS
#    Nomes de arquivo descritivos e padronizados (snake_case - padrão R).
# =============================================================================
write_csv(ancora_completa, file.path(dir_tabelas, "tab00_ancoras_censitarias.csv"))
write_csv(limites_ancora,  file.path(dir_tabelas, "tab01_envelope_demografico.csv"))
write_csv(resumo_spline,   file.path(dir_tabelas, "tab02_resumo_spline.csv"))
write_csv(resumo_comp,     file.path(dir_tabelas, "tab03_comparacao_metodos.csv"))

# =============================================================================
# 6. RELATÓRIO FINAL
# =============================================================================
message(glue(
  "\n{'='<60}",
  "\n Arquivos exportados com sucesso.",
  "\n Imagens : {dir_imagens}",
  "\n Tabelas : {dir_tabelas}",
  "\n{'='<60}"
))
