# Anubis
<div align="left">
<img src="imagens/anubis_standing.png" align="left" width="160" style="margin-right: 24px; margin-bottom: 8px;" />

Statistical analysis of behavioral health time series in Brazil, focusing on self-harm mortality by sex and macro-region. Methods include exploratory analysis and time series modeling to uncover temporal patterns and support public health research.

Análise estatística de séries temporais em saúde mental no Brasil, com foco na mortalidade por autolesão segundo sexo e macrorregião. Os métodos incluem análise exploratória e modelagem de séries temporais para identificar padrões temporais e subsidiar pesquisas em saúde pública.

<table>
<tr>
<td valign="top">

<!-- TABELA ESQUERDA -->
<table>
<tr>
<th>🌐 Language</th>
<th>🌐 Idioma</th>
</tr>
<tr>
<td><a href="#-english">🇺🇸 English</a></td>
<td><a href="#-português">🇧🇷 Português</a></td>
</tr>
</table>

</td>

<td valign="top">

<!-- TABELA DIREITA -->
<table>
<tr>
<th colspan="2" align="center">👥 Contato / Contact</th>
</tr>

<tr>
<td rowspan="2" align="center"><b>Majory Melo</b></td>
<td>E-mail: majorymelo@gmail.com</td>
</tr>
<tr>
<td>GitHub: <a href="https://github.com/EnigmaMajoris">EnigmaMajoris</a></td>
</tr>

<tr>
<td rowspan="2" align="center"><b>Henrique Pegorari</b></td>
<td>E-mail: henrique.pegorari@hotmail.com)</td>
</tr>
<tr>
<td>GitHub: <a href="github.com/HenriquePeg12s">HenriquePeg12</td>
</tr>

</table>

</td>
</tr>
</table>

<br clear="left"/>
</div>

---

## 🇧🇷 Português

# 🔧 Configuração do Ambiente

Este projeto utiliza uma estrutura automatizada para configuração do ambiente em R, garantindo reprodutibilidade e facilidade de uso entre diferentes usuários.

---

## ⚠️ Aviso sobre versões do ambiente

> Este projeto foi desenvolvido com as seguintes versões. Para garantir total reprodutibilidade, recomenda-se fortemente utilizar as mesmas versões ou superiores.

| Componente | Versão utilizada no desenvolvimento |
|---      |---           |
| R       | 4.5.13       |
| renv    | 1.2.0        |
| RStudio | 2026.1.2.418 |

> **Atenção para colaboradores:** divergências de versão do R podem causar incompatibilidades no `renv.lock`. Se possível, atualize o R para a versão 4.5.x antes de iniciar.

---

## 🚀 Como executar o projeto

### 1. Clone o repositório

```bash
git clone https://github.com/EnigmaMajoris/anubis.git
```

Abra o projeto no RStudio pelo arquivo `.Rproj`.

---

### 2. Configure o ambiente (obrigatório na primeira vez)

Este projeto utiliza o pacote `renv` para garantir que todos os usuários utilizem exatamente as mesmas versões de pacotes.

> ℹ️ O `.Rprofile` incluído no repositório **ativa** o `renv` automaticamente, mas **não instala** os pacotes. O passo abaixo é obrigatório na primeira vez.

No console do R, execute:

```r
# Instale o renv caso ainda não tenha (versão mínima recomendada: 1.2.0)
install.packages("renv")

# Restaure o ambiente do projeto
renv::restore()
```

Isso irá:
- instalar automaticamente todos os pacotes nas versões corretas
- garantir compatibilidade total com o ambiente original do projeto

---

### 3. Inicialização automática (uso diário)

Após a configuração inicial, basta abrir o projeto no RStudio:

- O `.Rprofile` ativará o `renv` automaticamente
- Os pacotes serão carregados via `scripts/00_setup.R`
- Nenhuma ação adicional será necessária

---

## 🔁 Fluxo de uso

### ✔ Uso diário
Abra o projeto no RStudio. O ambiente será carregado automaticamente.

### ✔ Caso ocorra erro de pacote
Execute novamente:

```r
renv::restore()
```

### ✔ Ao adicionar novos pacotes (apenas desenvolvedores)

> ⚠️ **Atenção:** `renv::snapshot()` altera o `renv.lock` para **todos os colaboradores**. Execute apenas se tiver certeza de que os novos pacotes devem ser incorporados ao projeto oficial. Em caso de dúvida, abra uma *issue* antes.

```r
renv::snapshot()
```

---

## 🏗️ Estrutura do projeto

```
anubis/
├── .Rprofile           # Ativa o renv automaticamente
├── renv.lock           # Registro das versões de todos os pacotes
├── scripts/
│   └── 00_setup.R      # Carregamento dos pacotes do projeto
├── docs/               # GitHub Pages
├── dados/              
├── imagens/
├── tabelas/
└── manuscrito/
```

---

## 👥 Colaboração

Para colaboradores:

1. Clone o repositório
2. Verifique sua versão do R (`R.version.string` no console) — recomendamos R ≥ 4.5.3
3. Execute `renv::restore()`
4. Comece a trabalhar

O ambiente será idêntico ao da autora (desde que as versões sejam compatíveis).

---

## 📌 Requisitos

| Requisito | Versão mínima |
|---|---|
| R | ≥ 4.5.3 |
| renv | ≥ 1.2.0 |
| RStudio | recomendado |

---

## 💎 Observações técnicas

- O `.Rprofile` **ativa** o `renv`, mas não instala pacotes — o `renv::restore()` manual é necessário na primeira vez
- O script `setup_inicial()` (definido em `scripts/00_setup.R`) é utilizado apenas em casos específicos de nova máquina sem `renv` previamente configurado — **não execute em ambientes já configurados**
- Este projeto segue boas práticas de reprodutibilidade em ciência de dados

---
---

## 🇺🇸 English

## ⚠️ Version Warning

> To ensure full reproducibility, use the same versions listed below, or higher.

| Component | Version |
|---|---|
| R | 4.5.3 |
| renv | 1.2.0 |
| RStudio | 2026.1.2.418 |

> **Collaborators:** version mismatches in R may cause `renv.lock` incompatibilities. Update to R ≥ 4.5.x before contributing.

---

## 🚀 How to Run the Project

### 1. Clone the Repository

```bash
git clone https://github.com/EnigmaMajoris/anubis.git
```

Open the project in RStudio via the `.Rproj` file.

---

### 2. Set Up the Environment *(required on first use)*

This project uses `renv` to ensure all users work with identical package versions.

> ℹ️ The `.Rprofile` included in the repository **activates** `renv` automatically, but does **not install** packages. The step below is required the first time.

In the R console, run:

```r
# Install renv if not yet available (minimum recommended version: 1.2.0)
install.packages("renv")

# Restore the project environment
renv::restore()
```

This will:
- automatically install all packages at the correct versions
- ensure full compatibility with the original development environment

---

### 3. Automatic Initialization *(daily use)*

After the initial setup, simply open the project in RStudio:

- `.Rprofile` will activate `renv` automatically
- Packages will be loaded via `scripts/00_setup.R`
- No further action required

---

## 🔁 Usage Workflow

**Daily use** — Open the project in RStudio. The environment loads automatically.

**If a package error occurs** — Run:
```r
renv::restore()
```

**When adding new packages** *(developers only)*

> ⚠️ `renv::snapshot()` modifies the `renv.lock` for **all collaborators**. Only run this if you are certain the new packages should be incorporated into the official project. When in doubt, open an *issue* first.

```r
renv::snapshot()
```

---

## 🏗️ Project Structure

```
anubis/
├── .Rprofile           # Activates renv automatically
├── renv.lock           # Package version registry
├── scripts/
│   └── 00_setup.R      # Project package loader
├── docs/               # GitHub Pages
├── dados/              
├── imagens/
├── tabelas/
└── manuscrito/
```

---

## 👥 Collaboration

1. Clone the repository
2. Check your R version (`R.version.string` in the console) — R ≥ 4.5.x recommended
3. Run `renv::restore()`
4. Start working

The environment will be identical to the author's (provided versions are compatible).

---

## 📌 Requirements

| Requirement | Minimum Version |
|---|---|
| R | ≥ 4.5.3 |
| renv | ≥ 1.2.0 |
| RStudio | recommended |

---

## 💎 Technical Notes

- `.Rprofile` **activates** `renv` — it does not install packages. Manual `renv::restore()` is required on first use.
- `setup_inicial()` (defined in `scripts/00_setup.R`) is intended for specific cases only (e.g., new machine without `renv`). **Do not run in already configured environments.**
- This project follows good data science reproducibility practices.
