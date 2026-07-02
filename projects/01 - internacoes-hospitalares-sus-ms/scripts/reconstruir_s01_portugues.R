library(dplyr)
library(ggplot2)
library(jsonlite)
library(readr)
library(rmarkdown)
library(scales)
library(sf)
library(stringr)

project_dir <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
dir.create(file.path(project_dir, "docs"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(project_dir, "reports", "figures"), showWarnings = FALSE, recursive = TRUE)

municipal <- read_csv(file.path(project_dir, "data/processed/sih_rd_ms_2024_resumo_municipal.csv"), show_col_types = FALSE)
monthly <- read_csv(file.path(project_dir, "data/processed/sih_rd_ms_2024_resumo_mensal.csv"), show_col_types = FALSE)
sih <- read_csv(file.path(project_dir, "data/processed/sih_rd_ms_2024_internacoes.csv"), show_col_types = FALSE)
munis <- read_csv(file.path(project_dir, "data/raw/ibge_ms_municipios.csv"), show_col_types = FALSE)

ms_geo_url <- "https://servicodados.ibge.gov.br/api/v3/malhas/estados/50?formato=application/vnd.geo+json&qualidade=minima&intrarregiao=municipio"
ms_geo <- st_read(ms_geo_url, quiet = TRUE) |>
  mutate(municipality_code_ibge = as.character(codarea)) |>
  left_join(
    munis |> transmute(
      municipality_code_ibge = as.character(municipality_code_ibge),
      municipality_code_sus = as.character(municipality_code_sus),
      municipality = municipality
    ),
    by = "municipality_code_ibge"
  ) |>
  left_join(
    municipal |> mutate(hospitalization_municipality_code = as.character(hospitalization_municipality_code)),
    by = c("municipality_code_sus" = "hospitalization_municipality_code")
  ) |>
  mutate(
    hospitalizations = coalesce(hospitalizations, 0),
    total_value_brl = coalesce(total_value_brl, 0),
    deaths = coalesce(deaths, 0),
    mean_length_of_stay = if_else(is.na(mean_length_of_stay), 0, mean_length_of_stay),
    mortality_rate_pct = if_else(is.na(mortality_rate_pct), 0, mortality_rate_pct),
    average_value_brl = if_else(is.na(average_value_brl), 0, average_value_brl),
    audit_priority_score = percent_rank(hospitalizations) * 0.35 +
      percent_rank(total_value_brl) * 0.25 +
      percent_rank(mortality_rate_pct) * 0.25 +
      percent_rank(mean_length_of_stay) * 0.15
  ) |>
  st_transform(4326)

st_write(ms_geo, file.path(project_dir, "docs/mapa_painel.geojson"), delete_dsn = TRUE, quiet = TRUE)

map_png <- file.path(project_dir, "reports/figures/s01_mapa_internacoes_municipais.png")
png(map_png, width = 1500, height = 1050, res = 160)
print(
  ggplot(ms_geo) +
    geom_sf(aes(fill = hospitalizations), color = "white", linewidth = 0.12) +
    scale_fill_viridis_c(option = "C", labels = comma, trans = "sqrt") +
    labs(
      title = "Internações SUS por município de internação",
      subtitle = "Mato Grosso do Sul, SIH/SUS RD, 2024",
      fill = "Internações",
      caption = "Fonte: DATASUS SIH/SUS e malha municipal IBGE."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      legend.position = "right"
    )
)
dev.off()

fmt_int <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
fmt_num <- function(x, digits = 1) format(round(x, digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = digits)
fmt_money <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 2))
fmt_pct <- function(x) paste0(fmt_num(x, 2), "%")

total_hosp <- nrow(sih)
total_value <- sum(sih$total_value_brl, na.rm = TRUE)
total_deaths <- sum(sih$death == 1, na.rm = TRUE)
overall_mortality <- total_deaths / total_hosp * 100
mean_los <- mean(sih$length_of_stay_days, na.rm = TRUE)
avg_value <- total_value / total_hosp
top_mun <- municipal |> slice_max(hospitalizations, n = 1, with_ties = FALSE)
top_month <- monthly |> slice_max(hospitalizations, n = 1, with_ties = FALSE)

top_table <- municipal |>
  slice_max(hospitalizations, n = 12, with_ties = FALSE) |>
  transmute(
    Municipio = hospitalization_municipality,
    Internacoes = hospitalizations,
    Valor_aprovado = total_value_brl,
    Obitos = deaths,
    Mortalidade = mortality_rate_pct,
    Permanencia_media = mean_length_of_stay,
    Valor_medio = average_value_brl
  )

priority_table <- ms_geo |>
  st_drop_geometry() |>
  filter(hospitalizations >= 500) |>
  arrange(desc(audit_priority_score)) |>
  slice_head(n = 15) |>
  transmute(
    Municipio = municipality,
    Internacoes = hospitalizations,
    Valor_aprovado = total_value_brl,
    Mortalidade = mortality_rate_pct,
    Permanencia_media = mean_length_of_stay,
    Escore_prioridade = audit_priority_score
  )

write_csv(priority_table, file.path(project_dir, "data/processed/s01_prioridade_auditoria_municipios.csv"))

dashboard_payload <- list(
  summary = list(
    total_hospitalizations = total_hosp,
    total_value_brl = total_value,
    total_deaths = total_deaths,
    mortality_rate_pct = overall_mortality,
    mean_length_of_stay = mean_los,
    average_value_brl = avg_value,
    municipalities = nrow(ms_geo),
    municipalities_with_hospitalization = sum(ms_geo$hospitalizations > 0)
  ),
  monthly = monthly,
  municipal = municipal |> arrange(desc(hospitalizations)),
  priority = priority_table,
  map = jsonlite::fromJSON(jsonlite::toJSON(ms_geo, dataframe = "rows", sf = "geojson"), simplifyVector = FALSE)
)

dashboard_html <- paste0(
'<!DOCTYPE html>
<html lang="pt-BR">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Painel de internações SUS em Mato Grosso do Sul</title>
<link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css">
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<style>
body{margin:0;font-family:Arial,Helvetica,sans-serif;background:#f4f7fb;color:#1f2937}
header{background:#0f2f4a;color:#fff;padding:28px 34px}
header h1{margin:0 0 8px;font-size:28px;line-height:1.2}
header p{margin:0;max-width:1120px;color:#dbeafe}
main{max-width:1280px;margin:auto;padding:22px 28px}
.grid{display:grid;grid-template-columns:repeat(12,1fr);gap:16px}
.card{background:#fff;border:1px solid #d8e1ea;border-radius:8px;padding:16px;box-shadow:0 1px 4px rgba(15,47,74,.06)}
.span3{grid-column:span 3}.span4{grid-column:span 4}.span5{grid-column:span 5}.span7{grid-column:span 7}.span12{grid-column:1/-1}
@media(max-width:900px){.span3,.span4,.span5,.span7{grid-column:1/-1}main{padding:16px}}
h2{font-size:18px;margin:0 0 10px;color:#0f2f4a} h3{font-size:15px;margin:14px 0 8px;color:#374151}
.metric{font-size:25px;font-weight:700;color:#0f2f4a}.label{font-size:12px;text-transform:uppercase;color:#64748b;letter-spacing:.03em}
.note{line-height:1.48;color:#475569}.small{font-size:12px;color:#64748b}
table{width:100%;border-collapse:collapse;font-size:13px} th,td{padding:7px;border-bottom:1px solid #e5e7eb;text-align:left;vertical-align:top} th{background:#eef3f8;color:#334155}
#map{height:560px;border:1px solid #cbd5e1;border-radius:8px}
.toolbar{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:10px}button{border:1px solid #cbd5e1;background:#fff;border-radius:6px;padding:8px 10px;cursor:pointer}button.active{background:#0f2f4a;color:#fff}
.bar{height:12px;background:#3b82f6;border-radius:999px}.warn{background:#fff7ed;border-left:4px solid #f59e0b;padding:10px}
a{color:#075985;text-decoration:none}a:hover{text-decoration:underline}
</style>
</head>
<body>
<header>
<h1>Painel de internações SUS em Mato Grosso do Sul</h1>
<p>Painel operacional para auditores e técnicos de saúde. O foco é leitura territorial, volume de produção hospitalar, valor aprovado, permanência média, mortalidade bruta e priorização de municípios para investigação.</p>
</header>
<main>
<section class="grid">
<div class="card span3"><div class="label">Internações SIH/SUS</div><div class="metric" id="mHosp"></div><div class="small">AIHs reduzidas, competência 2024.</div></div>
<div class="card span3"><div class="label">Valor aprovado</div><div class="metric" id="mValue"></div><div class="small">Pagamento administrativo, não custo total.</div></div>
<div class="card span3"><div class="label">Mortalidade bruta</div><div class="metric" id="mMort"></div><div class="small">Não ajustada por risco ou complexidade.</div></div>
<div class="card span3"><div class="label">Permanência média</div><div class="metric" id="mLos"></div><div class="small">Dias médios de internação.</div></div>

<div class="card span7">
<h2>Mapa municipal</h2>
<p class="note">Escolha o indicador. Clique em um município para ver os principais números. Cores mais fortes indicam maior valor do indicador selecionado.</p>
<div class="toolbar">
<button data-var="hospitalizations" class="active">Internações</button>
<button data-var="total_value_brl">Valor aprovado</button>
<button data-var="mortality_rate_pct">Mortalidade bruta</button>
<button data-var="mean_length_of_stay">Permanência média</button>
<button data-var="audit_priority_score">Prioridade de auditoria</button>
</div>
<div id="map"></div>
</div>
<div class="card span5">
<h2>Como interpretar para auditoria</h2>
<p class="note">O mapa mostra município de internação, não município de residência. Portanto, municípios com hospitais de referência concentram produção de outros municípios. A mortalidade é bruta: ela ajuda a levantar perguntas, mas não deve ser usada como ranking de qualidade sem ajuste por perfil do paciente, diagnóstico, complexidade e fluxo de referência.</p>
<div class="warn"><b>Uso recomendado:</b> priorizar perguntas de auditoria, selecionar municípios para revisão documental e cruzar produção, valor, permanência e óbitos. Não usar isoladamente para concluir irregularidade.</div>
<h3>Relatórios</h3>
<p><a href="../reports/01-internacoes-hospitalares-sus-ms.pdf">Abrir PDF técnico</a><br><a href="../reports/01-internacoes-hospitalares-sus-ms.html">Abrir relatório HTML</a><br><a href="../README.md">Abrir README do projeto</a></p>
</div>

<div class="card span6">
<h2>Série mensal</h2>
<div id="monthlyBars"></div>
</div>
<div class="card span6">
<h2>Municípios para priorização</h2>
<p class="small">Escore exploratório: combina volume, valor, mortalidade bruta e permanência média. Serve para triagem, não para julgamento.</p>
<div id="priorityTable"></div>
</div>
<div class="card span12">
<h2>Tabela municipal</h2>
<input id="search" placeholder="Filtrar município..." style="width:98%;padding:9px;border:1px solid #cbd5e1;border-radius:6px;margin-bottom:10px">
<div id="municipalTable"></div>
</div>
</section>
</main>
<script>
const DATA = ', toJSON(dashboard_payload, dataframe = "rows", auto_unbox = TRUE, digits = 10), ';
const fmtInt = n => Math.round(Number(n||0)).toLocaleString("pt-BR");
const fmtPct = n => Number(n||0).toLocaleString("pt-BR",{minimumFractionDigits:2,maximumFractionDigits:2})+"%";
const fmtMoney = n => Number(n||0).toLocaleString("pt-BR",{style:"currency",currency:"BRL",maximumFractionDigits:0});
const fmtNum = n => Number(n||0).toLocaleString("pt-BR",{minimumFractionDigits:1,maximumFractionDigits:1});
document.getElementById("mHosp").textContent = fmtInt(DATA.summary.total_hospitalizations);
document.getElementById("mValue").textContent = fmtMoney(DATA.summary.total_value_brl);
document.getElementById("mMort").textContent = fmtPct(DATA.summary.mortality_rate_pct);
document.getElementById("mLos").textContent = fmtNum(DATA.summary.mean_length_of_stay)+" dias";

function colorScale(v, min, max){
  if(!Number.isFinite(v) || max <= min) return "#dbeafe";
  const t = Math.max(0, Math.min(1, (v-min)/(max-min)));
  const r = Math.round(224 - 180*t), g = Math.round(242 - 95*t), b = Math.round(254 - 90*t);
  return `rgb(${r},${g},${b})`;
}
const labels = {
  hospitalizations:"Internações",
  total_value_brl:"Valor aprovado",
  mortality_rate_pct:"Mortalidade bruta",
  mean_length_of_stay:"Permanência média",
  audit_priority_score:"Prioridade de auditoria"
};
let currentVar = "hospitalizations";
const map = L.map("map").setView([-20.6,-54.7],6);
L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",{maxZoom:18,attribution:"OpenStreetMap"}).addTo(map);
let layer;
function drawMap(){
  if(layer) layer.remove();
  const values = DATA.map.features.map(f => Number(f.properties[currentVar])).filter(Number.isFinite);
  const min = Math.min(...values), max = Math.max(...values);
  layer = L.geoJSON(DATA.map,{
    style:f => ({color:"#31546f",weight:0.8,fillOpacity:0.70,fillColor:colorScale(Number(f.properties[currentVar]),min,max)}),
    onEachFeature:(f,l)=>{
      const p=f.properties;
      l.bindPopup(`<b>${p.municipality || p.hospitalization_municipality || "Município"}</b><br>
      ${labels[currentVar]}: ${currentVar.includes("value") ? fmtMoney(p[currentVar]) : currentVar.includes("pct") ? fmtPct(p[currentVar]) : currentVar.includes("stay") ? fmtNum(p[currentVar])+" dias" : fmtInt(p[currentVar])}<br>
      Internações: ${fmtInt(p.hospitalizations)}<br>
      Valor aprovado: ${fmtMoney(p.total_value_brl)}<br>
      Óbitos: ${fmtInt(p.deaths)}<br>
      Mortalidade bruta: ${fmtPct(p.mortality_rate_pct)}<br>
      Permanência média: ${fmtNum(p.mean_length_of_stay)} dias`);
    }
  }).addTo(map);
  map.fitBounds(layer.getBounds());
}
document.querySelectorAll("button[data-var]").forEach(btn=>{
  btn.addEventListener("click",()=>{
    document.querySelectorAll("button[data-var]").forEach(b=>b.classList.remove("active"));
    btn.classList.add("active"); currentVar = btn.dataset.var; drawMap();
  });
});
drawMap();

function table(rows, cols){
  return `<table><thead><tr>${cols.map(c=>`<th>${c.label}</th>`).join("")}</tr></thead><tbody>${rows.map(r=>`<tr>${cols.map(c=>`<td>${c.render?c.render(r):r[c.key]}</td>`).join("")}</tr>`).join("")}</tbody></table>`;
}
const maxMonth = Math.max(...DATA.monthly.map(d=>Number(d.hospitalizations)));
document.getElementById("monthlyBars").innerHTML = table(DATA.monthly,[
  {label:"Mês",render:r=>String(r.month).padStart(2,"0")+"/2024"},
  {label:"Internações",render:r=>`<div>${fmtInt(r.hospitalizations)}</div><div class="bar" style="width:${Number(r.hospitalizations)/maxMonth*100}%"></div>`},
  {label:"Mortalidade",render:r=>fmtPct(r.mortality_rate_pct)}
]);
document.getElementById("priorityTable").innerHTML = table(DATA.priority.slice(0,10),[
  {label:"Município",key:"Municipio"},
  {label:"Internações",render:r=>fmtInt(r.Internacoes)},
  {label:"Mortalidade",render:r=>fmtPct(r.Mortalidade)},
  {label:"Escore",render:r=>fmtNum(Number(r.Escore_prioridade)*100)}
]);
function drawMunicipalTable(){
  const q = document.getElementById("search").value.toLowerCase();
  const rows = DATA.municipal.filter(r => String(r.hospitalization_municipality).toLowerCase().includes(q)).slice(0,80);
  document.getElementById("municipalTable").innerHTML = table(rows,[
    {label:"Município",key:"hospitalization_municipality"},
    {label:"Internações",render:r=>fmtInt(r.hospitalizations)},
    {label:"Valor aprovado",render:r=>fmtMoney(r.total_value_brl)},
    {label:"Óbitos",render:r=>fmtInt(r.deaths)},
    {label:"Mortalidade",render:r=>fmtPct(r.mortality_rate_pct)},
    {label:"Permanência",render:r=>fmtNum(r.mean_length_of_stay)+" dias"}
  ]);
}
document.getElementById("search").addEventListener("input", drawMunicipalTable);
drawMunicipalTable();
</script>
</body>
</html>')
writeLines(dashboard_html, file.path(project_dir, "docs/index.html"), useBytes = TRUE)

rmd <- paste0('---
title: "Painel de internações SUS em Mato Grosso do Sul"
author: "Matheus Assis de Oliveira"
date: "`r format(Sys.Date(), \'%d/%m/%Y\')`"
abstract: |
  Este relatório técnico apresenta uma análise descritiva das internações financiadas pelo SUS em Mato Grosso do Sul em 2024, usando arquivos SIH/SUS RD (AIH reduzida) do DATASUS. O texto foi escrito para auditores e técnicos de saúde que precisam entender os indicadores sem depender de formação estatística avançada. O objetivo é mostrar volume, valor aprovado, permanência média, mortalidade bruta e distribuição municipal, com leitura operacional para triagem de auditoria. Os resultados não são ranking de qualidade hospitalar e não substituem auditoria documental, análise clínica ou ajuste de risco.
output:
  pdf_document:
    toc: true
    number_sections: true
    fig_caption: true
    latex_engine: pdflatex
  html_document:
    toc: true
    number_sections: true
    df_print: paged
fontsize: 11pt
geometry: margin=1in
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{float}
  - \\usepackage{caption}
  - \\captionsetup{font=small,labelfont=bf}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE, fig.align = "center", fig.width = 7, fig.height = 4.4, dpi = 150)
library(dplyr)
library(ggplot2)
library(knitr)
library(readr)
library(scales)
library(sf)
municipal <- read_csv("../data/processed/sih_rd_ms_2024_resumo_municipal.csv", show_col_types = FALSE)
monthly <- read_csv("../data/processed/sih_rd_ms_2024_resumo_mensal.csv", show_col_types = FALSE)
sih <- read_csv("../data/processed/sih_rd_ms_2024_internacoes.csv", show_col_types = FALSE)
priority <- read_csv("../data/processed/s01_prioridade_auditoria_municipios.csv", show_col_types = FALSE)
geo <- st_read("../docs/mapa_painel.geojson", quiet = TRUE)
fmt_int <- function(x) format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE)
fmt_num <- function(x, digits = 1) format(round(x, digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = digits)
fmt_money <- function(x) paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE, nsmall = 2))
fmt_pct <- function(x) paste0(fmt_num(x, 2), "%")
total_hosp <- nrow(sih)
total_value <- sum(sih$total_value_brl, na.rm = TRUE)
total_deaths <- sum(sih$death == 1, na.rm = TRUE)
overall_mortality <- total_deaths / total_hosp * 100
mean_los <- mean(sih$length_of_stay_days, na.rm = TRUE)
avg_value <- total_value / total_hosp
top_mun <- municipal |> slice_max(hospitalizations, n = 1, with_ties = FALSE)
top_month <- monthly |> slice_max(hospitalizations, n = 1, with_ties = FALSE)
```

# Resumo Executivo

Em 2024, a base analisada contém `r fmt_int(total_hosp)` internações SUS em Mato Grosso do Sul, com `r fmt_money(total_value)` de valor aprovado, `r fmt_int(total_deaths)` óbitos hospitalares registrados, mortalidade bruta de `r fmt_pct(overall_mortality)` e permanência média de `r fmt_num(mean_los, 1)` dias.

Para auditoria, o ponto mais importante é separar **produção hospitalar** de **risco populacional**. Este relatório usa o município de internação (`munic_mov`), portanto mede onde o atendimento foi produzido. Campo Grande, Dourados e outros municípios de referência naturalmente concentram casos de residentes de várias localidades. Essa concentração pode indicar papel assistencial regional, e não necessariamente problema de desempenho.

O painel interativo em `docs/index.html` foi reconstruído com mapa municipal. Ele permite alternar entre internações, valor aprovado, mortalidade bruta, permanência média e um escore exploratório de prioridade de auditoria.

# Problema de Auditoria

Auditores e técnicos de saúde precisam responder perguntas operacionais como:

- Onde se concentra a produção hospitalar SUS no estado?
- Quais municípios concentram maior valor aprovado?
- Onde a mortalidade bruta ou a permanência média chamam atenção?
- Quais municípios merecem revisão inicial antes de uma auditoria documental mais cara?

Essas perguntas exigem estatística descritiva bem explicada. A análise não procura provar causalidade. Ela organiza sinais para orientar revisão, comparação e priorização.

# Fonte de Dados e Unidade de Análise

Os dados vêm do SIH/SUS, arquivo RD de AIH reduzida, disponibilizado pelo DATASUS. A unidade de análise é a autorização de internação hospitalar registrada no sistema. Cada linha representa uma AIH, não uma pessoa única. Um mesmo paciente pode aparecer mais de uma vez se houver mais de uma internação.

```{r dataset-summary}
tibble::tribble(
  ~Indicador, ~Valor,
  "Sistema", "SIH/SUS - AIH reduzida (RD)",
  "Ano analisado", "2024",
  "Unidade federativa", "Mato Grosso do Sul",
  "Unidade estatística", "AIH / internação registrada",
  "Município usado no mapa", "Município de internação (munic_mov)",
  "Internações", fmt_int(total_hosp),
  "Municípios na malha", fmt_int(nrow(geo)),
  "Municípios com internação registrada", fmt_int(sum(geo$hospitalizations > 0))
) |> kable(booktabs = TRUE, caption = "Escopo da base analisada.")
```

# Método Estatístico, em Linguagem Operacional

## Contagem

A contagem de internações é simplesmente o número de AIHs no período. Ela mede volume de produção hospitalar. Municípios com mais hospitais ou serviços de referência tendem a ter maior volume.

## Valor aprovado

O valor aprovado soma `val_tot`, que é valor administrativo aprovado no SIH/SUS. Ele não deve ser lido como custo total do cuidado. Para auditoria, ele ajuda a localizar onde está o maior impacto financeiro registrado.

## Mortalidade bruta

A mortalidade bruta foi calculada por:

\\[
\\text{Mortalidade bruta} =
\\frac{\\text{óbitos hospitalares}}{\\text{internações}} \\times 100
\\]

Ela é chamada de bruta porque não ajusta idade, diagnóstico, gravidade, complexidade, comorbidades, transferência ou perfil do hospital. Por isso, deve ser usada como **sinal de triagem**, não como ranking de qualidade.

## Permanência média

A permanência média soma os dias de internação e divide pelo número de internações. Valores maiores podem refletir casos mais complexos, gargalo de alta, perfil assistencial diferente ou erro/heterogeneidade de registro.

## Escore exploratório de prioridade

O escore de prioridade combina quatro sinais: volume de internações, valor aprovado, mortalidade bruta e permanência média. Ele não é indicador oficial. É uma ferramenta exploratória para ordenar municípios que podem merecer revisão inicial.

# Resultados Estaduais

```{r state-overview}
tibble::tribble(
  ~Indicador, ~Valor, ~Leitura_para_auditoria,
  "Internações", fmt_int(total_hosp), "Dimensiona o volume total de AIHs no ano.",
  "Valor aprovado", fmt_money(total_value), "Mostra o impacto financeiro administrativo.",
  "Valor médio por internação", fmt_money(avg_value), "Ajuda a comparar intensidade financeira média.",
  "Óbitos hospitalares", fmt_int(total_deaths), "Contagem bruta de desfechos fatais registrados.",
  "Mortalidade bruta", fmt_pct(overall_mortality), "Sinal exploratório; exige ajuste de risco para comparar qualidade.",
  "Permanência média", paste0(fmt_num(mean_los, 1), " dias"), "Sinal de complexidade, fluxo assistencial ou permanência prolongada."
) |> kable(booktabs = TRUE, caption = "Indicadores estaduais e interpretação operacional.")
```

# Série Mensal

```{r monthly-plot, fig.cap="Internações mensais SUS em Mato Grosso do Sul, 2024."}
ggplot(monthly, aes(x = month, y = hospitalizations)) +
  geom_col(fill = "#2f6f9f") +
  geom_text(aes(label = fmt_int(hospitalizations)), vjust = -0.25, size = 3) +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, .12))) +
  labs(x = "Mês de competência", y = "Internações", caption = "Fonte: SIH/SUS RD, DATASUS.") +
  theme_minimal(base_size = 11)
```

O maior volume mensal ocorreu no mês `r top_month$month`, com `r fmt_int(top_month$hospitalizations)` internações. Para auditoria, variações mensais devem ser lidas com calendário, sazonalidade, disponibilidade de leitos, processamento administrativo e eventual represamento de produção.

# Mapa Municipal

```{r map-plot, fig.cap="Mapa municipal das internações SUS por município de internação, Mato Grosso do Sul, 2024."}
ggplot(geo) +
  geom_sf(aes(fill = hospitalizations), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(option = "C", labels = label_number(big.mark = ".", decimal.mark = ","), trans = "sqrt") +
  labs(fill = "Internações", caption = "Fonte: SIH/SUS RD e malha municipal IBGE.") +
  theme_void(base_size = 11)
```

O mapa evidencia a concentração espacial da produção hospitalar. Essa concentração é esperada em municípios com maior rede hospitalar e papel de referência regional. O mapa não mostra, sozinho, onde a população adoeceu; ele mostra onde a internação foi registrada.

# Municípios com Maior Volume

```{r top-table}
municipal |>
  slice_max(hospitalizations, n = 12, with_ties = FALSE) |>
  transmute(
    Municipio = hospitalization_municipality,
    Internacoes = fmt_int(hospitalizations),
    `Valor aprovado` = fmt_money(total_value_brl),
    Obitos = fmt_int(deaths),
    `Mortalidade bruta` = fmt_pct(mortality_rate_pct),
    `Permanencia media` = paste0(fmt_num(mean_length_of_stay, 1), " dias"),
    `Valor medio` = fmt_money(average_value_brl)
  ) |>
  kable(booktabs = TRUE, caption = "Municípios com maior volume de internações por local de internação.")
```

O município com maior volume foi `r top_mun$hospitalization_municipality`, com `r fmt_int(top_mun$hospitalizations)` internações. Isso deve ser interpretado como concentração de produção hospitalar e capacidade instalada, não como maior risco populacional.

# Priorização Exploratória para Auditoria

```{r priority-table}
priority |>
  mutate(
    Internacoes = fmt_int(Internacoes),
    `Valor aprovado` = fmt_money(Valor_aprovado),
    `Mortalidade bruta` = fmt_pct(Mortalidade),
    `Permanencia media` = paste0(fmt_num(Permanencia_media, 1), " dias"),
    `Escore` = fmt_num(Escore_prioridade * 100, 1)
  ) |>
  select(Municipio, Internacoes, `Valor aprovado`, `Mortalidade bruta`, `Permanencia media`, `Escore`) |>
  kable(booktabs = TRUE, caption = "Municípios priorizados por escore exploratório de auditoria.")
```

Este escore é uma régua de triagem. Ele aponta municípios onde volume, valor, mortalidade e permanência aparecem simultaneamente em patamar elevado. A etapa seguinte deve ser documental: perfil de procedimentos, diagnósticos, complexidade, CNES, AIHs específicas, glosas, permanências atípicas e compatibilidade entre produção e capacidade instalada.

# Cuidados de Interpretação

1. Município de internação não é município de residência.
2. Mortalidade bruta não é indicador ajustado de qualidade.
3. Valor aprovado não é custo total do serviço.
4. AIH é registro administrativo e pode refletir regras de processamento.
5. Diferenças territoriais podem refletir hierarquia da rede, referência regional e concentração de serviços.

# Recomendações para Auditores e Técnicos

- Usar o painel para escolher municípios e indicadores que merecem investigação.
- Cruzar municípios com alta produção e alto valor aprovado com tipo de procedimento e complexidade.
- Tratar mortalidade bruta elevada como sinal para aprofundamento, nunca como conclusão.
- Comparar permanência média com perfil de casos e disponibilidade de retaguarda.
- Construir uma segunda etapa com município de residência e denominadores populacionais para avaliar risco populacional.

# Reprodutibilidade

Arquivos principais:

- `data/processed/sih_rd_ms_2024_internacoes.csv`
- `data/processed/sih_rd_ms_2024_resumo_mensal.csv`
- `data/processed/sih_rd_ms_2024_resumo_municipal.csv`
- `data/processed/s01_prioridade_auditoria_municipios.csv`
- `docs/mapa_painel.geojson`
- `docs/index.html`

O script `scripts/reconstruir_s01_portugues.R` reconstrói o painel, o mapa e este relatório.

# Conclusão

O projeto S01 foi reestruturado como ferramenta de leitura operacional para auditoria em saúde. A principal entrega é um painel territorial que combina produção hospitalar, valor aprovado, mortalidade bruta, permanência média e priorização exploratória. O ganho metodológico está em tornar os indicadores claros, auditáveis e espacialmente interpretáveis, sem exagerar conclusões estatísticas.
')
writeLines(rmd, file.path(project_dir, "reports/s01_artigo_internacoes_sus.Rmd"), useBytes = TRUE)

readme <- paste0('# Painel de internações SUS em Mato Grosso do Sul

**Tema:** Saúde pública, auditoria SUS e produção hospitalar  
**Pasta:** `01 - internacoes-hospitalares-sus-ms`  
**Autor:** Matheus Assis de Oliveira  

## Para que serve este projeto

Este projeto organiza as internações financiadas pelo SUS em Mato Grosso do Sul em 2024. Ele foi refeito para servir a **auditores e técnicos de saúde**, inclusive leitores que não são especialistas em estatística.

O objetivo não é ranquear hospitais nem afirmar qualidade assistencial. O objetivo é criar uma base clara para triagem: onde há maior produção, maior valor aprovado, maior mortalidade bruta, maior permanência média e quais municípios merecem revisão documental mais cuidadosa.

## Entregáveis principais

| Entregável | Link |
|---|---|
| Painel interativo com mapa municipal | [abrir painel](<docs/index.html>) |
| Relatório técnico em PDF | [abrir PDF](<reports/01-internacoes-hospitalares-sus-ms.pdf>) |
| Relatório HTML | [abrir HTML](<reports/01-internacoes-hospitalares-sus-ms.html>) |
| Mapa municipal GeoJSON | [abrir GeoJSON](<docs/mapa_painel.geojson>) |
| Instruções de commit | [abrir instruções](<COMMIT_INSTRUCTIONS.txt>) |

## O que foi analisado

- ', fmt_int(total_hosp), ' internações SUS registradas no SIH/SUS RD em 2024.
- ', fmt_money(total_value), ' de valor aprovado.
- ', fmt_int(total_deaths), ' óbitos hospitalares registrados.
- Mortalidade bruta estadual de ', fmt_pct(overall_mortality), '.
- Permanência média de ', fmt_num(mean_los, 1), ' dias.
- Mapa dos 79 municípios de Mato Grosso do Sul.

## Como interpretar os indicadores

**Internações** medem produção hospitalar registrada em AIH.  
**Valor aprovado** é valor administrativo do SIH/SUS, não custo total.  
**Mortalidade bruta** é a proporção de internações com óbito registrado. Ela não é ajustada por idade, diagnóstico, gravidade ou complexidade.  
**Permanência média** mostra dias médios de internação e pode indicar complexidade, fluxo assistencial ou permanência prolongada.  
**Prioridade de auditoria** é um escore exploratório que combina volume, valor, mortalidade e permanência para orientar triagem.

## Mapa e análise espacial

O painel agora possui mapa municipal real, construído com malha do IBGE e indicadores do SIH/SUS. Ele permite alternar entre:

- internações;
- valor aprovado;
- mortalidade bruta;
- permanência média;
- prioridade exploratória de auditoria.

O mapa usa **município de internação**, não município de residência. Portanto, ele mostra concentração da produção hospitalar e papel regional dos serviços, não risco populacional direto.

## Arquivos de dados

- `data/processed/sih_rd_ms_2024_internacoes.csv`
- `data/processed/sih_rd_ms_2024_resumo_mensal.csv`
- `data/processed/sih_rd_ms_2024_resumo_municipal.csv`
- `data/processed/s01_prioridade_auditoria_municipios.csv`
- `docs/mapa_painel.geojson`

## Reprodutibilidade

O script abaixo reconstrói o mapa, o painel e os relatórios:

```r
source("scripts/reconstruir_s01_portugues.R")
```

## Limitações

Este projeto é descritivo. Ele não ajusta risco, não mede qualidade hospitalar isoladamente e não substitui auditoria documental. Para avaliação de desempenho, é necessário cruzar diagnóstico, procedimento, complexidade, idade, origem do paciente, CNES, perfil assistencial e regras de faturamento.
')
writeLines(readme, file.path(project_dir, "README.md"), useBytes = TRUE)

render_one <- function(format, output_file) {
  tmp <- file.path(tempdir(), paste0("s01_render_", format))
  dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
  render(
    input = file.path(project_dir, "reports/s01_artigo_internacoes_sus.Rmd"),
    output_format = format,
    output_file = output_file,
    output_dir = tmp,
    quiet = TRUE,
    clean = TRUE
  )
  file.copy(file.path(tmp, output_file), file.path(project_dir, "reports", output_file), overwrite = TRUE)
}
render_one("pdf_document", "01-internacoes-hospitalares-sus-ms.pdf")
render_one("html_document", "01-internacoes-hospitalares-sus-ms.html")

message("S01 reconstruido em portugues com mapa municipal, painel, README, PDF e HTML.")
