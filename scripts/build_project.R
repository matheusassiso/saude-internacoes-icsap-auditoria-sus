suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(rmarkdown)
  library(scales)
  library(sf)
  library(htmlwidgets)
})

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
dir.create("docs", recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 11))

scale01 <- function(x) {
  x <- as.numeric(x)
  if (all(is.na(x)) || diff(range(x, na.rm = TRUE)) == 0) return(rep(0, length(x)))
  (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
}

br_number <- label_number(big.mark = ".", decimal.mark = ",")
br_money <- label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
br_percent <- label_percent(decimal.mark = ",", accuracy = 0.1)

internacoes <- read_csv("data/source/internacoes_municipais_2024.csv", show_col_types = FALSE) |>
  transmute(
    cod_municipio = as.character(hospitalization_municipality_code),
    municipio = hospitalization_municipality,
    internacoes = hospitalizations,
    valor_aprovado = total_value_brl,
    obitos = deaths,
    permanencia_media = mean_length_of_stay,
    mortalidade_pct = mortality_rate_pct,
    valor_medio = average_value_brl
  )

icsap <- read_csv("data/source/icsap_municipal_2024.csv", show_col_types = FALSE) |>
  transmute(
    cod_municipio = as.character(residence_municipality_code),
    municipio_residencia = residence_municipality,
    populacao = population,
    internacoes_icsap = icsap_hospitalizations,
    obitos_icsap = icsap_deaths,
    valor_icsap = icsap_total_value_brl,
    permanencia_icsap = icsap_mean_length_of_stay,
    participacao_icsap = icsap_share_hospitalizations,
    taxa_icsap_10mil = icsap_rate_per_10000,
    mortalidade_icsap_pct = icsap_mortality_rate_pct,
    valor_medio_icsap = icsap_average_value_brl
  )

aih <- read_csv("data/source/aih_alertas_municipais_2024.csv", show_col_types = FALSE) |>
  transmute(
    cod_municipio = as.character(hospitalization_municipality_code),
    municipio_aih = hospitalization_municipality,
    prestadores = providers,
    registros_alerta = alert_records,
    escore_alerta_aih = alert_score,
    taxa_alerta_aih = alert_rate,
    mortalidade_aih = mortality_rate
  )

base <- full_join(internacoes, icsap, by = "cod_municipio") |>
  full_join(aih, by = "cod_municipio") |>
  mutate(
    municipio = coalesce(municipio, municipio_residencia, municipio_aih),
    across(where(is.numeric), ~coalesce(.x, 0)),
    prioridade_integrada = rowMeans(cbind(
      scale01(internacoes),
      scale01(valor_aprovado),
      scale01(mortalidade_pct),
      scale01(taxa_icsap_10mil),
      scale01(registros_alerta),
      scale01(taxa_alerta_aih)
    )),
    perfil = case_when(
      taxa_icsap_10mil >= quantile(taxa_icsap_10mil, 0.75, na.rm = TRUE) &
        taxa_alerta_aih >= quantile(taxa_alerta_aih, 0.75, na.rm = TRUE) ~ "Alta ICSAP e alto alerta AIH",
      internacoes >= quantile(internacoes, 0.75, na.rm = TRUE) ~ "Polo hospitalar",
      taxa_icsap_10mil >= quantile(taxa_icsap_10mil, 0.75, na.rm = TRUE) ~ "Pressão evitável na APS",
      registros_alerta >= quantile(registros_alerta, 0.75, na.rm = TRUE) ~ "Prioridade de auditoria",
      TRUE ~ "Baixa concentração relativa"
    )
  ) |>
  select(
    cod_municipio, municipio, perfil, prioridade_integrada, populacao,
    internacoes, valor_aprovado, obitos, mortalidade_pct, permanencia_media, valor_medio,
    internacoes_icsap, obitos_icsap, valor_icsap, participacao_icsap,
    taxa_icsap_10mil, mortalidade_icsap_pct, permanencia_icsap,
    prestadores, registros_alerta, escore_alerta_aih, taxa_alerta_aih
  ) |>
  arrange(desc(prioridade_integrada))

malha <- st_read("data/source/malha_ms_internacoes.geojson", quiet = TRUE) |>
  transmute(
    cod_municipio = as.character(municipality_code_sus),
    municipio_malha = municipality,
    geometry
  )

mapa <- malha |>
  left_join(base, by = "cod_municipio") |>
  mutate(
    municipio = coalesce(municipio, municipio_malha),
    across(where(is.numeric), ~coalesce(.x, 0)),
    perfil = coalesce(perfil, "Sem internação hospitalar registrada")
  )

resumo <- tibble(
  indicador = c(
    "Municípios no mapa",
    "Municípios com internação registrada",
    "Internações SUS",
    "Valor aprovado",
    "Óbitos hospitalares",
    "ICSAP",
    "Valor aprovado em ICSAP",
    "Registros com alerta AIH",
    "Escore total de alerta AIH"
  ),
  valor = c(
    nrow(mapa),
    sum(base$internacoes > 0),
    sum(base$internacoes),
    sum(base$valor_aprovado),
    sum(base$obitos),
    sum(base$internacoes_icsap),
    sum(base$valor_icsap),
    sum(base$registros_alerta),
    sum(base$escore_alerta_aih)
  )
)

write_csv(base, "data/processed/municipios_internacoes_icsap_auditoria.csv")
write_csv(resumo, "data/processed/resumo_indicadores.csv")
write_csv(count(st_drop_geometry(mapa), perfil, name = "municipios"), "data/processed/perfis_municipais.csv")
st_write(mapa, "data/processed/ms_internacoes_icsap_auditoria.geojson", delete_dsn = TRUE, quiet = TRUE)

map_plot <- function(var, titulo, subtitulo, legenda, arquivo, option = "C") {
  ggplot(mapa) +
    geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.15) +
    scale_fill_viridis_c(option = option, labels = br_number, name = legenda) +
    labs(title = titulo, subtitle = subtitulo, caption = "Fonte: SIH/SUS, ICSAP e regras exploratórias de auditoria AIH, 2024.") +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold")
    )
  ggsave(arquivo, width = 9, height = 6, dpi = 160)
}

map_plot(
  "prioridade_integrada",
  "Prioridade integrada de auditoria hospitalar",
  "Síntese exploratória de volume, valor aprovado, mortalidade, ICSAP e alertas AIH.",
  "Índice", "figures/01_mapa_prioridade_integrada.png", "A"
)
map_plot(
  "taxa_icsap_10mil",
  "Internações por condições sensíveis à atenção primária",
  "Taxa municipal por 10 mil habitantes.",
  "Taxa", "figures/02_mapa_icsap_taxa.png", "D"
)
map_plot(
  "taxa_alerta_aih",
  "Taxa de registros com alerta AIH",
  "Participação de registros com alerta administrativo por município de internação.",
  "Taxa", "figures/03_mapa_alertas_aih.png", "B"
)

p_internacoes <- base |>
  slice_max(internacoes, n = 15) |>
  ggplot(aes(internacoes, reorder(municipio, internacoes))) +
  geom_col(fill = "#2A6F97") +
  scale_x_continuous(labels = br_number) +
  labs(title = "Maiores volumes de internações SUS", x = NULL, y = NULL) +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/04_ranking_internacoes.png", p_internacoes, width = 9, height = 6, dpi = 160)

p_icsap <- base |>
  slice_max(taxa_icsap_10mil, n = 15) |>
  ggplot(aes(taxa_icsap_10mil, reorder(municipio, taxa_icsap_10mil))) +
  geom_col(fill = "#8B5E34") +
  scale_x_continuous(labels = br_number) +
  labs(title = "Maiores taxas de ICSAP por 10 mil habitantes", x = NULL, y = NULL) +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/05_ranking_icsap.png", p_icsap, width = 9, height = 6, dpi = 160)

p_dispersao <- base |>
  ggplot(aes(taxa_icsap_10mil, taxa_alerta_aih, size = internacoes, color = prioridade_integrada)) +
  geom_point(alpha = 0.75) +
  scale_color_viridis_c(option = "A", labels = br_number) +
  scale_size_continuous(labels = br_number, range = c(2, 12)) +
  labs(
    title = "ICSAP e alertas AIH no território",
    x = "Taxa ICSAP por 10 mil habitantes",
    y = "Taxa de registros com alerta AIH",
    color = "Prioridade",
    size = "Internações"
  ) +
  theme(plot.title = element_text(face = "bold"))
ggsave("figures/06_dispersao_icsap_alertas.png", p_dispersao, width = 9, height = 6, dpi = 160)

popup <- sprintf(
  "<strong>%s</strong><br>Perfil: %s<br>Internações: %s<br>Valor aprovado: %s<br>ICSAP: %s<br>Taxa ICSAP/10 mil: %s<br>Alertas AIH: %s<br>Taxa de alerta: %s",
  mapa$municipio,
  mapa$perfil,
  br_number(mapa$internacoes),
  br_money(mapa$valor_aprovado),
  br_number(mapa$internacoes_icsap),
  br_number(mapa$taxa_icsap_10mil),
  br_number(mapa$registros_alerta),
  br_percent(mapa$taxa_alerta_aih)
)

pal_prioridade <- colorNumeric("YlOrRd", mapa$prioridade_integrada)
pal_icsap <- colorNumeric("PuBuGn", mapa$taxa_icsap_10mil)
pal_alerta <- colorNumeric("OrRd", mapa$taxa_alerta_aih)

dashboard <- leaflet(mapa) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal_prioridade(prioridade_integrada), fillOpacity = 0.78,
    color = "#ffffff", weight = 0.8, popup = popup, group = "Prioridade integrada"
  ) |>
  addPolygons(
    fillColor = ~pal_icsap(taxa_icsap_10mil), fillOpacity = 0.78,
    color = "#ffffff", weight = 0.8, popup = popup, group = "Taxa ICSAP"
  ) |>
  addPolygons(
    fillColor = ~pal_alerta(taxa_alerta_aih), fillOpacity = 0.78,
    color = "#ffffff", weight = 0.8, popup = popup, group = "Alertas AIH"
  ) |>
  addLayersControl(
    baseGroups = c("Prioridade integrada", "Taxa ICSAP", "Alertas AIH"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  hideGroup(c("Taxa ICSAP", "Alertas AIH"))

saveWidget(dashboard, "docs/internacoes_icsap_auditoria_dashboard.html", selfcontained = TRUE)
invisible(file.copy("docs/internacoes_icsap_auditoria_dashboard.html", "docs/index.html", overwrite = TRUE))

render("internacoes_icsap_auditoria_sus.Rmd", output_format = "html_document", output_file = "relatorio.html", output_dir = "docs", quiet = TRUE)
render("internacoes_icsap_auditoria_sus.Rmd", output_format = "pdf_document", output_file = "internacoes_icsap_auditoria_sus.pdf", quiet = TRUE)

message("Projeto reconstruído: dashboard, PDF, HTML, dados e figuras atualizados.")
