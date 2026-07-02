library(datasusr)
library(dplyr)
library(geobr)
library(ggplot2)
library(httr2)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(tibble)
library(tidyr)

icsap_default_year <- 2024
icsap_default_uf <- "MS"

sih_selected_columns <- c(
  "ano_cmpt",
  "mes_cmpt",
  "munic_res",
  "munic_mov",
  "val_tot",
  "dias_perm",
  "morte",
  "idade",
  "sexo",
  "diag_princ",
  "proc_rea",
  "dt_inter",
  "dt_saida"
)

icsap_groups <- tibble::tribble(
  ~icsap_group, ~group_label,
  1L, "Vaccine-preventable diseases and sensitive conditions",
  2L, "Infectious gastroenteritis and complications",
  3L, "Anemia",
  4L, "Nutritional deficiencies",
  5L, "Ear, nose, and throat infections",
  6L, "Bacterial pneumonias",
  7L, "Asthma",
  8L, "Pulmonary diseases",
  9L, "Hypertension",
  10L, "Angina",
  11L, "Heart failure",
  12L, "Cerebrovascular diseases",
  13L, "Diabetes mellitus",
  14L, "Epilepsies",
  15L, "Kidney and urinary tract infection",
  16L, "Skin and subcutaneous tissue infection",
  17L, "Female pelvic inflammatory disease",
  18L, "Gastrointestinal ulcer",
  19L, "Prenatal and birth-related diseases"
)

normalize_icd10 <- function(code) {
  code |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]", "") |>
    str_squish()
}

icd3 <- function(code) {
  str_sub(normalize_icd10(code), 1, 3)
}

icd4 <- function(code) {
  str_sub(normalize_icd10(code), 1, 4)
}

icd_numeric <- function(code3) {
  suppressWarnings(as.integer(str_sub(code3, 2, 3)))
}

in_icd_range <- function(code, letter, start, end) {
  c3 <- icd3(code)
  str_sub(c3, 1, 1) == letter & between(icd_numeric(c3), start, end)
}

classify_icsap <- function(code) {
  c3 <- icd3(code)
  c4 <- icd4(code)

  case_when(
    in_icd_range(code, "A", 15, 19) |
      in_icd_range(code, "A", 33, 37) |
      in_icd_range(code, "A", 51, 53) |
      in_icd_range(code, "B", 5, 6) |
      c3 %in% c("B16", "B26", "G00", "A95") |
      in_icd_range(code, "B", 50, 54) |
      c3 == "B77" ~ 1L,
    in_icd_range(code, "A", 0, 9) | c3 == "E86" ~ 2L,
    c3 == "D50" ~ 3L,
    in_icd_range(code, "E", 40, 46) | in_icd_range(code, "E", 50, 64) ~ 4L,
    c3 %in% c("H66", "J00", "J01", "J02", "J03", "J06", "J31") ~ 5L,
    c3 %in% c("J13", "J14") |
      c4 %in% c("J153", "J154", "J158", "J159", "J181") ~ 6L,
    c3 %in% c("J45", "J46") ~ 7L,
    c3 %in% c("J20", "J21") | in_icd_range(code, "J", 40, 44) | c3 == "J47" ~ 8L,
    c3 %in% c("I10", "I11") ~ 9L,
    c3 == "I20" ~ 10L,
    c3 == "I50" | c3 == "J81" ~ 11L,
    in_icd_range(code, "I", 63, 67) | c3 == "I69" | c3 %in% c("G45", "G46") ~ 12L,
    in_icd_range(code, "E", 10, 14) ~ 13L,
    c3 %in% c("G40", "G41") ~ 14L,
    in_icd_range(code, "N", 10, 12) | c3 %in% c("N30", "N34") | c4 == "N390" ~ 15L,
    c3 == "A46" | in_icd_range(code, "L", 1, 4) | c3 == "L08" ~ 16L,
    in_icd_range(code, "N", 70, 73) | c3 %in% c("N75", "N76") ~ 17L,
    in_icd_range(code, "K", 25, 28) | c4 %in% c("K920", "K921", "K922") ~ 18L,
    c3 == "O23" | c3 == "A50" | c4 == "P350" ~ 19L,
    TRUE ~ NA_integer_
  )
}

fetch_sih_rd_month <- function(year, month, uf = icsap_default_uf) {
  datasusr::datasus_fetch(
    source = "SIHSUS",
    file_type = "RD",
    year = year,
    month = month,
    uf = uf,
    timeout = 240,
    verbose = FALSE
  ) |>
    as_tibble() |>
    select(any_of(sih_selected_columns)) |>
    mutate(
      source_uf = uf,
      file_year = year,
      file_month = month,
      .before = 1
    )
}

fetch_sih_rd_year <- function(year = icsap_default_year, uf = icsap_default_uf, months = 1:12) {
  map_dfr(months, function(month) {
    fetch_sih_rd_month(year = year, month = month, uf = uf)
  })
}

fetch_ms_municipalities <- function() {
  response <- request("https://servicodados.ibge.gov.br/api/v1/localidades/estados/MS/municipios") |>
    req_user_agent("portfolio-agro-health-r/0.1") |>
    req_perform()

  response |>
    resp_body_string() |>
    fromJSON(flatten = TRUE) |>
    as_tibble() |>
    transmute(
      municipality_code_ibge = as.character(id),
      municipality_code_sus = str_sub(municipality_code_ibge, 1, 6),
      municipality = nome
    )
}

fetch_ibge_population <- function(year = 2024) {
  url <- paste0(
    "https://apisidra.ibge.gov.br/values/t/6579/n6/all/v/9324/p/",
    year,
    "?formato=json"
  )

  response <- request(url) |>
    req_user_agent("portfolio-agro-health-r/0.1") |>
    req_perform()

  response |>
    resp_body_string() |>
    fromJSON(flatten = TRUE) |>
    as_tibble() |>
    filter(NC != "Nível Territorial (Código)") |>
    transmute(
      municipality_code_ibge = as.character(D1C),
      municipality_code_sus = str_sub(municipality_code_ibge, 1, 6),
      municipality = D1N,
      year = as.integer(D2C),
      population = parse_number(V, locale = locale(grouping_mark = ".", decimal_mark = ","))
    ) |>
    filter(str_sub(municipality_code_ibge, 1, 2) == "50")
}

prepare_sih_icsap <- function(raw_data, municipalities = fetch_ms_municipalities()) {
  raw_data |>
    mutate(
      year = as.integer(ano_cmpt),
      month = as.integer(mes_cmpt),
      period = make_date(year, month, 1),
      residence_municipality_code = as.character(munic_res),
      hospitalization_municipality_code = as.character(munic_mov),
      total_value_brl = as.numeric(val_tot),
      length_of_stay_days = as.numeric(dias_perm),
      death = as.integer(morte),
      age = as.numeric(idade),
      sex_code = as.character(sexo),
      main_diagnosis = normalize_icd10(diag_princ),
      procedure_code = as.character(proc_rea),
      admission_date = suppressWarnings(ymd(as.character(dt_inter))),
      discharge_date = suppressWarnings(ymd(as.character(dt_saida))),
      icsap_group = classify_icsap(main_diagnosis),
      is_icsap = !is.na(icsap_group)
    ) |>
    left_join(
      municipalities |>
        select(residence_municipality_code = municipality_code_sus, residence_municipality = municipality),
      by = "residence_municipality_code"
    ) |>
    left_join(icsap_groups, by = "icsap_group") |>
    mutate(
      sex = case_when(
        sex_code == "1" ~ "Male",
        sex_code == "3" ~ "Female",
        TRUE ~ "Unknown"
      ),
      age_group = cut(
        age,
        breaks = c(-Inf, 4, 14, 29, 44, 59, 74, Inf),
        labels = c("0-4", "5-14", "15-29", "30-44", "45-59", "60-74", "75+"),
        right = TRUE
      )
    )
}

summarise_icsap_municipal <- function(data, population) {
  data |>
    group_by(residence_municipality_code, residence_municipality) |>
    summarise(
      hospitalizations = n(),
      icsap_hospitalizations = sum(is_icsap, na.rm = TRUE),
      icsap_deaths = sum(death == 1 & is_icsap, na.rm = TRUE),
      icsap_total_value_brl = sum(total_value_brl[is_icsap], na.rm = TRUE),
      icsap_mean_length_of_stay = mean(length_of_stay_days[is_icsap], na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      population |> select(residence_municipality_code = municipality_code_sus, population),
      by = "residence_municipality_code"
    ) |>
    mutate(
      icsap_share_hospitalizations = icsap_hospitalizations / hospitalizations,
      icsap_rate_per_10000 = icsap_hospitalizations / population * 10000,
      icsap_mortality_rate_pct = icsap_deaths / icsap_hospitalizations * 100,
      icsap_average_value_brl = icsap_total_value_brl / icsap_hospitalizations
    ) |>
    arrange(desc(icsap_rate_per_10000))
}

summarise_icsap_groups <- function(data) {
  data |>
    filter(is_icsap) |>
    group_by(icsap_group, group_label) |>
    summarise(
      icsap_hospitalizations = n(),
      deaths = sum(death == 1, na.rm = TRUE),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      mean_length_of_stay = mean(length_of_stay_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      share_icsap = icsap_hospitalizations / sum(icsap_hospitalizations),
      mortality_rate_pct = deaths / icsap_hospitalizations * 100,
      average_value_brl = total_value_brl / icsap_hospitalizations
    ) |>
    arrange(desc(icsap_hospitalizations))
}

summarise_icsap_monthly <- function(data) {
  data |>
    group_by(period, year, month) |>
    summarise(
      hospitalizations = n(),
      icsap_hospitalizations = sum(is_icsap, na.rm = TRUE),
      icsap_deaths = sum(death == 1 & is_icsap, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      icsap_share_hospitalizations = icsap_hospitalizations / hospitalizations,
      icsap_mortality_rate_pct = icsap_deaths / icsap_hospitalizations * 100
    )
}

fetch_ms_geometry <- function(year = 2020) {
  geobr::read_municipality(code_muni = "MS", year = year, simplified = TRUE, showProgress = FALSE) |>
    st_transform(4674) |>
    transmute(
      residence_municipality_code = str_sub(as.character(code_muni), 1, 6),
      municipality_geo_name = name_muni,
      geometry
    )
}

build_icsap_outputs <- function(year = icsap_default_year, uf = icsap_default_uf, months = 1:12) {
  municipalities <- fetch_ms_municipalities()
  population <- fetch_ibge_population(year = year)
  raw <- fetch_sih_rd_year(year = year, uf = uf, months = months)
  processed <- prepare_sih_icsap(raw, municipalities = municipalities) |>
    filter(!is.na(residence_municipality))
  municipal <- summarise_icsap_municipal(processed, population)
  groups <- summarise_icsap_groups(processed)
  monthly <- summarise_icsap_monthly(processed)
  geometry <- fetch_ms_geometry()
  map_layer <- geometry |>
    left_join(municipal, by = "residence_municipality_code")

  list(
    municipalities = municipalities,
    population = population,
    raw = raw,
    processed = processed,
    municipal = municipal,
    groups = groups,
    monthly = monthly,
    map_layer = map_layer
  )
}
