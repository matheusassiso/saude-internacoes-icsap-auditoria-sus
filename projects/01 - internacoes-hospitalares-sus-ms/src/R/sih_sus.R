library(datasusr)
library(dplyr)
library(httr2)
library(jsonlite)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

sih_default_year <- 2024
sih_default_uf <- "MS"

sih_selected_columns <- c(
  "ano_cmpt",
  "mes_cmpt",
  "munic_res",
  "munic_mov",
  "val_tot",
  "dias_perm",
  "morte",
  "car_int",
  "idade",
  "sexo",
  "diag_princ",
  "proc_rea",
  "complex",
  "financ",
  "dt_inter",
  "dt_saida"
)

fetch_sih_rd_month <- function(year, month, uf = sih_default_uf) {
  datasus_fetch(
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

fetch_sih_rd_year <- function(year = sih_default_year, uf = sih_default_uf, months = 1:12) {
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

prepare_sih_rd <- function(raw_data, municipalities = fetch_ms_municipalities()) {
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
      admission_type_code = as.character(car_int),
      main_diagnosis = as.character(diag_princ),
      procedure_code = as.character(proc_rea),
      complexity_code = as.character(complex),
      financing_code = as.character(financ),
      admission_date = suppressWarnings(ymd(as.character(dt_inter))),
      discharge_date = suppressWarnings(ymd(as.character(dt_saida)))
    ) |>
    left_join(
      municipalities |>
        select(
          hospitalization_municipality_code = municipality_code_sus,
          hospitalization_municipality = municipality
        ),
      by = "hospitalization_municipality_code"
    ) |>
    left_join(
      municipalities |>
        select(
          residence_municipality_code = municipality_code_sus,
          residence_municipality = municipality
        ),
      by = "residence_municipality_code"
    ) |>
    mutate(
      sex = case_when(
        sex_code == "1" ~ "Male",
        sex_code == "3" ~ "Female",
        TRUE ~ "Unknown"
      ),
      admission_type = case_when(
        admission_type_code == "01" ~ "Elective",
        admission_type_code == "02" ~ "Urgency",
        admission_type_code == "03" ~ "Work accident",
        admission_type_code == "04" ~ "Traffic accident",
        admission_type_code == "05" ~ "Other accident",
        admission_type_code == "06" ~ "Other injury",
        TRUE ~ "Other or not classified"
      ),
      complexity = case_when(
        complexity_code == "02" ~ "Medium complexity",
        complexity_code == "03" ~ "High complexity",
        TRUE ~ "Other or not classified"
      ),
      age_group = cut(
        age,
        breaks = c(-Inf, 4, 14, 29, 44, 59, 74, Inf),
        labels = c("0-4", "5-14", "15-29", "30-44", "45-59", "60-74", "75+"),
        right = TRUE
      )
    )
}

summarise_sih_monthly <- function(data) {
  data |>
    group_by(period, year, month) |>
    summarise(
      hospitalizations = n(),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      mean_length_of_stay = mean(length_of_stay_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      mortality_rate_pct = deaths / hospitalizations * 100,
      average_value_brl = total_value_brl / hospitalizations
    )
}

summarise_sih_municipal <- function(data) {
  data |>
    group_by(hospitalization_municipality_code, hospitalization_municipality) |>
    summarise(
      hospitalizations = n(),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      mean_length_of_stay = mean(length_of_stay_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      mortality_rate_pct = deaths / hospitalizations * 100,
      average_value_brl = total_value_brl / hospitalizations
    ) |>
    arrange(desc(hospitalizations))
}

build_sih_outputs <- function(year = sih_default_year, uf = sih_default_uf, months = 1:12) {
  municipalities <- fetch_ms_municipalities()
  raw <- fetch_sih_rd_year(year = year, uf = uf, months = months)
  processed <- prepare_sih_rd(raw, municipalities = municipalities)

  list(
    municipalities = municipalities,
    raw = raw,
    processed = processed,
    monthly = summarise_sih_monthly(processed),
    municipal = summarise_sih_municipal(processed)
  )
}
