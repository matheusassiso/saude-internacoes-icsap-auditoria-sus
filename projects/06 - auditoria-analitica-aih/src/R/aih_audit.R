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

aih_default_year <- 2024
aih_default_uf <- "MS"

aih_selected_columns <- c(
  "ano_cmpt",
  "mes_cmpt",
  "n_aih",
  "munic_res",
  "munic_mov",
  "cnes",
  "proc_solic",
  "proc_rea",
  "val_sh",
  "val_sp",
  "val_sadt",
  "val_tot",
  "val_uti",
  "uti_mes_to",
  "uti_int_to",
  "diar_acom",
  "qt_diarias",
  "dias_perm",
  "morte",
  "car_int",
  "idade",
  "sexo",
  "diag_princ",
  "diag_secun",
  "complex",
  "financ",
  "dt_inter",
  "dt_saida"
)

fetch_sih_rd_month <- function(year, month, uf = aih_default_uf) {
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
    select(any_of(aih_selected_columns)) |>
    mutate(
      source_uf = uf,
      file_year = year,
      file_month = month,
      .before = 1
    )
}

fetch_sih_rd_year <- function(year = aih_default_year, uf = aih_default_uf, months = 1:12) {
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

prepare_aih <- function(raw_data, municipalities = fetch_ms_municipalities()) {
  raw_data |>
    mutate(
      year = as.integer(ano_cmpt),
      month = as.integer(mes_cmpt),
      period = make_date(year, month, 1),
      aih_number = as.character(n_aih),
      residence_municipality_code = as.character(munic_res),
      hospitalization_municipality_code = as.character(munic_mov),
      cnes = as.character(cnes),
      requested_procedure = as.character(proc_solic),
      performed_procedure = as.character(proc_rea),
      total_value_brl = as.numeric(val_tot),
      hospital_service_value_brl = as.numeric(val_sh),
      professional_service_value_brl = as.numeric(val_sp),
      sadt_value_brl = as.numeric(val_sadt),
      uti_value_brl = as.numeric(val_uti),
      adult_icu_days = as.numeric(uti_mes_to),
      intermediate_icu_days = as.numeric(uti_int_to),
      companion_days = as.numeric(diar_acom),
      billed_daily_units = as.numeric(qt_diarias),
      length_of_stay_days = as.numeric(dias_perm),
      death = as.integer(morte),
      age = as.numeric(idade),
      sex_code = as.character(sexo),
      admission_type_code = as.character(car_int),
      main_diagnosis = as.character(diag_princ),
      secondary_diagnosis = as.character(diag_secun),
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
    filter(!is.na(hospitalization_municipality)) |>
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
      ),
      has_icu_value = uti_value_brl > 0,
      has_icu_days = adult_icu_days > 0 | intermediate_icu_days > 0,
      cost_per_day_brl = total_value_brl / pmax(length_of_stay_days, 1)
    )
}

build_procedure_benchmarks <- function(data, min_records = 30) {
  data |>
    group_by(performed_procedure) |>
    summarise(
      procedure_records = n(),
      procedure_median_value_brl = median(total_value_brl, na.rm = TRUE),
      procedure_q1_value_brl = quantile(total_value_brl, 0.25, na.rm = TRUE),
      procedure_q3_value_brl = quantile(total_value_brl, 0.75, na.rm = TRUE),
      procedure_iqr_value_brl = IQR(total_value_brl, na.rm = TRUE),
      procedure_median_los = median(length_of_stay_days, na.rm = TRUE),
      procedure_q1_los = quantile(length_of_stay_days, 0.25, na.rm = TRUE),
      procedure_q3_los = quantile(length_of_stay_days, 0.75, na.rm = TRUE),
      procedure_iqr_los = IQR(length_of_stay_days, na.rm = TRUE),
      procedure_mortality_rate = mean(death == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(procedure_records >= min_records) |>
    mutate(
      high_cost_threshold_brl = procedure_q3_value_brl + 3 * procedure_iqr_value_brl,
      long_stay_threshold_days = procedure_q3_los + 3 * procedure_iqr_los
    )
}

flag_aih_records <- function(data, benchmarks) {
  data |>
    left_join(benchmarks, by = "performed_procedure") |>
    mutate(
      benchmark_available = !is.na(procedure_records),
      high_cost_flag = benchmark_available &
        procedure_iqr_value_brl > 0 &
        total_value_brl > high_cost_threshold_brl &
        total_value_brl >= 1000,
      long_stay_flag = benchmark_available &
        procedure_iqr_los > 0 &
        length_of_stay_days > long_stay_threshold_days &
        length_of_stay_days >= 10,
      high_cost_per_day_flag = benchmark_available &
        cost_per_day_brl >= quantile(cost_per_day_brl, 0.99, na.rm = TRUE) &
        total_value_brl >= 1000,
      icu_value_without_icu_days_flag = has_icu_value & !has_icu_days,
      death_low_mortality_procedure_flag = benchmark_available &
        death == 1 &
        procedure_mortality_rate < 0.01 &
        procedure_records >= 100,
      procedure_changed_flag = !is.na(requested_procedure) &
        !is.na(performed_procedure) &
        requested_procedure != performed_procedure,
      any_alert = high_cost_flag |
        long_stay_flag |
        high_cost_per_day_flag |
        icu_value_without_icu_days_flag |
        death_low_mortality_procedure_flag,
      alert_score = as.integer(high_cost_flag) * 2 +
        as.integer(long_stay_flag) * 2 +
        as.integer(high_cost_per_day_flag) * 1 +
        as.integer(icu_value_without_icu_days_flag) * 3 +
        as.integer(death_low_mortality_procedure_flag) * 2
    )
}

summarise_provider_alerts <- function(flagged_data) {
  flagged_data |>
    group_by(cnes, hospitalization_municipality_code, hospitalization_municipality) |>
    summarise(
      hospitalizations = n(),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      alert_records = sum(any_alert, na.rm = TRUE),
      alert_score = sum(alert_score, na.rm = TRUE),
      high_cost_records = sum(high_cost_flag, na.rm = TRUE),
      long_stay_records = sum(long_stay_flag, na.rm = TRUE),
      high_cost_per_day_records = sum(high_cost_per_day_flag, na.rm = TRUE),
      icu_value_without_icu_days_records = sum(icu_value_without_icu_days_flag, na.rm = TRUE),
      death_low_mortality_procedure_records = sum(death_low_mortality_procedure_flag, na.rm = TRUE),
      mean_value_brl = mean(total_value_brl, na.rm = TRUE),
      mean_length_of_stay = mean(length_of_stay_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      alert_rate = alert_records / hospitalizations,
      mortality_rate = deaths / hospitalizations
    ) |>
    arrange(desc(alert_score), desc(alert_records))
}

summarise_procedure_alerts <- function(flagged_data) {
  flagged_data |>
    group_by(performed_procedure) |>
    summarise(
      hospitalizations = n(),
      providers = n_distinct(cnes),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      alert_records = sum(any_alert, na.rm = TRUE),
      alert_score = sum(alert_score, na.rm = TRUE),
      high_cost_records = sum(high_cost_flag, na.rm = TRUE),
      long_stay_records = sum(long_stay_flag, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      median_value_brl = median(total_value_brl, na.rm = TRUE),
      median_length_of_stay = median(length_of_stay_days, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      alert_rate = alert_records / hospitalizations,
      mortality_rate = deaths / hospitalizations
    ) |>
    arrange(desc(alert_score), desc(alert_records))
}

summarise_monthly_alerts <- function(flagged_data) {
  flagged_data |>
    group_by(period, year, month) |>
    summarise(
      hospitalizations = n(),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      alert_records = sum(any_alert, na.rm = TRUE),
      alert_score = sum(alert_score, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      alert_rate = alert_records / hospitalizations,
      mortality_rate = deaths / hospitalizations
    )
}

fetch_ms_geometry <- function(year = 2020) {
  geobr::read_municipality(code_muni = "MS", year = year, simplified = TRUE, showProgress = FALSE) |>
    st_transform(4674) |>
    transmute(
      hospitalization_municipality_code = str_sub(as.character(code_muni), 1, 6),
      municipality_geo_name = name_muni,
      geometry
    )
}

summarise_municipal_alerts <- function(flagged_data) {
  flagged_data |>
    group_by(hospitalization_municipality_code, hospitalization_municipality) |>
    summarise(
      hospitalizations = n(),
      total_value_brl = sum(total_value_brl, na.rm = TRUE),
      providers = n_distinct(cnes),
      alert_records = sum(any_alert, na.rm = TRUE),
      alert_score = sum(alert_score, na.rm = TRUE),
      deaths = sum(death == 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      alert_rate = alert_records / hospitalizations,
      mortality_rate = deaths / hospitalizations
    ) |>
    arrange(desc(alert_score))
}

build_aih_audit_outputs <- function(year = aih_default_year, uf = aih_default_uf, months = 1:12) {
  municipalities <- fetch_ms_municipalities()
  raw <- fetch_sih_rd_year(year = year, uf = uf, months = months)
  processed <- prepare_aih(raw, municipalities = municipalities)
  benchmarks <- build_procedure_benchmarks(processed)
  flagged <- flag_aih_records(processed, benchmarks)
  provider_alerts <- summarise_provider_alerts(flagged)
  procedure_alerts <- summarise_procedure_alerts(flagged)
  monthly_alerts <- summarise_monthly_alerts(flagged)
  municipal_alerts <- summarise_municipal_alerts(flagged)
  geometry <- fetch_ms_geometry()
  map_layer <- geometry |>
    left_join(municipal_alerts, by = "hospitalization_municipality_code")

  list(
    municipalities = municipalities,
    raw = raw,
    processed = processed,
    benchmarks = benchmarks,
    flagged = flagged,
    provider_alerts = provider_alerts,
    procedure_alerts = procedure_alerts,
    monthly_alerts = monthly_alerts,
    municipal_alerts = municipal_alerts,
    map_layer = map_layer
  )
}
