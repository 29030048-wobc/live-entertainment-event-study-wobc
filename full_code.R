# =============================================================================
# EVENT STUDY: COUNTRY-LEVEL MARKETS AND LIVE ENTERTAINMENT DISRUPTIONS
# =============================================================================
# Author: Wendy Olivia Bazua Corrales
#
# This script contains the analysis workflow used for the thesis results.
# It assumes that all required input CSV files are placed in the same folder
# as this script and that the working directory is set to that folder.
#
# Required input files:
#   - events_clean.csv
#   - indices_daily.csv
#   - acwi_daily.csv
#   - vix_daily.csv
#   - msci_regional_daily.csv
#   - sectoral_daily.csv
#
# Refinitiv market data are not redistributed with the thesis because of
# licensing restrictions. Users with authorized access should place the
# required files in this folder using the names listed above.
#
# STRUCTURE:
#   PART 0    - Setup
#   PART I    - Data loading
#   PART II   - Data preparation and recoding
#   PART III  - Sample construction
#   PART IV   - Descriptive statistics
#   PART V    - Event study pipeline
#   PART VI   - Cross-sectional analysis
#   PART VIII - Sectoral analysis
#   PART IX   - Robustness checks
#   PART XI   - F-tests consolidation
#   PART XII  - Figures used in the thesis
#   PART XIII - Final figure export
# =============================================================================

# =============================================================================
# PART 0: SETUP
# =============================================================================

rm(list = ls())
gc()

invisible(lapply(
  c("tidyverse", "lubridate", "ggplot2", "dplyr", "tidyr", "scales",
    "sandwich", "purrr", "tibble", "lmtest", "stringr","car","corrplot"),
  library,
  character.only = TRUE
))

path_clean <- "."

required_files <- c(
  "events_clean.csv",
  "indices_daily.csv",
  "acwi_daily.csv",
  "vix_daily.csv",
  "msci_regional_daily.csv",
  "sectoral_daily.csv"
)

missing_files <- required_files[!file.exists(file.path(path_clean, required_files))]
if (length(missing_files) > 0) {
  stop("Missing required input files: ", paste(missing_files, collapse = ", "))
}

# =============================================================================
# PART I: DATA LOADING
# =============================================================================

events  <- read_csv(file.path(path_clean, "events_clean.csv"))
indices <- read_csv(file.path(path_clean, "indices_daily.csv"))
acwi    <- read_csv(file.path(path_clean, "acwi_daily.csv"))
vix_daily <- read_csv(file.path(path_clean, "vix_daily.csv"))
msci_regional <- read_csv(file.path(path_clean, "msci_regional_daily.csv"))


events <- events %>% mutate(date = ymd(date))
indices <- indices %>% mutate(date = ymd(date))
acwi <- acwi %>% mutate(date = ymd(date))
vix_daily <- vix_daily %>% mutate(date = ymd(date))
msci_regional <- msci_regional %>% mutate(date = ymd(date))


events_base <- events
indices_base <- indices
acwi_base <- acwi
msci_regional_base <- msci_regional
unique(events$country)

# =============================================================================
# PART II: DATA CLEANING & RECODING
# =============================================================================


# Create event_num if it is not already present so the rest of the pipeline works
if (!("event_num" %in% names(events_base))) {
  events_base <- events_base %>% mutate(event_num = row_number())
}

# Drop log used to document the final sample
drop_log <- tibble(
  stage = character(),
  n_before = integer(),
  n_drop = integer(),
  n_after = integer()
)

append_drop_log <- function(stage, before_events, drop_ids, after_events) {
  tibble(
    stage = stage,
    n_before = nrow(before_events),
    n_drop = length(unique(drop_ids)),
    n_after = nrow(after_events)
  )
}

# Quick structure checks
glimpse(events_base)
glimpse(indices_base)
glimpse(acwi_base)
glimpse(vix_daily)
glimpse(msci_regional)

# =========================================
# CLEANING AND RECODING
# =========================================

events_base <- events_base %>%
  distinct(country, date, description, .keep_all = TRUE)

# Remove duplicate PSI20 variants from the index file
indices_base <- indices_base[!indices_base$ticker %in% c("PSI20", "PSI20.LS"), ]

standardize_country_names <- function(x) {
  x %>%
    stringr::str_squish() %>%
    dplyr::recode(
      `USA` = "United States",
      `UK` = "United Kingdom",
      `SouthKorea` = "South Korea",
      `CzechRepublic` = "Czech Republic"
    )
}

n_before_recode <- nrow(events_base)

events_base <- events_base %>%
  mutate(
    disruption_type = stringr::str_squish(disruption_type),
    event_type = stringr::str_squish(event_type),
    country = standardize_country_names(country)
  )

indices_base <- indices_base %>%
  mutate(country = standardize_country_names(country))

events_base <- events_base %>%
  mutate(
    disruption_type = case_when(
      disruption_type == "Medical emergency - performer" ~ "Medical emergency - Performer",
      disruption_type == "Medical Emergency - Spectator" ~ "Medical emergency - Spectator",
      TRUE ~ disruption_type
    )
  )

events_base <- events_base %>%
  mutate(
    event_type_agg = case_when(
      event_type %in% c("Show", "Theater play") ~ "Other live entertainment",
      event_type == "Music festival / Concert" ~ "Music festival",
      TRUE ~ event_type
    ),
    disruption_type_agg = case_when(
      disruption_type %in% c("Medical emergency - Performer", "Medical emergency - Spectator")
      ~ "Medical emergencies",
      disruption_type %in% c("Accidental incident (non-structural)", "Structural or Physical Failure", "Logistical failure")
      ~ "Accidental and technical failures",
      disruption_type %in% c("Crowd-related Incident", "Drug-related Incident", "Threat or evacuation (no violence)")
      ~ "Crowd and behavioral incidents",
      disruption_type %in% c("Violence - non-terror", "Shooting", "Terrorist Attack")
      ~ "Violence and security-related incidents",
      disruption_type %in% c("Weather-related Incident", "Fire")
      ~ "Environmental and external incidents",
      TRUE ~ NA_character_
    )
  )

stopifnot(nrow(events_base) == n_before_recode)

# Mapping checks
unmapped_disruption <- events_base %>%
  filter(is.na(disruption_type_agg)) %>%
  distinct(disruption_type) %>%
  arrange(disruption_type)

if (nrow(unmapped_disruption) > 0) {
  message("Unmapped values in disruption_type; please review:")
  print(unmapped_disruption, n = Inf)
} else {
  message("OK: all disruption_type values were mapped to disruption_type_agg.")
}

event_type_cross <- table(events_base$event_type, events_base$event_type_agg, useNA = "ifany")
disruption_type_cross <- table(events_base$disruption_type, events_base$disruption_type_agg, useNA = "ifany")

event_type_cross
disruption_type_cross

events_base %>%
  count(event_type_agg, name = "n") %>%
  arrange(desc(n))

events_base %>%
  count(disruption_type_agg, name = "n") %>%
  arrange(desc(n))

# =========================================
# SAMPLE CONSTRUCTION (DROP 1: index coverage range)
# =========================================

index_ranges <- indices_base %>%
  group_by(country) %>%
  summarise(
    index_start = min(date, na.rm = TRUE),
    index_end = max(date, na.rm = TRUE),
    .groups = "drop"
  )

events_range_check <- events_base %>%
  left_join(index_ranges, by = "country") %>%
  mutate(within_range = !is.na(index_start) & date >= index_start & date <= index_end)

events_range_check %>% count(within_range)

events_range_check %>%
  filter(!within_range) %>%
  select(date, country, description, index_start, index_end) %>%
  arrange(country, date)

events_dropped_out_of_range <- events_range_check %>% filter(!within_range)
events_final <- events_range_check %>% filter(within_range)

drop_log <- bind_rows(
  drop_log,
  append_drop_log(
    stage = "drop_out_of_index_range",
    before_events = events_base,
    drop_ids = events_dropped_out_of_range$event_num,
    after_events = events_final
  )
)

c(
  n_total = nrow(events_range_check),
  n_dropped = nrow(events_dropped_out_of_range),
  n_final = nrow(events_final)
)

print(events_dropped_out_of_range, n = Inf)

# =============================================================================
# PART III: SAMPLE CONSTRUCTION
# =============================================================================

# =============================================================================
# PART IV: DESCRIPTIVE STATISTICS
# =============================================================================

# ---- 4.1 Event-level descriptives ------------------------------------------

event_types_summary <- events_final %>%
  count(event_type_agg, name = "n_events") %>%
  mutate(pct_events = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
event_types_summary

disruption_types_summary <- events_final %>%
  count(disruption_type, name = "n_events") %>%
  mutate(pct_events = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
disruption_types_summary

disruption_types_summary_agg <- events_final %>%
  count(disruption_type_agg, name = "n_events") %>%
  mutate(pct_events = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
disruption_types_summary_agg

event_type_severity <- events_final %>%
  group_by(event_type_agg) %>%
  summarise(
    n_events = n(),
    pct_with_deaths = mean(deaths > 0, na.rm = TRUE),
    pct_with_injuries_reported = mean(!is.na(total_injuries) & total_injuries > 0, na.rm = TRUE),
    pct_cancelled = mean(cancellation == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_events))
event_type_severity

severity_summary <- tibble(
  variable = c("deaths", "total_injuries", "cancellation"),
  n_total = nrow(events_final),
  n_non_missing = c(
    sum(!is.na(events_final$deaths)),
    sum(!is.na(events_final$total_injuries)),
    sum(!is.na(events_final$cancellation))
  ),
  pct_missing = c(
    mean(is.na(events_final$deaths)),
    mean(is.na(events_final$total_injuries)),
    mean(is.na(events_final$cancellation))
  ),
  mean = c(
    mean(events_final$deaths, na.rm = TRUE),
    mean(events_final$total_injuries, na.rm = TRUE),
    mean(events_final$cancellation, na.rm = TRUE)
  ),
  median = c(
    median(events_final$deaths, na.rm = TRUE),
    median(events_final$total_injuries, na.rm = TRUE),
    median(events_final$cancellation, na.rm = TRUE)
  ),
  min = c(
    min(events_final$deaths, na.rm = TRUE),
    min(events_final$total_injuries, na.rm = TRUE),
    min(events_final$cancellation, na.rm = TRUE)
  ),
  max = c(
    max(events_final$deaths, na.rm = TRUE),
    max(events_final$total_injuries, na.rm = TRUE),
    max(events_final$cancellation, na.rm = TRUE)
  )
)
severity_summary

severity_sd <- events %>%
  summarise(
    deaths_sd = sd(deaths, na.rm = TRUE),
    injuries_sd = sd(total_injuries, na.rm = TRUE),
    cancellation_sd = sd(cancellation, na.rm = TRUE)
  )

print(severity_sd)

events_final %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  arrange(desc(n_missing))

events_final %>%
  arrange(desc(total_injuries)) %>%
  select(date, event_type, event_type_agg, disruption_type, disruption_type_agg, country, total_injuries)

events_final %>%
  filter(!is.na(total_injuries)) %>%
  ggplot(aes(x = total_injuries)) +
  geom_histogram(bins = 30) +
  labs(x = "Total injuries", y = "Number of events")

sample_overview <- events_final %>%
  summarise(
    n_events = n(),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    n_countries = n_distinct(country),
    n_event_types = n_distinct(event_type_agg),
    n_disruption_types = n_distinct(disruption_type),
    n_disruption_types_agg = n_distinct(disruption_type_agg)
  )
sample_overview

events_by_country <- events_final %>%
  count(country, name = "n_events") %>%
  mutate(pct = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
print(events_by_country, n = Inf)


unique(events$country)



top10_countries <- events_by_country %>% slice_head(n = 10)
top10_countries

countries_by_event_type <- events_final %>%
  distinct(country, event_type_agg) %>%
  count(event_type_agg, name = "n_countries") %>%
  arrange(desc(n_countries))
countries_by_event_type

events_by_country_event_type <- events_final %>%
  count(country, event_type_agg, name = "n_events") %>%
  group_by(country) %>%
  mutate(pct_within_country = n_events / sum(n_events)) %>%
  ungroup() %>%
  arrange(desc(n_events))
events_by_country_event_type

events_by_event_type <- events_final %>%
  count(event_type_agg, name = "n_events") %>%
  mutate(pct = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
events_by_event_type

events_by_disruption_agg <- events_final %>%
  count(disruption_type_agg, name = "n_events") %>%
  mutate(pct = n_events / sum(n_events)) %>%
  arrange(desc(n_events))
events_by_disruption_agg

country_concentration <- events_by_country %>%
  summarise(
    share_top1 = first(pct),
    share_top5 = sum(pct[1:5]),
    share_top10 = sum(pct[1:10])
  )
country_concentration

cor(events_base |> dplyr::select(where(is.numeric)), 
    use = "pairwise.complete.obs")




# ---- 4.2 Non-trading day / weekend analysis --------------------------------

# =========================================
# EVENT DATE: FIN DE SEMANA / NO TRADING DAYS
# =========================================

weekend_stats <- events_final %>%
  mutate(
    dow = wday(date, label = TRUE, abbr = FALSE, week_start = 1),
    is_weekend = wday(date, week_start = 1) %in% c(6, 7)
  ) %>%
  summarise(
    n_events = n(),
    n_weekend = sum(is_weekend, na.rm = TRUE),
    pct_weekend = n_weekend / n_events
  )
weekend_stats

trading_days_by_country <- indices_base %>%
  distinct(country, date) %>%
  mutate(is_trading_day = TRUE)

nontrading_stats <- events_final %>%
  select(event_num, country, date) %>%
  left_join(trading_days_by_country, by = c("country", "date")) %>%
  mutate(is_trading_day = replace_na(is_trading_day, FALSE)) %>%
  mutate(next_trading_day = map2(country, date, ~{
    td <- trading_days_by_country %>%
      filter(country == .x, date >= .y) %>%
      arrange(date) %>%
      slice_head(n = 1) %>%
      pull(date)
    if (length(td) == 0) as.Date(NA) else td
  })) %>%
  mutate(next_trading_day = as.Date(unlist(next_trading_day))) %>%
  summarise(
    n_events = n(),
    n_nontrading = sum(!is_trading_day, na.rm = TRUE),
    pct_nontrading = n_nontrading / n_events,
    n_weekend = sum(wday(date, week_start = 1) %in% c(6, 7), na.rm = TRUE),
    pct_weekend = n_weekend / n_events
  )
nontrading_stats

nontrading_by_country <- events_final %>%
  select(country, date) %>%
  left_join(trading_days_by_country, by = c("country", "date")) %>%
  mutate(is_trading_day = replace_na(is_trading_day, FALSE)) %>%
  summarise(
    n_events = n(),
    n_nontrading = sum(!is_trading_day),
    pct_nontrading = n_nontrading / n_events,
    n_weekend = sum(wday(date, week_start = 1) %in% c(6, 7)),
    pct_weekend = n_weekend / n_events,
    .by = country
  ) %>%
  arrange(desc(pct_nontrading), desc(n_events))
print(nontrading_by_country, n = Inf)

# ---- 4.3 Financial series descriptives (indices, ACWI) ---------------------

# =========================================
# DESCRIPTIVAS / SUMMARY STATS (ÍNDICES Y ACWI)
# =========================================


sort(unique(indices_base$ticker))
nrow(indices_base)
n_distinct(indices_base$ticker)
glimpse(indices_base)
indices_base %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date   = max(date, na.rm = TRUE),
    n_days     = n_distinct(date)
  )

indices_base %>%
  distinct(country, ticker) %>%
  count(country) %>%
  filter(n > 1)

indices_base %>% count(country, sort = TRUE)

indices_base %>%
  summarise(
    total_obs = n(),
    na_returns = sum(is.na(ret)),
    share_na = mean(is.na(ret))
  )

indices_base %>%
  summarise(
    min_ret = min(ret, na.rm = TRUE),
    max_ret = max(ret, na.rm = TRUE)
  )

indices_table <- indices_base %>%
  distinct(country, ticker) %>%
  arrange(country)
indices_table

indices_base %>%
  group_by(country) %>%
  summarise(
    total_obs = n(),
    na_returns = sum(is.na(ret)),
    share_na = mean(is.na(ret)),
    .groups = "drop"
  ) %>%
  arrange(desc(share_na))



country_coverage <- indices_base %>%
  group_by(country, ticker) %>%
  summarise(
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    total_obs = n(),
    na_returns = sum(is.na(ret)),
    share_na = mean(is.na(ret)),
    .groups = "drop"
  ) %>%
  arrange(country)
print(country_coverage, n = Inf)

country_stats <- indices_base %>%
  group_by(country) %>%
  summarise(
    mean_ret = mean(ret, na.rm = TRUE),
    sd_ret = sd(ret, na.rm = TRUE),
    min_ret = min(ret, na.rm = TRUE),
    max_ret = max(ret, na.rm = TRUE),
    non_missing = sum(!is.na(ret)),
    missing_pct = mean(is.na(ret)) * 100,
    .groups = "drop"
  ) %>%
  arrange(country)
print(country_stats, n = Inf)

acwi_stats <- acwi_base %>%
  summarise(
    start_date = min(date),
    end_date = max(date),
    total_obs = n(),
    na_returns = sum(is.na(acwi_ret)),
    share_na = mean(is.na(acwi_ret)),
    mean_ret = mean(acwi_ret, na.rm = TRUE),
    sd_ret = sd(acwi_ret, na.rm = TRUE),
    median_ret = median(acwi_ret, na.rm = TRUE),
    min_ret = min(acwi_ret, na.rm = TRUE),
    max_ret = max(acwi_ret, na.rm = TRUE),
    p01 = quantile(acwi_ret, 0.01, na.rm = TRUE),
    p99 = quantile(acwi_ret, 0.99, na.rm = TRUE)
  )
acwi_stats

range(acwi_base$date)
nrow(acwi_base)

ggplot(acwi_base, aes(x = date, y = acwi_index)) +
  geom_line() +
  labs(
    title = "MSCI ACWI Index (1999–2025)",
    x = "Date",
    y = "Index level"
  ) +
  theme_minimal()

ggplot(acwi_base, aes(x = date, y = acwi_ret)) +
  geom_line() +
  labs(
    title = "MSCI ACWI Daily Returns (1999–2025)",
    x = "Date",
    y = "Daily return"
  ) +
  theme_minimal()

# =============================================================================
# PART V: EVENT STUDY PIPELINE
# =============================================================================

# ---- 5.1 Trading calendar & event date helpers -----------------------------

# =========================================
# EVENT STUDY PIPELINE (Market Model con ACWI)
# =========================================

trading_calendar <- indices_base %>%
  filter(!is.na(ret)) %>%      # <-- CLAVE
  distinct(country, date) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(t = row_number()) %>%
  ungroup()

# ---------------------------------------------------------
# Día 0: vamos a construir DOS definiciones:
#  (A) next trading day (tu baseline actual)
#  (B) same-day if trading day; otherwise next trading day
# ---------------------------------------------------------

# Helper: dado (country, event_date_calendar, mode) regresa (event_date, t0)
pick_event_date <- function(ctry, d, mode = c("next", "same_or_next")) {
  mode <- match.arg(mode)
  cal <- trading_calendar %>% filter(country == ctry)
  
  # next: primer trading day estrictamente DESPUÉS del evento
  # same_or_next: primer trading day EN O DESPUÉS del evento
  cand <- if (mode == "next") {
    cal %>% filter(date > d) %>% slice_head(n = 1)
  } else {
    cal %>% filter(date >= d) %>% slice_head(n = 1)
  }
  
  if (nrow(cand) == 0) return(tibble(event_date = as.Date(NA), t0 = NA_integer_))
  tibble(event_date = cand$date[1], t0 = cand$t[1])
}

# ---- Version A: baseline (next trading day)
events_w_event_date_next <- events_final %>%
  mutate(tmp = purrr::map2(country, date, ~ pick_event_date(.x, .y, "next"))) %>%
  tidyr::unnest(tmp)

events_w_event_date_next %>%
  summarise(
    n_events = n(),
    n_missing_event_date = sum(is.na(event_date)),
    n_missing_t0 = sum(is.na(t0))
  )

# ---- Version B: alternative (same day if trading day, else next)
events_w_event_date_same_or_next <- events_final %>%
  mutate(tmp = purrr::map2(country, date, ~ pick_event_date(.x, .y, "same_or_next"))) %>%
  tidyr::unnest(tmp)

events_w_event_date_same_or_next %>%
  summarise(
    n_events = n(),
    n_missing_event_date = sum(is.na(event_date)),
    n_missing_t0 = sum(is.na(t0))
  )

# ---------------------------------------------------------
# Construcción del panel por tau (misma lógica que ya tienes)
# ---------------------------------------------------------

tau_min <- -190
tau_max <- 10

make_event_tau_panel <- function(events_tbl, tau_min, tau_max) {
  events_tbl %>%
    filter(!is.na(t0)) %>%
    select(event_num, country, date, event_date, t0) %>%
    tidyr::crossing(tau = tau_min:tau_max) %>%
    mutate(t = t0 + tau) %>%
    left_join(trading_calendar %>% select(country, t, date_tau = date),
              by = c("country", "t"))
}

event_tau_panel_all_next <- make_event_tau_panel(events_w_event_date_next, tau_min, tau_max)
event_tau_panel_all_same_or_next <- make_event_tau_panel(events_w_event_date_same_or_next, tau_min, tau_max)

# ---------------------------------------------------------
# Drop events with incomplete estimation-window trading calendar
# (misma idea que tu bloque original)
# ---------------------------------------------------------

coverage_by_event_next <- event_tau_panel_all_next %>%
  group_by(event_num, country) %>%
  summarise(
    n_est_missing = sum((tau >= -190 & tau <= -10) & is.na(date_tau)),
    est_complete = (n_est_missing == 0),
    .groups = "drop"
  )

coverage_by_event_same_or_next <- event_tau_panel_all_same_or_next %>%
  group_by(event_num, country) %>%
  summarise(
    n_est_missing = sum((tau >= -190 & tau <= -10) & is.na(date_tau)),
    est_complete = (n_est_missing == 0),
    .groups = "drop"
  )

events_drop_calendar_next <- coverage_by_event_next %>%
  filter(!est_complete) %>%
  pull(event_num)

events_after_calendar_next <- events_w_event_date_next %>%
  filter(!(event_num %in% events_drop_calendar_next))

event_tau_panel_after_calendar_next <- event_tau_panel_all_next %>%
  filter(!(event_num %in% events_drop_calendar_next))

drop_log <- bind_rows(
  drop_log,
  append_drop_log(
    stage = "drop_calendar_incomplete_est_window_next",
    before_events = events_w_event_date_next,
    drop_ids = events_drop_calendar_next,
    after_events = events_after_calendar_next
  )
)

events_drop_calendar_same_or_next <- coverage_by_event_same_or_next %>%
  filter(!est_complete) %>%
  pull(event_num)

events_after_calendar_same_or_next <- events_w_event_date_same_or_next %>%
  filter(!(event_num %in% events_drop_calendar_same_or_next))

event_tau_panel_after_calendar_same_or_next <- event_tau_panel_all_same_or_next %>%
  filter(!(event_num %in% events_drop_calendar_same_or_next))

drop_log <- bind_rows(
  drop_log,
  append_drop_log(
    stage = "drop_calendar_incomplete_est_window_same_or_next",
    before_events = events_w_event_date_same_or_next,
    drop_ids = events_drop_calendar_same_or_next,
    after_events = events_after_calendar_same_or_next
  )
)

# ---------------------------------------------------------
# Agregar returns del índice local y ACWI al panel por tau
# (idéntico a tu bloque original)
# ---------------------------------------------------------
vix_daily <- vix_daily %>%
  arrange(date) %>%
  mutate(
    vix_log = log(vix),
    dvix = vix_log - dplyr::lag(vix_log)
  )
vix_daily %>%
  summarise(
    start_date = min(date),
    end_date   = max(date),
    total_obs  = n(),
    na_values  = sum(is.na(dvix)),
    share_na   = mean(is.na(dvix)),
    mean_dvix  = mean(dvix, na.rm = TRUE),
    sd_dvix    = sd(dvix, na.rm = TRUE),
    min_dvix   = min(dvix, na.rm = TRUE),
    max_dvix   = max(dvix, na.rm = TRUE)
  )

country_region_map <- tibble::tribble(
  ~country,            ~region,
  "Argentina",         "Emerging Markets",
  "Australia",         "Asia Pacific",
  "Austria",           "Europe",
  "Belgium",           "Europe",
  "Brazil",            "Emerging Markets",
  "Canada",            "North America",
  "Czech Republic",    "Emerging Markets",
  "Denmark",           "Europe",
  "Finland",           "Europe",
  "France",            "Europe",
  "Germany",           "Europe",
  "Greece",            "Emerging Markets",
  "Hungary",           "Emerging Markets",
  "India",             "Emerging Markets",
  "Ireland",           "Europe",
  "Italy",             "Europe",
  "Japan",             "Asia Pacific",
  "Mexico",            "Emerging Markets",
  "Netherlands",       "Europe",
  "Norway",            "Europe",
  "Philippines",       "Emerging Markets",
  "Poland",            "Emerging Markets",
  "South Korea",       "Emerging Markets",
  "Spain",             "Europe",
  "Sweden",            "Europe",
  "Switzerland",       "Europe",
  "Thailand",          "Emerging Markets",
  "Turkey",            "Emerging Markets",
  "Portugal",          "Europe",
  "United Kingdom",    "Europe",
  "United States",     "North America"
)

region_thesis_map <- tibble::tibble(
  country = c(
    "United States","Canada","Mexico",
    "United Kingdom","Austria","Germany","France","Belgium","Spain","Italy",
    "Czech Republic","Greece","Netherlands","Switzerland","Ireland","Norway",
    "Sweden","Turkey","Denmark","Poland","Finland","Hungary","Portugal",
    "Japan","Australia","Thailand","India","South Korea","Philippines",
    "Brazil","Argentina"
  ),
  region_thesis = c(
    rep("North America", 3),
    rep("Europe", 20),
    rep("Asia-Pacific", 6),
    rep("South America", 2)
  )
)

stopifnot(
  msci_regional_base %>%
    count(date, region) %>%
    summarise(max_n = max(n)) %>%
    pull(max_n) == 1
)

indices_base_uniq <- indices_base %>%
  arrange(country, date) %>%
  group_by(country, date) %>%
  summarise(ret = dplyr::first(ret), .groups = "drop")

add_returns <- function(panel) {
  panel %>%
    { if ("region" %in% names(.)) dplyr::select(., -region) else . } %>%
    left_join(indices_base_uniq, by = c("country" = "country", "date_tau" = "date")) %>%
    left_join(acwi_base %>% select(date, acwi_ret),
              by = c("date_tau" = "date")) %>%
    left_join(vix_daily %>% select(date, vix, dvix),
              by = c("date_tau" = "date")) %>%
    left_join(country_region_map, by = "country") %>%
    left_join(msci_regional_base %>% select(date, region, msci_reg_ret),
              by = c("date_tau" = "date", "region" = "region"),
              relationship = "many-to-one")
}

msci_regional_base %>%
  group_by(region) %>%
  summarise(
    non_missing = sum(!is.na(msci_reg_ret)),
    share_na    = mean(is.na(msci_reg_ret)),
    mean_ret    = mean(msci_reg_ret, na.rm = TRUE),
    sd_ret      = sd(msci_reg_ret, na.rm = TRUE),
    min_ret     = min(msci_reg_ret, na.rm = TRUE),
    max_ret     = max(msci_reg_ret, na.rm = TRUE)
  )

# ---- 5.2 Add returns to tau panels -----------------------------------------

event_tau_panel_after_calendar_next <- add_returns(event_tau_panel_after_calendar_next)
event_tau_panel_after_calendar_same_or_next <- add_returns(event_tau_panel_after_calendar_same_or_next)


event_tau_panel_after_calendar_next %>%
  summarise(
    n_na_ret = sum(is.na(ret)),
    n_na_acwi = sum(is.na(acwi_ret))
  )

event_tau_panel_after_calendar_same_or_next %>%
  summarise(
    n_na_ret = sum(is.na(ret)),
    n_na_acwi = sum(is.na(acwi_ret))
  )

# ---------------------------------------------------------
# Count valid observations in the estimation window for the market model
# Same logic as in the original analysis, applied to both event-date definitions
# ---------------------------------------------------------

estimation_counts_mm_from <- function(panel) {
  panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    mutate(valid_mm = !is.na(ret) & !is.na(acwi_ret)) %>%
    group_by(event_num) %>%
    summarise(
      n_rows = n(),
      n_valid = sum(valid_mm),
      .groups = "drop"
    )
}

estimation_counts_mm_from_ext <- function(panel, x_vars) {
  panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    mutate(valid_ext = is.finite(ret) & if_all(all_of(x_vars), ~ is.finite(.x))) %>%
    group_by(event_num) %>%
    summarise(
      n_rows = n(),
      n_valid = sum(valid_ext),
      .groups = "drop"
    )
}



estimation_counts_mm_next <- estimation_counts_mm_from(event_tau_panel_after_calendar_next)
estimation_counts_mm_same_or_next <- estimation_counts_mm_from(event_tau_panel_after_calendar_same_or_next)

estimation_counts_ext_next <- estimation_counts_mm_from_ext(event_tau_panel_after_calendar_next, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"))
estimation_counts_ext_same <- estimation_counts_mm_from_ext(event_tau_panel_after_calendar_same_or_next, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"))

required_n <- length(-190:-10)
threshold_strict <- required_n
threshold_relaxed <- ceiling(0.95 * required_n)

make_mm_sample_from <- function(events_tbl, panel_tbl, estimation_counts_tbl, threshold) {
  drop_ids <- estimation_counts_tbl %>%
    filter(n_valid < threshold) %>%
    pull(event_num)
  
  events_k <- events_tbl %>% filter(!(event_num %in% drop_ids))
  panel_k  <- panel_tbl %>% filter(!(event_num %in% drop_ids))
  
  list(
    threshold = threshold,
    drop_ids = drop_ids,
    events = events_k,
    panel = panel_k
  )
}

# ---- 5.3 Market model functions (simple & extended) ------------------------

# ---------------------------------------------------------
# Market model 
# ---------------------------------------------------------

run_mm <- function(panel) {
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(!is.na(ret) & !is.na(acwi_ret))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      fit = list(lm(ret ~ acwi_ret, data = pick(everything()))),
      .groups = "drop"
    ) %>%
    mutate(
      alpha = purrr::map_dbl(fit, ~ unname(coef(.x)[1])),
      beta  = purrr::map_dbl(fit, ~ unname(coef(.x)[2]))
    ) %>%
    select(event_num, alpha, beta)
  
  panel %>%
    left_join(params, by = "event_num") %>%
    mutate(
      exp_ret = alpha + beta * acwi_ret,
      ar = ret - exp_ret
    )
}


# ---------------------------------------------------------
# Extended market model 
# ---------------------------------------------------------

run_mm_extended <- function(panel, x_vars = c("acwi_ret", "dvix"), ar_name = "ar_ext") {
  stopifnot(all(c("event_num", "tau", "ret") %in% names(panel)))
  stopifnot(all(x_vars %in% names(panel)))
  
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(is.finite(ret)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  
  fml <- as.formula(paste("ret ~", paste(x_vars, collapse = " + ")))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      fit = list(lm(fml, data = pick(everything()))),
      .groups = "drop"
    ) %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  
  panel2 <- panel %>%
    left_join(params, by = "event_num")
  
  exp_ret <- purrr::map2_dbl(panel2$coefs, seq_len(nrow(panel2)), ~ {
    b <- .x
    if (is.null(b)) return(NA_real_)
    xb <- b[["(Intercept)"]]
    
    for (v in x_vars) {
      bv <- unname(b[[v]])
      xv <- panel2[[v]][.y]
      if (!is.finite(bv) || !is.finite(xv)) return(NA_real_)
      xb <- xb + bv * xv
    }
    xb
  })
  
  panel2 %>%
    mutate(
      exp_ret_ext = exp_ret,
      !!ar_name := ret - exp_ret_ext
    )
}


# ---------------------------------------------------------
# Overlaps
# ---------------------------------------------------------

drop_overlaps_by_window <- function(event_tau_panel, events_tbl, k_end) {
  event_days <- event_tau_panel %>%
    filter(tau >= 0 & tau <= k_end) %>%
    select(event_num, country, date_tau) %>%
    filter(!is.na(date_tau)) %>%
    distinct()
  
  overlap_days <- event_days %>%
    group_by(country, date_tau) %>%
    summarise(n_events = n_distinct(event_num), .groups = "drop") %>%
    filter(n_events > 1)
  
  if (nrow(overlap_days) == 0) return(integer(0))
  
  overlap_events <- event_days %>%
    semi_join(overlap_days, by = c("country", "date_tau"))
  
  edges <- overlap_events %>%
    inner_join(overlap_events, by = c("country", "date_tau"), relationship = "many-to-many") %>%
    filter(event_num.x != event_num.y) %>%
    transmute(
      country,
      e1 = pmin(event_num.x, event_num.y),
      e2 = pmax(event_num.x, event_num.y)
    ) %>%
    distinct()
  
  events_to_drop <- c()
  
  for (ctry in unique(edges$country)) {
    edges_c <- edges %>% filter(country == ctry)
    
    adj <- split(edges_c$e2, edges_c$e1)
    adj_rev <- split(edges_c$e1, edges_c$e2)
    
    neighbors <- function(x) unique(c(adj[[as.character(x)]], adj_rev[[as.character(x)]]))
    
    visited <- c()
    comps <- list()
    nodes <- unique(c(edges_c$e1, edges_c$e2))
    
    for (n in nodes) {
      if (n %in% visited) next
      stack <- c(n)
      comp <- c()
      
      while (length(stack) > 0) {
        v <- stack[1]
        stack <- stack[-1]
        if (v %in% visited) next
        visited <- c(visited, v)
        comp <- c(comp, v)
        nb <- neighbors(v)
        stack <- c(stack, nb[!nb %in% visited])
      }
      
      comps[[length(comps) + 1]] <- comp
    }
    
    for (comp in comps) {
      comp_df <- events_tbl %>%
        filter(country == ctry, event_num %in% comp) %>%
        arrange(event_date, event_num)
      
      if (nrow(comp_df) > 1) {
        events_to_drop <- c(events_to_drop, comp_df$event_num[-1])
      }
    }
  }
  
  unique(events_to_drop)
}

# ---------------------------------------------------------
# compute_cars ORIGINAL (0..k) lo dejamos tal cual
# ---------------------------------------------------------

compute_cars <- function(panel, events_tbl, ar_col, k_values = 0:10) {
  sample_sizes <- tibble(k = integer(), n = integer(), n_dropped_overlap = integer())
  car_list <- vector("list", length(k_values))
  names(car_list) <- paste0(ar_col, "_", sprintf("%02d", k_values))
  
  for (k in k_values) {
    sample_k <- panel %>%
      filter(tau >= 0 & tau <= k) %>%
      group_by(event_num, country) %>%
      summarise(complete = all(!is.na(.data[[ar_col]])), .groups = "drop") %>%
      filter(complete) %>%
      select(event_num, country)
    
    drops_overlap_k <- drop_overlaps_by_window(panel, events_tbl, k_end = k)
    
    sample_k_final <- sample_k %>%
      filter(!(event_num %in% drops_overlap_k))
    
    sample_sizes <- bind_rows(
      sample_sizes,
      tibble(
        k = k,
        n = nrow(sample_k_final),
        n_dropped_overlap = length(intersect(sample_k$event_num, drops_overlap_k))
      )
    )
    
    car_k <- panel %>%
      semi_join(sample_k_final, by = c("event_num", "country")) %>%
      filter(tau >= 0 & tau <= k) %>%
      group_by(event_num, country) %>%
      summarise(car = sum(.data[[ar_col]], na.rm = FALSE), .groups = "drop") %>%
      rename(!!paste0("car_", sprintf("%02d", k)) := car)
    
    car_list[[paste0(ar_col, "_", sprintf("%02d", k))]] <- car_k
  }
  
  car_event_level <- reduce(car_list, full_join, by = c("event_num", "country"))
  list(sample_sizes = sample_sizes, car_event_level = car_event_level)
}

# ---------------------------------------------------------
# compute_cars version for windows that start at -1, e.g. [-1,+1] and [-1,+2]
# Nota: esto NO reemplaza compute_cars; lo complementa.
# ---------------------------------------------------------

compute_cars_m1 <- function(panel, events_tbl, ar_col, end_values = c(0, 1, 2, 5)) {
  # Ventanas: [-1,0], [-1,1], [-1,2], ...
  sample_sizes <- tibble(k = integer(), n = integer(), n_dropped_overlap = integer())
  car_list <- vector("list", length(end_values))
  names(car_list) <- paste0(ar_col, "_m1_", sprintf("%02d", end_values))
  
  for (k in end_values) {
    sample_k <- panel %>%
      filter(tau >= -1 & tau <= k) %>%
      group_by(event_num, country) %>%
      summarise(complete = all(!is.na(.data[[ar_col]])), .groups = "drop") %>%
      filter(complete) %>%
      select(event_num, country)
    
    # Overlap: seguimos tu regla original (checar solapamiento en [0,k])
    drops_overlap_k <- drop_overlaps_by_window(panel, events_tbl, k_end = k)
    
    sample_k_final <- sample_k %>%
      filter(!(event_num %in% drops_overlap_k))
    
    sample_sizes <- bind_rows(
      sample_sizes,
      tibble(
        k = k,
        n = nrow(sample_k_final),
        n_dropped_overlap = length(intersect(sample_k$event_num, drops_overlap_k))
      )
    )
    
    car_k <- panel %>%
      semi_join(sample_k_final, by = c("event_num", "country")) %>%
      filter(tau >= -1 & tau <= k) %>%
      group_by(event_num, country) %>%
      summarise(car = sum(.data[[ar_col]], na.rm = FALSE), .groups = "drop") %>%
      rename(!!paste0("car_m1_", sprintf("%02d", k)) := car)
    
    car_list[[paste0(ar_col, "_m1_", sprintf("%02d", k))]] <- car_k
  }
  
  car_event_level <- reduce(car_list, full_join, by = c("event_num", "country"))
  list(sample_sizes = sample_sizes, car_event_level = car_event_level)
}

# ---- 5.4 Run all market-model samples (base & extended, next & same_or_next)

# ---------------------------------------------------------
# Run the market model with two thresholds (100% and 95%) for both day-0 definitions
# (manteniendo tu estructura strict/relaxed)
# ---------------------------------------------------------

# ---- NEXT (BASE)
strict_next <- make_mm_sample_from(
  events_after_calendar_next,
  event_tau_panel_after_calendar_next,
  estimation_counts_mm_next,
  threshold_strict
)
relaxed_next <- make_mm_sample_from(
  events_after_calendar_next,
  event_tau_panel_after_calendar_next,
  estimation_counts_mm_next,
  threshold_relaxed
)

drop_log <- bind_rows(
  drop_log,
  append_drop_log("drop_mm_incomplete_returns_est_window_next_strict100",  events_after_calendar_next, strict_next$drop_ids,  strict_next$events),
  append_drop_log("drop_mm_incomplete_returns_est_window_next_relaxed95", events_after_calendar_next, relaxed_next$drop_ids, relaxed_next$events)
)

panel_mm_next_strict  <- run_mm(strict_next$panel)
panel_mm_next_relaxed <- run_mm(relaxed_next$panel)

cars_next_strict  <- compute_cars(panel_mm_next_strict,  strict_next$events,  ar_col = "ar")
cars_next_relaxed <- compute_cars(panel_mm_next_relaxed, relaxed_next$events, ar_col = "ar")

cars_next_strict_m1  <- compute_cars_m1(panel_mm_next_strict,  strict_next$events,  ar_col = "ar", end_values = c(0,1,2,5))
cars_next_relaxed_m1 <- compute_cars_m1(panel_mm_next_relaxed, relaxed_next$events, ar_col = "ar", end_values = c(0,1,2,5))

# ---- SAME_OR_NEXT (BASE)
strict_same <- make_mm_sample_from(
  events_after_calendar_same_or_next,
  event_tau_panel_after_calendar_same_or_next,
  estimation_counts_mm_same_or_next,
  threshold_strict
)
relaxed_same <- make_mm_sample_from(
  events_after_calendar_same_or_next,
  event_tau_panel_after_calendar_same_or_next,
  estimation_counts_mm_same_or_next,
  threshold_relaxed
)

drop_log <- bind_rows(
  drop_log,
  append_drop_log("drop_mm_incomplete_returns_est_window_same_or_next_strict100",  events_after_calendar_same_or_next, strict_same$drop_ids,  strict_same$events),
  append_drop_log("drop_mm_incomplete_returns_est_window_same_or_next_relaxed95", events_after_calendar_same_or_next, relaxed_same$drop_ids, relaxed_same$events)
)

panel_mm_same_strict  <- run_mm(strict_same$panel)
panel_mm_same_relaxed <- run_mm(relaxed_same$panel)

cars_same_strict  <- compute_cars(panel_mm_same_strict,  strict_same$events,  ar_col = "ar")
cars_same_relaxed <- compute_cars(panel_mm_same_relaxed, relaxed_same$events, ar_col = "ar")

cars_same_strict_m1  <- compute_cars_m1(panel_mm_same_strict,  strict_same$events,  ar_col = "ar", end_values = c(0,1,2,5))
cars_same_relaxed_m1 <- compute_cars_m1(panel_mm_same_relaxed, relaxed_same$events, ar_col = "ar", end_values = c(0,1,2,5))

# ---- NEXT (EXT: ACWI + dVIX + MSCI regional)
strict_next_ext <- make_mm_sample_from(
  events_after_calendar_next,
  event_tau_panel_after_calendar_next,
  estimation_counts_ext_next,
  threshold_strict
)
relaxed_next_ext <- make_mm_sample_from(
  events_after_calendar_next,
  event_tau_panel_after_calendar_next,
  estimation_counts_ext_next,
  threshold_relaxed
)

# ---- SAME_OR_NEXT (EXT: ACWI + dVIX + MSCI regional)
strict_same_ext <- make_mm_sample_from(
  events_after_calendar_same_or_next,
  event_tau_panel_after_calendar_same_or_next,
  estimation_counts_ext_same,
  threshold_strict
)
relaxed_same_ext <- make_mm_sample_from(
  events_after_calendar_same_or_next,
  event_tau_panel_after_calendar_same_or_next,
  estimation_counts_ext_same,
  threshold_relaxed
)

panel_mm_next_strict_ext  <- run_mm_extended(strict_next_ext$panel,  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"), ar_name = "ar_ext")
panel_mm_next_relaxed_ext <- run_mm_extended(relaxed_next_ext$panel, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"), ar_name = "ar_ext")

panel_mm_same_strict_ext  <- run_mm_extended(strict_same_ext$panel,  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"), ar_name = "ar_ext")
panel_mm_same_relaxed_ext <- run_mm_extended(relaxed_same_ext$panel, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"), ar_name = "ar_ext")

cars_next_strict_ext  <- compute_cars(panel_mm_next_strict_ext,  strict_next_ext$events,  ar_col = "ar_ext")
cars_next_relaxed_ext <- compute_cars(panel_mm_next_relaxed_ext, relaxed_next_ext$events, ar_col = "ar_ext")

cars_same_strict_ext  <- compute_cars(panel_mm_same_strict_ext,  strict_same_ext$events,  ar_col = "ar_ext")
cars_same_relaxed_ext <- compute_cars(panel_mm_same_relaxed_ext, relaxed_same_ext$events, ar_col = "ar_ext")

# ---------------------------------------------------------
# Quick summaries for sanity checks
# ---------------------------------------------------------

summary_samples <- tibble(
  panel = c(
    "NEXT","NEXT","NEXT","NEXT",
    "SAME","SAME","SAME","SAME"
  ),
  spec = c(
    "base","ext","base","ext",
    "base","ext","base","ext"
  ),
  threshold_type = c(
    "strict_100","strict_100","relaxed_95","relaxed_95",
    "strict_100","strict_100","relaxed_95","relaxed_95"
  ),
  threshold_value = c(
    threshold_strict, threshold_strict,
    threshold_relaxed, threshold_relaxed,
    threshold_strict, threshold_strict,
    threshold_relaxed, threshold_relaxed
  ),
  n_events = c(
    n_distinct(panel_mm_next_strict$event_num),
    n_distinct(panel_mm_next_strict_ext$event_num),
    n_distinct(panel_mm_next_relaxed$event_num),
    n_distinct(panel_mm_next_relaxed_ext$event_num),
    n_distinct(panel_mm_same_strict$event_num),
    n_distinct(panel_mm_same_strict_ext$event_num),
    n_distinct(panel_mm_same_relaxed$event_num),
    n_distinct(panel_mm_same_relaxed_ext$event_num)
  )
)
summary_samples

# Sample sizes by k (0..10) and m1 windows
cars_next_strict$sample_sizes
cars_next_strict_m1$sample_sizes

cars_same_strict$sample_sizes
cars_same_strict_m1$sample_sizes

cars_next_strict_ext$sample_sizes
cars_next_relaxed_ext$sample_sizes

cars_same_strict_ext$sample_sizes
cars_same_relaxed_ext$sample_sizes

drop_log
# =============================================================================
# PART VI: CROSS-SECTIONAL ANALYSIS
# =============================================================================

# =========================================
# REGRESIONES CROSS-SECTION (CAR ~ X) + TESTS
# =========================================

k_keep <- c(0, 1, 2, 5, 10)

rhs <- "event_type_agg + disruption_type_agg + log_deaths + log_injuries + cancellation + region"
fml <- as.formula(paste0("car ~ ", rhs))


make_cs_data <- function(events_tbl, car_obj, k) {
  car_col <- paste0("car_", sprintf("%02d", k))
  
  df <- car_obj$car_event_level %>%
    select(event_num, country, all_of(car_col)) %>%
    rename(car = all_of(car_col)) %>%
    left_join(events_tbl, by = c("event_num", "country")) %>%
    left_join(region_thesis_map, by = "country") %>%
    filter(is.finite(car)) %>%
    mutate(
      car = as.numeric(car),
      country = as.factor(country),
      log_deaths = log(deaths + 1),
      log_injuries = log(total_injuries + 1),
      region = relevel(as.factor(region_thesis), ref = "North America")
    )
  
  if ("event_type_agg" %in% names(df)) {
    df <- df %>% mutate(event_type_agg = as.factor(event_type_agg))
  }
  
  if ("disruption_type_agg" %in% names(df)) {
    df <- df %>% mutate(disruption_type_agg = as.factor(disruption_type_agg))
  }
  
  needed <- unique(c("car", "country", all.vars(fml)))
  df %>% select(any_of(needed))
}

events_next_strict <- strict_next$events %>% distinct(event_num, country, .keep_all = TRUE)
events_same_strict <- strict_same$events %>% distinct(event_num, country, .keep_all = TRUE)

k_focus <- 2

df_next_k <- make_cs_data(events_next_strict, cars_next_strict, k_focus) %>%
  drop_na(all.vars(fml))

df_same_k <- make_cs_data(events_same_strict, cars_same_strict, k_focus) %>%
  drop_na(all.vars(fml))

corr_next <- df_next_k %>%
  select(car, log_deaths, log_injuries) %>%
  cor(use = "pairwise.complete.obs")

corr_same <- df_same_k %>%
  select(car, log_deaths, log_injuries) %>%
  cor(use = "pairwise.complete.obs")

print(round(corr_next, 3))
print(round(corr_same, 3))

plot_corr <- function(M, title) {
  as.data.frame(M) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "corr") %>%
    ggplot(aes(x = var1, y = var2, fill = corr)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", corr)), size = 3) +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal()
}

plot_corr(corr_next, "Correlation matrix (NEXT, k=5)")
plot_corr(corr_same, "Correlation matrix (SAME, k=5)")

heterosk_tests <- function(fit) {
  bp <- lmtest::bptest(fit)
  
  r2 <- resid(fit)^2
  f  <- fitted(fit)
  aux <- lm(r2 ~ f + I(f^2))
  w_stat <- summary(aux)$r.squared * length(r2)
  w_df <- 2
  w_p <- 1 - pchisq(w_stat, df = w_df)
  
  tibble::tibble(
    test = c("Breusch-Pagan", "White (aux: e^2 ~ fitted + fitted^2)"),
    statistic = c(unname(bp$statistic), unname(w_stat)),
    df = c(unname(bp$parameter), unname(w_df)),
    p_value = c(unname(bp$p.value), unname(w_p))
  )
}

coefs_hc1 <- function(fit) {
  V <- sandwich::vcovHC(fit, type = "HC1")
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble::tibble(
    term = rownames(ct),
    estimate = ct[, 1], se = ct[, 2], t = ct[, 3], p = ct[, 4],
    se_type = "HC1"
  )
}

coefs_cluster <- function(fit, cluster_vec) {
  V <- sandwich::vcovCL(fit, cluster = cluster_vec)
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble::tibble(
    term = rownames(ct),
    estimate = ct[, 1], se = ct[, 2], t = ct[, 3], p = ct[, 4],
    se_type = "CL_country"
  )
}

f_test_cluster <- function(fit, cluster_vec) {
  V <- sandwich::vcovCL(fit, cluster = cluster_vec)
  
  coef_names <- names(coef(fit))
  coef_names <- setdiff(coef_names, "(Intercept)")
  
  if (length(coef_names) == 0) {
    return(tibble::tibble(
      F_stat = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_value = NA_real_
    ))
  }
  
  lh <- car::linearHypothesis(
    fit,
    coef_names,
    vcov. = V,
    test = "F"
  )
  
  tibble::tibble(
    F_stat = lh$F[2],
    df1 = lh$Df[2],
    df2 = lh$Res.Df[2],
    p_value = lh$`Pr(>F)`[2]
  )
}
joint_test_block <- function(fit, cluster_vec, vars) {
  V <- sandwich::vcovCL(fit, cluster = cluster_vec)
  
  vars <- intersect(vars, names(coef(fit)))
  
  if (length(vars) == 0) {
    return(tibble::tibble(
      F_stat = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_value = NA_real_
    ))
  }
  
  lh <- car::linearHypothesis(
    fit,
    vars,
    vcov. = V,
    test = "F"
  )
  
  tibble::tibble(
    F_stat = lh$F[2],
    df1 = lh$Df[2],
    df2 = lh$Res.Df[2],
    p_value = lh$`Pr(>F)`[2]
  )
}

run_model_k <- function(events_tbl, car_obj, model_name, k) {
  df <- make_cs_data(events_tbl, car_obj, k)
  
  if (!is.data.frame(df)) {
    stop("make_cs_data() did not return a data.frame for model = ", model_name, ", k = ", k)
  }
  
  vars_needed <- all.vars(fml)
  df_fit <- tidyr::drop_na(df, dplyr::all_of(vars_needed))
  
  if (!"country" %in% names(df_fit)) {
    stop("country is missing in df_fit for model = ", model_name, ", k = ", k)
  }
  
  if (nrow(df_fit) == 0) {
    stop("df_fit has 0 rows for model = ", model_name, ", k = ", k)
  }
  
  G <- dplyr::n_distinct(df_fit$country)
  
  if (G < 2) {
    stop("Less than 2 clusters for model = ", model_name, ", k = ", k)
  }
  
  fit <- lm(fml, data = df_fit)
  n_obs <- nobs(fit)
  
  tests <- heterosk_tests(fit)
  tests$model <- model_name
  tests$k <- k
  tests$n <- n_obs
  tests$G <- G
  
  coefs_hc1_tbl <- coefs_hc1(fit)
  coefs_hc1_tbl$G <- NA_integer_
  
  coefs_cl_tbl <- coefs_cluster(fit, df_fit$country)
  coefs_cl_tbl$G <- G
  
  coefs <- dplyr::bind_rows(coefs_hc1_tbl, coefs_cl_tbl)
  coefs$model <- model_name
  coefs$k <- k
  coefs$n <- n_obs
  
  f_tests <- f_test_cluster(fit, df_fit$country)
  f_tests$model <- model_name
  f_tests$k <- k
  f_tests$n <- n_obs
  f_tests$G <- G
  
  vars_event_type <- grep("^event_type_agg", names(coef(fit)), value = TRUE)
  vars_disruption <- grep("^disruption_type_agg", names(coef(fit)), value = TRUE)
  vars_severity <- intersect(names(coef(fit)), c("log_deaths", "log_injuries", "cancellation"))
  vars_region <- grep("^region", names(coef(fit)), value = TRUE)
  
  f_blocks <- dplyr::bind_rows(
    joint_test_block(fit, df_fit$country, vars_event_type) %>%
      dplyr::mutate(block = "event_type"),
    joint_test_block(fit, df_fit$country, vars_disruption) %>%
      dplyr::mutate(block = "disruption_type"),
    joint_test_block(fit, df_fit$country, vars_severity) %>%
      dplyr::mutate(block = "severity"),
    joint_test_block(fit, df_fit$country, vars_region) %>%
      dplyr::mutate(block = "region")
  )
  
  f_blocks$model <- model_name
  f_blocks$k <- k
  f_blocks$n <- n_obs
  f_blocks$G <- G
  
  list(
    tests = tests,
    coefs = coefs,
    f_tests = f_tests,
    f_blocks = f_blocks
  )
}

run_cs_suite <- function(events_tbl, car_obj, label) {
  out <- purrr::map(k_keep, ~ run_model_k(events_tbl, car_obj, label, .x))
  list(
    tests = purrr::map_dfr(out, "tests"),
    coefs = purrr::map_dfr(out, "coefs"),
    f_tests = purrr::map_dfr(out, "f_tests"),
    f_blocks = purrr::map_dfr(out, "f_blocks")
  )
}

cs_next_base <- run_cs_suite(events_next_strict, cars_next_strict, "NEXT_base")
cs_next_ext  <- run_cs_suite(events_next_strict, cars_next_strict_ext, "NEXT_ext")

cs_same_base <- run_cs_suite(events_same_strict, cars_same_strict, "SAME_base")
cs_same_ext  <- run_cs_suite(events_same_strict, cars_same_strict_ext, "SAME_ext")

heterosk_tests_all <- bind_rows(
  cs_next_base$tests, cs_next_ext$tests,
  cs_same_base$tests, cs_same_ext$tests
)

coefs_all_hc1_vs_cluster <- bind_rows(
  cs_next_base$coefs, cs_next_ext$coefs,
  cs_same_base$coefs, cs_same_ext$coefs
)

k_focus <- 2

coefs_k5_cluster <- coefs_all_hc1_vs_cluster %>%
  filter(k == k_focus, se_type == "CL_country") %>%
  arrange(model, term)


print(coefs_k5_cluster, n = Inf)

heterosk_tests_all %>% arrange(model, k, test) %>% print(n = Inf)

compare_mean_car <- function(car_obj, version_name) {
  car_obj$car_event_level %>%
    pivot_longer(
      cols = starts_with("car_"),
      names_to = "window",
      values_to = "car"
    ) %>%
    filter(is.finite(car)) %>%
    group_by(window) %>%
    summarise(
      version = version_name,
      n = n(),
      mean_car = mean(car),
      sd_car = sd(car),
      t_stat = mean_car / (sd_car / sqrt(n)),
      p_value = 2 * pt(-abs(t_stat), df = n - 1),
      .groups = "drop"
    )
}

results_next_base <- compare_mean_car(cars_next_strict, "NEXT_base_0plus")

results_next_ext  <- compare_mean_car(cars_next_strict_ext, "NEXT_ext_0plus")

results_same_base <- compare_mean_car(cars_same_strict, "SAME_base_0plus")
results_same_ext  <- compare_mean_car(cars_same_strict_ext, "SAME_ext_0plus")

results_compare <- bind_rows(
  results_next_base, results_next_ext,
  results_same_base, results_same_ext
) %>%
  arrange(window, version)


print(results_compare, n = Inf)

# =============================================================================
# PART VIII: SECTORAL ANALYSIS
#   S&P 500 Consumer Discretionary (USA) and
#   STOXX Europe 600 Travel & Leisure (Europe)
# =============================================================================

# ======================================== 
# ADDITIONAL ANALYSIS: SECTORAL INDICES 
# =========================================
run_model_k_fml <- function(events_tbl, car_obj, model_name, k, fml_local) {
  df <- make_cs_data(events_tbl, car_obj, k)
  
  if (!is.data.frame(df)) {
    stop("make_cs_data() did not return a data.frame for model = ", model_name, ", k = ", k)
  }
  
  vars_needed <- all.vars(fml_local)
  df_fit <- tidyr::drop_na(df, dplyr::all_of(vars_needed))
  
  if (!"country" %in% names(df_fit)) {
    stop("country is missing in df_fit for model = ", model_name, ", k = ", k)
  }
  
  if (nrow(df_fit) == 0) {
    stop("df_fit has 0 rows for model = ", model_name, ", k = ", k)
  }
  
  G <- dplyr::n_distinct(df_fit$country)
  
  if (G < 2) {
    stop("Less than 2 clusters for model = ", model_name, ", k = ", k)
  }
  
  fit <- lm(fml_local, data = df_fit)
  n_obs <- nobs(fit)
  
  tests <- heterosk_tests(fit)
  tests$model <- model_name
  tests$k <- k
  tests$n <- n_obs
  tests$G <- G
  
  coefs_hc1_tbl <- coefs_hc1(fit)
  coefs_hc1_tbl$G <- NA_integer_
  
  coefs_cl_tbl <- coefs_cluster(fit, df_fit$country)
  coefs_cl_tbl$G <- G
  
  coefs <- dplyr::bind_rows(coefs_hc1_tbl, coefs_cl_tbl)
  coefs$model <- model_name
  coefs$k <- k
  coefs$n <- n_obs
  
  f_tests <- f_test_cluster(fit, df_fit$country)
  f_tests$model <- model_name
  f_tests$k <- k
  f_tests$n <- n_obs
  f_tests$G <- G
  
  list(
    tests = tests,
    coefs = coefs,
    f_tests = f_tests
  )
}



sectoral_raw <- read_csv(file.path(path_clean, "sectoral_daily.csv"), show_col_types = FALSE) %>%
  mutate(date = ymd(date))

sectoral_conflicts <- sectoral_raw %>%
  group_by(sector_group, ticker, date) %>%
  summarise(
    n_rows = n(),
    n_close_non_na = n_distinct(close[!is.na(close)]),
    n_ret_non_na = n_distinct(ret[!is.na(ret)]),
    .groups = "drop"
  ) %>%
  filter(n_rows > 1, n_close_non_na > 1 | n_ret_non_na > 1)


print(sectoral_conflicts, n = Inf)

sectoral <- sectoral_raw %>%
  group_by(sector_group, ticker, date) %>%
  summarise(
    close = {
      x <- unique(close[!is.na(close)])
      if (length(x) > 1) NA_real_ else if (length(x) == 1) x else NA_real_
    },
    ret = {
      x <- unique(ret[!is.na(ret)])
      if (length(x) > 1) NA_real_ else if (length(x) == 1) x else NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(
    (sector_group == "USA" & ticker == "SPLRCD") |
      (sector_group == "Europe" & ticker == "SXTP")
  )

sector_coverage <- sectoral %>%
  group_by(sector_group, ticker) %>%
  summarise(
    start_date = min(date[!is.na(ret)], na.rm = TRUE),
    end_date = max(date[!is.na(ret)], na.rm = TRUE),
    non_missing_ret = sum(!is.na(ret)),
    missing_pct = mean(is.na(ret)) * 100,
    mean_ret = mean(ret, na.rm = TRUE),
    sd_ret = sd(ret, na.rm = TRUE),
    min_ret = min(ret, na.rm = TRUE),
    max_ret = max(ret, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(sector_group, start_date)

print(sector_coverage, n = Inf)

sector_stats <- sectoral %>%
  filter(is.finite(ret)) %>%
  group_by(sector_group, ticker) %>%
  summarise(
    Obs = n(),
    NA_pct = mean(is.na(ret)) * 100,
    Mean = mean(ret, na.rm = TRUE),
    SD = sd(ret, na.rm = TRUE),
    Min = min(ret, na.rm = TRUE),
    Max = max(ret, na.rm = TRUE),
    .groups = "drop"
  )

print(sector_stats, n = Inf)

events_sector <- events_final %>%
  mutate(
    sector_group = case_when(
      country %in% c(
        "Austria","Belgium","Denmark","Finland","France","Germany","Ireland","Italy",
        "Netherlands","Norway","Spain","Sweden","Switzerland","Portugal","United Kingdom"
      ) ~ "Europe",
      country == "United States" ~ "USA",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(sector_group))

sector_calendar <- sectoral %>%
  filter(is.finite(ret)) %>%
  distinct(sector_group, date) %>%
  arrange(sector_group, date) %>%
  group_by(sector_group) %>%
  mutate(t = row_number()) %>%
  ungroup()

pick_event_date_sector <- function(sgrp, d, mode = c("next", "same_or_next")) {
  mode <- match.arg(mode)
  cal <- sector_calendar %>% filter(sector_group == sgrp)
  
  cand <- if (mode == "next") {
    cal %>% filter(date > d) %>% slice_head(n = 1)
  } else {
    cal %>% filter(date >= d) %>% slice_head(n = 1)
  }
  
  if (nrow(cand) == 0) return(tibble(event_date = as.Date(NA), t0 = NA_integer_))
  tibble(event_date = cand$date[1], t0 = cand$t[1])
}

events_sector_next <- events_sector %>%
  mutate(tmp = purrr::map2(sector_group, date, ~ pick_event_date_sector(.x, .y, "next"))) %>%
  tidyr::unnest(tmp)

make_sector_tau_panel <- function(events_tbl, tau_min, tau_max) {
  events_tbl %>%
    filter(!is.na(t0)) %>%
    select(event_num, country, sector_group, date, event_date, t0) %>%
    tidyr::crossing(tau = tau_min:tau_max) %>%
    mutate(t = t0 + tau) %>%
    left_join(
      sector_calendar %>% select(sector_group, t, date_tau = date),
      by = c("sector_group", "t")
    )
}

sector_returns_daily <- sectoral %>%
  select(sector_group, date, ret_sector = ret)

sector_tau_panel_all <- make_sector_tau_panel(events_sector_next, tau_min, tau_max) %>%
  left_join(
    sector_returns_daily,
    by = c("sector_group", "date_tau" = "date")
  ) %>%
  left_join(
    acwi_base %>% select(date, acwi_ret),
    by = c("date_tau" = "date")
  ) %>%
  left_join(
    vix_daily %>% select(date, dvix),
    by = c("date_tau" = "date")
  ) %>%
  left_join(country_region_map, by = "country") %>%
  left_join(
    msci_regional_base %>% select(date, region, msci_reg_ret),
    by = c("date_tau" = "date", "region" = "region"),
    relationship = "many-to-one"
  )

est_counts_sector_mm <- sector_tau_panel_all %>%
  filter(tau >= -190 & tau <= -10) %>%
  mutate(valid = is.finite(ret_sector) & is.finite(acwi_ret)) %>%
  group_by(event_num) %>%
  summarise(n_valid = sum(valid), .groups = "drop")

est_counts_sector_ext <- sector_tau_panel_all %>%
  filter(tau >= -190 & tau <= -10) %>%
  mutate(valid = is.finite(ret_sector) & is.finite(acwi_ret) & is.finite(dvix) & is.finite(msci_reg_ret)) %>%
  group_by(event_num) %>%
  summarise(n_valid = sum(valid), .groups = "drop")

drop_ids_sector_strict <- est_counts_sector_mm %>%
  filter(n_valid < threshold_strict) %>%
  pull(event_num)

drop_ids_sector_ext_strict <- est_counts_sector_ext %>%
  filter(n_valid < threshold_strict) %>%
  pull(event_num)

events_sector_strict <- events_sector_next %>%
  filter(!(event_num %in% drop_ids_sector_strict)) %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_strict <- sector_tau_panel_all %>%
  filter(!(event_num %in% drop_ids_sector_strict))

events_sector_ext_strict <- events_sector_next %>%
  filter(!(event_num %in% drop_ids_sector_ext_strict)) %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_ext_strict <- sector_tau_panel_all %>%
  filter(!(event_num %in% drop_ids_sector_ext_strict))

events_sector_dropped_mm <- events_sector_next %>%
  filter(event_num %in% drop_ids_sector_strict) %>%
  select(event_num, country, sector_group, date, event_date)

events_sector_dropped_ext <- events_sector_next %>%
  filter(event_num %in% drop_ids_sector_ext_strict) %>%
  select(event_num, country, sector_group, date, event_date)

print(events_sector_dropped_mm, n = Inf)
print(events_sector_dropped_ext, n = Inf)

run_mm_sector <- function(panel) {
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(is.finite(ret_sector), is.finite(acwi_ret)) %>%
    mutate(
      ret_sector = as.numeric(ret_sector),
      acwi_ret = as.numeric(acwi_ret)
    )
  
  safe_lm <- function(d) {
    d <- d %>% select(ret_sector, acwi_ret) %>% drop_na()
    if (nrow(d) < 30) return(c(alpha = NA_real_, beta = NA_real_))
    if (sd(d$acwi_ret) == 0) return(c(alpha = NA_real_, beta = NA_real_))
    fit <- lm(ret_sector ~ acwi_ret, data = d)
    c(alpha = unname(coef(fit)[1]), beta = unname(coef(fit)[2]))
  }
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      tmp = list(safe_lm(pick(ret_sector, acwi_ret))),
      .groups = "drop"
    ) %>%
    mutate(
      alpha = purrr::map_dbl(tmp, 1),
      beta = purrr::map_dbl(tmp, 2)
    ) %>%
    select(event_num, alpha, beta)
  
  panel %>%
    mutate(
      ret_sector = as.numeric(ret_sector),
      acwi_ret = as.numeric(acwi_ret)
    ) %>%
    left_join(params, by = "event_num") %>%
    mutate(
      exp_ret = alpha + beta * acwi_ret,
      ar_sector = ret_sector - exp_ret
    )
}

run_mm_sector_extended <- function(panel, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"), ar_name = "ar_sector_ext") {
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(is.finite(ret_sector)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  
  fml_local <- as.formula(paste("ret_sector ~", paste(x_vars, collapse = " + ")))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      fit = list(lm(fml_local, data = pick(everything()))),
      .groups = "drop"
    ) %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  
  panel2 <- panel %>%
    left_join(params, by = "event_num")
  
  exp_ret <- purrr::map2_dbl(panel2$coefs, seq_len(nrow(panel2)), ~ {
    b <- .x
    if (is.null(b)) return(NA_real_)
    xb <- b[["(Intercept)"]]
    
    for (v in x_vars) {
      bv <- unname(b[[v]])
      xv <- panel2[[v]][.y]
      if (!is.finite(bv) || !is.finite(xv)) return(NA_real_)
      xb <- xb + bv * xv
    }
    
    xb
  })
  
  panel2 %>%
    mutate(
      exp_ret_sector_ext = exp_ret,
      !!ar_name := ret_sector - exp_ret_sector_ext
    )
}

panel_sector_strict_mm <- run_mm_sector(panel_sector_strict)
panel_sector_strict_ext <- run_mm_sector_extended(
  panel_sector_ext_strict,
  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)

cars_sector_strict <- compute_cars(
  panel_sector_strict_mm,
  events_sector_strict,
  ar_col = "ar_sector"
)

cars_sector_strict_ext <- compute_cars(
  panel_sector_strict_ext,
  events_sector_ext_strict,
  ar_col = "ar_sector_ext"
)

results_sector_base <- compare_mean_car(cars_sector_strict, "SECTOR_base")
results_sector_ext <- compare_mean_car(cars_sector_strict_ext, "SECTOR_ext")

results_compare <- bind_rows(
  results_next_base,
  results_next_ext,
  results_same_base,
  results_same_ext,
  results_sector_base,
  results_sector_ext
) %>%
  arrange(window, version)

print(results_sector_base, n = Inf)
print(results_sector_ext, n = Inf)
print(results_compare, n = Inf)

cs_sector_base <- run_cs_suite(events_sector_strict, cars_sector_strict, "SECTOR_base")
cs_sector_ext <- run_cs_suite(events_sector_ext_strict, cars_sector_strict_ext, "SECTOR_ext")

heterosk_tests_sector <- bind_rows(
  cs_sector_base$tests,
  cs_sector_ext$tests
)

coefs_sector_hc1_vs_cluster <- bind_rows(
  cs_sector_base$coefs,
  cs_sector_ext$coefs
)

k_focus <- 2

coefs_sector_k_cluster <- coefs_sector_hc1_vs_cluster %>%
  filter(k == k_focus, se_type == "CL_country") %>%
  arrange(model, term)

print(coefs_sector_k_cluster, n = Inf)

heterosk_tests_sector %>%
  arrange(model, k, test) %>%
  print(n = Inf)

plot_data <- results_compare %>%
  mutate(k = as.numeric(stringr::str_extract(window, "\\d+"))) %>%
  filter(version %in% c(
    "NEXT_base_0plus",
    "NEXT_ext_0plus",
    "SAME_base_0plus",
    "SAME_ext_0plus",
    "SECTOR_base",
    "SECTOR_ext"
  ))

ggplot(plot_data, aes(x = k, y = mean_car, color = version)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 0:10) +
  labs(
    x = "Event window (k)",
    y = "Mean cumulative abnormal return (CAR)",
    color = "Model",
    title = "Average CAR across event windows"
  ) +
  theme_minimal()



# Separate the United States from Europe
safe_run_cs_suite <- function(events_tbl, car_obj, label) {
  out <- tryCatch(
    run_cs_suite(events_tbl, car_obj, label),
    error = function(e) NULL
  )
  out
}

events_sector_usa_strict <- events_sector_strict %>%
  filter(sector_group == "USA") %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_usa_strict <- panel_sector_strict %>%
  filter(sector_group == "USA")

events_sector_usa_ext_strict <- events_sector_ext_strict %>%
  filter(sector_group == "USA") %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_usa_ext_strict <- panel_sector_ext_strict %>%
  filter(sector_group == "USA")

panel_sector_usa_strict_mm <- run_mm_sector(panel_sector_usa_strict)

panel_sector_usa_strict_ext <- run_mm_sector_extended(
  panel_sector_usa_ext_strict,
  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)

cars_sector_usa_strict <- compute_cars(
  panel_sector_usa_strict_mm,
  events_sector_usa_strict,
  ar_col = "ar_sector"
)

cars_sector_usa_strict_ext <- compute_cars(
  panel_sector_usa_strict_ext,
  events_sector_usa_ext_strict,
  ar_col = "ar_sector_ext"
)

results_sector_usa_base <- compare_mean_car(cars_sector_usa_strict, "SECTOR_USA_base")
results_sector_usa_ext <- compare_mean_car(cars_sector_usa_strict_ext, "SECTOR_USA_ext")

sample_sizes_sector_usa <- bind_rows(
  cars_sector_usa_strict$sample_sizes %>% mutate(version = "SECTOR_USA_base"),
  cars_sector_usa_strict_ext$sample_sizes %>% mutate(version = "SECTOR_USA_ext")
)

events_sector_europe_strict <- events_sector_strict %>%
  filter(sector_group == "Europe") %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_europe_strict <- panel_sector_strict %>%
  filter(sector_group == "Europe")

events_sector_europe_ext_strict <- events_sector_ext_strict %>%
  filter(sector_group == "Europe") %>%
  distinct(event_num, country, .keep_all = TRUE)

panel_sector_europe_ext_strict <- panel_sector_ext_strict %>%
  filter(sector_group == "Europe")

panel_sector_europe_strict_mm <- run_mm_sector(panel_sector_europe_strict)

panel_sector_europe_strict_ext <- run_mm_sector_extended(
  panel_sector_europe_ext_strict,
  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)

cars_sector_europe_strict <- compute_cars(
  panel_sector_europe_strict_mm,
  events_sector_europe_strict,
  ar_col = "ar_sector"
)

cars_sector_europe_strict_ext <- compute_cars(
  panel_sector_europe_strict_ext,
  events_sector_europe_ext_strict,
  ar_col = "ar_sector_ext"
)

results_sector_europe_base <- compare_mean_car(cars_sector_europe_strict, "SECTOR_EUROPE_base")
results_sector_europe_ext <- compare_mean_car(cars_sector_europe_strict_ext, "SECTOR_EUROPE_ext")

sample_sizes_sector_europe <- bind_rows(
  cars_sector_europe_strict$sample_sizes %>% mutate(version = "SECTOR_EUROPE_base"),
  cars_sector_europe_strict_ext$sample_sizes %>% mutate(version = "SECTOR_EUROPE_ext")
)

results_sector_split_compare <- bind_rows(
  results_next_base,
  results_next_ext,
  results_same_base,
  results_same_ext,
  results_sector_usa_base,
  results_sector_usa_ext,
  results_sector_europe_base,
  results_sector_europe_ext
) %>%
  arrange(window, version)

print(sample_sizes_sector_usa, n = Inf)
print(sample_sizes_sector_europe, n = Inf)
print(results_sector_usa_base, n = Inf)
print(results_sector_usa_ext, n = Inf)
print(results_sector_europe_base, n = Inf)
print(results_sector_europe_ext, n = Inf)
print(results_sector_split_compare, n = Inf)

cs_sector_europe_base <- safe_run_cs_suite(
  events_sector_europe_strict,
  cars_sector_europe_strict,
  "SECTOR_EUROPE_base"
)

cs_sector_europe_ext <- safe_run_cs_suite(
  events_sector_europe_ext_strict,
  cars_sector_europe_strict_ext,
  "SECTOR_EUROPE_ext"
)

if (!is.null(cs_sector_europe_base) && !is.null(cs_sector_europe_ext)) {
  heterosk_tests_sector_europe <- bind_rows(
    cs_sector_europe_base$tests,
    cs_sector_europe_ext$tests
  )
  
  coefs_sector_europe_hc1_vs_cluster <- bind_rows(
    cs_sector_europe_base$coefs,
    cs_sector_europe_ext$coefs
  )
  
  f_tests_sector_europe <- bind_rows(
    cs_sector_europe_base$f_tests,
    cs_sector_europe_ext$f_tests
  )
  
  f_blocks_sector_europe <- bind_rows(
    cs_sector_europe_base$f_blocks,
    cs_sector_europe_ext$f_blocks
  )
  
  coefs_sector_europe_k_cluster <- coefs_sector_europe_hc1_vs_cluster %>%
    filter(k == k_focus, se_type == "CL_country") %>%
    arrange(model, term)
  
  print(heterosk_tests_sector_europe, n = Inf)
  print(coefs_sector_europe_k_cluster, n = Inf)
  print(f_tests_sector_europe, n = Inf)
  print(f_blocks_sector_europe, n = Inf)
}

sector_split_counts <- tibble(
  version = c(
    "SECTOR_USA_base",
    "SECTOR_USA_ext",
    "SECTOR_EUROPE_base",
    "SECTOR_EUROPE_ext"
  ),
  n_events = c(
    n_distinct(events_sector_usa_strict$event_num),
    n_distinct(events_sector_usa_ext_strict$event_num),
    n_distinct(events_sector_europe_strict$event_num),
    n_distinct(events_sector_europe_ext_strict$event_num)
  ),
  n_countries = c(
    n_distinct(events_sector_usa_strict$country),
    n_distinct(events_sector_usa_ext_strict$country),
    n_distinct(events_sector_europe_strict$country),
    n_distinct(events_sector_europe_ext_strict$country)
  )
)

print(sector_split_counts, n = Inf)

#sanity checks
events_sector_usa_strict %>% count(country)
events_sector_europe_strict %>% count(country, sort = TRUE)

panel_sector_usa_strict %>%
  summarise(
    n_events = n_distinct(event_num),
    n_dates = n_distinct(date_tau),
    n_na_ret = sum(!is.finite(ret_sector))
  )

panel_sector_europe_strict %>%
  summarise(
    n_events = n_distinct(event_num),
    n_dates = n_distinct(date_tau),
    n_na_ret = sum(!is.finite(ret_sector))
  )

# =============================================================================
# PART IX: ROBUSTNESS CHECKS
# =============================================================================

# ---- 9.1 Placebo test -------------------------------------------------------

# =========================
# PLACEBO TEST (NEXT, STRICT)
# =========================

season_from_date <- function(x) {
  m <- lubridate::month(x)
  dplyr::case_when(
    m %in% c(12, 1, 2) ~ "winter",
    m %in% c(3, 4, 5) ~ "spring",
    m %in% c(6, 7, 8) ~ "summer",
    m %in% c(9, 10, 11) ~ "autumn",
    TRUE ~ NA_character_
  )
}

candidate_calendar_dates <- index_ranges %>%
  rowwise() %>%
  mutate(date_seq = list(seq.Date(index_start, index_end, by = "day"))) %>%
  tidyr::unnest(date_seq) %>%
  ungroup() %>%
  transmute(
    country,
    candidate_date = date_seq,
    candidate_year = lubridate::year(candidate_date),
    candidate_wday = lubridate::wday(candidate_date, week_start = 1),
    candidate_season = season_from_date(candidate_date)
  )

real_event_dates_by_country <- events_final %>%
  select(country, real_event_date = date)

pick_placebo_date <- function(ctry, d, exclusion_days = 10) {
  target_year <- lubridate::year(d)
  target_wday <- lubridate::wday(d, week_start = 1)
  target_season <- season_from_date(d)
  
  candidates <- candidate_calendar_dates %>%
    filter(
      country == ctry,
      candidate_wday == target_wday,
      candidate_season == target_season
    )
  
  country_events <- real_event_dates_by_country %>%
    filter(country == ctry) %>%
    pull(real_event_date)
  
  candidates <- candidates %>%
    mutate(
      min_dist_to_real_event = purrr::map_dbl(
        candidate_date,
        ~ min(abs(as.numeric(.x - country_events)), na.rm = TRUE)
      )
    ) %>%
    filter(
      candidate_date != d,
      is.finite(min_dist_to_real_event),
      min_dist_to_real_event > exclusion_days
    )
  
  candidates_same_year <- candidates %>%
    filter(candidate_year == target_year)
  
  if (nrow(candidates_same_year) > 0) {
    return(sample(candidates_same_year$candidate_date, 1))
  }
  
  if (nrow(candidates) > 0) {
    return(sample(candidates$candidate_date, 1))
  }
  
  as.Date(NA)
}

set.seed(646)

events_next_placebo <- events_next_strict %>%
  mutate(
    placebo_date = as.Date(
      purrr::map2_chr(country, date, ~ as.character(pick_placebo_date(.x, .y)))
    )
  )

# reconstruir event_date
events_w_event_date_placebo_next <- events_next_placebo %>%
  filter(!is.na(placebo_date)) %>%
  select(-event_date, -t0) %>%
  mutate(tmp = purrr::map2(country, placebo_date, ~ pick_event_date(.x, .y, "next"))) %>%
  tidyr::unnest(tmp)

# panel tau
event_tau_panel_placebo_next_all <- make_event_tau_panel(
  events_w_event_date_placebo_next,
  tau_min,
  tau_max
)

# cobertura
coverage_by_event_placebo_next <- event_tau_panel_placebo_next_all %>%
  group_by(event_num, country) %>%
  summarise(
    n_est_missing = sum((tau >= -190 & tau <= -10) & is.na(date_tau)),
    est_complete = (n_est_missing == 0),
    .groups = "drop"
  )

events_drop_calendar_placebo_next <- coverage_by_event_placebo_next %>%
  dplyr::filter(!est_complete) %>%
  dplyr::pull(event_num)

events_after_calendar_placebo_next <- events_w_event_date_placebo_next %>%
  dplyr::filter(!(event_num %in% events_drop_calendar_placebo_next))

event_tau_panel_after_calendar_placebo_next <- event_tau_panel_placebo_next_all %>%
  filter(event_num %in% events_after_calendar_placebo_next$event_num) %>%
  add_returns()

# MM sample
estimation_counts_mm_placebo_next <- estimation_counts_mm_from(event_tau_panel_after_calendar_placebo_next)

strict_placebo_next <- make_mm_sample_from(
  events_after_calendar_placebo_next,
  event_tau_panel_after_calendar_placebo_next,
  estimation_counts_mm_placebo_next,
  threshold_strict
)

# market model
panel_mm_placebo_next_strict <- run_mm(strict_placebo_next$panel)

# CARs
cars_placebo_next_strict <- compute_cars(
  panel_mm_placebo_next_strict,
  strict_placebo_next$events,
  ar_col = "ar"
)


# medias
results_placebo_next_base <- compare_mean_car(
  cars_placebo_next_strict,
  "PLACEBO_NEXT_base_0plus"
)

# cross-section
cs_placebo_next_base <- run_cs_suite(
  strict_placebo_next$events %>% distinct(event_num, country, .keep_all = TRUE),
  cars_placebo_next_strict,
  "PLACEBO_NEXT_base"
)

# Direct comparison
placebo_vs_real_next <- bind_rows(
  compare_mean_car(cars_next_strict, "NEXT_base_0plus"),
  results_placebo_next_base
) %>%
  arrange(window, version)

# Print selected outputs
print(results_placebo_next_base, n = Inf)
print(cs_placebo_next_base$tests, n = Inf)
print(cs_placebo_next_base$coefs, n = Inf)
print(placebo_vs_real_next, n = Inf)



# =========================
# F-Tests
# =========================




f_tests_all <- bind_rows(
  cs_next_base$f_tests,
  cs_next_ext$f_tests,
  cs_same_base$f_tests,
  cs_same_ext$f_tests,
  cs_sector_base$f_tests,
  cs_sector_ext$f_tests,
  cs_placebo_next_base$f_tests
) %>%
  arrange(model, k)

print(f_tests_all, n = Inf)




# F-tests by variable group


f_blocks_all <- bind_rows(
  cs_next_base$f_blocks,
  cs_next_ext$f_blocks,
  cs_same_base$f_blocks,
  cs_same_ext$f_blocks,
  cs_sector_base$f_blocks,
  cs_sector_ext$f_blocks,
  cs_placebo_next_base$f_blocks
) %>%
  arrange(model, k, block)

print(f_blocks_all, n = Inf)

# =============================================================================
# PART IX (cont.) — Estimation window sensitivity & CS robustness (see below,
# search "ESTIMATION WINDOW ROBUSTNESS").
# =============================================================================

# =============================================================================
# PART IX (cont.): ROBUSTNESS CHECKS
# ---- 9.2 Estimation window sensitivity ([-150,-10] and [-120,-10]) ----------
# =========================================================
# ESTIMATION WINDOW ROBUSTNESS: NEXT strict only
# =========================================================


run_mm_window <- function(panel, est_start, est_end) {
  estimation <- panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    filter(!is.na(ret) & !is.na(acwi_ret))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      fit = list(lm(ret ~ acwi_ret, data = pick(everything()))),
      .groups = "drop"
    ) %>%
    mutate(
      alpha = purrr::map_dbl(fit, ~ unname(coef(.x)[1])),
      beta  = purrr::map_dbl(fit, ~ unname(coef(.x)[2]))
    ) %>%
    select(event_num, alpha, beta)
  
  panel %>%
    left_join(params, by = "event_num") %>%
    mutate(
      exp_ret = alpha + beta * acwi_ret,
      ar = ret - exp_ret
    )
}

run_mm_extended_window <- function(panel, est_start, est_end,
                                   x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
                                   ar_name = "ar_ext") {
  estimation <- panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    filter(is.finite(ret)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  
  fml_local <- as.formula(paste("ret ~", paste(x_vars, collapse = " + ")))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(
      fit = list(lm(fml_local, data = pick(everything()))),
      .groups = "drop"
    ) %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  
  panel2 <- panel %>%
    left_join(params, by = "event_num")
  
  exp_ret <- purrr::map2_dbl(panel2$coefs, seq_len(nrow(panel2)), ~ {
    b <- .x
    if (is.null(b)) return(NA_real_)
    xb <- b[["(Intercept)"]]
    
    for (v in x_vars) {
      bv <- unname(b[[v]])
      xv <- panel2[[v]][.y]
      if (!is.finite(bv) || !is.finite(xv)) return(NA_real_)
      xb <- xb + bv * xv
    }
    xb
  })
  
  panel2 %>%
    mutate(
      exp_ret_ext = exp_ret,
      !!ar_name := ret - exp_ret_ext
    )
}

estimation_counts_mm_window <- function(panel, est_start, est_end) {
  panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    mutate(valid_mm = !is.na(ret) & !is.na(acwi_ret)) %>%
    group_by(event_num) %>%
    summarise(
      n_rows = n(),
      n_valid = sum(valid_mm),
      .groups = "drop"
    )
}

estimation_counts_mm_window_ext <- function(panel, est_start, est_end,
                                            x_vars = c("acwi_ret", "dvix", "msci_reg_ret")) {
  panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    mutate(valid_ext = is.finite(ret) & if_all(all_of(x_vars), ~ is.finite(.x))) %>%
    group_by(event_num) %>%
    summarise(
      n_rows = n(),
      n_valid = sum(valid_ext),
      .groups = "drop"
    )
}

est_windows <- list(
  ew150 = c(-150, -10),
  ew120 = c(-120, -10)
)

results_estimation_robustness <- purrr::map_dfr(names(est_windows), function(vname) {
  
  est_start <- est_windows[[vname]][1]
  est_end   <- est_windows[[vname]][2]
  threshold_local <- length(est_start:est_end)
  
  counts_base <- estimation_counts_mm_window(
    event_tau_panel_after_calendar_next,
    est_start, est_end
  )
  
  counts_ext <- estimation_counts_mm_window_ext(
    event_tau_panel_after_calendar_next,
    est_start, est_end,
    x_vars = c("acwi_ret", "dvix", "msci_reg_ret")
  )
  
  sample_base <- make_mm_sample_from(
    events_tbl = events_after_calendar_next,
    panel_tbl = event_tau_panel_after_calendar_next,
    estimation_counts_tbl = counts_base,
    threshold = threshold_local
  )
  
  sample_ext <- make_mm_sample_from(
    events_tbl = events_after_calendar_next,
    panel_tbl = event_tau_panel_after_calendar_next,
    estimation_counts_tbl = counts_ext,
    threshold = threshold_local
  )
  
  panel_base_fit <- run_mm_window(sample_base$panel, est_start, est_end)
  panel_ext_fit  <- run_mm_extended_window(sample_ext$panel, est_start, est_end,
                                           x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
                                           ar_name = "ar_ext")
  
  cars_base <- compute_cars(panel_base_fit, sample_base$events, ar_col = "ar")
  cars_ext  <- compute_cars(panel_ext_fit, sample_ext$events, ar_col = "ar_ext")
  
  bind_rows(
    compare_mean_car(cars_base, paste0("NEXT_base_", vname)),
    compare_mean_car(cars_ext,  paste0("NEXT_ext_", vname))
  )
})

print(results_estimation_robustness, n = Inf)




# =========================================================
# CROSS-SECTION ROBUSTNESS (EW = [-120,-10], EXT MODEL, k = 2)
# =========================================================

est_start <- -120
est_end   <- -10
threshold_local <- length(est_start:est_end)

# estimation counts
counts_ext_cs <- estimation_counts_mm_window_ext(
  event_tau_panel_after_calendar_next,
  est_start, est_end,
  x_vars = c("acwi_ret", "dvix", "msci_reg_ret")
)

# sample
sample_ext_cs <- make_mm_sample_from(
  events_tbl = events_after_calendar_next,
  panel_tbl = event_tau_panel_after_calendar_next,
  estimation_counts_tbl = counts_ext_cs,
  threshold = threshold_local
)

# run extended MM with new window
panel_ext_cs <- run_mm_extended_window(
  sample_ext_cs$panel,
  est_start, est_end,
  x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_ext"
)

# compute CARs
cars_ext_cs <- compute_cars(
  panel_ext_cs,
  sample_ext_cs$events,
  ar_col = "ar_ext"
)

# Prepare the cross-sectional dataset
cs_data_ext_ew120 <- make_cs_data(
  events_tbl = sample_ext_cs$events,
  car_obj = cars_ext_cs,
  k = 2
)

# Run only k = 2
cs_results_ext_ew120_k2 <- run_model_k(
  events_tbl = sample_ext_cs$events,
  car_obj = cars_ext_cs,
  model_name = "NEXT_ext_ew120",
  k = 2
)

print(cs_results_ext_ew120_k2$coefs, n = Inf)


# =============================================================================
# PART XI: F-TESTS CONSOLIDATION
# (F-test objects already built above; this section is complete as-is)
# =============================================================================

# =============================================================================
# PART XII: FIGURES — ORIGINAL (for thesis, as currently in document)
# =============================================================================

# ---- Figure 5.1: Mean CAR across event windows (baseline models) -----------
to_plot_windows <- function(df) {
  df %>%
    mutate(
      k = as.integer(stringr::str_extract(window, "\\d+")),
      se = sd_car / sqrt(n),
      ci_low = mean_car - 1.96 * se,
      ci_high = mean_car + 1.96 * se
    ) %>%
    arrange(k)
}



baseline_plot_data <- bind_rows(
  results_next_base %>% mutate(model = "Simple market model"),
  results_next_ext %>% mutate(model = "Extended market model")
) %>%
  to_plot_windows()

p_baseline <- ggplot(
  baseline_plot_data,
  aes(x = k, y = mean_car, linetype = model, shape = model, group = model)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, color = "black") +
  geom_line(linewidth = 0.9, color = "black") +
  geom_point(size = 2.2, color = "black") +
  scale_x_continuous(
    breaks = sort(unique(baseline_plot_data$k)),
    labels = sort(unique(baseline_plot_data$k))
  ) +
  labs(
    x = "Event window length (k)",
    y = "Mean CAR",
    linetype = NULL,
    shape = NULL
  ) +
  scale_y_continuous(
    breaks = c(-0.0050, -0.00375, -0.0025, -0.00125, 0.0000, 0.00125, 0.0025),
    labels = scales::number_format(accuracy = 0.0001)
  )+
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

p_baseline


sectoral_plot_data <- bind_rows(
  results_sector_usa_ext %>% mutate(region_plot = "United States"),
  results_sector_europe_ext %>% mutate(region_plot = "Europe")
) %>%
  to_plot_windows()

sectoral_plot_data$region_plot <- factor(
  sectoral_plot_data$region_plot,
  levels = c("United States", "Europe")
)

p_sectoral <- ggplot(
  sectoral_plot_data,
  aes(x = k, y = mean_car)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.10,
    linewidth = 0.6,
    color = "black"
  ) +
  geom_line(linewidth = 0.9, color = "black") +
  geom_point(size = 2.2, color = "black") +
  facet_wrap(~region_plot, nrow = 2, scales = "free_y") +
  scale_x_continuous(
    breaks = 0:10,
    labels = 0:10
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = scales::number_format(accuracy = 0.0001),
    expand = expansion(mult = c(0.04, 0.04))
  ) +
  labs(
    x = "Event window length (k)",
    y = "Mean CAR"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_sectoral

# ---- Figure 5.2 label (facet strip already shows "United States" / "Europe")

# =============================================================================
# PART XIII: FIGURES — IMPROVED
#   Addresses checklist items:
#     ✓ Larger axis/legend labels and tick text
#     ✓ Higher contrast line styles (solid vs dashed, different point shapes)
#     ✓ Better use of whitespace (plot.margin, expand)
#     ✓ Explicit panel tags "(a)" / "(b)" in strip labels
#     ✓ Saved as PDF (vector, zoomable without pixelation)
#   NOTE: Item 4 of the checklist (matching section headers in the LaTeX
#   document) cannot be fixed from R; edit the .tex source directly.
# =============================================================================

# (to_plot_windows() and baseline_plot_data / sectoral_plot_data
#  are already defined in Part XII above — reused here directly.)

# ---- Figure 5.1 improved ---------------------------------------------------

p_baseline_improved <- ggplot(
  baseline_plot_data,
  aes(x = k, y = mean_car, linetype = model, shape = model, group = model)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.18, linewidth = 0.55, color = "black"
  ) +
  geom_line(linewidth = 1.1, color = "black") +
  geom_point(size = 3.0, color = "black", fill = "white", stroke = 1.2) +
  scale_linetype_manual(values = c(
    "Extended market model" = "solid",
    "Simple market model"   = "dashed"
  )) +
  scale_shape_manual(values = c(
    "Extended market model" = 16,
    "Simple market model"   = 21
  )) +
  scale_x_continuous(
    breaks = sort(unique(baseline_plot_data$k)),
    labels = sort(unique(baseline_plot_data$k)),
    expand = expansion(add = 0.3)
  ) +
  scale_y_continuous(
    breaks = c(-0.0050, -0.00375, -0.0025, -0.00125, 0.0000, 0.00125, 0.0025),
    labels = scales::number_format(accuracy = 0.0001),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(
    x        = "Event window length (k)",
    y        = "Mean CAR",
    linetype = NULL,
    shape    = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 11),
    legend.key.width   = unit(1.8, "cm"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    plot.margin        = margin(t = 8, r = 12, b = 8, l = 8, unit = "pt")
  )

p_baseline_improved

# ---- Figure 5.2 improved ---------------------------------------------------

p_sectoral_improved <- ggplot(
  sectoral_plot_data,
  aes(x = k, y = mean_car)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.6) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.12, linewidth = 0.55, color = "black"
  ) +
  geom_line(linewidth = 1.1, color = "black") +
  geom_point(size = 3.0, color = "black") +
  facet_wrap(
    ~region_plot,
    nrow     = 2,
    scales   = "free_y",
    labeller = labeller(region_plot = c(
      "United States" = "(a)  United States",
      "Europe"        = "(b)  Europe"
    ))
  ) +
  scale_x_continuous(
    breaks = 0:10,
    labels = 0:10,
    expand = expansion(add = 0.3)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 5),
    labels = scales::number_format(accuracy = 0.0001),
    expand = expansion(mult = c(0.10, 0.10))
  ) +
  labs(
    x = "Event window length (k)",
    y = "Mean CAR"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    strip.text         = element_text(size = 12, face = "bold", hjust = 0),
    strip.background   = element_rect(fill = "grey96", color = NA),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    plot.margin        = margin(t = 8, r = 12, b = 8, l = 8, unit = "pt"),
    panel.spacing      = unit(1.2, "lines")
  )

p_sectoral_improved

# ---- Save both figures as PDF (vector, zoomable without pixelation) --------
# Create the output folder if it does not yet exist
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

ggsave(
  filename = "figures/fig5_1_baseline_car.pdf",
  plot     = p_baseline_improved,
  width    = 16, height = 9, units = "cm",
  device   = "pdf"
)

ggsave(
  filename = "figures/fig5_2_sectoral_car.pdf",
  plot     = p_sectoral_improved,
  width    = 16, height = 13, units = "cm",
  device   = "pdf"
)

message("Figures saved as PDF to figures/")


message("Script complete.")
