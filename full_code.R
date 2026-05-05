# =============================================================================
# EVENT STUDY: COUNTRY-LEVEL MARKETS AND LIVE ENTERTAINMENT TRAGEDY SHOCKS
# Master's Thesis in Finance/Econometrics
# Author: Wendy Olivia Bazua Corrales
#
# REPRODUCIBILITY SCRIPT
# Place this file and the six required CSV files in one folder.
# Run from that folder (set working directory in RStudio, or use setwd()).
# All results will be printed to the console. Figures will display in the
# R graphics window. No external folders or files are created.
#
# Required input files (all in the same folder as this script):
#   events_final.csv
#   indices_daily.csv
#   acwi_daily.csv
#   vix_daily.csv
#   msci_regional_daily.csv
#   sectoral_daily.csv
#
#
# Note on financial data:
# The proprietary stock market data used in the thesis cannot be redistributed.
# Users who reconstruct the financial CSV files from Refinitiv or another data
# provider should ensure that prices, currency denomination, trading calendars,
# and return calculations match the thesis definitions. Small numerical differences
# may arise if the financial data are downloaded at a different date or constructed
# using different adjustment, missing-value, or calendar conventions.
#
#
# Expected counts:
#   events_final.csv    : 267 events
#   Analytical sample   : 266 events (one dropped for insufficient coverage)
# =============================================================================

# =============================================================================
# PART 0: SETUP
# =============================================================================

rm(list = ls())
gc()

invisible(lapply(
  c("tidyverse", "lubridate", "ggplot2", "dplyr", "tidyr", "scales",
    "sandwich", "purrr", "tibble", "lmtest", "stringr", "car"),
  library,
  character.only = TRUE
))

# =============================================================================
# PART I: DATA LOADING
# =============================================================================

events_final    <- read_csv("events_final.csv",           show_col_types = FALSE)
indices         <- read_csv("indices_daily.csv",          show_col_types = FALSE)
acwi            <- read_csv("acwi_daily.csv",             show_col_types = FALSE)
vix_daily       <- read_csv("vix_daily.csv",              show_col_types = FALSE)
msci_regional   <- read_csv("msci_regional_daily.csv",    show_col_types = FALSE)
sectoral_raw    <- read_csv("sectoral_daily.csv",         show_col_types = FALSE)

# Parse dates
events_final  <- events_final  %>% mutate(date = ymd(date))
indices       <- indices       %>% mutate(date = ymd(date))
acwi          <- acwi          %>% mutate(date = ymd(date))
vix_daily     <- vix_daily     %>% mutate(date = ymd(date))
msci_regional <- msci_regional %>% mutate(date = ymd(date))
sectoral_raw  <- sectoral_raw  %>% mutate(date = ymd(date))

events_final <- events_final %>%
  distinct(country, date, description, .keep_all = TRUE)

# =============================================================================
# PART II: VALIDATE INPUTS
# =============================================================================

# ---- 2.1 Required columns ---------------------------------------------------

required_events <- c("country", "date", "event_type_agg", "disruption_type_agg",
                     "deaths", "total_injuries", "cancellation")
required_indices  <- c("date", "ticker", "close", "country", "ret")
required_acwi     <- c("date", "acwi_ret")
required_vix      <- c("date", "vix")
required_msci     <- c("date", "region", "msci_reg_ret")
required_sectoral <- c("date", "sector_group", "ticker", "close", "ret")

check_cols <- function(df, required, name) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste0(name, " is missing required columns: ",
                paste(missing, collapse = ", ")))
  }
}
check_cols(events_final,  required_events,  "events_final.csv")
check_cols(indices,        required_indices,  "indices_daily.csv")
check_cols(acwi,           required_acwi,     "acwi_daily.csv")
check_cols(vix_daily,      required_vix,      "vix_daily.csv")
check_cols(msci_regional,  required_msci,     "msci_regional_daily.csv")
check_cols(sectoral_raw,   required_sectoral, "sectoral_daily.csv")

# Create event_num if not already present
if (!("event_num" %in% names(events_final))) {
  events_final <- events_final %>% mutate(event_num = row_number())
}

# ---- 2.2 Country name consistency check ------------------------------------
# Country names in events_final.csv must match those in indices_daily.csv.
# If your CSV files use non-standard names, correct them directly in the CSV
# (e.g. USA -> United States, UK -> United Kingdom,
#  CzechRepublic -> Czech Republic, SouthKorea -> South Korea).

countries_events  <- sort(unique(events_final$country))
countries_indices <- sort(unique(indices$country))
unmatched <- setdiff(countries_events, countries_indices)

if (length(unmatched) > 0) {
  stop(paste0(
    "Country name mismatch: the following countries appear in events_final.csv ",
    "but not in indices_daily.csv:\n  ",
    paste(unmatched, collapse = "\n  "),
    "\nPlease standardise country names in the CSV files before running ",
    "this script (e.g. 'USA' -> 'United States', 'UK' -> 'United Kingdom', ",
    "'CzechRepublic' -> 'Czech Republic', 'SouthKorea' -> 'South Korea')."
  ))
}
message("OK: all event country names found in indices_daily.csv.")

# ---- 2.3 Event count check (267 expected) ----------------------------------

n_events_input <- nrow(events_final)
if (n_events_input != 267) {
  stop(paste0(
    "events_final.csv contains ", n_events_input, " rows; expected 267. ",
    "Check that you are using the correct input file."
  ))
}
message(paste0("OK: events_final.csv contains ", n_events_input, " events (expected 267)."))

# ---- 2.4 Analytical sample construction -----------------------------------
# The analytical sample excludes only events outside the available index coverage.
# Model-specific reductions due to missing estimation-window observations are
# handled later in the event-study pipeline.

index_ranges <- indices %>%
  filter(!is.na(ret)) %>%
  group_by(country) %>%
  summarise(
    index_start = min(date, na.rm = TRUE),
    index_end   = max(date, na.rm = TRUE),
    .groups = "drop"
  )

events_out_of_index_range <- events_final %>%
  left_join(index_ranges, by = "country") %>%
  filter(is.na(index_start) | date < index_start | date > index_end)

print(
  events_out_of_index_range %>%
    select(event_num, date, country, description, index_start, index_end),
  n = Inf
)

events_analytical <- events_final %>%
  anti_join(events_out_of_index_range %>% select(event_num), by = "event_num")

n_analytical <- n_distinct(events_analytical$event_num)

if (n_analytical != 266) {
  stop(paste0(
    "Analytical sample contains ", n_analytical, " events; expected 266. ",
    "Check index coverage and country names."
  ))
}

message(paste0("OK: analytical sample contains ", n_analytical, " events (expected 266)."))

# =============================================================================
# PART III: DESCRIPTIVE STATISTICS
# =============================================================================

event_types_summary <- events_analytical %>%
  count(event_type_agg, name = "n_events") %>%
  mutate(pct_events = n_events / sum(n_events)) %>%
  arrange(desc(n_events))

disruption_types_summary <- events_analytical %>%
  count(disruption_type_agg, name = "n_events") %>%
  mutate(pct_events = n_events / sum(n_events)) %>%
  arrange(desc(n_events))

sample_overview <- events_analytical %>%
  summarise(
    n_events          = n(),
    start_date        = min(date, na.rm = TRUE),
    end_date          = max(date, na.rm = TRUE),
    n_countries       = n_distinct(country),
    n_event_types     = n_distinct(event_type_agg),
    n_disruption_agg  = n_distinct(disruption_type_agg)
  )

events_by_country <- events_analytical %>%
  count(country, name = "n_events") %>%
  mutate(pct = n_events / sum(n_events)) %>%
  arrange(desc(n_events))

severity_summary <- tibble(
  variable     = c("deaths", "total_injuries", "cancellation"),
  n_total      = nrow(events_analytical),
  n_non_missing = c(
    sum(!is.na(events_analytical$deaths)),
    sum(!is.na(events_analytical$total_injuries)),
    sum(!is.na(events_analytical$cancellation))
  ),
  pct_missing   = c(
    mean(is.na(events_analytical$deaths)),
    mean(is.na(events_analytical$total_injuries)),
    mean(is.na(events_analytical$cancellation))
  ),
  mean  = c(
    mean(events_analytical$deaths,        na.rm = TRUE),
    mean(events_analytical$total_injuries, na.rm = TRUE),
    mean(events_analytical$cancellation,   na.rm = TRUE)
  ),
  median = c(
    median(events_analytical$deaths,        na.rm = TRUE),
    median(events_analytical$total_injuries, na.rm = TRUE),
    median(events_analytical$cancellation,   na.rm = TRUE)
  ),
  sd    = c(
    sd(events_analytical$deaths,        na.rm = TRUE),
    sd(events_analytical$total_injuries, na.rm = TRUE),
    sd(events_analytical$cancellation,   na.rm = TRUE)
  ),
  max   = c(
    max(events_analytical$deaths,        na.rm = TRUE),
    max(events_analytical$total_injuries, na.rm = TRUE),
    max(events_analytical$cancellation,   na.rm = TRUE)
  )
)

# =============================================================================
# PART IV: EVENT STUDY PIPELINE
# =============================================================================

# ---- 4.1 Compute dvix (if not already in vix_daily) ------------------------

if (!("dvix" %in% names(vix_daily))) {
  vix_daily <- vix_daily %>%
    arrange(date) %>%
    mutate(dvix = log(vix) - dplyr::lag(log(vix)))
}

# ---- 4.2 Country-to-region mapping (for extended market model) -------------

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
  "Portugal",          "Europe",
  "South Korea",       "Emerging Markets",
  "Spain",             "Europe",
  "Sweden",            "Europe",
  "Switzerland",       "Europe",
  "Thailand",          "Emerging Markets",
  "Turkey",            "Emerging Markets",
  "United Kingdom",    "Europe",
  "United States",     "North America"
)

# Thesis region map used in cross-sectional regressions
region_thesis_map <- tibble::tibble(
  country = c(
    "United States", "Canada", "Mexico",
    "United Kingdom", "Austria", "Germany", "France", "Belgium", "Spain",
    "Italy", "Czech Republic", "Greece", "Netherlands", "Switzerland",
    "Ireland", "Norway", "Sweden", "Turkey", "Denmark", "Poland",
    "Finland", "Hungary", "Portugal",
    "Japan", "Australia", "Thailand", "India", "South Korea", "Philippines",
    "Brazil", "Argentina"
  ),
  region_thesis = c(
    rep("North America", 3),
    rep("Europe", 20),
    rep("Asia-Pacific", 6),
    rep("South America", 2)
  )
)

# ---- 4.3 Deduplicate index data (take first return per country-date) --------

indices_base_uniq <- indices %>%
  arrange(country, date) %>%
  group_by(country, date) %>%
  summarise(ret = dplyr::first(ret), .groups = "drop")

# ---- 4.4 Trading calendar (country-level) -----------------------------------

trading_calendar <- indices %>%
  filter(!is.na(ret)) %>%
  distinct(country, date) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(t = row_number()) %>%
  ungroup()

# ---- 4.5 Index date ranges (used later in placebo test) --------------------

index_ranges <- indices %>%
  group_by(country) %>%
  summarise(
    index_start = min(date, na.rm = TRUE),
    index_end   = max(date, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 4.6 Event date assignment helpers -------------------------------------

# Baseline: first trading day STRICTLY AFTER the event date
# Alternative: first trading day ON OR AFTER the event date
pick_event_date <- function(ctry, d, mode = c("next", "same_or_next")) {
  mode <- match.arg(mode)
  cal  <- trading_calendar %>% filter(country == ctry)
  cand <- if (mode == "next") {
    cal %>% filter(date > d) %>% slice_head(n = 1)
  } else {
    cal %>% filter(date >= d) %>% slice_head(n = 1)
  }
  if (nrow(cand) == 0) return(tibble(event_date = as.Date(NA), t0 = NA_integer_))
  tibble(event_date = cand$date[1], t0 = cand$t[1])
}

# Baseline (next trading day)
events_w_event_date_next <- events_analytical %>%
  mutate(tmp = purrr::map2(country, date, ~ pick_event_date(.x, .y, "next"))) %>%
  tidyr::unnest(tmp)

# Alternative (same or next)
events_w_event_date_same_or_next <- events_analytical %>%
  mutate(tmp = purrr::map2(country, date, ~ pick_event_date(.x, .y, "same_or_next"))) %>%
  tidyr::unnest(tmp)

# ---- 4.7 Build tau panels --------------------------------------------------

tau_min <- -190
tau_max <-   10

make_event_tau_panel <- function(events_tbl, tau_min, tau_max) {
  events_tbl %>%
    filter(!is.na(t0)) %>%
    select(event_num, country, date, event_date, t0) %>%
    tidyr::crossing(tau = tau_min:tau_max) %>%
    mutate(t = t0 + tau) %>%
    left_join(
      trading_calendar %>% select(country, t, date_tau = date),
      by = c("country", "t")
    )
}

event_tau_panel_all_next         <- make_event_tau_panel(events_w_event_date_next,         tau_min, tau_max)
event_tau_panel_all_same_or_next <- make_event_tau_panel(events_w_event_date_same_or_next, tau_min, tau_max)

# ---- 4.8 Keep analytical sample; model-specific filters are applied later ----
# The analytical sample excludes only events outside index coverage.
# Events with insufficient estimation-window observations are handled later
# as model-specific sample reductions.

events_after_calendar_next <- events_w_event_date_next
events_after_calendar_same_or_next <- events_w_event_date_same_or_next

event_tau_panel_after_calendar_next <- event_tau_panel_all_next
event_tau_panel_after_calendar_same_or_next <- event_tau_panel_all_same_or_next


# ---- 4.9 Add returns to tau panels -----------------------------------------

add_returns <- function(panel) {
  panel %>%
    { if ("region" %in% names(.)) dplyr::select(., -region) else . } %>%
    left_join(indices_base_uniq,
              by = c("country" = "country", "date_tau" = "date")) %>%
    left_join(acwi %>% select(date, acwi_ret),
              by = c("date_tau" = "date")) %>%
    left_join(vix_daily %>% select(date, vix, dvix),
              by = c("date_tau" = "date")) %>%
    left_join(country_region_map, by = "country") %>%
    left_join(msci_regional %>% select(date, region, msci_reg_ret),
              by = c("date_tau" = "date", "region" = "region"),
              relationship = "many-to-one")
}

event_tau_panel_after_calendar_next         <- add_returns(event_tau_panel_after_calendar_next)
event_tau_panel_after_calendar_same_or_next <- add_returns(event_tau_panel_after_calendar_same_or_next)

# ---- 4.10 Estimation-window data-quality counts ----------------------------

estimation_counts_mm <- function(panel) {
  panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    mutate(valid_mm = !is.na(ret) & !is.na(acwi_ret)) %>%
    group_by(event_num) %>%
    summarise(n_rows = n(), n_valid = sum(valid_mm), .groups = "drop")
}

estimation_counts_ext <- function(panel, x_vars = c("acwi_ret", "dvix", "msci_reg_ret")) {
  panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    mutate(valid_ext = is.finite(ret) & if_all(all_of(x_vars), ~ is.finite(.x))) %>%
    group_by(event_num) %>%
    summarise(n_rows = n(), n_valid = sum(valid_ext), .groups = "drop")
}

required_n      <- length(-190:-10)   # 181 obs
threshold_strict <- required_n        # 100% complete estimation window

# Next — base and extended
counts_mm_next  <- estimation_counts_mm(event_tau_panel_after_calendar_next)
counts_ext_next <- estimation_counts_ext(event_tau_panel_after_calendar_next)

# Same-or-next — base and extended
counts_mm_same  <- estimation_counts_mm(event_tau_panel_after_calendar_same_or_next)
counts_ext_same <- estimation_counts_ext(event_tau_panel_after_calendar_same_or_next)

# ---- 4.11 Build strict-threshold samples -----------------------------------

make_mm_sample <- function(events_tbl, panel_tbl, counts_tbl, threshold) {
  drop_ids  <- counts_tbl %>% filter(n_valid < threshold) %>% pull(event_num)
  events_k  <- events_tbl %>% filter(!(event_num %in% drop_ids))
  panel_k   <- panel_tbl  %>% filter(!(event_num %in% drop_ids))
  list(drop_ids = drop_ids, events = events_k, panel = panel_k)
}

strict_next     <- make_mm_sample(events_after_calendar_next, event_tau_panel_after_calendar_next, counts_mm_next,  threshold_strict)
strict_same     <- make_mm_sample(events_after_calendar_same_or_next, event_tau_panel_after_calendar_same_or_next, counts_mm_same,  threshold_strict)
strict_next_ext <- make_mm_sample(events_after_calendar_next, event_tau_panel_after_calendar_next, counts_ext_next, threshold_strict)
strict_same_ext <- make_mm_sample(events_after_calendar_same_or_next, event_tau_panel_after_calendar_same_or_next, counts_ext_same, threshold_strict)

# ---- 4.12 Market model functions -------------------------------------------

# Simple market model: ret ~ acwi_ret
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
    mutate(exp_ret = alpha + beta * acwi_ret, ar = ret - exp_ret)
}

# Extended market model: ret ~ acwi_ret + dvix + msci_reg_ret (or custom x_vars)
run_mm_extended <- function(panel, x_vars = c("acwi_ret", "dvix", "msci_reg_ret"),
                            ar_name = "ar_ext") {
  stopifnot(all(c("event_num", "tau", "ret") %in% names(panel)))
  stopifnot(all(x_vars %in% names(panel)))
  
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(is.finite(ret)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  
  fml <- as.formula(paste("ret ~", paste(x_vars, collapse = " + ")))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(fit = list(lm(fml, data = pick(everything()))), .groups = "drop") %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  
  panel2 <- panel %>% left_join(params, by = "event_num")
  
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
  
  panel2 %>% mutate(exp_ret_ext = exp_ret, !!ar_name := ret - exp_ret_ext)
}

# ---- 4.13 Overlap-exclusion rule -------------------------------------------

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
    inner_join(overlap_events, by = c("country", "date_tau"),
               relationship = "many-to-many") %>%
    filter(event_num.x != event_num.y) %>%
    transmute(country,
              e1 = pmin(event_num.x, event_num.y),
              e2 = pmax(event_num.x, event_num.y)) %>%
    distinct()
  
  events_to_drop <- c()
  for (ctry in unique(edges$country)) {
    edges_c  <- edges %>% filter(country == ctry)
    adj      <- split(edges_c$e2, edges_c$e1)
    adj_rev  <- split(edges_c$e1, edges_c$e2)
    neighbors <- function(x) unique(c(adj[[as.character(x)]], adj_rev[[as.character(x)]]))
    visited  <- c()
    comps    <- list()
    nodes    <- unique(c(edges_c$e1, edges_c$e2))
    for (n in nodes) {
      if (n %in% visited) next
      stack <- c(n); comp <- c()
      while (length(stack) > 0) {
        v <- stack[1]; stack <- stack[-1]
        if (v %in% visited) next
        visited <- c(visited, v); comp <- c(comp, v)
        nb <- neighbors(v)
        stack <- c(stack, nb[!nb %in% visited])
      }
      comps[[length(comps) + 1]] <- comp
    }
    for (comp in comps) {
      comp_df <- events_tbl %>%
        filter(country == ctry, event_num %in% comp) %>%
        arrange(event_date, event_num)
      if (nrow(comp_df) > 1)
        events_to_drop <- c(events_to_drop, comp_df$event_num[-1])
    }
  }
  unique(events_to_drop)
}

# ---- 4.14 CAR computation (event windows [0, k]) ---------------------------

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
    sample_k_final  <- sample_k %>% filter(!(event_num %in% drops_overlap_k))
    
    sample_sizes <- bind_rows(
      sample_sizes,
      tibble(k = k, n = nrow(sample_k_final),
             n_dropped_overlap = length(intersect(sample_k$event_num, drops_overlap_k)))
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

# ---- 4.15 Run baseline panels ----------------------------------------------

# Simple market model — NEXT and SAME_OR_NEXT
panel_mm_next_strict <- run_mm(strict_next$panel)
panel_mm_same_strict <- run_mm(strict_same$panel)

# Extended market model — NEXT and SAME_OR_NEXT
panel_mm_next_strict_ext <- run_mm_extended(
  strict_next_ext$panel,
  x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_ext"
)
panel_mm_same_strict_ext <- run_mm_extended(
  strict_same_ext$panel,
  x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_ext"
)

# ---- 4.16 Compute CARs -----------------------------------------------------

cars_next_strict     <- compute_cars(panel_mm_next_strict,     strict_next$events,     ar_col = "ar")
cars_same_strict     <- compute_cars(panel_mm_same_strict,     strict_same$events,     ar_col = "ar")
cars_next_strict_ext <- compute_cars(panel_mm_next_strict_ext, strict_next_ext$events, ar_col = "ar_ext")
cars_same_strict_ext <- compute_cars(panel_mm_same_strict_ext, strict_same_ext$events, ar_col = "ar_ext")




cat("\nEvents dropped from NEXT simple estimation sample:\n")
events_analytical %>%
  select(event_num, date, country, description) %>%
  filter(event_num %in% strict_next$drop_ids) %>%
  arrange(date) %>%
  print(n = Inf)

cat("\nEvents included in NEXT simple estimation sample but close to the threshold:\n")
counts_mm_next %>%
  filter(n_valid <= 185) %>%
  left_join(
    events_analytical %>% select(event_num, date, country, description),
    by = "event_num"
  ) %>%
  arrange(n_valid, date) %>%
  select(event_num, date, country, description, n_rows, n_valid) %>%
  print(n = Inf)
# ---- 4.17 Mean-CAR summary helper ------------------------------------------

compare_mean_car <- function(car_obj, version_name) {
  car_obj$car_event_level %>%
    pivot_longer(cols = starts_with("car_"), names_to = "window", values_to = "car") %>%
    filter(is.finite(car)) %>%
    group_by(window) %>%
    summarise(
      version = version_name,
      n       = n(),
      mean_car = mean(car),
      sd_car   = sd(car),
      t_stat   = mean_car / (sd_car / sqrt(n)),
      p_value  = 2 * pt(-abs(t_stat), df = n - 1),
      .groups = "drop"
    )
}

results_next_base <- compare_mean_car(cars_next_strict,     "NEXT_base")
results_next_ext  <- compare_mean_car(cars_next_strict_ext, "NEXT_ext")
results_same_base <- compare_mean_car(cars_same_strict,     "SAME_base")
results_same_ext  <- compare_mean_car(cars_same_strict_ext, "SAME_ext")

# =============================================================================
# PART V: CROSS-SECTIONAL ANALYSIS
# =============================================================================

k_keep <- c(0, 1, 2, 5, 10)

rhs <- "event_type_agg + disruption_type_agg + log_deaths + log_injuries + cancellation + region"
fml <- as.formula(paste0("car ~ ", rhs))

make_cs_data <- function(events_tbl, car_obj, k) {
  car_col <- paste0("car_", sprintf("%02d", k))
  car_obj$car_event_level %>%
    select(event_num, country, all_of(car_col)) %>%
    rename(car = all_of(car_col)) %>%
    left_join(events_tbl, by = c("event_num", "country")) %>%
    left_join(region_thesis_map, by = "country") %>%
    filter(is.finite(car)) %>%
    mutate(
      car          = as.numeric(car),
      country      = as.factor(country),
      log_deaths   = log(deaths + 1),
      log_injuries = log(total_injuries + 1),
      region       = relevel(as.factor(region_thesis), ref = "North America"),
      event_type_agg    = as.factor(event_type_agg),
      disruption_type_agg = as.factor(disruption_type_agg)
    )
}

coefs_hc1 <- function(fit) {
  V  <- sandwich::vcovHC(fit, type = "HC1")
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble(term = rownames(ct), estimate = ct[,1], se = ct[,2], t = ct[,3], p = ct[,4],
         se_type = "HC1")
}

coefs_cluster <- function(fit, cluster_vec) {
  V  <- sandwich::vcovCL(fit, cluster = cluster_vec)
  ct <- lmtest::coeftest(fit, vcov. = V)
  tibble(term = rownames(ct), estimate = ct[,1], se = ct[,2], t = ct[,3], p = ct[,4],
         se_type = "CL_country")
}

f_test_cluster <- function(fit, cluster_vec) {
  V          <- sandwich::vcovCL(fit, cluster = cluster_vec)
  coef_names <- setdiff(names(coef(fit)), "(Intercept)")
  if (length(coef_names) == 0)
    return(tibble(F_stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_))
  lh <- car::linearHypothesis(fit, coef_names, vcov. = V, test = "F")
  tibble(F_stat = lh$F[2], df1 = lh$Df[2], df2 = lh$Res.Df[2], p_value = lh$`Pr(>F)`[2])
}

joint_test_block <- function(fit, cluster_vec, vars) {
  V    <- sandwich::vcovCL(fit, cluster = cluster_vec)
  vars <- intersect(vars, names(coef(fit)))
  if (length(vars) == 0)
    return(tibble(F_stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_))
  lh <- car::linearHypothesis(fit, vars, vcov. = V, test = "F")
  tibble(F_stat = lh$F[2], df1 = lh$Df[2], df2 = lh$Res.Df[2], p_value = lh$`Pr(>F)`[2])
}

run_model_k <- function(events_tbl, car_obj, model_name, k) {
  df      <- make_cs_data(events_tbl, car_obj, k)
  df_fit  <- tidyr::drop_na(df, dplyr::all_of(all.vars(fml)))
  G       <- n_distinct(df_fit$country)
  if (G < 2) stop("Less than 2 clusters for model = ", model_name, ", k = ", k)
  fit     <- lm(fml, data = df_fit)
  n_obs   <- nobs(fit)
  
  coefs <- bind_rows(
    coefs_hc1(fit)                       %>% mutate(G = NA_integer_),
    coefs_cluster(fit, df_fit$country)   %>% mutate(G = G)
  ) %>% mutate(model = model_name, k = k, n = n_obs)
  
  f_tests <- f_test_cluster(fit, df_fit$country) %>%
    mutate(model = model_name, k = k, n = n_obs, G = G)
  
  vars_event_type <- grep("^event_type_agg",    names(coef(fit)), value = TRUE)
  vars_disruption <- grep("^disruption_type_agg", names(coef(fit)), value = TRUE)
  vars_severity   <- intersect(names(coef(fit)), c("log_deaths", "log_injuries", "cancellation"))
  vars_region     <- grep("^region",             names(coef(fit)), value = TRUE)
  
  f_blocks <- bind_rows(
    joint_test_block(fit, df_fit$country, vars_event_type) %>% mutate(block = "event_type"),
    joint_test_block(fit, df_fit$country, vars_disruption) %>% mutate(block = "disruption_type"),
    joint_test_block(fit, df_fit$country, vars_severity)   %>% mutate(block = "severity"),
    joint_test_block(fit, df_fit$country, vars_region)     %>% mutate(block = "region")
  ) %>% mutate(model = model_name, k = k, n = n_obs, G = G)
  
  list(coefs = coefs, f_tests = f_tests, f_blocks = f_blocks)
}

run_cs_suite <- function(events_tbl, car_obj, label) {
  out <- purrr::map(k_keep, ~ run_model_k(events_tbl, car_obj, label, .x))
  list(
    coefs    = purrr::map_dfr(out, "coefs"),
    f_tests  = purrr::map_dfr(out, "f_tests"),
    f_blocks = purrr::map_dfr(out, "f_blocks")
  )
}

# Events deduplicated by event_num + country for CS input
events_next_strict <- strict_next$events %>% distinct(event_num, country, .keep_all = TRUE)
events_same_strict <- strict_same$events %>% distinct(event_num, country, .keep_all = TRUE)

cs_next_base <- run_cs_suite(events_next_strict, cars_next_strict,     "NEXT_base")
cs_next_ext  <- run_cs_suite(events_next_strict, cars_next_strict_ext, "NEXT_ext")
cs_same_base <- run_cs_suite(events_same_strict, cars_same_strict,     "SAME_base")
cs_same_ext  <- run_cs_suite(events_same_strict, cars_same_strict_ext, "SAME_ext")

coefs_all_hc1_vs_cluster <- bind_rows(
  cs_next_base$coefs, cs_next_ext$coefs,
  cs_same_base$coefs, cs_same_ext$coefs
)

# =============================================================================
# PART VI: SECTORAL ANALYSIS
#   USA: S&P 500 Consumer Discretionary (SPLRCD)
#   Europe: STOXX Europe 600 Travel & Leisure (SXTP)
# =============================================================================

# ---- 6.1 Prepare sectoral returns ------------------------------------------

sectoral <- sectoral_raw %>%
  group_by(sector_group, ticker, date) %>%
  summarise(
    close = { x <- unique(close[!is.na(close)]); if (length(x) == 1) x else NA_real_ },
    ret   = { x <- unique(ret[!is.na(ret)]);     if (length(x) == 1) x else NA_real_ },
    .groups = "drop"
  ) %>%
  filter(
    (sector_group == "USA"    & ticker == "SPLRCD") |
      (sector_group == "Europe" & ticker == "SXTP")
  )

sector_calendar <- sectoral %>%
  filter(is.finite(ret)) %>%
  distinct(sector_group, date) %>%
  arrange(sector_group, date) %>%
  group_by(sector_group) %>%
  mutate(t = row_number()) %>%
  ungroup()

sector_returns_daily <- sectoral %>% select(sector_group, date, ret_sector = ret)

# ---- 6.2 Event assignment for sectoral calendar ----------------------------

events_sector <- events_final %>%
  mutate(sector_group = case_when(
    country %in% c("Austria","Belgium","Denmark","Finland","France","Germany",
                   "Ireland","Italy","Netherlands","Norway","Spain","Sweden",
                   "Switzerland","Portugal","United Kingdom") ~ "Europe",
    country == "United States" ~ "USA",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(sector_group))

pick_event_date_sector <- function(sgrp, d, mode = c("next", "same_or_next")) {
  mode <- match.arg(mode)
  cal  <- sector_calendar %>% filter(sector_group == sgrp)
  cand <- if (mode == "next") cal %>% filter(date > d)  %>% slice_head(n = 1) else
    cal %>% filter(date >= d) %>% slice_head(n = 1)
  if (nrow(cand) == 0) return(tibble(event_date = as.Date(NA), t0 = NA_integer_))
  tibble(event_date = cand$date[1], t0 = cand$t[1])
}

events_sector_next <- events_sector %>%
  mutate(tmp = purrr::map2(sector_group, date, ~ pick_event_date_sector(.x, .y, "next"))) %>%
  tidyr::unnest(tmp)

# ---- 6.3 Build sector tau panel --------------------------------------------

make_sector_tau_panel <- function(events_tbl, tau_min, tau_max) {
  events_tbl %>%
    filter(!is.na(t0)) %>%
    select(event_num, country, sector_group, date, event_date, t0) %>%
    tidyr::crossing(tau = tau_min:tau_max) %>%
    mutate(t = t0 + tau) %>%
    left_join(sector_calendar %>% select(sector_group, t, date_tau = date),
              by = c("sector_group", "t"))
}

sector_tau_panel_all <- make_sector_tau_panel(events_sector_next, tau_min, tau_max) %>%
  left_join(sector_returns_daily, by = c("sector_group", "date_tau" = "date")) %>%
  left_join(acwi %>% select(date, acwi_ret), by = c("date_tau" = "date")) %>%
  left_join(vix_daily %>% select(date, dvix), by = c("date_tau" = "date")) %>%
  left_join(country_region_map, by = "country") %>%
  left_join(msci_regional %>% select(date, region, msci_reg_ret),
            by = c("date_tau" = "date", "region" = "region"),
            relationship = "many-to-one")

# ---- 6.4 Strict samples for sectoral analysis ------------------------------

est_counts_sector_mm <- sector_tau_panel_all %>%
  filter(tau >= -190 & tau <= -10) %>%
  mutate(valid = is.finite(ret_sector) & is.finite(acwi_ret)) %>%
  group_by(event_num) %>%
  summarise(n_valid = sum(valid), .groups = "drop")

est_counts_sector_ext <- sector_tau_panel_all %>%
  filter(tau >= -190 & tau <= -10) %>%
  mutate(valid = is.finite(ret_sector) & is.finite(acwi_ret) &
           is.finite(dvix) & is.finite(msci_reg_ret)) %>%
  group_by(event_num) %>%
  summarise(n_valid = sum(valid), .groups = "drop")

drop_sector_mm  <- est_counts_sector_mm  %>% filter(n_valid < threshold_strict) %>% pull(event_num)
drop_sector_ext <- est_counts_sector_ext %>% filter(n_valid < threshold_strict) %>% pull(event_num)

events_sector_strict     <- events_sector_next %>% filter(!(event_num %in% drop_sector_mm))  %>% distinct(event_num, country, .keep_all = TRUE)
panel_sector_strict      <- sector_tau_panel_all %>% filter(!(event_num %in% drop_sector_mm))

events_sector_ext_strict <- events_sector_next %>% filter(!(event_num %in% drop_sector_ext)) %>% distinct(event_num, country, .keep_all = TRUE)
panel_sector_ext_strict  <- sector_tau_panel_all %>% filter(!(event_num %in% drop_sector_ext))

# ---- 6.5 Sectoral market model functions -----------------------------------

run_mm_sector <- function(panel) {
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10) %>%
    filter(is.finite(ret_sector), is.finite(acwi_ret))
  
  safe_lm <- function(d) {
    d <- d %>% select(ret_sector, acwi_ret) %>% drop_na()
    if (nrow(d) < 30 || sd(d$acwi_ret) == 0)
      return(c(alpha = NA_real_, beta = NA_real_))
    fit <- lm(ret_sector ~ acwi_ret, data = d)
    c(alpha = unname(coef(fit)[1]), beta = unname(coef(fit)[2]))
  }
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(tmp = list(safe_lm(pick(ret_sector, acwi_ret))), .groups = "drop") %>%
    mutate(alpha = purrr::map_dbl(tmp, 1), beta = purrr::map_dbl(tmp, 2)) %>%
    select(event_num, alpha, beta)
  
  panel %>%
    left_join(params, by = "event_num") %>%
    mutate(exp_ret = alpha + beta * acwi_ret, ar_sector = ret_sector - exp_ret)
}

run_mm_sector_extended <- function(panel,
                                   x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
                                   ar_name = "ar_sector_ext") {
  estimation <- panel %>%
    filter(tau >= -190 & tau <= -10, is.finite(ret_sector)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  
  fml_local <- as.formula(paste("ret_sector ~", paste(x_vars, collapse = " + ")))
  
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(fit = list(lm(fml_local, data = pick(everything()))), .groups = "drop") %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  
  panel2 <- panel %>% left_join(params, by = "event_num")
  
  exp_ret <- purrr::map2_dbl(panel2$coefs, seq_len(nrow(panel2)), ~ {
    b <- .x; if (is.null(b)) return(NA_real_)
    xb <- b[["(Intercept)"]]
    for (v in x_vars) {
      bv <- unname(b[[v]]); xv <- panel2[[v]][.y]
      if (!is.finite(bv) || !is.finite(xv)) return(NA_real_)
      xb <- xb + bv * xv
    }
    xb
  })
  
  panel2 %>% mutate(exp_ret_sector_ext = exp_ret, !!ar_name := ret_sector - exp_ret_sector_ext)
}

# ---- 6.6 USA sectoral analysis (extended model) ----------------------------

events_sector_usa_ext_strict <- events_sector_ext_strict %>%
  filter(sector_group == "USA")
panel_sector_usa_ext_strict  <- panel_sector_ext_strict  %>%
  filter(sector_group == "USA")

panel_sector_usa_strict_ext <- run_mm_sector_extended(
  panel_sector_usa_ext_strict,
  x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)
cars_sector_usa_strict_ext <- compute_cars(
  panel_sector_usa_strict_ext, events_sector_usa_ext_strict, ar_col = "ar_sector_ext"
)
results_sector_usa_ext <- compare_mean_car(cars_sector_usa_strict_ext, "SECTOR_USA_ext")

# ---- 6.7 Europe sectoral analysis (extended model) -------------------------

events_sector_europe_ext_strict <- events_sector_ext_strict %>%
  filter(sector_group == "Europe")
panel_sector_europe_ext_strict  <- panel_sector_ext_strict  %>%
  filter(sector_group == "Europe")

panel_sector_europe_strict_ext <- run_mm_sector_extended(
  panel_sector_europe_ext_strict,
  x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)
cars_sector_europe_strict_ext <- compute_cars(
  panel_sector_europe_strict_ext, events_sector_europe_ext_strict, ar_col = "ar_sector_ext"
)
results_sector_europe_ext <- compare_mean_car(cars_sector_europe_strict_ext, "SECTOR_EUROPE_ext")

# ---- 6.8 Combined sectoral CS (for F-tests, Table F.2) ---------------------

panel_sector_strict_mm  <- run_mm_sector(panel_sector_strict)
panel_sector_strict_ext <- run_mm_sector_extended(
  panel_sector_ext_strict,
  x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
  ar_name = "ar_sector_ext"
)
cars_sector_strict     <- compute_cars(panel_sector_strict_mm,  events_sector_strict,     ar_col = "ar_sector")
cars_sector_strict_ext <- compute_cars(panel_sector_strict_ext, events_sector_ext_strict, ar_col = "ar_sector_ext")

cs_sector_base <- run_cs_suite(events_sector_strict,     cars_sector_strict,     "SECTOR_base")
cs_sector_ext  <- run_cs_suite(events_sector_ext_strict, cars_sector_strict_ext, "SECTOR_ext")

# =============================================================================
# PART VII: ROBUSTNESS CHECKS
# =============================================================================

# ---- 7.1 Placebo test -------------------------------------------------------

season_from_date <- function(x) {
  m <- lubridate::month(x)
  dplyr::case_when(
    m %in% c(12, 1, 2) ~ "winter",
    m %in% c(3, 4, 5)  ~ "spring",
    m %in% c(6, 7, 8)  ~ "summer",
    m %in% c(9, 10, 11) ~ "autumn",
    TRUE ~ NA_character_
  )
}

# Candidate calendar dates for placebo (all calendar dates within index range)
candidate_calendar_dates <- index_ranges %>%
  rowwise() %>%
  mutate(date_seq = list(seq.Date(index_start, index_end, by = "day"))) %>%
  tidyr::unnest(date_seq) %>%
  ungroup() %>%
  transmute(
    country,
    candidate_date   = date_seq,
    candidate_year   = lubridate::year(candidate_date),
    candidate_wday   = lubridate::wday(candidate_date, week_start = 1),
    candidate_season = season_from_date(candidate_date)
  )

real_event_dates_by_country <- events_final %>%
  select(country, real_event_date = date)

pick_placebo_date <- function(ctry, d, exclusion_days = 10) {
  target_year   <- lubridate::year(d)
  target_wday   <- lubridate::wday(d, week_start = 1)
  target_season <- season_from_date(d)
  
  candidates <- candidate_calendar_dates %>%
    filter(country == ctry,
           candidate_wday   == target_wday,
           candidate_season == target_season)
  
  country_events <- real_event_dates_by_country %>%
    filter(country == ctry) %>% pull(real_event_date)
  
  candidates <- candidates %>%
    mutate(min_dist_to_real_event = purrr::map_dbl(
      candidate_date,
      ~ min(abs(as.numeric(.x - country_events)), na.rm = TRUE)
    )) %>%
    filter(candidate_date != d,
           is.finite(min_dist_to_real_event),
           min_dist_to_real_event > exclusion_days)
  
  candidates_same_year <- candidates %>% filter(candidate_year == target_year)
  if (nrow(candidates_same_year) > 0) return(sample(candidates_same_year$candidate_date, 1))
  if (nrow(candidates)           > 0) return(sample(candidates$candidate_date, 1))
  as.Date(NA)
}

run_placebo_next_base <- function(seed_value) {
  set.seed(seed_value)
  
  events_next_placebo <- events_next_strict %>%
    mutate(placebo_date = as.Date(
      purrr::map2_chr(country, date, ~ as.character(pick_placebo_date(.x, .y)))
    ))
  
  events_w_event_date_placebo_next <- events_next_placebo %>%
    filter(!is.na(placebo_date)) %>%
    select(-event_date, -t0) %>%
    mutate(tmp = purrr::map2(country, placebo_date, ~ pick_event_date(.x, .y, "next"))) %>%
    tidyr::unnest(tmp)
  
  event_tau_panel_placebo_next_all <- make_event_tau_panel(
    events_w_event_date_placebo_next, tau_min, tau_max
  )
  
  coverage_placebo <- event_tau_panel_placebo_next_all %>%
    group_by(event_num, country) %>%
    summarise(
      n_est_missing = sum((tau >= -190 & tau <= -10) & is.na(date_tau)),
      est_complete  = (n_est_missing == 0),
      .groups = "drop"
    )
  
  drop_calendar_placebo <- coverage_placebo %>%
    filter(!est_complete) %>%
    pull(event_num)
  
  events_after_calendar_placebo <- events_w_event_date_placebo_next %>%
    filter(!(event_num %in% drop_calendar_placebo))
  
  event_tau_panel_after_calendar_placebo <- event_tau_panel_placebo_next_all %>%
    filter(event_num %in% events_after_calendar_placebo$event_num) %>%
    add_returns()
  
  counts_mm_placebo <- estimation_counts_mm(event_tau_panel_after_calendar_placebo)
  
  strict_placebo_next <- make_mm_sample(
    events_after_calendar_placebo,
    event_tau_panel_after_calendar_placebo,
    counts_mm_placebo,
    threshold_strict
  )
  
  panel_mm_placebo_next_strict <- run_mm(strict_placebo_next$panel)
  
  cars_placebo_next_strict <- compute_cars(
    panel_mm_placebo_next_strict,
    strict_placebo_next$events,
    ar_col = "ar"
  )
  
  results_placebo_next_base <- compare_mean_car(
    cars_placebo_next_strict,
    paste0("PLACEBO_NEXT_base_", seed_value)
  )
  
  cs_placebo_next_base <- run_cs_suite(
    strict_placebo_next$events %>% distinct(event_num, country, .keep_all = TRUE),
    cars_placebo_next_strict,
    paste0("PLACEBO_NEXT_base_", seed_value)
  )
  
  list(
    seed = seed_value,
    results = results_placebo_next_base,
    cs = cs_placebo_next_base
  )
}

placebo_646 <- run_placebo_next_base(646)
placebo_8566 <- run_placebo_next_base(8566)

results_placebo_next_base <- bind_rows(
  placebo_646$results,
  placebo_8566$results
)

cs_placebo_next_base <- list(
  coefs = bind_rows(placebo_646$cs$coefs, placebo_8566$cs$coefs),
  f_tests = bind_rows(placebo_646$cs$f_tests, placebo_8566$cs$f_tests),
  f_blocks = bind_rows(placebo_646$cs$f_blocks, placebo_8566$cs$f_blocks)
)
# ---- 7.2 Estimation window sensitivity (Table 6.2) -------------------------

run_mm_window <- function(panel, est_start, est_end) {
  estimation <- panel %>%
    filter(tau >= est_start & tau <= est_end, !is.na(ret), !is.na(acwi_ret))
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(fit = list(lm(ret ~ acwi_ret, data = pick(everything()))), .groups = "drop") %>%
    mutate(alpha = purrr::map_dbl(fit, ~ unname(coef(.x)[1])),
           beta  = purrr::map_dbl(fit, ~ unname(coef(.x)[2]))) %>%
    select(event_num, alpha, beta)
  panel %>%
    left_join(params, by = "event_num") %>%
    mutate(exp_ret = alpha + beta * acwi_ret, ar = ret - exp_ret)
}

run_mm_extended_window <- function(panel, est_start, est_end,
                                   x_vars  = c("acwi_ret", "dvix", "msci_reg_ret"),
                                   ar_name = "ar_ext") {
  estimation <- panel %>%
    filter(tau >= est_start & tau <= est_end, is.finite(ret)) %>%
    filter(if_all(all_of(x_vars), ~ is.finite(.x)))
  fml_local <- as.formula(paste("ret ~", paste(x_vars, collapse = " + ")))
  params <- estimation %>%
    group_by(event_num) %>%
    summarise(fit = list(lm(fml_local, data = pick(everything()))), .groups = "drop") %>%
    mutate(coefs = purrr::map(fit, coef)) %>%
    select(event_num, coefs)
  panel2 <- panel %>% left_join(params, by = "event_num")
  exp_ret <- purrr::map2_dbl(panel2$coefs, seq_len(nrow(panel2)), ~ {
    b <- .x; if (is.null(b)) return(NA_real_)
    xb <- b[["(Intercept)"]]
    for (v in x_vars) {
      bv <- unname(b[[v]]); xv <- panel2[[v]][.y]
      if (!is.finite(bv) || !is.finite(xv)) return(NA_real_)
      xb <- xb + bv * xv
    }
    xb
  })
  panel2 %>% mutate(exp_ret_ext = exp_ret, !!ar_name := ret - exp_ret_ext)
}

estimation_counts_mm_window <- function(panel, est_start, est_end) {
  panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    mutate(valid_mm = !is.na(ret) & !is.na(acwi_ret)) %>%
    group_by(event_num) %>%
    summarise(n_rows = n(), n_valid = sum(valid_mm), .groups = "drop")
}

estimation_counts_ext_window <- function(panel, est_start, est_end,
                                         x_vars = c("acwi_ret", "dvix", "msci_reg_ret")) {
  panel %>%
    filter(tau >= est_start & tau <= est_end) %>%
    mutate(valid_ext = is.finite(ret) & if_all(all_of(x_vars), ~ is.finite(.x))) %>%
    group_by(event_num) %>%
    summarise(n_rows = n(), n_valid = sum(valid_ext), .groups = "drop")
}

est_windows <- list(ew150 = c(-150, -10), ew120 = c(-120, -10))

results_estimation_robustness <- purrr::map_dfr(names(est_windows), function(vname) {
  est_start       <- est_windows[[vname]][1]
  est_end         <- est_windows[[vname]][2]
  threshold_local <- length(est_start:est_end)
  
  counts_base <- estimation_counts_mm_window(event_tau_panel_after_calendar_next, est_start, est_end)
  counts_ext  <- estimation_counts_ext_window(event_tau_panel_after_calendar_next, est_start, est_end)
  
  samp_base <- make_mm_sample(events_after_calendar_next, event_tau_panel_after_calendar_next, counts_base, threshold_local)
  samp_ext  <- make_mm_sample(events_after_calendar_next, event_tau_panel_after_calendar_next, counts_ext,  threshold_local)
  
  panel_base_fit <- run_mm_window(samp_base$panel, est_start, est_end)
  panel_ext_fit  <- run_mm_extended_window(samp_ext$panel, est_start, est_end, ar_name = "ar_ext")
  
  cars_base <- compute_cars(panel_base_fit, samp_base$events, ar_col = "ar")
  cars_ext  <- compute_cars(panel_ext_fit,  samp_ext$events,  ar_col = "ar_ext")
  
  bind_rows(
    compare_mean_car(cars_base, paste0("NEXT_base_", vname)),
    compare_mean_car(cars_ext,  paste0("NEXT_ext_",  vname))
  )
})

# ---- 7.3 CS robustness: extended model with [-120,-10] window, k=2 --------

est_start_cs    <- -120
est_end_cs      <- -10
threshold_cs    <- length(est_start_cs:est_end_cs)

counts_ext_cs <- estimation_counts_ext_window(
  event_tau_panel_after_calendar_next, est_start_cs, est_end_cs
)
samp_ext_cs <- make_mm_sample(
  events_after_calendar_next, event_tau_panel_after_calendar_next,
  counts_ext_cs, threshold_cs
)
panel_ext_cs <- run_mm_extended_window(
  samp_ext_cs$panel, est_start_cs, est_end_cs, ar_name = "ar_ext"
)
cars_ext_cs <- compute_cars(panel_ext_cs, samp_ext_cs$events, ar_col = "ar_ext")

cs_results_ext_ew120_k2 <- run_model_k(
  events_tbl  = samp_ext_cs$events,
  car_obj     = cars_ext_cs,
  model_name  = "NEXT_ext_ew120",
  k = 2
)

# =============================================================================
# PART VIII: F-TESTS
# =============================================================================

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

# =============================================================================
# PART IX: FIGURES
# =============================================================================

to_plot_windows <- function(df) {
  df %>%
    mutate(
      k      = as.integer(stringr::str_extract(window, "\\d+")),
      se     = sd_car / sqrt(n),
      ci_low  = mean_car - 1.96 * se,
      ci_high = mean_car + 1.96 * se
    ) %>%
    arrange(k)
}

# ---- Figure 5.1 ------------------------------------------------------------

baseline_plot_data <- bind_rows(
  results_next_base %>% mutate(model = "Simple market model"),
  results_next_ext  %>% mutate(model = "Extended market model")
) %>%
  to_plot_windows()

p_figure51 <- ggplot(
  baseline_plot_data,
  aes(x = k, y = mean_car, linetype = model, shape = model, group = model)
) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.18, linewidth = 0.55, color = "black") +
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
  labs(x = "Event window length (k)", y = "Mean CAR",
       linetype = NULL, shape = NULL) +
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

print(p_figure51)
# ggsave("figure_5_1.pdf", p_figure51, width = 6, height = 4.5)

# ---- Figure 5.2 ------------------------------------------------------------

sectoral_plot_data <- bind_rows(
  results_sector_usa_ext    %>% mutate(region_plot = "United States"),
  results_sector_europe_ext %>% mutate(region_plot = "Europe")
) %>%
  to_plot_windows()

sectoral_plot_data$region_plot <- factor(
  sectoral_plot_data$region_plot,
  levels = c("United States", "Europe")
)

p_figure52 <- ggplot(sectoral_plot_data, aes(x = k, y = mean_car)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40", linewidth = 0.6) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.12, linewidth = 0.55, color = "black") +
  geom_line(linewidth = 1.1, color = "black") +
  geom_point(size = 3.0, color = "black") +
  facet_wrap(
    ~region_plot, nrow = 2, scales = "free_y",
    labeller = labeller(region_plot = c(
      "United States" = "(a)  United States",
      "Europe"        = "(b)  Europe"
    ))
  ) +
  scale_x_continuous(breaks = 0:10, labels = 0:10,
                     expand = expansion(add = 0.3)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = scales::number_format(accuracy = 0.0001),
    expand = expansion(mult = c(0.06, 0.06))
  ) +
  labs(x = "Event window length (k)", y = "Mean CAR") +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.4),
    strip.text         = element_text(size = 12, face = "bold"),
    axis.title         = element_text(size = 13),
    axis.text          = element_text(size = 11),
    plot.margin        = margin(t = 8, r = 12, b = 8, l = 8, unit = "pt")
  )

print(p_figure52)
# ggsave("figure_5_2.pdf", p_figure52, width = 6, height = 6)

# =============================================================================
# PART X: PRINT ALL RESULTS
# =============================================================================

cat("\n\n")
cat("=============================================================\n")
cat("  THESIS RESULTS — in order of appearance\n")
cat("=============================================================\n\n")

# ─── SAMPLE SIZE CHECKS ───────────────────────────────────────────────────────
cat("─── SAMPLE SIZE CHECKS ───\n")
cat("    Events in events_final.csv:    ", n_events_input, "(expected 267)\n")
cat("    Events in analytical sample:   ", n_analytical,   "(expected 266)\n\n")

cat("    Events by country:\n")
print(events_by_country, n = Inf)
cat("\n")

# ─── DESCRIPTIVE STATISTICS ──────────────────────────────────────────────────
cat("─── DESCRIPTIVE STATISTICS ───\n")
cat("    Sample overview:\n");   print(sample_overview)
cat("    Event types:\n");       print(event_types_summary, n = Inf)
cat("    Disruption types (aggregated):\n"); print(disruption_types_summary, n = Inf)
cat("    Severity variables:\n"); print(severity_summary, n = Inf)
cat("\n")

# ─── TABLE 5.1 ────────────────────────────────────────────────────────────────
cat("─── TABLE 5.1 — Baseline CAR (next trading day, [0,+1] and [0,+2]) ───\n")
cat("    Simple market model:\n")
results_next_base %>%
  filter(window %in% c("car_01", "car_02")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n    Extended market model:\n")
results_next_ext %>%
  filter(window %in% c("car_01", "car_02")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n")

# ─── FIGURE 5.1 (data) ────────────────────────────────────────────────────────
cat("─── FIGURE 5.1 — Mean CAR across windows (both models, [0,+0] to [0,+10]) ───\n")
bind_rows(
  results_next_base %>% mutate(model = "Simple"),
  results_next_ext  %>% mutate(model = "Extended")
) %>%
  mutate(k = as.integer(stringr::str_extract(window, "\\d+"))) %>%
  arrange(k, model) %>%
  select(model, k, n, mean_car, t_stat, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE 5.2 ────────────────────────────────────────────────────────────────
cat("─── TABLE 5.2 — Cross-sectional regressions (next trading day) ───\n")
cat("    Panel A: [0,+2] window — clustered SE by country\n")
coefs_all_hc1_vs_cluster %>%
  filter(model %in% c("NEXT_base", "NEXT_ext"), k == 2, se_type == "CL_country") %>%
  select(model, term, estimate, se, t, p) %>%
  arrange(model, term) %>%
  print(n = Inf)
cat("\n    Panel B: [0,+5] window — clustered SE by country\n")
coefs_all_hc1_vs_cluster %>%
  filter(model %in% c("NEXT_base", "NEXT_ext"), k == 5, se_type == "CL_country") %>%
  select(model, term, estimate, se, t, p) %>%
  arrange(model, term) %>%
  print(n = Inf)
cat("\n    N per panel:\n")
coefs_all_hc1_vs_cluster %>%
  filter(model %in% c("NEXT_base", "NEXT_ext"), k %in% c(2, 5), se_type == "CL_country") %>%
  distinct(model, k, n) %>%
  arrange(k, model) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE 6.1 ────────────────────────────────────────────────────────────────
cat("─── TABLE 6.1 — Robustness: Alternative timing (same-or-next trading day) ───\n")
cat("    Simple market model:\n")
results_same_base %>%
  filter(window %in% c("car_01", "car_02", "car_03", "car_05", "car_10")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n    Extended market model:\n")
results_same_ext %>%
  filter(window %in% c("car_01", "car_02", "car_03", "car_05", "car_10")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n")

# ─── TABLE 6.2 ────────────────────────────────────────────────────────────────
cat("─── TABLE 6.2 — Robustness: Estimation window sensitivity ───\n")
results_estimation_robustness %>%
  filter(window %in% c("car_01", "car_02", "car_03", "car_05", "car_10")) %>%
  arrange(version, window) %>%
  select(version, window, n, mean_car, t_stat, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE 6.3 ────────────────────────────────────────────────────────────────
cat("─── TABLE 6.3 — Robustness: Placebo test ───\n")
results_placebo_next_base %>%
  filter(window %in% c("car_00", "car_01", "car_02", "car_05", "car_10")) %>%
  mutate(seed = stringr::str_extract(version, "\\d+$")) %>%
  select(seed, window, n, mean_car, t_stat, p_value) %>%
  arrange(seed, window) %>%
  print(n = Inf)

# ─── TABLE E.1 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.1 — Alternative event windows (Appendix E) ───\n")
cat("    Simple market model:\n")
results_next_base %>%
  filter(window %in% c("car_00", "car_03", "car_05", "car_10")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n    Extended market model:\n")
results_next_ext %>%
  filter(window %in% c("car_00", "car_03", "car_05", "car_10")) %>%
  select(window, n, mean_car, t_stat, p_value) %>%
  print()
cat("\n")

# ─── TABLE E.2 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.2 — Sectoral USA, Extended Model ───\n")
results_sector_usa_ext %>%
  filter(window %in% c("car_01", "car_02", "car_05", "car_10")) %>%
  select(window, n, mean_car, sd_car, t_stat, p_value) %>%
  print()
cat("\n")

# ─── TABLE E.3 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.3 — Sectoral Europe, Extended Model ───\n")
results_sector_europe_ext %>%
  filter(window %in% c("car_01", "car_02", "car_05", "car_10")) %>%
  select(window, n, mean_car, sd_car, t_stat, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE E.4 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.4 — CS regressions: same-or-next, [0,+2] ───\n")
coefs_all_hc1_vs_cluster %>%
  filter(model %in% c("SAME_base", "SAME_ext"), k == 2, se_type == "CL_country") %>%
  select(model, term, estimate, se, t, p) %>%
  arrange(model, term) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE E.5 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.5 — CS regressions: same-or-next, [0,+10] ───\n")
coefs_all_hc1_vs_cluster %>%
  filter(model %in% c("SAME_base", "SAME_ext"), k == 10, se_type == "CL_country") %>%
  select(model, term, estimate, se, t, p) %>%
  arrange(model, term) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE E.6 ────────────────────────────────────────────────────────────────
cat("─── TABLE E.6 — CS regression: estimation window [-120,-10], k=2 ───\n")
cs_results_ext_ew120_k2$coefs %>%
  filter(se_type == "CL_country") %>%
  select(term, estimate, se, t, p) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE F.1 ────────────────────────────────────────────────────────────────
cat("─── TABLE F.1 — F-tests: joint significance (national indices) ───\n")
f_blocks_all %>%
  filter(model %in% c("NEXT_base", "NEXT_ext"), k %in% c(2, 5, 10)) %>%
  arrange(model, k, block) %>%
  select(model, k, block, F_stat, df1, df2, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── TABLE F.2 ────────────────────────────────────────────────────────────────
cat("─── TABLE F.2 — F-tests: joint significance (sectoral indices) ───\n")
bind_rows(cs_sector_base$f_blocks, cs_sector_ext$f_blocks) %>%
  filter(k %in% c(2, 5, 10)) %>%
  arrange(model, k, block) %>%
  select(model, k, block, F_stat, df1, df2, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── FIGURE 5.2 (data) ────────────────────────────────────────────────────────
cat("─── FIGURE 5.2 — Sectoral mean CAR, all windows ───\n")
bind_rows(
  results_sector_usa_ext    %>% mutate(region = "United States"),
  results_sector_europe_ext %>% mutate(region = "Europe")
) %>%
  mutate(k = as.integer(stringr::str_extract(window, "\\d+"))) %>%
  arrange(region, k) %>%
  select(region, k, n, mean_car, t_stat, p_value) %>%
  print(n = Inf)
cat("\n")

# ─── FINAL SAMPLE-SIZE SUMMARY ────────────────────────────────────────────────
cat("─── FINAL SAMPLE-SIZE SUMMARY (N per window) ───\n")
bind_rows(
  cars_next_strict$sample_sizes     %>% mutate(spec = "NEXT_base"),
  cars_next_strict_ext$sample_sizes %>% mutate(spec = "NEXT_ext"),
  cars_same_strict$sample_sizes     %>% mutate(spec = "SAME_base"),
  cars_same_strict_ext$sample_sizes %>% mutate(spec = "SAME_ext")
) %>%
  filter(k %in% c(0, 1, 2, 5, 10)) %>%
  select(spec, k, n, n_dropped_overlap) %>%
  arrange(spec, k) %>%
  print(n = Inf)

cat("\n=============================================================\n")
cat("  END OF RESULTS\n")
cat("=============================================================\n\n")





