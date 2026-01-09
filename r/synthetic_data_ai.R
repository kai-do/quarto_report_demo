# Synthetic device-inspection dataset (simstudy-compatible, no "lognormal" needed)
# Required fields: occupancy_id, occupancy_type, device_type, device_id,
#                  inspection_datetime, inspection_time_min, failure (yes/no)

suppressPackageStartupMessages({
  library(simstudy)
  library(data.table)
  library(lubridate)
})

ps_default_config <- function() {
  list(
    seed = 123,
    
    # Scope
    n_occupancies = 800,
    start_date = as.Date("2024-01-01"),
    end_date   = as.Date("2025-12-31"),
    
    # Occupancy mix
    occupancy_types = c("Assembly","Business","Education","Healthcare",
                        "Industrial","Mercantile","Residential","Storage"),
    occupancy_probs = c(0.07, 0.28, 0.08, 0.05, 0.10, 0.15, 0.22, 0.05),
    
    # Device mix
    device_types = c("Smoke Detector","Sprinkler Head","Fire Extinguisher",
                     "Fire Alarm Panel","Emergency Lighting","Standpipe",
                     "Kitchen Hood Suppression"),
    device_probs = c(0.22, 0.18, 0.24, 0.05, 0.15, 0.08, 0.08),
    
    # Devices per occupancy (negBinomial -> clamp)
    devices_per_occ = list(mean = 18, dispersion = 1.2, min = 2L, max = 140L),
    
    # Inspection frequency per year by device type
    insp_freq_per_year = c(
      "Smoke Detector" = 2,
      "Sprinkler Head" = 1,
      "Fire Extinguisher" = 1,
      "Fire Alarm Panel" = 2,
      "Emergency Lighting" = 1,
      "Standpipe" = 1,
      "Kitchen Hood Suppression" = 2
    ),
    
    # Baseline inspection time (minutes) by device type
    base_time_min = c(
      "Smoke Detector" = 6,
      "Sprinkler Head" = 8,
      "Fire Extinguisher" = 10,
      "Fire Alarm Panel" = 22,
      "Emergency Lighting" = 9,
      "Standpipe" = 14,
      "Kitchen Hood Suppression" = 18
    ),
    
    schedule_jitter_days_sd = 10,
    
    # Failure model (logit scale)
    failure_model = list(
      intercept = -2.3,
      beta_age_years = 0.08,
      beta_occ_risk = 0.55,
      device_re_sd = 0.55,
      
      device_type_beta = c(
        "Smoke Detector" = 0.10,
        "Sprinkler Head" = 0.00,
        "Fire Extinguisher" = 0.18,
        "Fire Alarm Panel" = 0.25,
        "Emergency Lighting" = 0.15,
        "Standpipe" = 0.20,
        "Kitchen Hood Suppression" = 0.30
      ),
      
      occupancy_type_beta = c(
        "Assembly" = 0.20,
        "Business" = 0.05,
        "Education" = 0.10,
        "Healthcare" = 0.15,
        "Industrial" = 0.18,
        "Mercantile" = 0.08,
        "Residential" = 0.12,
        "Storage" = 0.06
      )
    ),
    
    # Gamma params for building sqft (right-skewed like real buildings)
    # mean = shape*scale, var = shape*scale^2
    sqft_gamma = list(shape = 3.0, scale = 4500, min = 600, max = 450000),
    
    # Gamma params for inspection time multiplier noise (right-skewed)
    time_noise_gamma = list(shape = 12, scale = 0.10) # mean ~1.2
  )
}

simulate_ps_device_inspections <- function(cfg = ps_default_config()) {
  stopifnot(cfg$start_date < cfg$end_date)
  set.seed(cfg$seed)
  
  years_span <- as.numeric(difftime(cfg$end_date, cfg$start_date, units = "days")) / 365.25
  
  # ---------- 1) OCCUPANCIES ----------
  defOcc <- defData(
    varname = "occ_type_code",
    dist = "categorical",
    formula = paste(cfg$occupancy_probs, collapse = ";"),
    id = "occupancy_id"
  )
  
  # Right-skewed sqft using gamma (no lognormal required)
  defOcc <- defData(
    defOcc,
    varname = "sqft",
    dist = "gamma",
    formula = paste0(cfg$sqft_gamma$shape, ";", cfg$sqft_gamma$scale)
  )
  
  defOcc <- defData(defOcc, varname = "year_built", dist = "uniformInt", formula = "1950;2024")
  defOcc <- defData(defOcc, varname = "occ_risk_z", dist = "normal", formula = "0;1")
  
  occ <- as.data.table(genData(cfg$n_occupancies, defOcc))
  occ[, occupancy_type := cfg$occupancy_types[occ_type_code]]
  
  # Clamp sqft to a plausible range
  occ[, sqft := pmin(pmax(round(sqft), cfg$sqft_gamma$min), cfg$sqft_gamma$max)]
  
  # Risk proxy: combine sqft (logged) + noise
  occ[, occ_risk := scale(log1p(sqft))[,1] * 0.6 + occ_risk_z * 0.7]
  
  # Pretty IDs
  occ[, occupancy_id := sprintf("OCC%06d", occupancy_id)]
  
  # Devices per occupancy (negBinomial)
  defOccAdd <- defDataAdd(
    varname = "n_devices",
    dist = "negBinomial",
    formula = cfg$devices_per_occ$mean,
    variance = cfg$devices_per_occ$dispersion
  )
  occ <- addColumns(defOccAdd, occ)
  occ[, n_devices := pmin(pmax(as.integer(n_devices), cfg$devices_per_occ$min), cfg$devices_per_occ$max)]
  
  # ---------- 2) DEVICES ----------
  dev <- occ[rep.int(seq_len(.N), n_devices)][
    , .(occupancy_id, occupancy_type, sqft, occ_risk)
  ]
  dev[, device_id := sprintf("DEV%09d", .I)]
  
  # Device types
  defDevType <- defDataAdd(
    varname = "dev_type_code",
    dist = "categorical",
    formula = paste(cfg$device_probs, collapse = ";")
  )
  dev <- addColumns(defDevType, dev)
  dev[, device_type := cfg$device_types[dev_type_code]]
  dev[, dev_type_code := NULL]
  
  # Device age (years): gamma, capped
  defDevAge <- defDataAdd(varname = "device_age_years", dist = "gamma", formula = "8;0.7")
  dev <- addColumns(defDevAge, dev)
  dev[, device_age_years := pmin(pmax(as.integer(round(device_age_years)), 0L), 35L)]
  
  # Device-level random effect for failures
  defDevRE <- defDataAdd(varname = "device_re", dist = "normal", formula = paste0("0;", cfg$failure_model$device_re_sd))
  dev <- addColumns(defDevRE, dev)
  
  # Inspection frequency lookup
  dev[, insp_freq := as.numeric(cfg$insp_freq_per_year[device_type])]
  dev[is.na(insp_freq), insp_freq := 1]
  
  # Number of inspections in window: Poisson around expected count (>=1)
  dev[, lambda_insp := insp_freq * years_span * runif(.N, 0.85, 1.15)]
  defInspN <- defDataAdd(varname = "n_inspections", dist = "poisson", formula = "lambda_insp")
  dev <- addColumns(defInspN, dev)
  dev[, n_inspections := pmax(as.integer(n_inspections), 1L)]
  
  # ---------- 3) INSPECTIONS (schedule-ish) ----------
  insp <- dev[rep.int(seq_len(.N), n_inspections)]
  insp[, inspection_index := seq_len(.N), by = device_id]
  
  insp[, interval_days := 365.25 / insp_freq]
  insp[, max_first := cfg$end_date - as.integer((n_inspections - 1L) * interval_days), by = device_id]
  insp[, max_first := pmax(max_first, cfg$start_date)]
  
  insp[, first_date := as.Date(cfg$start_date + floor(runif(1, 0, as.numeric(max_first - cfg$start_date) + 1))),
       by = device_id]
  
  insp[, inspection_date := first_date +
         as.integer(round((inspection_index - 1L) * interval_days +
                            rnorm(.N, mean = 0, sd = cfg$schedule_jitter_days_sd)))]
  
  insp[, inspection_date := pmin(pmax(inspection_date, cfg$start_date), cfg$end_date)]
  
  # Work-hours datetime
  insp[, inspection_datetime := as.POSIXct(inspection_date) +
         hours(sample(8:16, .N, replace = TRUE)) +
         minutes(sample(c(0, 10, 20, 30, 40, 50), .N, replace = TRUE))]
  
  # ---------- 4) Inspection time (minutes): base * (sqft factor) * gamma noise ----------
  insp[, base_time := as.numeric(cfg$base_time_min[device_type])]
  insp[is.na(base_time), base_time := 10]
  
  # sqft factor: larger buildings => slower inspections (mild effect)
  insp[, sqft_factor := exp(0.10 * as.numeric(scale(log1p(sqft))[,1]))]
  
  # right-skewed noise multiplier (gamma)
  insp[, time_noise := rgamma(.N, shape = cfg$time_noise_gamma$shape, scale = cfg$time_noise_gamma$scale)]
  insp[, inspection_time_min := round(base_time * sqft_factor * time_noise, 1)]
  insp[, inspection_time_min := pmax(inspection_time_min, 1.0)]
  
  # ---------- 5) Failure (yes/no): logistic model via simstudy binary ----------
  fm <- cfg$failure_model
  insp[, b_dev := as.numeric(fm$device_type_beta[device_type])]
  insp[, b_occ := as.numeric(fm$occupancy_type_beta[occupancy_type])]
  insp[is.na(b_dev), b_dev := 0]
  insp[is.na(b_occ), b_occ := 0]
  
  defFail <- defDataAdd(
    varname = "failure_bin",
    dist = "binary",
    link = "logit",
    formula = paste0(
      fm$intercept,
      " + ", fm$beta_age_years, " * device_age_years",
      " + ", fm$beta_occ_risk, " * occ_risk",
      " + b_dev + b_occ + device_re"
    )
  )
  insp <- addColumns(defFail, insp)
  insp[, failure := fifelse(failure_bin == 1, "yes", "no")]
  
  # Failures tend to take longer (re-check, notes)
  insp[failure == "yes", inspection_time_min := round(inspection_time_min * runif(.N, 1.15, 1.55), 1)]
  
  # ---------- Final output ----------
  out <- insp[, .(
    occupancy_id,
    occupancy_type,
    device_type,
    device_id,
    inspection_datetime,
    inspection_time_min,
    failure
  )]
  
  setorder(out, occupancy_id, device_id, inspection_datetime)
  out[]
}

# -------------------------
# Example usage
# -------------------------
cfg <- ps_default_config()
cfg$n_occupancies <- 1200
cfg$start_date <- as.Date("2023-01-01")
cfg$end_date   <- as.Date("2025-12-31")

dt <- simulate_ps_device_inspections(cfg)

# quick checks
dt[, .N]
dt[, .(fail_rate = mean(failure == "yes"))]
dt[, .N, by = occupancy_type][order(-N)]
dt[, .(p50 = median(inspection_time_min), p90 = quantile(inspection_time_min, 0.9)),
   by = device_type][order(-p50)]
