library(tidyverse)
library(dotenv)
library(simstudy)
library(charlatan)


load_dot_env()

inspection_device_failures_reasons_raw_df <- readRDS(file.path(Sys.getenv("DEVICE_FAILURES"), "device_failures.rds"))

inspection_device_passed_df <- inspection_device_failures_reasons_raw_df %>%
  filter(!is.na(application_name), !is.na(year), !is.na(inspections), !is.na(device), !is.na(tested), !is.na(failed)) %>%
  transmute(application, application_name, year, device, device_status = "Passed", passed_flag = TRUE, count = tested - failed) %>%
  distinct()

inspection_device_failed_df <- inspection_device_failures_reasons_raw_df %>%
  filter(!is.na(application_name), !is.na(year), !is.na(inspections), !is.na(device), !is.na(tested), !is.na(failed), !is.na(failure_reason_category), failures > 0) %>%
  transmute(application, application_name, year, device, device_status = failure_reason_category, passed_flag = FALSE, count = failures)

inspection_device_status_df <- bind_rows(inspection_device_passed_df, inspection_device_failed_df) %>%
  arrange(application, year, device, passed_flag) %>%
  inner_join(inspection_device_failures_reasons_raw_df %>% distinct(application_name, year, device, inspections)) %>%
  mutate(device_inspection_per_occ_inspection = count / inspections,
         event_prob = device_inspection_per_occ_inspection / sum(device_inspection_per_occ_inspection))


### probs

application_prob_df <- inspection_device_failures_reasons_raw_df %>%
  distinct(application_name, year, inspections) %>%
  group_by(application_name) %>%
  summarise(total_inspections = sum(inspections, na.rm = TRUE)) %>%
  mutate(event_prob = total_inspections / sum(total_inspections)) %>%
  arrange(application_name)

device_prob_df <- inspection_device_failures_reasons_raw_df %>%
  filter(tested > 0, !is.na(device), device != "") %>%
  distinct(device, tested) %>%
  group_by(device) %>%
  summarise(total_tested = sum(tested, na.rm = TRUE)) %>%
  mutate(event_prob = total_tested / sum(total_tested)) %>%
  arrange(device)

device_app_prob_df <- inspection_device_failures_reasons_raw_df %>%
  filter(tested > 50, !is.na(application_name), application_name != "",
         !is.na(device), device != "") %>%
  group_by(application_name, device) %>%
  summarise(total_tested = sum(tested, na.rm = TRUE), .groups = "drop") %>%
  group_by(application_name) %>%
  mutate(event_prob = total_tested / sum(total_tested)) %>%
  ungroup()

device_app_per_inspection_df <- inspection_device_failures_reasons_raw_df %>%
  filter(!is.na(application_name), application_name != "",
         !is.na(device), device != "",
         !is.na(inspections), inspections > 0,
         !is.na(tested), tested > 0) %>%
  distinct(application_name, year, device, tested, inspections) %>%
  mutate(devices_per_inspection = tested / inspections) %>%
  arrange(application_name, device) %>%
  group_by(application_name, device) %>%
  summarise(mean_devices_per_inspection = mean(devices_per_inspection, na.rm = TRUE),
            sd_devices_per_inspection   = sd(devices_per_inspection, na.rm = TRUE),
            .groups = "drop") 

app_device_count_df <- device_app_per_inspection_df %>%
  group_by(application_name) %>%
  summarise(
    total_devices = sum(mean_devices_per_inspection, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(application_name)


device_inspection_time_df <- inspection_device_failures_reasons_raw_df %>%
  filter(!is.na(device), device != "",
         !is.na(total_seconds_to_test), total_seconds_to_test > 0) %>%
  distinct(device, total_seconds_to_test) %>%
  group_by(device) %>%
  summarise(
    avg_inspection_time_sec = mean(total_seconds_to_test, na.rm = TRUE),
    sd_inspection_time_sec  = sd(total_seconds_to_test, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(device)

device_status_rate_df <- inspection_device_status_df %>%
  filter(!is.na(device), device != "") %>%
  group_by(device, device_status) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(device) %>%
  mutate(
    total_device_count = sum(total_count, na.rm = TRUE),
    status_rate = total_count / total_device_count
  ) %>%
  ungroup() %>%
  arrange(device, device_status)

device_status_year_rate_df <- inspection_device_status_df %>%
  filter(!is.na(device), device != "",
         !is.na(year), year >= 2020) %>%
  group_by(device, year, device_status) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(device, year) %>%
  mutate(
    total_device_count = sum(total_count, na.rm = TRUE),
    status_rate = total_count / total_device_count
  ) %>%
  ungroup() %>%
  arrange(device, year, device_status)

occupancy_types <- c(
  "Assembly",
  "Business",
  "Detention",
  "Educational",
  "Healthcare",
  "Industrial",
  "Mercantile",
  "Mixed",
  "Residential",
  "Storage"
)



set.seed(343)

# --- knobs ---
n_occupancies <- 2000

# occupancy type probabilities (simple: uniform; replace later if you want)
p_occ <- rep(1/length(occupancy_types), length(occupancy_types))

# size class probabilities (drives device roster size later)
size_levels <- c("S", "M", "L")
p_size <- c(0.55, 0.35, 0.10)

# number of applications per occupancy
napp_levels <- c(1, 2, 3)
p_napp <- c(0.60, 0.30, 0.10)

cadence_levels <- c(1, 2, 3, 4)
p_cadence <- c(0.55, 0.30, 0.10, 0.05)

size_base <- c(S = 8, M = 18, L = 35)           # typical roster sizes
size_jitter_frac <- 0.25 

p_to_formula <- function(p) paste(sprintf("%.12f", p), collapse = ";")

# --- simstudy definition ---
def_occ <- defData(
  varname = "occupancy_type_idx",
  dist    = "categorical",
  formula = p_to_formula(p_occ),
  id      = "occupancy_id"
)

def_occ <- defData(def_occ,
                   varname = "size_idx",
                   dist    = "categorical",
                   formula = p_to_formula(p_size)
)

def_occ <- defData(def_occ,
                   varname = "n_apps_idx",
                   dist    = "categorical",
                   formula = p_to_formula(p_napp)
)



applications <- trimws(application_prob_df$application_name)
p_app <- application_prob_df$event_prob


random_first_inspection_date <- function(n, k, year = 2026) {
  
  end_date <- switch(
    as.character(k),
    "1" = as.Date(paste0(year, "-12-31")),
    "2" = as.Date(paste0(year, "-06-30")),
    "3" = as.Date(paste0(year, "-04-30")),
    "4" = as.Date(paste0(year, "-03-31"))
  )
  
  start_date <- as.Date(paste0(year, "-01-01"))
  
  as.Date(
    runif(
      n,
      as.numeric(start_date),
      as.numeric(end_date)
    ),
    origin = "1970-01-01"
  )
}

years <- 2026:2030
# --- generate base occupancy rows ---
occupancies_df <- genData(n_occupancies, def_occ) %>%
  mutate(
    occupancy_type = occupancy_types[occupancy_type_idx],
    size_class     = size_levels[size_idx],
    n_apps         = napp_levels[n_apps_idx]
  ) %>%
  select(occupancy_id, occupancy_type, size_class, n_apps) %>%
  rowwise() %>%
  mutate(
    inspections_per_year = sample(
      cadence_levels,
      size = n(),
      replace = TRUE,
      prob = p_cadence
    ),
    applications = list(
      sample(
        applications,
        size    = n_apps,
        replace = FALSE,
        prob    = p_app
      )
    ),
      #base_device_n = unname(size_base[size_class]),
      #device_n = pmax(
      #  1L,
      #  round(base_device_n * (1 + runif(n(), -size_jitter_frac, size_jitter_frac))))
  ) %>%
  ungroup() %>%
  crossing(year = years) %>%
  rowwise() %>%
  mutate(first_inspection_date = random_first_inspection_date(1, inspections_per_year, year))



jitter_days <- 14

inspections_df <- occupancies_df %>%
  uncount(inspections_per_year, .id = "inspection_number") %>%
  group_by(occupancy_id) %>%
  mutate(inspections_per_year = max(inspection_number),
         base_days_to_add = floor((inspection_number - 1) * (365 / inspections_per_year)),
         days_to_add = base_days_to_add + sample(-jitter_days:jitter_days, n(), replace = TRUE),
         inspection_date = first_inspection_date + days_to_add) %>%
  arrange(inspection_date) %>%
  ungroup() %>%
  mutate(inspection_id = row_number()) %>%
  select(occupancy_id, inspection_id, inspection_number, inspection_date, year)

occupancy_device_count_df <- occupancies_df %>%
  unnest(applications, keep_empty = TRUE) %>%
  rename(application_name = applications) %>%
  left_join(app_device_count_df, by = "application_name") %>%
  group_by(occupancy_id, application_name) %>%
  mutate(
    size_mult = case_when(
      size_class == "S" ~ 0.7,
      size_class == "M" ~ 1.0,
      size_class == "L" ~ 1.5,
      TRUE ~ 1.0
    ),
    jitter_mult = 1 + runif(1, -size_jitter_frac, size_jitter_frac),
    n_devices = pmax(
      1L,
      round(total_devices * size_mult * jitter_mult)
    )
  ) %>%
  ungroup() %>%
  distinct(occupancy_id, application_name, n_devices)



dev_lookup <- device_app_prob_df %>%
  group_by(application_name) %>%
  summarise(
    device_levels = list(device),
    device_probs  = list(event_prob),
    .groups = "drop"
  )


application_codes_df <- inspection_device_failures_reasons_raw_df %>%
  distinct(application, application_name)


inspection_devices_df <- occupancy_device_count_df %>%
  uncount(n_devices, .id = "device_number") %>%
  group_by(occupancy_id) %>%
  mutate(
    n_devices = max(device_number)
  ) %>%
  ungroup() %>%
  left_join(application_codes_df, by = "application_name") %>%
  mutate(
    occ_width = nchar(max(occupancy_id)),
    dev_width = nchar(max(device_number)),
    device_id = paste0(
      sprintf(paste0("%0", occ_width, "d"), occupancy_id),
      "-",
      application,
      "-",
      sprintf(paste0("%0", dev_width, "d"), device_number)
    )
  ) %>%
  left_join(dev_lookup, by = "application_name") %>%
  rowwise() %>%
  mutate(
    device = sample(device_levels, size = 1, prob = device_probs)
  ) %>%
  ungroup() %>%
  select(occupancy_id, application_name, device_id, device)

synth_inspections_df <- inspections_df %>%
  inner_join(inspection_devices_df, by = c("occupancy_id")) %>%
  mutate(
    device_inspection_id = row_number()
  )



status_lookup <- device_status_rate_df %>%
  group_by(device) %>%
  summarise(
    status_levels = list(device_status),
    status_probs  = list(status_rate),
    .groups = "drop"
  )

global_status <- device_status_rate_df %>%
  group_by(device_status) %>%
  summarise(total = sum(status_rate), .groups = "drop") %>%
  mutate(p = total / sum(total))

global_status_levels <- global_status$device_status
global_status_probs  <- global_status$p


synth_inspections_static_df <- synth_inspections_df %>%
  left_join(status_lookup, by = "device") %>%
  rowwise() %>%
  mutate(
    device_status = if (!is.null(status_levels) && length(status_levels) > 0) {
      sample(status_levels, size = 1, prob = status_probs)
    } else {
      sample(global_status_levels, size = 1, prob = global_status_probs)
    }
  ) %>%
  ungroup() %>%
  select(-status_levels, -status_probs) %>%
  left_join(occupancies_df %>% select(occupancy_id, occupancy_type, size_class), by = "occupancy_id") %>%
  distinct()


### With Device Year Probs


status_year_lookup <- device_status_year_rate_df %>%
  mutate(year = year + 6) %>%
  group_by(device, year) %>%
  summarise(
    status_levels = list(device_status),
    status_probs  = list(status_rate),
    .groups = "drop"
  )

global_status_year <- device_status_year_rate_df %>%
  group_by(device_status) %>%
  summarise(total = sum(status_rate), .groups = "drop") %>%
  mutate(p = total / sum(total))

global_status_year_levels <- global_status_year$device_status
global_status_year_probs  <- global_status_year$p


synth_inspections_year_df <- synth_inspections_df %>%
  left_join(status_year_lookup, by = c("device", "year")) %>%
  rowwise() %>%
  mutate(
    device_status = if (!is.null(status_levels) && length(status_levels) > 0) {
      sample(status_levels, size = 1, prob = status_probs)
    } else {
      sample(global_status_year_levels, size = 1, prob = global_status_year_probs)
    }
  ) %>%
  ungroup() %>%
  select(-status_levels, -status_probs) %>%
  left_join(occupancies_df %>% select(occupancy_id, occupancy_type, size_class), by = "occupancy_id") %>%
  distinct()

charlatan::ch_company()

### Next steps: add inspection time based on device type distribution
### population density correlated random lat/long for occupancies to showcase map based analytics
### Verify the probabilities for device statuses and device inspection times follow a trend to showcase temporal trends
### Add inspection companies (custom AI generated names could be interesting as well)

