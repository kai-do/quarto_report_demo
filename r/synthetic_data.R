library(tidyverse)
library(dotenv)
library(simstudy)

load_dot_env()

inspection_device_failures_reasons_raw_df <- readRDS(file.path(Sys.getenv("DEVICE_FAILURES"), "device_failures.rds"))


inspection_device_passed_df <- inspection_device_failures_reasons_raw_df %>%
  filter(!is.na(application_name), !is.na(year), !is.na(inspections), !is.na(device), !is.na(tested), !is.na(failed)) %>%
  transmute(application, application_name, year, device, device_status = "Passed", passed_flag = TRUE, count = tested - failed) %>%
  distinct()

inspection_device_failed_df <- inspection_device_failures_reasons_df %>%
  filter(!is.na(application_name), !is.na(year), !is.na(inspections), !is.na(device), !is.na(tested), !is.na(failed), !is.na(failure_reason_category), failures > 0) %>%
  transmute(application, application_name, year, device, device_status = failure_reason_category, passed_flag = FALSE, count = failures)

inspection_device_status_df <- bind_rows(inspection_device_passed_df, inspection_device_failed_df) %>%
  arrange(application, year, device, passed_flag)

# now I just need to get the proportions of all of those and then I can distribute them according to inspections. 


def <- defData(varname = "application", 
               dist = "categorical", 
               formula = "0.50;0.20;0.20;0.10"
)

set.seed(343)

test_df <- genData(1000, def)

