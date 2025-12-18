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
  arrange(application, year, device, passed_flag) %>%
  inner_join(inspection_device_failures_reasons_raw_df %>% distinct(application_name, year, device, inspections)) %>%
  mutate(device_inspection_per_occ_inspection = count / inspections,
         event_prob = device_inspection_per_occ_inspection / sum(device_inspection_per_occ_inspection))

# now I just need to get the proportions of all of those and then I can distribute them according to inspections. 


def <- defData(varname = "application", 
               dist = "categorical", 
               formula = "0.50;0.20;0.20;0.10"
)

set.seed(343)

test_df <- genData(1000, def)


gen_company <- defData(varname="comp0", dist = "normal", 
                      formula = 0, variance = 3, id = "company_id"
)
gen_company <- defData(gen_company, varname = "n_occupancies", 
                      dist = "noZeroPoisson", formula = 20
)

company_df <- genData(8, gen_company)


gen_occupancy <- defDataAdd(varname = "occ0", 
                            dist = "normal", 
                            formula = 0, 
                            variance = 2)

gen_occupancy <- defDataAdd(gen_occupancy,
                            varname = "occupancy_category",
                            formula = "0.65;0.25;0.10",
                            dist = "categorical",
                            link = "identity"
                            )

gen_occupancy <- defDataAdd(gen_occupancy, 
                            varname = "n_inspections", 
                            dist = "noZeroPoisson", 
                            formula = 1
)

class_df <- genCluster(company_df, "company_id", numIndsVar = "n_occupancies", level1ID = "occupancy_id")
class_df <- addColumns(gen_occupancy, class_df)

head(dtClass, 10)

gen.student <- defDataAdd(varname="Male", dist="binary", formula=0.5)
gen.student <- defDataAdd(gen.student, varname="age", dist = "uniform", formula="9.5; 10.5")
gen.student <- defDataAdd(gen.student, varname="test", dist = "normal",
                          formula = "50 - 5*Male + s0 + c0 + 8 * trtGrp",                           variance = 2)
dtStudent <- genCluster(dtClass,cLevelVar="idClass", numIndsVar = "nStudents",                        level1ID = "idChild")

dtStudent <- addColumns(gen.student, dtStudent)
