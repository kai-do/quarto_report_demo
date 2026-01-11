library(tidycensus)
library(sf)
library(dplyr)
library(purrr)


options(tigris_use_cache = TRUE)
readRenviron("~/.Renviron")

pop_var <- "B01003_001"
year <- 2022


get_state_tract_pop <- function(st) {

  get_acs(
    geography = "tract",
    variables = pop_var,
    state = st,
    year = year,
    survey = "acs5",
    geometry = TRUE,
    cache_table = TRUE
  ) %>%
    transmute(GEOID, pop = estimate)
}

states <- c(state.abb, "DC")

tract_pop <- map_dfr(states, get_state_tract_pop)

saveRDS(tract_pop, file = "datasets/tract_pop_2022.rds")

tract_pop$area_km2 <- as.numeric(st_area(tract_pop)) / 1e6

tract_pop_df <- tract_pop %>%
  mutate(pop_density = pop / area_km2)

weighted_tract_sample_df <- tract_pop_df %>%
  filter(pop > 0) %>%
  slice_sample(n = 10000, weight_by = pop_density, replace = TRUE)

pts <- st_sample(weighted_tract_sample_df$geometry, size = nrow(weighted_tract_sample_df), type = "random")

random_lat_long_df <- st_coordinates(pts) %>%
  as.data.frame() %>%
  transmute(longitude = X, latitude = Y)

saveRDS(random_lat_long_df, file = "datasets/random_lat_long_2022.rds")

# library(leaflet)
# library(dplyr)
# 
# leaflet(random_lat_long_df) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircleMarkers(
#     lng = ~longitude,
#     lat = ~latitude,
#     radius = 2,
#     stroke = FALSE,
#     fillOpacity = 0.4
#   )

