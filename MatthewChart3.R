library(ggplot2)
library(maps)
library(dplyr)
library(viridis)

jail_data <- read.csv("jail_pop_per_county_state.csv")

selected_states <- c("WA", "CA", "OR")
state_jail_data <- jail_data %>%
  filter(state %in% selected_states) %>%
  select(fips, state, county_name, total_jail_pop_rate) %>%
  mutate(county_name = tolower(gsub(" County", "", county_name)))

map_data <- map_data("county")
selected_map_data <- map_data %>%
  filter(region %in% c("washington", "california", "oregon"))

selected_map_data <- selected_map_data %>%
  left_join(state_jail_data, by = c("subregion" = "county_name"))

ggplot(data = selected_map_data, aes(x = long, y = lat, group = group, fill = total_jail_pop_rate)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  labs(title = "Jail Population Density in WA, CA, and OR",
       fill = "Jail Population Rate") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_fixed(1)
