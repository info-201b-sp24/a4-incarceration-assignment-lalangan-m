library(ggplot2)
library(dplyr)

jail_pop_per_county_state <- read.csv("jail_pop_per_county_state.csv")

colnames(jail_pop_per_county_state)

jail_pop_per_county_state_cleaned <- jail_pop_per_county_state %>%
  filter(!is.na(year) & year >= 1990 & 
           !is.na(black_jail_pop_rate) & !is.na(white_jail_pop_rate) & 
           !is.na(black_prison_pop_rate) & !is.na(white_prison_pop_rate))

total_pop_per_state <- jail_pop_per_county_state_cleaned %>%
  group_by(state) %>%
  summarise(
    total_jail_pop_rate = sum(total_jail_pop_rate, na.rm = TRUE),
    total_prison_pop_rate = sum(total_prison_pop_rate, na.rm = TRUE)
  ) %>%
  mutate(total_combined_pop_rate = total_jail_pop_rate + total_prison_pop_rate) %>%
  arrange(desc(total_combined_pop_rate)) %>%
  slice(1:10)

top_10_states <- total_pop_per_state$state
top_10_states_data <- jail_pop_per_county_state_cleaned %>%
  filter(state %in% top_10_states)

racial_disparity_grouped <- top_10_states_data %>%
  group_by(year) %>%
  summarise(
    black_jail_pop_rate = mean(black_jail_pop_rate, na.rm = TRUE),
    white_jail_pop_rate = mean(white_jail_pop_rate, na.rm = TRUE),
    black_prison_pop_rate = mean(black_prison_pop_rate, na.rm = TRUE),
    white_prison_pop_rate = mean(white_prison_pop_rate, na.rm = TRUE)
  )

ggplot(racial_disparity_grouped, aes(x = year)) +
  geom_line(aes(y = black_jail_pop_rate, color = "Black Jail Population Rate"), size = 1) +
  geom_line(aes(y = white_jail_pop_rate, color = "White Jail Population Rate"), size = 1) +
  geom_line(aes(y = black_prison_pop_rate, color = "Black Prison Population Rate"), size = 1) +
  geom_line(aes(y = white_prison_pop_rate, color = "White Prison Population Rate"), size = 1) +
  labs(
    title = "Avg. Jail/Prison Rates in Top 10 States",
    x = "Year (1990-2020)",
    y = "Avg. Population Rate",
    color = "Population Rates"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
