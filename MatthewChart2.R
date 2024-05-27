library(ggplot2)

data <- read.csv("jail_pop_per_county_state.csv")

urban_data <- na.omit(data[data$urbanicity == "urban", c("white_jail_pop_rate", "black_jail_pop_rate")])

ggplot(urban_data, aes(x = white_jail_pop_rate, y = black_jail_pop_rate)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Comparison of Jail Population Rates in Urban Areas: Whites vs Blacks",
       x = "White Jail Population Rate",
       y = "Black Jail Population Rate") +
  theme_minimal()
