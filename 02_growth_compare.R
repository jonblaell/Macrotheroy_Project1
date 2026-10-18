#Jonathan Elliott
#Part 2

library(tidyverse)

clean = read_csv("data/clean_three_countries.csv")

#adding somemore variables for growth.Since China doesnt show growth rate until 1953, the date range is 1953 to 2019
growth = clean %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    hc_growth  = 100 * (log(hc)     - lag(log(hc))),
    tfp_growth = 100 * (log(rtfpna) - lag(log(rtfpna)))
  ) %>%
  ungroup() %>%
  filter(year >= 1953, year <= 2019)

#summary table for speed and volatility
growth_summary = growth %>%
  group_by(country) %>%
  summarise(
    mean_growth = mean(gdp_growth, na.rm = TRUE),   # speed
    sd_growth   = sd(gdp_growth,   na.rm = TRUE),   # volatility
    min_growth  = min(gdp_growth,  na.rm = TRUE),
    max_growth  = max(gdp_growth,  na.rm = TRUE),
    worst_year  = year[which.min(gdp_growth)],
    best_year   = year[which.max(gdp_growth)]
  )
print(growth_summary)

#summary table for explanatory variables
var_summary = growth %>%
  group_by(country) %>%
  summarise(
    mean_pop_growth = mean(pop_growth,  na.rm = TRUE),
    mean_invest     = mean(csh_i,       na.rm = TRUE),
    mean_hc_growth  = mean(hc_growth,   na.rm = TRUE),
    mean_tfp_growth = mean(tfp_growth,  na.rm = TRUE)
  )
print(var_summary)

#Now making some plots
dir.create("figures", showWarnings = FALSE)

#Plot for Growth
p1 = ggplot(growth, aes(x = year, y = gdp_growth, color = country)) +
  geom_hline(yintercept = 0, color = "grey70") +
  geom_line(linewidth = 0.7) +
  labs(title = "Annual GDP per capita growth, 1953-2019",
       x = "Year", y = "Growth rate (%)", color = "Country",
       caption = "Source: Penn World Table 10.0 (rgdpna / pop); log differences") +
  theme_minimal()
ggsave("figures/gdp_pc_growth.png", p1, width = 9, height = 5, dpi = 300)

#plot for GDP per capita
p2 = ggplot(growth, aes(x = year, y = gdp_pc, color = country)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(title = "GDP per capita (log scale)",
       x = "Year", y = "Real GDP per capita (2017 USD, log scale)",
       color = "Country",
       caption = "Source: Penn World Table 10.0") +
  theme_minimal()
ggsave("figures/gdp_pc_levels.png", p2, width = 9, height = 5, dpi = 300)

#plot for distribution of growth
p3 = ggplot(growth, aes(x = country, y = gdp_growth, fill = country)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Distribution of annual growth rates",
       x = NULL, y = "Growth rate (%)") +
  theme_minimal()
ggsave("figures/growth_boxplot.png", p3, width = 6, height = 5, dpi = 300)

p1; p2; p3