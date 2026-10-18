#Jonathan Elliott
#Part 3

library(tidyverse)

clean = read_csv("data/clean_three_countries.csv")

#Using Japan only and with period labels
japan = clean %>%
  filter(iso == "JPN") %>%
  arrange(year) %>%
  mutate(
    hc_growth  = 100 * (log(hc)     - lag(log(hc))),
    tfp_growth = 100 * (log(rtfpna) - lag(log(rtfpna)))
  ) %>%
  filter(!is.na(gdp_growth)) %>%               # drops 1950 (no prior year)
  mutate(
    period = case_when(
      year <= 1973 ~ "1951-1973",
      year <= 1990 ~ "1974-1990",
      year <= 2007 ~ "1991-2007",
      TRUE         ~ "2008-2019"
    ),
    period = factor(period,
                    levels = c("1951-1973", "1974-1990", "1991-2007", "2008-2019"))
  )

#checking average growth and drivers
period_summary = japan %>%
  group_by(period) %>%
  summarise(
    n_years         = n(),
    mean_growth     = mean(gdp_growth, na.rm = TRUE),
    sd_growth       = sd(gdp_growth,   na.rm = TRUE),
    mean_pop_growth = mean(pop_growth, na.rm = TRUE),
    mean_invest     = mean(csh_i,      na.rm = TRUE),
    mean_hc_growth  = mean(hc_growth,  na.rm = TRUE),
    mean_tfp_growth = mean(tfp_growth, na.rm = TRUE)   # TFP starts 1954
  )
print(period_summary, width = Inf)

#ploting the averages
bounds = japan %>%
  group_by(period) %>%
  summarise(start = min(year) - 0.5,
            end   = max(year) + 0.5,
            mean_growth = mean(gdp_growth))

events = tibble(
  year  = c(1973, 1991, 2008, 2011),
  label = c("Oil shock", "Bubble bursts", "Global financial crisis", "Tohoku earthquake")
)

dir.create("figures", showWarnings = FALSE)

p = ggplot(japan, aes(x = year, y = gdp_growth)) +
  geom_rect(data = bounds,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = period),
            inherit.aes = FALSE, alpha = 0.15) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_line(linewidth = 0.7) +
  geom_segment(data = bounds,
               aes(x = start, xend = end, y = mean_growth, yend = mean_growth),
               inherit.aes = FALSE, linetype = "dashed", linewidth = 0.8) +
  geom_vline(data = events, aes(xintercept = year),
             color = "grey30", linetype = "dotted") +
  geom_text(data = events, aes(x = year, y = Inf, label = label),
            angle = 90, hjust = 1.1, vjust = -0.4, size = 3, inherit.aes = FALSE) +
  labs(title = "Japan: GDP per capita growth by period",
       subtitle = "Dashed lines show the average growth rate in each period",
       x = "Year", y = "Growth rate (%)", fill = "Period",
       caption = "Source: Penn World Table 10.0") +
  theme_minimal()

ggsave("figures/japan_periods.png", p, width = 9, height = 5, dpi = 300)
p