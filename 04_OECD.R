#Jonathan Elliott
#Part 4
library(tidyverse)
library(pwt10)

data("pwt10.0")

#OECD Members
oecd = c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN","FRA",
         "DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR","LVA","LTU","LUX",
         "MEX","NLD","NZL","NOR","POL","PRT","SVK","SVN","ESP","SWE","CHE","TUR",
         "GBR","USA")

panel = as_tibble(pwt10.0) %>%
  filter(isocode %in% oecd) %>%
  transmute(iso = as.character(isocode), country = as.character(country),
            year, rgdpna, pop, csh_i, hc, rtfpna) %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(
    gdp_pc     = rgdpna / pop,
    growth     = 100 * (log(gdp_pc) - lag(log(gdp_pc))),
    pop_growth = 100 * (log(pop)    - lag(log(pop))),
    hc_growth  = 100 * (log(hc)     - lag(log(hc))),
    tfp_growth = 100 * (log(rtfpna) - lag(log(rtfpna)))
  ) %>%
  ungroup()

#Checking the data for issues
cat("\nGDP coverage by country:\n")
panel %>%
  filter(!is.na(gdp_pc)) %>%
  group_by(country) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  print(n = Inf)

#1991 to 2019, one row
start_year = 1990
end_year   = 2019

initial = panel %>%
  filter(year == start_year) %>%
  transmute(iso, init_log_gdp = log(gdp_pc), init_hc = hc)

cross = panel %>%
  filter(year > start_year, year <= end_year) %>%
  group_by(iso, country) %>%
  summarise(
    avg_growth     = mean(growth,     na.rm = TRUE),
    avg_pop_growth = mean(pop_growth, na.rm = TRUE),
    avg_invest     = 100 * mean(csh_i, na.rm = TRUE),   # % of GDP
    avg_hc_growth  = mean(hc_growth,  na.rm = TRUE),
    avg_tfp_growth = mean(tfp_growth, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(initial, by = "iso")


#Dropping countries without 1990 data
dropped = cross %>% filter(is.na(init_log_gdp)) %>% pull(country)
cat("\nDropped (no 1990 data):", ifelse(length(dropped) == 0, "none",
                                        paste(dropped, collapse = ", ")), "\n")
cross = cross %>% filter(!is.na(init_log_gdp))
cat("Countries in analysis:", nrow(cross), "\n")

#Correlation
vars = c("avg_growth", "avg_pop_growth", "avg_invest", "avg_hc_growth",
         "avg_tfp_growth", "init_log_gdp")
cor_mat = cor(cross[, vars], use = "pairwise.complete.obs")
print(round(cor_mat, 2))

cor_long = as_tibble(cor_mat, rownames = "var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "r")

dir.create("figures", showWarnings = FALSE)

p_cor = ggplot(cor_long, aes(var1, var2, fill = r)) +
  geom_tile() +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue",
                       limits = c(-1, 1)) +
  labs(title = "Correlation matrix: OECD averages, 1991-2019",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/oecd_corr_matrix.png", p_cor, width = 7, height = 6, dpi = 300)

#scatterplots
scatter_long = cross %>%
  select(iso, avg_growth, avg_pop_growth, avg_invest, avg_hc_growth,
         avg_tfp_growth, init_log_gdp) %>%
  pivot_longer(-c(iso, avg_growth), names_to = "variable", values_to = "value")

p_sc = ggplot(scatter_long, aes(x = value, y = avg_growth)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(title = "Average GDP per capita growth vs. determinants (OECD, 1991-2019)",
       x = NULL, y = "Average annual growth (%)",
       caption = "Source: Penn World Table 10.0") +
  theme_minimal()
ggsave("figures/oecd_scatter.png", p_sc, width = 10, height = 6, dpi = 300)

#regression
model = lm(avg_growth ~ init_log_gdp + avg_pop_growth + avg_invest + avg_hc_growth,
           data = cross)
print(summary(model))

p_cor; p_sc