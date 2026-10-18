#Jonathan Elliott
#Project 1
#Macroeconomic Theory

#adding the libraries packages
library(tidyverse)
library(zoo)
library(pwt10)

#Loading raw data and cleaning it
data("pwt10.0")
pwt = as_tibble(pwt10.0)

countries = c("JPN", "CHN", "IND")   #JPN, Japan, is for advanced. CHN, China, is for emerging. IND, India, is for lower.

raw = pwt %>%
  filter(isocode %in% countries) %>%
  transmute(
    iso     = as.character(isocode),
    country = as.character(country),
    year    = year,
    rgdpna  = rgdpna,   
    pop     = pop,      
    csh_i   = csh_i,    
    hc      = hc,       
    rtfpna  = rtfpna,   
    emp     = emp       
  )

#checking for missing values
cat("\nMissing values by country:\n")
raw %>%
  group_by(country) %>%
  summarise(across(c(rgdpna, pop, csh_i, hc, rtfpna, emp), ~ sum(is.na(.)))) %>%
  print()

cat("\nYear range with non-missing GDP, by country:\n")
raw %>%
  filter(!is.na(rgdpna)) %>%
  group_by(country) %>%
  summarise(first_year = min(year), last_year = max(year)) %>%
  print()

#Cleaning the data again
clean = raw %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(across(c(csh_i, hc, rtfpna, emp),
                ~ na.approx(., x = year, na.rm = FALSE))) %>%
  #Creating additional variables
  mutate(
    gdp_pc      = rgdpna / pop,
    gdp_growth  = 100 * (log(gdp_pc) - lag(log(gdp_pc))),
    pop_growth  = 100 * (log(pop) - lag(log(pop)))
  ) %>%
  ungroup()

#Saving the clean data locally
dir.create("data", showWarnings = FALSE)
write_csv(clean, "data/clean_three_countries.csv")

print(clean, n = 10)
  