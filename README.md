Project 1, for Macroeconomic Theory. Author: Jonathan B. Elliott

## Overview
Project 1 compares the GDP per capita growth rates for Japan, China, and India. Then, examines the change over time of the growth for Japan. Lastly, the project examines the determinants of growth for OECD countries.

## Project Structure
- `01_download_clean.R`: loads the data, handles missing values, builds the variables
- `02_growth_compare.R`: Part 2, growth rates, summary tables and plots for Japan, China, and India
- `03_japan_periods.R`: Part 3, Japan's growth by period
- `04_oecd_determinants.R`: Part 4, OECD correlations, scatterplots and regression

## Data Source
This project gathers its data from the Penn World Table 10.0 (Feenstra, Inklaar and Timmer, 2015). It does this by using the 'pwt10' R package.
The variables used are as follows:
- `rgdpna`: real GDP at constant national prices
- `pop`: population
- `csh_i`: investment share of GDP
- `hc`: human capital index
- `rtfpna`: total factor productivity at constant national prices

## Processing
- GDP per capita is tracked as 'rgdpna / pop'. Growth rates are then seen as log differences multiplied by 100.
- Missing Values: gaps in data that are added using linear interpolation using the 'zoo' package. Data that is missing at the start is ignored and left as missing.
- Growth is compared over 1953 to 2019 due to China's growth first being noted in 1953.

## How to replicate
1. Install R and then RStudio
2. Install the packages using: `install.packages(c("tidyverse", "zoo", "pwt10"))`
3. Open the project using RStudio to ensure the working directory uses the repo folder
4. Lastly, run the scripts in order 01 to 04. 
- This allows 01 to create 'clean_three_countries.csv', which scripts 02 and 03 both use. 
- Script 04 loads a fresh PWT data directly.
