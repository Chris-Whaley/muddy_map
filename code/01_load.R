# load data

library(tidyverse)  # for data manipulation
library(urbnmapr)   # for plotting map
library(here)       # for easy directory navigation

# county vote data
county_votes <- read_csv(here("data", "raw", "us_countydata.csv"))
