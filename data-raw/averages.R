## code to prepare `averages` dataset goes here
library(tidyverse)

averages <- readxl::read_excel("inst/client_assets/WDIEXCEL_averages.xlsx", sheet = 1) %>% 
  pivot_longer(2:6, names_to = "year", values_to = "average") %>% 
  rename(Indicator = ...1) %>% 
  mutate(Indicator = str_remove(Indicator, "Average ")) %>% 
  mutate(Indicator = case_when(Indicator == "Health Exp" ~ "Health Expenditure",
                               Indicator == "Water" ~ "Drinking Water",
                               TRUE ~ Indicator))

usethis::use_data(averages, overwrite = TRUE)
