## code to prepare `all_indicators` dataset goes here
library(tidyverse)

# all_indicators ----------------------------------------------------------

sheets <- readxl::excel_sheets("inst/client_assets/WDIEXCEL_v2.xlsx")
data_list <- map(sheets, ~ readxl::read_excel("inst/client_assets/WDIEXCEL_v2.xlsx", sheet = .))
names(data_list) <- c("Poverty", "Health_Exp", "Water", "Sanitation", "handwashing", "Tuberculosis", "Malaria", "Infant_Mortality", "HIV")
N <- map(data_list, nrow) %>% unlist()

data_long_list <- map2(data_list, names(data_list),
                       ~pivot_longer(.x, cols = 5:9, names_to = "year", values_to = .y))

all_indicators <- purrr::reduce(data_long_list, full_join, by = c("Country Name", "Country Code", "year")) %>% 
  select(1:2, year, all_of(names(data_list)))

usethis::use_data(all_indicators, overwrite = TRUE)


# poverty_avarage ---------------------------------------------------------


poverty_average <- data_long_list$Poverty %>% 
  select(`Country Name`, year, Poverty) %>% 
  group_by(year) %>% 
  mutate(Average_poverty = mean(Poverty, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(average_compare = case_when(Poverty > Average_poverty ~ "Above Average",
                                     Poverty < Average_poverty ~ "Below Average"),
         average_compare = ifelse(`Country Name` == "Average", "Average", average_compare))



usethis::use_data(poverty_average, overwrite = TRUE)



# highest incidence -------------------------------------------------------

highest_incidence <- data_list$Poverty %>% 
  slice(-50) %>% 
  select(1:4) %>% 
  left_join(all_indicators) %>% 
  select(1,2, year, Tuberculosis, Malaria, HIV) %>% 
  pivot_longer(cols = c("Tuberculosis", "Malaria", "HIV"), names_to = "TMH", values_to = "Incidence")

usethis::use_data(highest_incidence, overwrite = TRUE)


# incidence growth --------------------------------------------------------

incidence_growth <- all_indicators

usethis::use_data(incidence_growth, overwrite = TRUE)
