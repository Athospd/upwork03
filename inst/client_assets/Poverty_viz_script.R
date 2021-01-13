library(tidyverse)


# Reading in data ---------------------------------------------------------

sheets <- readxl::excel_sheets("inst/client_assets/WDIEXCEL_v2.xlsx")
data_list <- map(sheets, ~ readxl::read_excel("inst/client_assets/WDIEXCEL_v2.xlsx", sheet = .))
names(data_list) <- c("Poverty", "Health_Exp", "Water", "Sanitation", "handwashing", "Tuberculosis", "Malaria", "Infant_Mortality", "HIV")
N <- map(data_list, nrow) %>% unlist()

data_long_list <- map2(data_list, names(data_list),
                       ~pivot_longer(.x, cols = 5:9, names_to = "year", values_to = .y))

all_indicators <- purrr::reduce(data_long_list, full_join, by = c("Country Name", "Country Code", "year")) %>% 
  select(1:2, year, all_of(names(data_list)))



# 1) Poverty Scatterplot -----------------------------------------------------

averages <- readxl::read_excel("inst/client_assets/WDIEXCEL_averages.xlsx", sheet = 1) %>% 
  pivot_longer(2:6, names_to = "year", values_to = "average") %>% 
  rename(Indicator = ...1) %>% 
  mutate(Indicator = str_remove(Indicator, "Average ")) %>% 
  mutate(Indicator = case_when(Indicator == "Health Exp" ~ "Health Expenditure",
                               Indicator == "Water" ~ "Drinking Water",
                               TRUE ~ Indicator))

filter(averages, Indicator != "Poverty") %>% 
  left_join(filter(averages, Indicator == "Poverty"), by = "year") %>% 
  rename(Indicator = Indicator.x, poverty_rate = average.y) %>% 
  ggplot(aes(average.x, poverty_rate))+
  geom_point(size = 4, shape = 19, aes(color = Indicator))+
  geom_text(aes(label = year), size = 2, vjust = 2)+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(breaks = seq(17, 20, by = 0.4),
                     labels = seq(17, 20, by = 0.4))+
  facet_wrap(~ Indicator, scales = "free")+
  labs(x = "", y = "Average Poverty Rate",
       title = "Average poverty rate against average health expenditures as % of GDP,
  against average % of population with basic sanitation, and against average % of population drinking clean water",
       subtitle = "Average of top 40 countries")+
  theme_bw()+
  theme(legend.position = "none",
        text = element_text(face = "bold"), title = element_text(size = 7))

# Correlation Heatmap -----------------------------------------------------

correlation_2017 <- filter(all_indicators, year == 2017) %>% 
  ungroup() %>% select(4:12) %>%
  select(-handwashing, -Infant_Mortality) %>% 
  cor(use = "complete.obs") %>% 
  reshape2::melt()

#A

filter(correlation_2017, Var1 %in% c("Poverty", "Health_Exp", "Water", "Sanitation"),
       Var2 %in% c("Poverty", "Health_Exp", "Water", "Sanitation")) %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(label = round(value, 2)))+
  scale_fill_gradient2(low = "orangered4", high = "royalblue4",
                       mid = "white", midpoint = 0,
                       limit = c(-1,1), name="Correlation")+
  scale_x_discrete(labels = c("Poverty Ratio", "Health Expenditure", "Drinking Water", "Sanitation"))+
  scale_y_discrete(labels = c("Poverty Ratio", "Health Expenditure", "Drinking Water", "Sanitation"))+
  labs(x = "", y = "", title = "Correlation Heatmap",
       subtitle = "Poverty ratio against health expenditure, % of population drinking clean water, and % of population with basic sanitation")+
  theme(rect = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90), axis.text = element_text(face = "bold"))

#B

filter(correlation_2017, Var1 %in% c("Poverty", "Tuberculosis", "Malaria", "HIV"),
       Var2 %in% c("Poverty", "Tuberculosis", "Malaria", "HIV")) %>% 
  ggplot(aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  geom_text(aes(label = round(value, 2)))+
  scale_fill_gradient2(low = "orangered4", high = "royalblue4",
                       mid = "white", midpoint = 0,
                       limit = c(-1,1), name="Correlation")+
  scale_x_discrete(labels = c("Poverty Ratio", "Tuberculosis", "Malaria", "HIV"))+
  scale_y_discrete(labels = c("Poverty Ratio", "Tuberculosis", "Malaria", "HIV"))+
  labs(x = "", y = "", title = "Correlation Heatmap",
       subtitle = "Poverty ratio against the incidence of Tuberculosis, Malaria and HIV")+
  theme(rect = element_blank(), axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 90), axis.text = element_text(face = "bold"))


# 3) Time series trend -------------------------------------------------------

Poverty_average <- data_long_list$Poverty %>% 
  select(`Country Name`, year, Poverty) %>% 
  group_by(year) %>% 
  mutate(Average_poverty = mean(Poverty, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(average_compare = case_when(Poverty > Average_poverty ~ "Above Average",
                                     Poverty < Average_poverty ~ "Below Average"),
         average_compare = ifelse(`Country Name` == "Average", "Average", average_compare))


filter(Poverty_average, year == 2017) %>% 
  mutate(Country_Name = reorder(`Country Name`, Poverty)) %>% 
  ggplot(aes(x = Country_Name, y = Poverty, fill = average_compare, alpha = average_compare))+
  geom_col()+
  geom_text(aes(x = Country_Name, y = Poverty, label = round(Poverty, 1)), size = 2.5, hjust = 1)+
  scale_alpha_manual(values = c(0.8, 1, 0.6))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(x ="", y = "Poverty ratio",
       title = "Poverty ratio in 2017",
       subtitle = "(Above average in red, below average in green)")+
  theme_bw()+
  theme(legend.position = "none", panel.grid = element_blank(),
        text = element_text(face = "bold"))


# 4) Highest Incidence -------------------------------------------------------


Highest_Incidence <- data_list$Poverty %>% 
  slice(-50) %>% 
  select(1:4) %>% 
  left_join(all_indicators) %>% 
  select(1,2, year, Tuberculosis, Malaria, HIV) %>% 
  pivot_longer(cols = c("Tuberculosis", "Malaria", "HIV"), names_to = "TMH", values_to = "Incidence") %>% 
  group_by(TMH) %>% 
  filter(year == 2017) %>% 
  mutate(r = rank(-Incidence, ties.method = "first"), top_10 = r <= 10) %>% 
  filter(!is.na(Incidence), Incidence != 0)

Highest_Incidence %>% filter(TMH == "Tuberculosis") %>% 
  ggplot(aes(x = reorder(`Country Name`, Incidence), y = Incidence, alpha = top_10))+
  geom_col(fill = "indianred")+
  geom_text(aes(x = reorder(`Country Name`, Incidence), y = Incidence, label = Incidence), size = 2.5, hjust = 1)+
  scale_alpha_manual(values = c(0.6, 1))+
  coord_flip()+
  labs(x ="", y = "Incidence",
       title = "Incidence of Tuberculosis in 2017")+
  theme_bw()+
  theme(legend.position = "none", panel.grid = element_blank(),
        text = element_text(face = "bold"))


Highest_Incidence %>% filter(TMH == "Malaria") %>% 
  ggplot(aes(x = reorder(`Country Name`, Incidence), y = Incidence, alpha = top_10))+
  geom_col(fill = "gold2")+
  geom_text(aes(x = reorder(`Country Name`, Incidence), y = Incidence, label = round(Incidence, 1)), size = 3.5, hjust = 0.7)+
  scale_alpha_manual(values = c(0.6, 1))+
  coord_flip()+
  labs(x ="", y = "Incidence",
       title = "Incidence of Malaria in 2017")+
  theme_bw()+
  theme(legend.position = "none", panel.grid = element_blank(),
        text = element_text(face = "bold"))


Highest_Incidence %>% filter(TMH == "HIV") %>% 
  ggplot(aes(x = reorder(`Country Name`, Incidence), y = Incidence, alpha = top_10))+
  geom_col(fill = "cadetblue4")+
  geom_text(aes(x = reorder(`Country Name`, Incidence), y = Incidence, label = round(Incidence, 2)), size = 3, hjust = 1)+
  scale_alpha_manual(values = c(0.6, 1))+
  coord_flip()+
  labs(x ="", y = "Incidence",
       title = "Incidence of HIV in 2017")+
  theme_bw()+
  theme(legend.position = "none", panel.grid = element_blank(),
        text = element_text(face = "bold"))


# 5) Incidence Growth --------------------------------------------------------

Incidence_Growth <- all_indicators %>% 
  select(1:3, "Tuberculosis", "Malaria", "HIV") %>% 
  group_by(year) %>% 
  summarise(across(3:5, mean, na.rm = T)) %>% 
  mutate(year = as.numeric(year),
         Tuberculosis_1 = lag(Tuberculosis, order_by = year),
         Malaria_1 = lag(Malaria, order_by = year),
         HIV_1 = lag(HIV, order_by = year)) %>% 
  mutate(Tuberculosis_gr = (Tuberculosis - Tuberculosis_1)/Tuberculosis_1,
         Malaria_gr = (Malaria - Malaria_1)/Malaria_1,
         HIV_gr = (HIV - HIV_1)/HIV_1) %>% 
  select(year, contains("gr")) %>% 
  filter(year > 2013) %>% 
  mutate(across(2:4, ~ round(.*100, 2))) %>% 
  pivot_longer(cols = 2:4, names_to = "Infection", values_to = "Growth_rate") %>% 
  mutate(Infection = str_remove(Infection, "_gr"))


ggplot(Incidence_Growth)+
  geom_bar(aes(x = year, y = Growth_rate, fill = Infection), stat = "identity")+
  geom_text(aes(x = year,y = Growth_rate, label = paste(Growth_rate, "%")),
            vjust = -0.5, size = 3)+
  facet_wrap(~Infection)+
  scale_y_continuous(breaks = seq(-7,4, by = 1),
                     labels = paste0(seq(-7,4, by = 1), "%"))+
  scale_fill_manual(values = c("cadetblue4", "gold2", "indianred"))+
  labs(x = "", y = "Growth Rate",
       title = "Growth rates trend of average incidence of HIV, Malaria and Tuberculosis")+
  theme_bw()+
  theme(legend.position = "none", panel.grid = element_blank(),
        text = element_text(face = "bold"))
