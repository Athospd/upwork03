#' Viz 01 Porverty Scatterplot
#' 
#' @param data data.frame. Made for use with [upwork03::averages]
#' @param fontsize numeric.
#' 
#' @export
viz_01_poverty_scatterplot <- function(data, fontsize = 16) {
  data = upwork03::averages
  data %>%
    dplyr::filter(Indicator != "Poverty") %>% 
    dplyr::left_join(dplyr::filter(averages, Indicator == "Poverty"), by = "year") %>% 
    dplyr::rename(Indicator = Indicator.x, poverty_rate = average.y) %>% 
    ggplot2::ggplot(ggplot2::aes(average.x, poverty_rate)) +
    ggplot2::geom_point(size = 4, shape = 19, ggplot2::aes(color = Indicator)) +
    ggplot2::geom_text(ggplot2::aes(label = year), size = 2, vjust = 2) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::scale_y_continuous(breaks = seq(17, 20, by = 0.4),
                     labels = seq(17, 20, by = 0.4)) +
    ggplot2::facet_wrap(~ Indicator, scales = "free") +
    ggplot2::labs(x = "", y = "Average Poverty Rate") +
    ggplot2::theme_bw(fontsize) +
    ggplot2::theme(legend.position = "none",
        text = ggplot2::element_text(face = "bold"), title = ggplot2::element_text(size = 7))
}

relabel_vars <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% "Poverty" ~ "Poverty Ratio", 
    x %in% "Health_Exp" ~ "Health Expenditure",
    x %in% "Water" ~ "Drinking Water",
    x %in% "Sanitation" ~ "Sanitation",
    TRUE ~ x
  )
}

data_for_viz_02_correlation_heatmap <- function(year, country) {
  upwork03::all_indicators %>%
    dplyr::filter(year == year, `Country Name` %in% country) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(4:12) %>%
    dplyr::select(-handwashing, -Infant_Mortality) %>% 
    stats::cor(use = "complete.obs") %>% 
    reshape2::melt() %>%
    dplyr::mutate(
      Var1 = relabel_vars(Var1),
      Var2 = relabel_vars(Var2)
    )
}

#' Viz 02 Correlation Heatmap
#' 
#' @param year numeric.
#' @param country char vector.
#' @param fontsize numeric.
#' @param vars vars to be showed on correlation heatmap
#' 
#' @export
viz_02_correlation_heatmap <- function(
  year, 
  country,
  fontsize = 16, 
  vars = c("Poverty Ratio", "Health Expenditure", "Drinking Water", "Sanitation")
) {
  data_for_viz_02_correlation_heatmap(year, country) %>%
    dplyr::filter(
      Var1 %in% vars,
      Var2 %in% vars
    ) %>% 
    ggplot2::ggplot(ggplot2::aes(x=Var1, y=Var2, fill=value)) + 
    ggplot2::geom_tile()+
    ggplot2::geom_text(ggplot2::aes(label = round(value, 2)))+
    ggplot2::scale_fill_gradient2(
      low = "orangered4", 
      high = "royalblue4",
      mid = "white", midpoint = 0,
      limit = c(-1,1), name="Correlation"
    ) +
    ggplot2::labs(
      x = "", 
      y = ""
    ) +
    ggplot2::theme_bw(fontsize) +
    ggplot2::theme(
      rect = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90),
      axis.text = ggplot2::element_text(face = "bold")
    )
}

TMH_to_colour <- function(x) {
  dplyr::case_when(
    x %in% "Malaria" ~ "gold2",
    x %in% "HIV" ~ "cadetblue4",
    x %in% c("Tuberculoses", "Tuberculosis") ~ "indianred",
    TRUE ~ "grey"
  )
}


#' Viz 03 Poverty Ratio
#' 
#' @param year numeric.
#' @param fontsize numeric.
#' 
#' @export
viz_03_poverty_ratio <- function(
  year, 
  country,
  fontsize = 16,
  ylab = "Poverty ratio"
) {
  upwork03::poverty_average %>%
    dplyr::filter(year == year, `Country Name` %in% country) %>% 
    dplyr::mutate(Country_Name = stats::reorder(`Country Name`, Poverty)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = Country_Name, y = Poverty, fill = average_compare, alpha = average_compare))+
    ggplot2::geom_col()+
    # ggplot2::geom_text(ggplot2::aes(x = Country_Name, y = Poverty, label = round(Poverty, 1)), size = 2.5, hjust = 1)+
    ggplot2::scale_alpha_manual(values = c(0.8, 1, 0.6))+
    ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::coord_flip()+
    ggplot2::labs(x ="", y = ylab)+
    ggplot2::theme_bw(fontsize)+
    ggplot2::theme(
      legend.position = "none", 
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(face = "bold")
    )
}


data_for_viz_04_highest_incidence <- function(year, country) {
  upwork03::highest_incidence %>% 
    dplyr::group_by(TMH) %>% 
    dplyr::filter(year == year, `Country Name` %in% country) %>% 
    dplyr::mutate(r = rank(-Incidence, ties.method = "first"), top_10 = r <= 10) %>% 
    dplyr::filter(!is.na(Incidence), Incidence != 0)
}


#' Viz 04 Highest Incidence
#' 
#' @param year numeric.
#' @param TMH character.
#' @param fontsize numeric.
#' 
#' @export
viz_04_highest_incidence <- function(year, TMH, country, fontsize = 16) {
  data_for_viz_04_highest_incidence(year, country) %>% 
    dplyr::filter(TMH == TMH) %>%
    ggplot2::ggplot(ggplot2::aes(x = reorder(`Country Name`, Incidence), y = Incidence, alpha = top_10)) +
    ggplot2::geom_col(fill = TMH_to_colour(TMH))+
    # ggplot2::geom_text(ggplot2::aes(x = reorder(`Country Name`, Incidence), y = Incidence, label = Incidence), size = 2.5, hjust = 1)+
    ggplot2::scale_alpha_manual(values = c(0.6, 1))+
    ggplot2::coord_flip()+
    ggplot2::labs(x ="", y = "Incidence")+
    ggplot2::theme_bw(fontsize)+
    ggplot2::theme(
      legend.position = "none", 
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(face = "bold")
    )
}



data_for_viz_05_incidence_growth <- function(country) {
  upwork03::incidence_growth  %>% 
    dplyr::filter(`Country Name` %in% country) %>%
    dplyr::select(1:3, "Tuberculosis", "Malaria", "HIV") %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(dplyr::across(3:5, mean, na.rm = T)) %>% 
    dplyr::mutate(year = as.numeric(year),
           Tuberculosis_1 = dplyr::lag(Tuberculosis, order_by = year),
           Malaria_1 = dplyr::lag(Malaria, order_by = year),
           HIV_1 = dplyr::lag(HIV, order_by = year)) %>% 
    dplyr::mutate(Tuberculosis_gr = (Tuberculosis - Tuberculosis_1)/Tuberculosis_1,
           Malaria_gr = (Malaria - Malaria_1)/Malaria_1,
           HIV_gr = (HIV - HIV_1)/HIV_1) %>% 
    dplyr::select(year, dplyr::contains("gr")) %>% 
    dplyr::mutate(dplyr::across(2:4, ~ round(.*100, 2))) %>% 
    tidyr::pivot_longer(cols = 2:4, names_to = "Infection", values_to = "Growth_rate") %>% 
    dplyr::mutate(Infection = stringr::str_remove(Infection, "_gr"))
}


#' Viz 05 Poverty Ratio
#' 
#' @param fontsize numeric.
#' 
#' @export
viz_05_incidence_growth <- function(
  country,
  fontsize = 16
) { 
  data_for_viz_05_incidence_growth(country) %>%
  ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = year, y = Growth_rate, fill = Infection), stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(x = year,y = Growth_rate, label = paste(Growth_rate, "%")),
                       vjust = -0.5, size = 3) +
    ggplot2::facet_wrap(~Infection)+
    ggplot2::scale_y_continuous(breaks = seq(-7,4, by = 1),
                                labels = paste0(seq(-7,4, by = 1), "%"))+
    ggplot2::scale_fill_manual(values = c("cadetblue4", "gold2", "indianred"))+
    ggplot2::labs(x = "", 
                  y = "Growth Rate")+
    ggplot2::theme_bw(fontsize) +
    ggplot2::theme(legend.position = "none", panel.grid = ggplot2::element_blank(),
                   text = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 90))
  
}
