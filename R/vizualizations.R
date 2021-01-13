#' Viz 01 Porverty Scatterplot
#' 
#' @param data data.frame. Made for use with [upwork03::averages]
#' 
#' @export
viz_01_poverty_scatterplot <- function(data) {
  data %>%
    dplyr::filter(Indicator != "Poverty") %>% 
    dplyr::left_join(filter(averages, Indicator == "Poverty"), by = "year") %>% 
    dplyr::rename(Indicator = Indicator.x, poverty_rate = average.y) %>% 
    ggplot2::ggplot(ggplot2::aes(average.x, poverty_rate))+
    ggplot2::geom_point(size = 4, shape = 19, aes(color = Indicator))+
    ggplot2::geom_text(aes(label = year), size = 2, vjust = 2)+
    ggplot2::scale_color_brewer(palette = "Set1")+
    ggplot2::scale_y_continuous(breaks = seq(17, 20, by = 0.4),
                     labels = seq(17, 20, by = 0.4))+
    ggplot2::facet_wrap(~ Indicator, scales = "free")+
    ggplot2::labs(x = "", y = "Average Poverty Rate",
       title = "Average poverty rate against average health expenditures as % of GDP,
  against average % of population with basic sanitation, and against average % of population drinking clean water",
       subtitle = "Average of top 40 countries")+
    ggplot2::theme_bw()+
    ggplot2::theme(legend.position = "none",
        text = ggplot2::element_text(face = "bold"), title = ggplot2::element_text(size = 7))
}
