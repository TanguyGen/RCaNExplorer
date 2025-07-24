#' Proportion Plot
#'
#' Creates a bar plot showing the proportion of the value of each targetted species relative to the total for each year.
#' This function makes proportion plots for the functions ConsumptionSeries,ProductionSeries, BiomassSeries and FluxSeries.
#'
#' @param Data RCaNSample_long data-frame calculated from the RData RCaNSample.
#' @param info A data frame that contains additional metadata for each target. It should have the columns  `value` ,`series`, `Year`, `target`, and `Color`.
#' @param session The Shiny session object, used for responsive plot sizing based on the UI.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return A ggplot object representing the proportion bar plot.
#'
#'
Proportion_plot <- function(Data, session) {
  width <- session$clientData$output_Plots_width # Get the width of the plot from the session to adjust text sizes accordingly
  title_size <- max(ceiling(width / 40), 16)  # Adjust title size based on plot width
  axistitle_size <- max(ceiling(width / 60), 12)  # Adjust axis title size
  text_size <- max(ceiling(width / 80), 12)  # Adjust axis text size
  

  
  Data <- Data %>%
    select(Year, target, value,Color_target) %>%
    group_by(Year) %>%
    mutate(proportion = value / sum(value, na.rm = TRUE)) %>%
    ungroup()

  # Create named vector of colors with target names
  color_map <- unique(Data[, c("target", "Color_target")])
  # Convert to named vector for scale_fill_manual
  color_map <- setNames(color_map$Color_target, color_map$target)


  g <- ggplot(Data, aes(x = Year, y = proportion, fill = target)) +
    geom_bar(stat = "identity",
             width = 1,
             colour = "black") +
    labs(y = "", x = "Year") +
    scale_fill_manual(values = color_map) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_line(
        colour = 'grey',
        linewidth = 0.25,
        linetype = 'dashed'
      ),
      strip.background = element_blank(),
      title = element_text(size = title_size),
      axis.title = element_text(size = axistitle_size),
      axis.text = element_text(size = text_size),
      legend.title = element_blank(),
      legend.text = element_text(size = text_size),
      aspect.ratio = 1/1
    )
  
  return(g)
}
