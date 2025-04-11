#' Proportion Plot
#'
#' Creates a bar plot showing the proportion of the value of each targetted species relative to the total for each year.
#' This function makes proportion plots for the functions ConsumptionSeries and ProductionSeries.
#'
#' @param Data RCaNSample_long data-frame calculated from the RData RCaNSample.
#' @param info A data frame that contains additional metadata for each target. It should have the columns `series`, `FullName`, and `Color`.
#' @param session The Shiny session object, used for responsive plot sizing based on the UI.
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return A ggplot object representing the proportion bar plot.
#'
#'
Proportion_plot <- function(Data, info, session) {
  width <- session$clientData$output_Plots_width # Get the width of the plot from the session to adjust text sizes accordingly
  axistitle_size <- max(ceiling(width / 60), 12)  # Adjust axis title size
  text_size <- max(ceiling(width / 80), 12)  # Adjust axis text size
  
  info <- info %>% #Rename the metadata to assign a name to the preys or predators
    rename(target = series)
  
  Data <- Data %>%
    select(Year, target, value) %>%
    group_by(Year) %>%
    mutate(proportion = value / sum(value, na.rm = TRUE)) %>% #Get the proportion of yearly biomass of the targeted species eaten by predators or the biomass consumed by the targeted species
    ungroup() %>%
    left_join(info, by = "target") %>%
    mutate(Year = as.numeric(Year))
  
  g <- ggplot(Data, aes(x = Year, y = proportion, fill = target)) +
    geom_bar(stat = "identity",
             width = 1,
             colour = "black") +
    labs(y = "", x = "Year") +
    scale_fill_manual(values = Data$Color, labels = Data$FullName) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_line(
        colour = 'grey',
        linewidth = 0.25,
        linetype = 'dashed'
      ),
      strip.background = element_blank(),
      axis.title = element_text(size = axistitle_size),
      axis.text = element_text(size = text_size),
      legend.title = element_blank(),
      legend.text = element_text(size = text_size),
      aspect.ratio = 1/1
    )
  
  return(g)
}
