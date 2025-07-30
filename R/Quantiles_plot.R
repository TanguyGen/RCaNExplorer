#' Quantiles Plot with Confidence Intervals and Random trajectories
#'
#' This function generates a plot with confidence ribbons for quantiles (0%, 2.5%, 25%, 50%, 75%, 97.5%, 100%)
#' and sample lines representing selected data. It includes options to facet the plot and customize axis labels.
#'
#' @param quantiles A data frame containing quantile values (e.g., q0, q2.5, q25, q50, q75, q97.5, q100). Must include columns `Year`, `series`, and the quantiles.
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample.
#' @param selectedsamples A vector of random trajectories IDs (e.g., 1, 2, 3) that will be used to plot trajectories.
#' @param facet A logical value indicating whether the plot should be faceted by `series`.
#' @param ylab A character string for the label of the y-axis.
#' @param session The Shiny session object, passed to retrieve the width of the plot area from the client.
#'
#' @return A ggplot2 object that contains the quantiles plot, including confidence ribbons and sample lines.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2

Quantiles_plot <- function(quantiles,
                           Data,
                           selectedsamples,
                           facet,
                           ylab,
                           session) {
  width <- session$clientData$output_Plots_width # Get the width of the plot from the session to adjust text sizes accordingly
  title_size <- max(ceiling(width / 40), 16)  # Adjust title size based on plot width
  axistitle_size <- max(ceiling(width / 60), 12)  # Adjust axis title size
  text_size <- max(ceiling(width / 80), 12)  # Adjust axis text size
  
  # Base plot with confidence ribbons
  g <- ggplot() +
    geom_ribbon(data = quantiles,
                aes(
                  x = Year,
                  ymin = q0,
                  ymax = q100,
                  fill = series
                ),
                alpha = 0.33) +
    geom_ribbon(
      data = quantiles,
      aes(
        x = Year,
        ymin = q2.5,
        ymax = q97.5,
        fill = series
      ),
      alpha = 0.33
    ) +
    geom_ribbon(data = quantiles,
                aes(
                  x = Year,
                  ymin = q25,
                  ymax = q75,
                  fill = series
                ),
                alpha = 0.33) +
    ylab(ylab)
  
  # Facet option
  if (facet) {
    g <- g + facet_wrap( ~ series, scales = "free", ncol = 2)
  }
  # Prepare sample lines
  fewseries <- Data %>%
    filter(Sample_id %in% selectedsamples) %>%
    pivot_wider(names_from = Sample_id, values_from = value) 
  
  # Rename last columns to S1, S2, S3
  colnames(fewseries)[(ncol(fewseries) - 2):ncol(fewseries)] <- c("S1", "S2", "S3")
  
  colour_map <- fewseries %>%
    distinct(series, Colour) %>%
    tibble::deframe()


  # Add sample lines to plot
  g <- g +
    geom_path(data = fewseries,
              aes(x = Year, y = S1, group = series),
              lty = "solid") +
    geom_path(data = fewseries,
              aes(x = Year, y = S2, group = series),
              lty = "twodash") +
    geom_path(data = fewseries,
              aes(x = Year, y = S3, group = series),
              lty = "longdash") +
    ylim(0, NA) +
    theme_classic() +
    guides(group = "none", fill = "none") +
    scale_colour_manual(values = colour_map) +
    scale_fill_manual(values = colour_map) +
    theme(
      title = element_text(size = title_size),
      axis.title = element_text(size = axistitle_size),
      axis.text = element_text(size = text_size),
      legend.title = element_text(size = axistitle_size),
      legend.text = element_text(size = text_size),
      strip.text = element_text(size = title_size),
      aspect.ratio = 1/1
    )
  return(g)
}
