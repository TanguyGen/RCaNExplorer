#' ConsumptionSeries Function
#'
#' This function calculates and visualizes consumption fluxes over time for a set of species. It generates
#' two plots for each species: one for the quantiles of consumption and another showing the proportion
#' of each prey item.
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample
#' @param param A character vector containing the species for which the biomass and consumption ratios will be calculated.
#' @param info A data frame containing additional species information (e.g., FullName, Color, Biomass).
#' @param group Logical; if TRUE, data will be grouped and summarized. Default is FALSE.
#' @param grouplabel A string that will be used as the label for the grouped flux series when `group = TRUE`.
#' @param ylab A string specifying the label for the y-axis of the plot. Default is "Consumption (1000t)".
#' @param facet Logical; if TRUE, the plot will be faceted for each species. Default is FALSE.
#' @param session The Shiny session object, used for adjusting plot text size based on plot width.
#'
#' @return A `ggplot` object that visualizes the consumption data for the selected species, with two plots combined
#'         using `patchwork`.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import patchwork
#' @import data.table
#'
#'
ConsumptionSeries <- function(Data,
                              param,
                              info,
                              group,
                              grouplabel,
                              ylab = "Consumption (1000t)",
                              facet = FALSE,
                              session) {
  # Select a few sample lines for overlay in the plot
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  # Rename the ID to a value compatible with the data for further left_join
  info <- info %>%
    rename(series = ID)
  
  # Transform to data.table for faster computing
  Data <- data.table::as.data.table(Data)
  
  #  Create the patterns of interest <Prey>_<Targeted species>
  pattern <- paste0("_(", paste(param, collapse = "|"), ")$")
  
  # Filter the data to include only rows with containing the fluxes from the targeted species
  Filtered_data <- Data[grepl(pattern, Var), # Use grepl for faster pattern matching
                        .(target = tstrsplit(Var, "_")[[1]],
                          # Extract the target (prey)
                          series = tstrsplit(Var, "_")[[2]],
                          # Extract the series (predator)
                          Year = Year,
                          #Keep the other variables in the Data
                          Sample_id = Sample_id,
                          value = value
                        )]
  # If no matching data is found, stop and display an error message
  if (nrow(Filtered_data) == 0)
    stop("param not recognized")
  
  # If grouping is enabled, summarize the data by targeted species
  if (group == TRUE) {
    Filtered_data <- Filtered_data %>%
      group_by(Year, target, Sample_id) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(series = grouplabel)  # Set the grouped label for the series
    
    # Add the grouped label information to the 'info' data frame and a color
    info <- bind_rows(
      info,
      tibble::tibble(
        series = grouplabel,
        FullName = grouplabel,
        Color = "#27548A",
        Biomass = FALSE
      )
    )
  }
  
  # Create a list of plots for each unique series
  listplot <- unique(Filtered_data$series) %>%
    purrr::map(function(.x) {
      # Calculate quantiles for the current series
      quantiles <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        group_by(Year, series) %>%
        summarise(quantiles = list(stats::quantile(
          value, c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)
        )),
        .groups = "drop") %>%
        unnest_wider(quantiles) %>%  # Unnest the quantiles into separate columns
        mutate(Year = as.numeric(Year))
      
      # Rename the quantile columns for clarity
      colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
      
      # Merge the quantiles with species information
      quantiles <- quantiles %>%
        left_join(info, by = "series")
      
      # Prepare total consumption data for the plot
      Data_total <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        left_join(info, by = "series")
      
      # Prepare average consumption data by prey species
      Data_byprey <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series) %>%
        summarise(value = mean(value), .groups = "drop") %>%
        left_join(info, by = "series")
      
      # Get the full name of the species for the plot title
      name <- info %>%
        filter(series == .x) %>%
        pull(FullName)
      
      # Create the quantile plot for total consumption
      p1 <- Quantiles_plot(quantiles,
                           Data_total,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session) +
        ggtitle(name)
      
      # Create the proportion plot for consumption by prey species
      p2 <- Proportion_plot(Data_byprey, info = info, session = session)
      
      # Combine the two plots using patchwork layout
      p <- p1 + p2
      return(p)
    })
  
  # Get the names of the selected species from 'info'
  Names <- info %>%
    filter(series %in% param) %>%
    distinct(FullName) %>%
    pull(FullName)
  
  # Adjust the title size based on plot width
  width <- session$clientData$output_Graphs_width
  bigtitle_size <- max(ceiling(width / 10), 30)
  
  # Combine all individual plots into a single plot with a title
  plot_result <- wrap_plots(listplot, ncol = 1) +
    plot_annotation(
      title = "Consumption series",
      theme = theme(
        text = element_text(size = bigtitle_size),
        plot.margin = margin(70, 10, 50, 10)
      )
    )
  
  # Return the final combined plot
  return(plot_result)
}