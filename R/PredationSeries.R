#' PredationSeries Function
#'
#' This function calculates and visualizes predation fluxes over time for a set of species. It generates
#' two plots for each species: one for the quantiles of predation and another showing the proportion
#' of each predator.
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
#' @return A `ggplot` object that visualizes the predation data for the selected species, with two plots combined
#'         using `patchwork`.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import patchwork
#' @import data.table
#'
PredationSeries <- function(Data,
                            param,
                            info,
                            group,
                            grouplabel,
                            ylab = "Predation (1000t)",
                            facet = FALSE,
                            session) {
  # Take consistent random samples for overlay in the plot
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  # Rename the ID to a value compatible with the data for further left_join
  info <- info %>%
    rename(series = ID)
  
  # Transform to data.table for faster computing
  Data <- data.table::as.data.table(Data)
  
  #  Create the patterns of interest <Prey>_<Targeted species>
  pattern <- paste0("^(", paste(param, collapse = "|"), ")_") #Get the patterns of interest <Targeted species>_<Predator>
  
  # Filter the data to include only rows with containing the fluxes to the targeted species
  Filtered_data <- Data[grepl(pattern, Var), # Use grepl for faster pattern matching
                        .(target = tstrsplit(Var, "_")[[2]],
                          # Extract the target (predator)
                          series = tstrsplit(Var, "_")[[1]],
                          # Extract the series (prey)
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
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(series = grouplabel)  # Set the grouped label for the series
    
    # Add the grouped label information to the 'info' data frame
    info <- rbind(info, c(grouplabel, grouplabel, "#27548A", FALSE))
  }
  
  # Create a list of plots for each unique series
  listplot <- unique(Filtered_data$series) %>%
    purrr::map(function(.x) {
      # Calculate quantiles for the current series
      quantiles <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        group_by(Year, series) %>%
        summarise(quantiles = list(stats::quantile(
          value, c(0, 0.025, 0.25, 0.50, 0.75, 0.975, 1)
        )),
        .groups = "drop") %>%
        unnest_wider(quantiles) %>% # Unnest the quantiles into separate columns
        mutate(Year = as.numeric(Year))
      
      colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <-
        c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
      
      # Merge the quantiles with species information
      quantiles <- quantiles %>%
        left_join(info, by = "series")
      
      # Prepare total predation data for the plot
      Data_total <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        left_join(info, by = "series")
      
      # Prepare average predation data by target (prey) species
      Data_byPredator <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series) %>%
        summarise(value = mean(value)) %>%
        ungroup() %>%
        left_join(info, by = "series")
      
      # Get the full name of the predator for the plot title
      name <- info %>%
        filter(series == .x) %>%
        pull(FullName)
      
      # Create the first plot (quantiles for total predation)
      p1 <- Quantiles_plot(quantiles,
                           Data_total,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session) +
        ggtitle(name)
      
      # Create the second plot (proportion of predation by predator)
      p2 <- Proportion_plot(Data_byPredator, info = info, session = session)
      
      # Combine the two plots using patchwork layout
      p <- p1 + p2  # patchwork layout
      return(p)
    })
  
  # Get the names of the selected species for the plot title
  Names <- info %>%
    filter(series %in% param) %>%
    select(FullName) %>%
    unique() %>%
    pull(FullName)
  
  # Adjust the title size based on the plot width
  width <- session$clientData$output_Graphs_width
  bigtitle_size <- max(ceiling(width / 20), 20)
  
  # Combine all individual plots into a single plot with a title
  plot_result <- patchwork::wrap_plots(listplot, ncol = 1) +
    patchwork::plot_annotation(
      title = paste("Predation on", paste(Names, collapse = ", ")),
      theme = theme(
        text = element_text(size = bigtitle_size),
        plot.margin = margin(70, 10, 50, 10)
      )
    )
  
  # Return the final combined plot
  return(plot_result)
}
