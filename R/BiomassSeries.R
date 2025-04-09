#' BiomassSeries Function
#'
#' This function calculates and visualizes biomass data over time for a set of species. It generates a plot 
#' showing quantiles of biomass for each species over the years. 
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample 
#' @param param A character vector containing the species for which the biomass and consumption ratios will be calculated.
#' @param info A data frame containing additional species information (e.g., FullName, Color, Biomass).
#' @param plot_series Logical; if TRUE, plots for each series will be created. Default is TRUE.
#' @param group Logical; if TRUE, data will be grouped and summarized by year and sample ID. Default is FALSE.
#' @param grouplabel A string to be used as the label for the grouped series when `group = TRUE`.
#' @param ylab A string specifying the label for the y-axis of the plot. Default is "Biomass (1000t)".
#' @param facet Logical; if TRUE, the plot will be faceted for each species. Default is TRUE.
#' @param session The Shiny session object, used for adjusting plot text size based on plot width.
#'
#' @return A `ggplot` object that visualizes the biomass data for the selected species, showing quantiles over time.
#'
#' @import dplyr
#' @import tidyr
#' @import data.table

BiomassSeries <- function(Data,
                          param,
                          info,
                          plot_series = TRUE,
                          group,
                          grouplabel,
                          ylab = "Biomass (1000t)",
                          facet = TRUE,
                          session) {
  
  # Select 3 sample lines for consistent overlay in the plot
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  # Rename the ID to a value compatible with the data for further left_join
  info <- info %>%
    rename(series = ID)
  
  Data<-data.table(Data)
  
  # Filter the data to include only biomasses of targeted species
  Filtered_data <- Data[Var %in% param, .(series = Var, value = value,Year=Year,Sample_id=Sample_id)]
  
  # If no matching data is found, stop and display an error message
  if (nrow(Filtered_data) == 0) 
    stop("param not recognized")
  
  # If grouping is enabled, summarize the data by year and sample ID
  if (group == TRUE) {
    Filtered_data <- Filtered_data %>%
      group_by(Year, Sample_id) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(series = grouplabel)  # Set the grouped label for the series
    
    # Add the grouped label information to the 'info' data frame
    info <- rbind(info, tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE))
  }
  
  # Calculate quantiles (0%, 2.5%, 25%, 50%, 75%, 97.5%, and 100%) for biomass over time
  quantiles <- Filtered_data %>%
    group_by(Year, series) %>%
    summarise(
      quantiles = list(stats::quantile(value, c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))),
      .groups = "drop"
    ) %>%
    unnest_wider(quantiles) %>%
    mutate(Year = as.numeric(Year)) 
  
  colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <- 
    c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  # Merge the quantiles with species information
  quantiles <- quantiles %>%
    left_join(info, by = "series")
  
  # Merge the filtered data with species information
  Filtered_data <- Filtered_data %>%
    left_join(info, by = "series")
  
  # Generate the plot using the 'Quantiles_plot' function
  g <- Quantiles_plot(quantiles, Filtered_data, selectedsamples, facet = facet, ylab = ylab, session = session)
  
  # Return the generated plot
  return(g)
}
