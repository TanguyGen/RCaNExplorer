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
  
  # Transform to data.table for faster computing
  Data <- data.table::as.data.table(Data)
  
  #  Create the patterns of interest <Prey>_<Targeted species>
  pattern <- paste0("_(", paste(param, collapse = "|"), ")$")
  
  # Filter the data to include only rows with containing the fluxes from the targeted species
  Consumption_data <- Data[grepl(pattern, Var), # Use grepl for faster pattern matching
                        .(ID = tstrsplit(Var, "_")[[1]],  # Extract the prey
                          series = tstrsplit(Var, "_")[[2]], # Extract the series (predator)
                          Year = Year,
                          #Keep the other variables in the Data
                          Sample_id = Sample_id,
                          value = value
                        )]
  # If no matching data is found, stop and display an error message
  if (nrow(Consumption_data) == 0)
    stop("param not recognized")
  
  Consumption_data[, Year := as.numeric(Year)]
  
  # Join species info for Prey
  Consumption_data <- merge(Consumption_data, info, by = "ID", all.x = TRUE)
  Consumption_data[, `:=`(target = FullName,
                          Color_target=Color,
              ID = series  # Update ID to Predator ID for next join
  )]
  
  # Drop unused columns before next merge
  Consumption_data[, c("FullName", "Color") := NULL]
  
  # Join species info for Predator
  Consumption_data <- merge(Consumption_data, info, by = "ID", all.x = TRUE)
  # Final assignments
  Consumption_data[, `:=`(series = FullName)]
  Consumption_data[, c("FullName") := NULL]
  
  # If grouping is enabled, summarize the data by targeted species
  if (group == TRUE & length(param)>1) {
    
    # Summarize by Year and Sample_id
    Consumption_data <- Consumption_data[, .(value = sum(value)), by = .(Year,target, Sample_id,Color_target)]
    
    # Add the 'series' column with grouped label
    Consumption_data[, series := grouplabel]
    
    # Add the grouped label information to the data
    Consumption_data$Color = "#27548A"
    Consumption_data$series = grouplabel
    
  }
  
  # Create a list of plots for each unique series
  listplot <- unique(Consumption_data$series) %>%
    purrr::map(function(.x) {
      # Calculate quantiles for the current series
      
      Data_total <- Consumption_data[series == .x,
                                     .(value = sum(value)),
                                     by = .(Year, Sample_id, series, Color)]
      
      # Calculate quantiles by Year and series
      quantiles <- Data_total[, .(
        quantiles = list(quantile(value, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)))
      ), by = .(Year, series)]
      
      # Expand quantiles into separate columns
      quantiles[, c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100") := transpose(quantiles)]
      
      # Remove the list column
      quantiles[, quantiles := NULL]
      

      # Prepare average consumption data by prey species
      Data_byprey <- Consumption_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series, Color,Color_target) %>%
        summarise(value = mean(value), .groups = "drop")

      # Create the quantile plot for total consumption
      p1 <- Quantiles_plot(quantiles,
                           Data_total,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session) +
        ggtitle(.x)
      

      # Create the proportion plot for consumption by prey species
      p2 <- Proportion_plot(Data_byprey,  session = session)

      # Combine the two plots using patchwork layout
      p <- p1 + p2
      return(p)
    })
  
  
  # Adjust the title size based on plot width
  width <- session$clientData$output_Graphs_width
  bigtitle_size <- max(ceiling(width / 10), 30)
  
  # Combine all individual plots into a single plot with a title
  plot_result <- wrap_plots(listplot, ncol = 1) +
    plot_annotation(
      title = "Consumption series",
      theme = theme(
        text = element_text(size = bigtitle_size)
      )
    )
  
  # Return the final combined plot
  return(plot_result)
}