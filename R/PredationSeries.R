#' PredationSeries Function
#'
#' This function calculates and visualizes predation fluxes over time for a set of species. It generates
#' two plots for each species: one for the quantiles of predation and another showing the proportion
#' of each predator.
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample
#' @param param A character vector containing the species for which the biomass and Predation ratios will be calculated.
#' @param info A data frame containing additional species information (e.g., FullName, Colour, Biomass).
#' @param group Logical; if TRUE, data will be grouped and summarized. Default is FALSE.
#' @param grouplabel A string that will be used as the label for the grouped flux series when `group = TRUE`.
#' @param ylab A string specifying the label for the y-axis of the plot. Default is "Predation (1000t)".
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
  
  
  # Transform to data.table for faster computing
  Data <- data.table::as.data.table(Data)
  
  #  Create the patterns of interest <Prey>_<Targeted species>
  pattern <- paste0("^(", paste(param, collapse = "|"), ")_") #Get the patterns of interest <Targeted species>_<Predator>
  
  # Filter the data to include only rows with containing the fluxes to the targeted species
  Predation_data <- Data[grepl(pattern, Var), # Use grepl for faster pattern matching
                        .(ID = tstrsplit(Var, "_")[[2]],
                          # Extract the target (predator)
                          series = tstrsplit(Var, "_")[[1]],
                          # Extract the series (prey)
                          Year = Year,
                          #Keep the other variables in the Data
                          Sample_id = Sample_id,
                          value = value
                        )]
  
  Predation_data[, Year := as.numeric(Year)]
  
  # Join species info for Prey
  Predation_data <- merge(Predation_data, info, by = "ID", all.x = TRUE)
  Predation_data[, `:=`(target = FullName,
                        Colour_target=Colour,
              ID = series  # Update ID to Predator ID for next join
  )]
  
  # Drop unused columns before next merge
  Predation_data[, c("FullName", "Colour") := NULL]
  
  # Join species info for Predator
  Predation_data <- merge(Predation_data, info, by = "ID", all.x = TRUE)
  # Final assignments
  Predation_data[, `:=`(series = FullName)]
  Predation_data[, c("FullName") := NULL]
  
  # If no matching data is found, stop and display an error message
  if (nrow(Predation_data) == 0)
    stop("param not recognized")
  
  # If grouping is enabled, summarize the data by targeted species
  if (group == TRUE & length(param)>1) {
    # Summarize by Year and Sample_id
    Predation_data <- Predation_data[, .(value = sum(value)), by = .(Year,target, Sample_id,Colour_target)]
    
    # Add the grouped label information to the data
    Predation_data$Colour = "#27548A"
    Predation_data$series = grouplabel
  }
  
  listplot <- unique(Predation_data$series) %>%
    purrr::map(function(.x) {
      # Calculate quantiles for the current series
      
      Data_total <- Predation_data[series == .x,
                                     .(value = sum(value)),
                                     by = .(Year, Sample_id, series, Colour)]
      
      # Calculate quantiles by Year and series
      quantiles <- Data_total[, .(
        quantiles = list(quantile(value, probs = c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)))
      ), by = .(Year, series)]
      
      # Expand quantiles into separate columns
      quantiles[, c("q0","q2.5","q25","q50","q75","q97.5","q100") :=
                  as.list(quantiles[[1]]), 
                by = .(Year, series)]
      
      # Remove the list column
      quantiles[, quantiles := NULL]
      
      
      # Prepare average Predation data by prey species
      Data_byprey <- Predation_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series, Colour,Colour_target) %>%
        summarise(value = mean(value), .groups = "drop")
      
      # Create the quantile plot for total Predation
      p1 <- Quantiles_plot(quantiles,
                           Data_total,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session) +
        ggtitle(.x)
      
      
      # Create the proportion plot for Predation by prey species
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
      title = "Predation series",
      theme = theme(
        text = element_text(size = bigtitle_size)
      )
    )
  
  # Return the final combined plot
  return(plot_result)
}