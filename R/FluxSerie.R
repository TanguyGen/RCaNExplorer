#' FluxSerie Function
#'
#' This function calculates quantiles for fluxes (transitions between prey and predator) over time,
#' and visualizes them using the `Quantiles_plot` function.
#'
#' @param Data RCaNSample_long data-frame calculated from the RData RCaNSample.
#' @param param A vector of strings specifying the fluxes.
#' @param info A data frame containing additional information for the species, including their names and colors.
#' @param plot_series Logical; whether or not to plot the individual series. Default is TRUE.
#' @param grouplabel A string used to label the fluxes (e.g., a custom label for the flux).
#' @param ylab A string specifying the label for the y-axis of the plot. Default is "Flux (1000t)".
#' @param facet Logical; whether or not to create separate facets for each flux. Default is TRUE.
#' @param session The Shiny session object, which is used to get the width of the plot to adjust text size.
#'
#' @return A `ggplot` object that visualizes the quantiles of the flux data.
#'
#' @import dplyr
#' @import tidyr
#' @import data.table

FluxSerie <- function(Data,
                      param,
                      info,
                      plot_series = TRUE,
                      grouplabel,
                      ylab = "Flux (1000t)",
                      facet = TRUE,
                      session) {
  # Take consistent random samples for overlay
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  
  # Ensure both Data and info are data.tables
  Data <- as.data.table(Data)
  info <- as.data.table(info)
  
  # Filter only needed rows
  Filtered_data <- Data[Var %in% param]
  
  # Split Var into Prey (ID) and Predator
  Filtered_data[, c("ID", "Predator") := tstrsplit(Var, "_")]
  
  # Join species info for Prey
  Filtered_data <- merge(Filtered_data, info, by = "ID", all.x = TRUE)
  Filtered_data[, `:=`(PreyName = FullName,
                       Prey = ID,
                       ID = Predator  # Update ID to Predator ID for next join
                       )]
                       
  # Drop unused columns before next merge
  Filtered_data[, c("FullName", "Color") := NULL]
  
  # Join species info for Predator
  Filtered_data <- merge(Filtered_data, info, by = "ID", all.x = TRUE)
  # Final assignments
  Filtered_data[, `:=`(Predator = ID,
                       PredatorName = FullName,
                       Color = "#27548A")]
  
  Filtered_data[, `:=`(FullName = paste0("From ", PreyName, " to ", PredatorName))]
  
  # Check if the data contains any valid fluxes, stop with an error message if not
  if (nrow(Filtered_data) == 0) {
    stop("param not recognized")
  }
  
  # Calculate quantiles for the flux time series (0%, 2.5%, 25%, 50%, 75%, 97.5%, 100%)
  quantiles <- Filtered_data %>%
    group_by(Year, FullName, Color) %>%
    summarise(quantiles = list(stats::quantile(value, c(
      0, 0.025, 0.25, 0.5, 0.75, 0.975, 1
    ))), .groups = "drop") %>%
    unnest_wider(quantiles) %>%  # Unnest the quantiles into separate columns
    mutate(Year = as.numeric(Year))
  
  # Rename the quantile columns
  colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  # Generate plot
  g <- Quantiles_plot(
    quantiles,
    Filtered_data,
    selectedsamples,
    facet = facet,
    ylab = ylab,
    session = session
  )
  
  return(g)
}
