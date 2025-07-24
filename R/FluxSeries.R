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
                      group,
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
  Flux_data <- Data[Var %in% param]
  
  Flux_data[, Year := as.numeric(Year)]
  
  # Split Var into Prey (ID) and Predator
  Flux_data[, c("ID", "Predator") := tstrsplit(Var, "_")]
  
  # Join species info for Prey
  Flux_data <- merge(Flux_data, info, by = "ID", all.x = TRUE)
  Flux_data[, `:=`(PreyName = FullName,
                       Prey = ID,
                       ID = Predator  # Update ID to Predator ID for next join
                       )]
  
  
  # Drop unused columns before next merge
  Flux_data[, c("FullName", "Color") := NULL]
  
  # Join species info for Predator
  Flux_data <- merge(Flux_data, info, by = "ID", all.x = TRUE)
  # Final assignments
  Flux_data[, `:=`(Predator = ID,
                       PredatorName = FullName)]
  Flux_data[, c("FullName", "Color") := NULL]
  
  Flux_data[, `:=`(series = stringr::str_wrap(paste0("From ", PreyName, " to ", PredatorName), width = 30))]
  
  colors_vector <- c(
    "#5050ff", "#ce3d32", "#749b58", "#f0e685", "#466983", "#ba6338", "#5db1dd",
    "#802268", "#6bd76b", "#d595a7", "#924822", "#837b8d", "#c75127", "#d58f5c",
    "#7a65a5", "#e4af69", "#3b1b53", "#cddeb7", "#612a79", "#ae1f63", "#e7c76f",
    "#5a655e", "#cc9900", "#99cc00", "#a9a9a9", "#cc9900", "#99cc00", "#33cc00",
    "#00cc33", "#00cc99", "#0099cc", "#0a47ff", "#4775ff", "#ffc20a", "#ffd147",
    "#990033", "#991a00", "#996600", "#809900", "#339900", "#00991a", "#009966",
    "#008099", "#003399", "#1a0099", "#660099", "#990080", "#d60047", "#ff1463",
    "#00d68f", "#14ffb1"
  )
  
  # Assign colors to series (loop through colors_vector if more fluxes than colors)
  unique_series <- unique(Flux_data$series)
  n_fluxes <- length(unique_series)
  assigned_colors <- colors_vector[ (seq_len(n_fluxes) - 1) %% length(colors_vector) + 1 ]
  
  color_map <- data.table(series = unique_series, Color = assigned_colors)
  
  # Merge assigned colors back into Flux_data
  Flux_data <- merge(Flux_data, color_map, by = "series", all.x = TRUE)
  
  
  if (group == TRUE & length(param)>1) {
    Series_prop <- Flux_data[, .(value = mean(value)), by = .(Year, series, Color)]
    setnames(Series_prop, "series", "target")
    setnames(Series_prop, "Color", "Color_target")
    
    Flux_data <- Flux_data[, .(value = sum(value)), by = .(Year, Sample_id)]
    Flux_data[, `:=`(series=grouplabel,
                         Color="#27548A"
    )]
  }
  
  # Check if the data contains any valid fluxes, stop with an error message if not
  if (nrow(Flux_data) == 0) {
    stop("param not recognized")
  }
  
  # Calculate quantiles for the flux time series (0%, 2.5%, 25%, 50%, 75%, 97.5%, 100%)
  quantiles <- Flux_data %>%
    group_by(Year, series, Color) %>%
    summarise(quantiles = list(stats::quantile(value, c(
      0, 0.025, 0.25, 0.5, 0.75, 0.975, 1
    ))), .groups = "drop") %>%
    unnest_wider(quantiles)
  
  # Rename the quantile columns
  colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  # Generate plot
  g1 <- Quantiles_plot(
    quantiles,
    Flux_data,
    selectedsamples,
    facet = facet,
    ylab = ylab,
    session = session
  )
  
  if (group==TRUE & length(param)>1){
    g2<-Proportion_plot(Series_prop,  session = session)
    g<- g1 + g2
  }else{
    g<-g1
  }
  
  return(g)
}
