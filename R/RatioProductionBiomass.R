#' Ratio Production to Biomass Plot
#'
#' This function calculates the ratio of production to biomass for a set of species and
#' generates a plot of the range of this ratio over the iterations of RCaN with the example of 3 trajectories.
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample
#' @param param A character vector containing the species for which the biomass and production
#'        ratios will be calculated.
#' @param info A data frame containing metadata about the species, including columns like `ID`,
#'        `FullName`, `Colour`.
#' @param plot_series If yes, draw three example of trajectories. Default is `TRUE`.
#' @param group A logical value indicating whether the species should be grouped together. Default is `FALSE`.
#' @param grouplabel A character string representing the label for the grouped species if `group` is `TRUE`.
#'        Default is `"Default Group Name"`.
#' @param ylab A character string for the label of the y-axis. Default is `"Unitless"`.
#' @param facet A logical value indicating whether to facet the plot by species. Default is `TRUE`.
#' @param session The Shiny session object, passed for client-side session data.
#'
#' @return A ggplot2 object containing the plot of production-to-biomass ratios.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import data.table

RatioProductionBiomass <- function(Data,
                                   param,
                                   info,
                                   plot_series = TRUE,
                                   group,
                                   grouplabel,
                                   ylab = "Unitless",
                                   facet = TRUE,
                                   session) {
  # Select 3 random samples for illustrative trajectories
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  # Ensure Data is a data.table
  Data <- data.table::as.data.table(Data)
  
  # --- Biomass Extraction ---
  Biomass <- Data[Var %in% param, .(
    ID = Var,
    biomass = value,
    Year = as.numeric(Year),
    Sample_id
  )]
  Biomass <- merge(Biomass, info, by = "ID", all.x = TRUE)
  Biomass[, series := FullName]
  Biomass[, c("FullName", "ID") := NULL]
  
  # --- Production Extraction ---
  pattern <- paste0("^(", paste(param, collapse = "|"), ")_")
  Productions <- Data[grepl(pattern, Var), .(
    ID = tstrsplit(Var, "_")[[1]],
    Predator = tstrsplit(Var, "_")[[2]],
    value = value,
    Year = as.numeric(Year),
    Sample_id
  )]
  
  if (nrow(Productions) == 0) stop("param not recognized")
  
  Productions <- merge(Productions, info, by = "ID", all.x = TRUE)
  Productions[, series := FullName]
  Productions[, c("FullName", "ID") := NULL]
  
  Productions <- Productions[, .(
    production = sum(value)
  ), by = .(series, Year, Sample_id, Colour)]
  
  # --- Merge Biomass and Production ---
  merged_data <- merge(Biomass, Productions,
                       by = c("series", "Year", "Sample_id", "Colour"),
                       all = TRUE)
  merged_data[is.na(merged_data)] <- 0
  merged_data <- merged_data[Year != max(Year)]
  
  # --- Optional Grouping ---
  if (group == TRUE & length(param) > 1) {
    merged_data <- merged_data[, .(
      biomass = sum(biomass),
      production = sum(production)
    ), by = .(Year, Sample_id)]
    merged_data[, `:=`(
      series = grouplabel,
      Colour = "#27548A"
    )]
    setcolorder(merged_data, c("series", "biomass", "production", "Year", "Colour", "Sample_id"))
  }
  
  # --- Compute Ratio ---
  merged_data[, value := production / biomass]
  ratio_data <- merged_data[, .(series, value, Year, Sample_id, Colour)]
  
  # --- Quantile Calculation ---
  quantiles <- ratio_data[, as.list(quantile(value, probs = c(0, .025, .25, .5, .75, .975, 1))),
                          by = .(Year, series, Colour)]
  setnames(quantiles,
           old = c("0%", "2.5%", "25%", "50%", "75%", "97.5%", "100%"),
           new = c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100"))
  
  # --- Plot ---
  g <- Quantiles_plot(
    quantiles,
    ratio_data,
    selectedsamples,
    facet = facet,
    ylab = "Ratio Production/Biomass",
    session = session
  )
  
  return(g)
}
