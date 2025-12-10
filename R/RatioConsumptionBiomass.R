#' Ratio Consumption to Biomass Plot
#'
#' This function calculates the ratio of consumption to biomass for a set of species and
#' generates a plot of the range of this ratio over the iterations of RCaN with the example of 3 trajectories.
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample
#' @param param A character vector containing the species for which the biomass and consumption
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
#' @return A ggplot2 object containing the plot of consumption-to-biomass ratios.
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import data.table

RatioConsumptionBiomass <- function(Data,
                                    param,
                                    info,
                                    plot_series = TRUE,
                                    group,
                                    grouplabel,
                                    ylab = "Unitless",
                                    facet = TRUE,
                                    session) {
  # Transform to data.table for faster computing
  Data <- data.table::as.data.table(Data)%>%
    filter(Trophic==1)
  
  # Select a few sample lines for overlay in the plot
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)

  # Extract Biomass
  Biomass <- Data[Var %in% param, .(
    ID = Var,
    biomass = value,
    Year = as.numeric(Year),
    Sample_id
  )]
  
  # Merge with info and clean
  Biomass <- merge(Biomass, info, by = "ID", all.x = TRUE)
  Biomass[, series := FullName]
  Biomass[, c("FullName") := NULL]
  
  # Pattern to detect relevant consumption fluxes
  pattern <- paste0("_(", paste(param, collapse = "|"), ")$")
  
  
  # Extract Consumption
  Consumptions <- Data[grepl(pattern, Var),
                       .(
                         prey = tstrsplit(Var, "_")[[1]],
                         temp = tstrsplit(Var, "_")[[2]],
                         ID=tstrsplit(Var, "_")[[2]],
                         Year = as.numeric(Year),
                         Sample_id,
                         value
                       )
  ]
  if (nrow(Consumptions) == 0) stop("param not recognized")
  
  # Merge with info and clean
  Consumptions <- merge(Consumptions, info, by = c("temp"="ID"), all.x = TRUE)
  Consumptions[, series := FullName]
  Consumptions[, c("FullName","temp") := NULL]
  
  # Sum consumption by species, year, sample
  Consumptions <- Consumptions[, .(
    consumption = sum(value)
  ), by = .(series,ID, Year, Sample_id, Colour)]
  
  # Merge Biomass and Consumption
  merged_data <- merge(Biomass, Consumptions,
                       by = c("series", "ID","Year", "Sample_id", "Colour"),
                       all = TRUE)
  merged_data[is.na(merged_data)] <- 0
  merged_data <- merged_data[Year != max(Year)]  # Remove last year
  
  # Optional grouping of species into a super-species
  if (group == TRUE & length(param) > 1) {
    merged_data <- merged_data[, .(
      biomass = sum(biomass),
      consumption = sum(consumption)
    ), by = .(Year, Sample_id)]
    merged_data[, `:=`(
      series = grouplabel,
      Colour = "#27548A",
      ID=grouplabel
    )]
    setcolorder(merged_data, c("series","ID", "biomass", "consumption", "Year", "Colour", "Sample_id"))
  }
  
  # Compute ratio
  merged_data[, value := consumption / biomass]
  ratio_data <- merged_data[, .(series,ID, value, Year, Sample_id, Colour)]
  
  
  # Compute quantiles
  quantiles <- ratio_data[, as.list(quantile(value, probs = c(0, .025, .25, .5, .75, .975, 1))),
                          by = .(Year, series,ID, Colour)]
  setnames(quantiles,
           old = c("0%", "2.5%", "25%", "50%", "75%", "97.5%", "100%"),
           new = c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100"))
  
  # Plotting (kept external)
  g <- Quantiles_plot(
    quantiles,
    ratio_data,
    selectedsamples,
    facet = facet,
    ylab = "Ratio Consumption/Biomass",
    session = session
  )
  
  Quantiles <- quantiles%>%
    pivot_longer(cols = starts_with("q"), names_to = "Stat",values_to = "Value")%>%
    mutate(
      Var="Ratio Consumption/Biomass",
      Unit="Unitless"
    )%>%
    select(Year,Var,Unit,ID,Stat,Value)
  
  
  return(
    list(Plot=g,Quant=Quantiles)
  )
}
