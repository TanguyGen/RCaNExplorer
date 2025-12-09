#' MortalitySeries Function
#'
#' This function calculates and visualizes mortality over time for a set species. It generates
#' three plots for each species: one showing the total mortality (Z), one showing the fishing
#' mortality (F) and one showing the total mortality (G) over time with quantiles.
#' It is also possible to group the species to compute the mortality of the group.
#' 
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample
#' @param param A character vector containing the species for which the biomass and consumption ratios will be calculated.
#' @param info A data frame containing additional species information (e.g., FullName, Colour, Biomass).
#' @param group Logical; if TRUE, data will be grouped and summarized. Default is FALSE.
#' @param grouplabel A string that will be used as the label for the grouped flux series when `group = TRUE`.
#' @param facet Logical; if TRUE, the plot will be faceted for each species. Default is FALSE.
#' @param session The Shiny session object, used for adjusting plot text size based on plot width.
#'
#' @return A `ggplot` object that visualizes the predation data for the selected species, with two plots combined
#'         using `patchwork`.
#'
#' @import ggplot2
#' @import patchwork
#' @import data.table
#'
MortalitySeries <- function(Data,
                            param,
                            info,
                            group,
                            grouplabel,
                            ylab="Rate (y-1)",
                            facet = FALSE,
                            session) {
  
  # Select 3 random sample lines for consistent overlay in the plot
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  #standardise names
  Data <- Data %>%
    rename(Sample_id = Sample_id, Year = Year, Var = Var)
  
  # Convert Data to data.table for fast computing
  setDT(Data)
  
  pattern_predation <- paste0("^(", paste(param, collapse = "|"), ")_")
  
  #Biomass at the beginning of Year y
  Biomass <- Data[Var %in% param, .(
    ID = Var,
    biomass = value,
    Sample_id=Sample_id,
    Year=Year
  )]
  Biomass[, Year := as.numeric(Year)]
  
  Biomass <- merge(Biomass, info, by = "ID", all.x = TRUE)
  Biomass[, `:=`(series = FullName)]
  
  Biomass[, c("FullName", "ID") := NULL]
  

  #  Create the patterns of interest <Prey>_<Predator>
  pattern <- paste0("^(", paste(param, collapse = "|"), ")_") 
  
  Flow <- Data[grepl(pattern, Var), # Use grepl for faster pattern matching
                        .(ID = tstrsplit(Var, "_")[[2]], # Extract the predator
                          series = tstrsplit(Var, "_")[[1]], # Extract the series (prey)
                          Year=Year,
                          Sample_id=Sample_id,
                          value=value
                        )]
  Flow[, Year := as.numeric(Year)]
  
  # Join species info for Prey
  Flow <- merge(Flow, info, by = "ID", all.x = TRUE)
  Flow[, `:=`(predator = FullName,
              PredatorID=ID,
              ID = series  # Update ID to Predator ID for next join
  )]
  
  # Drop unused columns before next merge
  Flow[, c("FullName", "Colour") := NULL]
  
  # Join species info for Predator
  Flow <- merge(Flow, info, by = "ID", all.x = TRUE)
  # Final assignments
  Flow[, `:=`(series = FullName)]
  Flow[, c("FullName") := NULL]
  
  if (group == TRUE & length(param)>1) {
    
    # Summarize by Year and Sample_id
    Biomass <- Biomass[, .(biomass = sum(biomass)), by = .(Year, Sample_id)]
    Flow <- Flow[, .(value = sum(value)), by = .(Year,predator, Sample_id,PredatorID)]
    # Add the grouped label information to the 'info' data frame
    Biomass$Colour = "#27548A"
    Flow$Colour = "#27548A" 
    
    Biomass$series = grouplabel
    Flow$series = grouplabel

  }
  
  #Biomass at the beginning of Year y+1
  Biomasst1 <- Biomass[
    , .(series, Sample_id, Year, biomass, Colour)
  ][
    , .(series, Sample_id, Year = Year - 1, biomasst1 = biomass, Colour)
  ][
    Biomass, on = c("series", "Sample_id", "Year","Colour"), nomatch = 0
  ][
    , .(series, Sample_id, Year, biomasst1, Colour)
  ]
  
  Biomass <- Biomass[
    , .SD[Year < max(Year)], 
    by = .(series, Sample_id)
  ]
  
  # Separate flows into catches and predation
  Catches <- Flow[
    startsWith(PredatorID, "F"),
    .(C = sum(value, na.rm = TRUE)),
    by = .(Year, Sample_id, series, Colour)
  ][, C := fifelse(is.na(C), 0, C)]

  Predation <- Flow[
    !startsWith(PredatorID, "F"),
    .(P = sum(value, na.rm = TRUE)),
    by = .(Year, Sample_id, series, Colour)
  ][, P := fifelse(is.na(P), 0, P)]
  

  Mortalities <- merge(Biomass, Biomasst1, by = c("Year", "Sample_id", "series","Colour"), all.x = TRUE) # Join the total catch
  Mortalities <- merge(Mortalities, Catches, by = c("Year", "Sample_id", "series","Colour"), all.x = TRUE) # Join the total catch
  Mortalities <- merge(Mortalities, Predation , by = c("Year", "Sample_id", "series","Colour"), all.x = TRUE) # Join the total predation
  Mortalities[, `:=`(B = biomass, B.t1 = biomasst1)]
  

  Mortalities[, `:=`(
    Z = -log(B.t1 / (B.t1 + C + P))
  ), by = .(series, Sample_id)]
  
  Mortalities[, `:=`(
    F = (C / (C + P)) * Z,
    M = (P / (C + P)) * Z,
    G = log((B.t1 + C + P) / B)
  ), by = .(series, Sample_id)]
  
  Mortalities <- Mortalities[, .SD, .SDcols = c("Year", "Sample_id", "Colour","series","Z","F","M","G")]
  
  # Create a list of plots for each unique series
  listplot <- unique(Mortalities$series) %>%
    purrr::map(function(.x) {
      
      mortal <- Mortalities[series == .x, 
                            .(Mortality = rep(c("Z", "F", "M", "G"), each = .N),
                              value = c(Z, F, M, G)),
                            by = .(Year, series, Colour,Sample_id)]
      quantiles<-mortal
      # Calculate quantiles by group with na.rm = TRUE to avoid errors
      quantiles<-quantiles[, {
        q <- stats::quantile(value, c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1), na.rm = TRUE)
        .(q0 = q[1], q2.5 = q[2], q25 = q[3], q50 = q[4], q75 = q[5], q97.5 = q[6], q100 = q[7])
      }, by = .(Year, series, Colour, Mortality)]
      
      Z_quantiles <- quantiles[Mortality == "Z"][, Mortality := NULL]

      Z_mortal <- mortal[Mortality == "Z"][, Mortality := NULL]
      
      width <- session$clientData$output_Graphs_width
      sub_title_size <- max(ceiling(width / 20), 14)
      
      #Quantile plot for total mortality
      p1 <- Quantiles_plot(Z_quantiles,
                           Z_mortal,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session)+
        labs(title=.x,subtitle = "Total mortality")   +
        theme(
          plot.subtitle = element_text(size = sub_title_size)
        )
      
      
      F_quantiles <- quantiles[Mortality == "F"][, Mortality := NULL]
      F_mortal    <- mortal[Mortality == "F"][, Mortality := NULL]
      
      
      
      p2 <- Quantiles_plot(F_quantiles,
                           F_mortal,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session)+
        labs(subtitle = "Fishing mortality") +
        theme(
          plot.subtitle = element_text(size = sub_title_size)
        )
      
      M_quantiles <- quantiles[Mortality == "M"][, Mortality := NULL]
      M_mortal    <- mortal[Mortality == "M"][, Mortality := NULL]
      
      p3 <- Quantiles_plot(M_quantiles,
                           M_mortal,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session)+
        labs(subtitle = "Natural mortality")   +
        theme(
          plot.subtitle = element_text(size = sub_title_size)
        )
      
      G_quantiles <- quantiles[Mortality == "G"][, Mortality := NULL]
      G_mortal    <- mortal[Mortality == "G"][, Mortality := NULL]
      
      p4 <- Quantiles_plot(G_quantiles,
                           G_mortal,
                           selectedsamples,
                           facet,
                           ylab,
                           session = session)+
        labs(subtitle = "Growth")   +
        theme(
          plot.subtitle = element_text(size = sub_title_size)
        )

      Ratio_mortal<-mortal%>%
        filter(Mortality %in% c("F","M"))%>%
        pivot_wider(
          names_from = Mortality,
          values_from = value
        )
        
      p5<-Fuzzy_proportion_plot(Ratio_mortal,  session = session)+
        labs(subtitle = "Mortality ratio")  +
        theme(
          plot.subtitle = element_text(size = sub_title_size)
        )
        
      
      
      # Combine the two plots using patchwork layout
      p <- (p1 + p2 + p3)/(p4 + p5)
  
      return(p)
    })
  
  width <- session$clientData$output_Graphs_width
  # Adjust the title size based on plot width
  bigtitle_size <- max(ceiling(width / 10), 30)
  
  # Combine all individual plots into a single plot with a title
  plot_result <- wrap_plots(listplot, ncol = 1) +
    plot_annotation(
      title = "Mortality series",
      theme = theme(
        text = element_text(size = bigtitle_size)
      )
    )
  
  # Return the final combined plot
  return(list(Plot=plot_result))
}


