#' Ratio Consumption to Biomass Plot
#'
#' This function calculates the ratio of consumption to biomass for a set of species and
#' generates a plot of the range of this ratio over the iterations of RCaN with the example of 3 trajectories.
#'
#' @param Data RCaNSample_long data-frame computed from the RData RCaNSample 
#' @param param A character vector containing the species for which the biomass and consumption 
#'        ratios will be calculated.
#' @param info A data frame containing metadata about the species, including columns like `ID`, 
#'        `FullName`, `Color`.
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

RatioConsumptionBiomass<- function(Data,
                                   param,
                                   info,
                                   plot_series = TRUE,
                                   group,
                                   grouplabel,
                                   ylab = "Unitless",
                                   facet = TRUE,
                                   session) {
  
  # Take some random lines that can be drawn consistently among series
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  # Rename the ID to a value compatible with the data for further left_join
  info<-info%>%
    rename(series=ID)
  
  #Get the Biomass of each selected species
  Biomasses <- Data %>%
    filter(Var %in% param)%>%
    rename(series = Var, biomass = value)
  
  #Get the consumption of each selected species by summing the value of fluxes entering the targets
  Consumptions <- Data %>%
    filter(stringr::str_detect(Var, paste0("_(", paste(param, collapse = "|"), ")$"))) %>% #Select all flux with the pattern <Prey>_<Targeted species>
    mutate( #Create 2 columns with the names of the Preys and Predators
      Prey = stringr::word(Var, 1, sep = "_"),
      Predator = stringr::word(Var, 2, sep = "_")
    )%>% 
    group_by(Predator,Year,Sample_id) %>%
    summarise(consumption = sum(value), .groups = "drop") %>% #Sum the entering fluxes for each species
    rename(series = Predator)
  
  #Merge the consumptions and biomasses
  merged_data <- full_join(Biomasses, Consumptions, by = c("series","Year","Sample_id")) %>% #Join the biomasses and consumptions for each species
    replace_na(list(biomass = 0, consumption = 0))%>%
    filter(Year!=max(Year)) #Remove the last year because it only contain the biomass
  
  #Merge the series if the user choose to group the species into a super species
  if (group) {
    merged_data <- merged_data %>%
      group_by(Year,Sample_id)%>%
      summarise( #Sum all the biomasses and consumptions for each year and iterations
        biomass = sum(biomass),
        consumption = sum(consumption)
      ) %>%
      ungroup()%>%
      mutate(series = grouplabel)%>%
      select(series, biomass, consumption,Year,Sample_id)
    
    #Create a new line in the metadata describing the new superspecies
    info <- bind_rows(
      info,
      tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE)
    )
  }
  
  #Calculate the ratio consumption/biomass for each series
  ratio_data<-merged_data%>%
    mutate(value=consumption/biomass)%>%
    select(series, value,Year,Sample_id)
  
  # Calculate the quantiles (0%, 2.5%, 25%, 50%, 75%, 97.5%, 100%) for each series,
  # indicating the value below which the specified percentage of iterations fall.
  quantiles <-ratio_data%>%
    group_by(Year,series)%>%
    summarise(
      quantiles = list(quantile(value, c(0, .025, 0.25, .50, .75, .975, 1))),
      .groups = "drop"
    ) %>%
    unnest_wider(quantiles)%>% # Unnest the quantiles into separate columns
    mutate(
      Year=as.numeric(Year)
    )
  colnames(quantiles)[(ncol(quantiles)-6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  quantiles<-quantiles%>%
    left_join(info,by="series") #Add metadata to all the series in order to obtain their fullnames and associated color
  
  
  ratio_data<-ratio_data %>%
    left_join(info,by="series") #Add metadata to all the series in order to obtain their fullnames and associated color
  
  
  g<-Quantiles_plot(quantiles,ratio_data,selectedsamples,facet=TRUE,ylab="Ratio Consumption/Biomass",session) #Make the plot
  
  return(g)
}