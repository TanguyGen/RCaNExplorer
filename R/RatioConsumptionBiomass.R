
RatioConsumptionBiomass<- function(Data,
                                   param,
                                   info,
                                   plot_series = TRUE,
                                   group,
                                   grouplabel,
                                   ylab = "Biomass (1000t)",
                                   facet = TRUE,
                                   session) {
  
  # take some random lines that can be drawn consistently among series
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  info<-info%>%
    rename(series=ID)
  
  Biomasses <- Data %>%
    filter(Var %in% param)%>%
    rename(series = Var, biomass = value)
  
  Consumptions <- Data %>%
    filter(stringr::str_detect(Var, paste0("_(", paste(param, collapse = "|"), ")$"))) %>%
    mutate(
      Prey = stringr::word(Var, 1, sep = "_"),
      Predator = stringr::word(Var, 2, sep = "_")
    )%>%
    filter(Predator %in% param) %>%
    group_by(Predator,Year,Sample_id) %>%
    summarise(consumption = sum(value), .groups = "drop") %>%
    rename(series = Predator)
  
  new_data <- full_join(Biomasses, Consumptions, by = c("series","Year","Sample_id")) %>%
    replace_na(list(biomass = 0, consumption = 0))%>%
    filter(Year!=max(Year))

  
  if (group) {
    new_data <- new_data %>%
      group_by(Year,Sample_id)%>%
      summarise(
        biomass = sum(biomass),
        consumption = sum(consumption)
      ) %>%
      ungroup()%>%
      mutate(series = grouplabel)%>%
      select(series, biomass, consumption,Year,Sample_id)
    
    info <- bind_rows(
      info,
      tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE)
    )
  }
  
  new_data<-new_data%>%
    mutate(value=consumption/biomass)%>%
    select(series, value,Year,Sample_id)
  
  quantiles <-new_data%>%
    group_by(Year,series)%>%
    summarise(
      quantiles = list(quantile(value, c(0, .025, 0.25, .50, .75, .975, 1))),
      .groups = "drop"
    ) %>%
    unnest_wider(quantiles)%>%
    mutate(
      Year=as.numeric(Year)
    )
  colnames(quantiles)[(ncol(quantiles)-6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  quantiles<-quantiles%>%
    left_join(info,by="series")
  
  
  new_data<-new_data %>%
    left_join(info,by="series")
  
  
  g<-Quantiles_plot(quantiles,new_data,selectedsamples,facet=TRUE,ylab="Ratio Consumption/Biomass",session)
  
  return(g)
}