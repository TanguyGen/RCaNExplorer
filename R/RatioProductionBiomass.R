
RatioProductionBiomass<- function(Data,
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
  
  Productions <- Data %>%
    filter(stringr::str_detect(Var, paste0("^(", paste(param, collapse = "|"), ")_"))) %>%
    mutate(
      Prey = stringr::word(Var, 1, sep = "_"),
      Predator = stringr::word(Var, 2, sep = "_")
    )%>%
    filter(Prey %in% param) %>%
    group_by(Prey,Year,Sample_id) %>%
    summarise(production = sum(value), .groups = "drop") %>%
    rename(series = Prey)
  
  new_data <- full_join(Biomasses, Productions, by = c("series","Year","Sample_id")) %>%
    replace_na(list(biomass = 0, production = 0))%>%
    filter(Year!=max(Year))
  
  
  if (group) {
    new_data <- new_data %>%
      group_by(Year,Sample_id)%>%
      summarise(
        biomass = sum(biomass),
        production = sum(production)
      ) %>%
      ungroup()%>%
      mutate(series = grouplabel)%>%
      select(series, biomass, production,Year,Sample_id)
    
    info <- bind_rows(
      info,
      tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE)
    )
  }
  
  new_data<-new_data%>%
    mutate(value=production/biomass)%>%
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
  
  
  g<-Quantiles_plot(quantiles,new_data,selectedsamples,facet=TRUE,ylab="Ratio Production/Biomass",session)
  
  return(g)
}