
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
  
  Filtered_data<-Data%>%
    filter(grepl(paste0(param, collapse = "$|"), Var))%>%
    mutate(series=Var,
           isBiomass=series %in% param)
  
  if (length(Filtered_data) == 0) 
    stop("param not recognized")
  
  if (group==TRUE){
    Filtered_data<-Filtered_data%>%
      group_by(Year,Sample_id,isBiomass)%>%
      summarise(value=sum(value))%>%
      mutate(series=grouplabel)
    facet=FALSE
    info <- rbind(info, c(grouplabel,grouplabel,"#27548A",FALSE))
  }
  
  quantiles <-Filtered_data%>%
    group_by(Year,isBiomass)
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
  
  Filtered_data<-Filtered_data %>%
    left_join(info,by="series")
  
  g<-Quantiles_plot(quantiles,Filtered_data,selectedsamples,facet=TRUE,ylab,session)
  
  return(g)
}