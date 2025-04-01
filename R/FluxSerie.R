
FluxSerie<-function(Data,
                     param,
                    info,
                     plot_series = TRUE,
                     grouplabel,
                     ylab = "Flux (1000t)",
                     facet = TRUE,
                     session) {
  
  # take some random lines that can be drawn consistently among series
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)

  
  Flux_series <- Data %>%
    filter(Var %in% param)%>%
    mutate(
      series = Var
    )
  
  
  Filtered_data<-Data%>%
    filter(Var%in%param)%>%
    rename(series=Var)%>%
    separate(series,sep="_", into =c("ID","Predator"),remove=FALSE)%>%
    left_join(info,by="ID")%>%
    mutate(PreyName=FullName,
           Prey=ID,
           ID=Predator)%>%
    select(-c(FullName,Color))%>%
    left_join(info,by="ID")%>%
    mutate(Predator=ID,
           PredatorName=FullName,
           FullName=paste0("From ",PreyName," to ",PredatorName),
           Color="#27548A")
  
  if (length(Filtered_data) == 0) 
    stop("param not recognized")

  
  quantiles <-Filtered_data%>%
    group_by(Year,FullName,Color)%>%
    summarise(
      quantiles = list(quantile(value, c(0, .025, 0.25, .50, .75, .975, 1))),
      .groups = "drop"
    ) %>%
    unnest_wider(quantiles)%>%
    mutate(
      Year=as.numeric(Year)
    )
  colnames(quantiles)[(ncol(quantiles)-6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  g<-Quantiles_plot(quantiles,Filtered_data,selectedsamples,facet=TRUE,ylab,session)
  
  return(g)
  
}