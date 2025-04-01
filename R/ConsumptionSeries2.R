
ConsumptionSeries2<-function(components,Data,Info_table,session){
  width <- session$clientData$output_Graphs_width
  title_size <- max(ceiling(width / 50), 16)
  axistitle_size <- max(ceiling(width / 40), 12)
  text_size <- max(ceiling(width / 80), 12)
  
  Consumption_series <- Data %>%
    filter(str_detect(Var, paste0("_(", paste(components, collapse = "|"), ")$")))%>%
    rename(Flux=Var)%>%
    group_by(Year,Flux) %>%
    summarise(value = median(value))%>%
    ungroup()%>%
    mutate(ID = word(Flux, 1, sep = "_"))%>%
    group_by(Year,ID) %>%
    summarise(Consumption = sum(value), .groups = 'drop')%>%
    ungroup()%>%
    left_join(Info_table,by="ID")%>%
    rename(
      Prey=FullName
    )%>%
    mutate(
      Year = as.numeric(Year)
    )
  name_mapping <- setNames(Info_table$FullName, Info_table$ID)
  # Simplified ggplot
  p <- ggplot(Consumption_series, aes(x = Year, y = Consumption*10^-3, fill = Prey)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(
      title = paste("Consumption of", paste(name_mapping[components], collapse = ", ")),
      y = "Consumption (10000t)",
      x = NULL
    ) +
    scale_fill_manual(values = Consumption_series$Color,labels=Consumption_series$Prey)+
    theme_classic() +
    theme(axis.text.x=element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x=element_line(colour = 'grey', linewidth = 0.25, linetype='dashed'),
          strip.background = element_blank(),
          plot.title = element_text(size = title_size),
          axis.title = element_text(size = axistitle_size)) +
    scale_x_continuous(breaks = c(1990,2000,2010,2020)) +
    scale_y_continuous(limits = c(0, NA),expand = c(0, 0))
  
  
  # Convert to Plotly with Layout adjustments
  return(ggplotly(p))
}