
Proportion_plot<-function(Data,info,session){
  
  width <- session$clientData$output_Plots_width
  axistitle_size <- max(ceiling(width / 60), 12)
  text_size <- max(ceiling(width / 80), 12)
  
  info<-info%>%
    rename(target=series)
  
  Data<-Data%>%
    select(Year,target,value)%>%
    group_by(Year) %>%
    mutate(proportion = (value / sum(value,na.rm = TRUE))) %>%
    ungroup()%>%
    left_join(info,by="target")%>%
    mutate(Year=as.numeric(Year))
  
  
  g <- ggplot(Data, aes(x = Year, y = proportion, fill = target)) +
    geom_bar(stat = "identity", width = 1) +
    labs(
      y = "",
      x = "Year"
    ) +
    scale_fill_manual(values = Data$Color,labels=Data$FullName)+
    theme_classic() +
    theme(axis.text.x=element_text(angle = 0, hjust = 0.5),
          panel.grid.major.x=element_line(colour = 'grey', linewidth = 0.25, linetype='dashed'),
          strip.background = element_blank(),
          axis.title = element_text(size = axistitle_size),
          axis.text = element_text(size = text_size),
          legend.title = element_blank(),
          legend.text = element_text(size = text_size))
}      