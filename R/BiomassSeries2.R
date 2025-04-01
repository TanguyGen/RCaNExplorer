

BiomassSeries2<-function(components,Data,Info_table,session){
  width <- session$clientData$output_Graphs_width
  title_size <- max(ceiling(width / 50), 16)
  axistitle_size <- max(ceiling(width / 40), 12)
  text_size <- max(ceiling(width / 80), 12)
  
      Biomass_series <- Data %>%
        filter(Var %in% components) %>%
        group_by(Year, Var) %>%
        summarise(total_value = sum(value), .groups = 'drop')%>%
        rename(ID=Var)%>%
        left_join(Info_table,by="ID")%>%
        mutate(
          Year = as.numeric(Year),
          Biomass = total_value,
          Component=ID
        )
      # Simplified ggplot
      p <- ggplot(Biomass_series, aes(x = Year, y = Biomass, fill = Component,)) +
        geom_ribbon(aes(ymax=Biomass,fill=Component),ymin=0,alpha=1) +
        geom_point(size=1,aes(color=Component)) +
        labs(
          title = paste("Biomass of", paste(unique(Biomass_series$FullName), collapse = ", ")),
          y = "Biomass (1000t)",
          x = NULL
        ) +
        scale_fill_manual(values = Biomass_series$Color,labels=Biomass_series$FullName)+
        scale_color_manual(values =  Biomass_series$Color,labels=NULL)+
        guides(color = "none") +
        theme_classic() +
        theme(axis.text.x=element_text(angle = 0, hjust = 0.5),
              panel.grid.major.x=element_line(colour = 'grey', linewidth = 0.25, linetype='dashed'),
              legend.position = "none",
              strip.background = element_blank(),
              plot.title = element_text(size = title_size),
              axis.title = element_text(size = axistitle_size)) +
        scale_x_continuous(breaks = c(1990,2000,2010,2020)) +
        scale_y_continuous(limits = c(0, NA),expand = c(0, 0))

      
      # Convert to Plotly with Layout adjustments
      ggplotly(p, tooltip = c("y", "x")) 
      return(ggplotly(p))
}