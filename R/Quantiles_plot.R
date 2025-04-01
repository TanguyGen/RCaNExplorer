

Quantiles_plot<-function(quantiles,Data,selectedsamples,facet,ylab,session){
  
  width <- session$clientData$output_Plots_width
  title_size <- max(ceiling(width / 40), 16)
  axistitle_size <- max(ceiling(width / 60), 12)
  text_size <- max(ceiling(width / 80), 12)
  
  
  
  g <- ggplot() +
    geom_ribbon(data = quantiles,
                aes(x = Year,
                    ymin = q0,
                    ymax = q100,
                    fill=FullName),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes(x = Year,
                    ymin = q2.5,
                    ymax = q97.5,
                    fill=FullName),
                alpha = .33) +
    geom_ribbon(data = quantiles,
                aes(x = Year,
                    ymin = q25,
                    ymax = q75,
                    fill=FullName),
                alpha = .33) +
    ylab(ylab) 
  if (facet){
    g <- g + 
      facet_wrap(~FullName, scales = "free",ncol=1)
  }
    fewseries <- Data%>%
      filter(Sample_id %in% selectedsamples)%>%
      pivot_wider(names_from = Sample_id,values_from = value)%>%
      mutate(
        Year=as.numeric(Year))
    
    colnames(fewseries)[(ncol(fewseries)-2):ncol(fewseries)] <- c("S1", "S2", "S3")
    
    
    g <- g + geom_path(data = fewseries,
                       aes(x = Year,
                           y = S1,
                           color=FullName),
                       lty = "solid") +
      geom_path(data = fewseries,
                aes(x = Year,
                    y = S2,
                    color=FullName),
                lty = "twodash") +
      geom_path(data = fewseries,
                aes(x = Year,
                    y = S3,
                    color=FullName),
                lty = "longdash")+
      ylim(0,NA)+
      theme_classic()+ 
      guides(color="none",fill="none")+
      scale_color_manual(values =  fewseries$Color,labels=NULL)+
      scale_fill_manual(values =  fewseries$Color,labels=NULL)+
      theme(
        title=element_text(size = title_size),
        axis.title = element_text(size = axistitle_size),
        axis.text = element_text(size = text_size),
        legend.title = element_text(size = axistitle_size),
        legend.text = element_text(size = text_size),
        strip.text = element_text(size = title_size)
      )
    
  return(g)
  
}