#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import patchwork
PredationSeries <- function(Data,
                            param,
                            info,
                            group,
                            grouplabel,
                            ylab = "Predation (1000t)",
                            facet = FALSE,
                            session) {
  
  # Take consistent random samples for overlay
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  # Prepare info for matching
  info <- info %>%
    rename(series = ID)
  
  # Filter the data based on provided parameter
  Filtered_data <- Data %>%
    filter(stringr::str_detect(Var, paste0("^(", paste(param, collapse = "|"), ")_"))) %>%
    mutate(target = stringr::word(Var, 2, sep = "_"),
                  series = stringr::word(Var, 1, sep = "_"))
  
  # Error handling for no matching data
  if (nrow(Filtered_data) == 0) 
    stop("param not recognized")
  
  # Group data if necessary
  if (group == TRUE) {
    Filtered_data <- Filtered_data %>%
      group_by(Year, target, Sample_id) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(series = grouplabel)
    
    # Add the new group to info
    info <- rbind(info, c(grouplabel, grouplabel, "#27548A", FALSE))
  }
  
  # Prepare the list of plots
  listplot <- unique(Filtered_data$series) %>%
    purrr::map(function(.x) {
      
      # Calculate quantiles for each series
      quantiles <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        group_by(Year, series) %>%
        summarise(
          quantiles = list(stats::quantile(value, c(0, 0.025, 0.25, 0.50, 0.75, 0.975, 1))),
          .groups = "drop"
        ) %>%
        unnest_wider(quantiles) %>%
        mutate(Year = as.numeric(Year))
      
      # Rename quantile columns
      colnames(quantiles)[(ncol(quantiles)-6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
      
      # Add info for the plot
      quantiles <- quantiles %>%
        left_join(info, by = "series")
      
      # Total value data
      Data_total <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        left_join(info, by = "series")
      
      # Data by Predator
      Data_byPredator <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series) %>%
        summarise(value = mean(value)) %>%
        ungroup() %>%
        left_join(info, by = "series")
      
      # Get the full name for the title
      name <- info %>%
        filter(series == .x) %>%
        pull(FullName)
      
      # Create the first plot (quantiles)
      p1 <- Quantiles_plot(quantiles, Data_total, selectedsamples, facet, ylab, session = session) +
        ggtitle(name)
      
      # Create the second plot (proportion by predator)
      p2 <- Proportion_plot(Data_byPredator, info = info, session = session)
      
      # Combine the two plots
      p <- p1 + p2
      
      return(p)
    })
  
  # Extract names for the plot title
  Names <- info %>%
    filter(series %in% param) %>%
    select(FullName) %>%
    unique() %>%
    pull(FullName)
  
  # Adjust title size based on the session width
  width <- session$clientData$output_Graphs_width
  bigtitle_size <- max(ceiling(width / 20), 20)
  
  # Combine all plots into one layout and add a title
  plot_result <- wrap_plots(listplot, ncol = 1) + 
    plot_annotation(
      title = paste("Predation on", paste(Names, collapse = ", ")),
      theme = theme(
        text = element_text(size = bigtitle_size),
        plot.margin = margin(70, 10, 50, 10)
      )
    )
  
  return(plot_result)
}
