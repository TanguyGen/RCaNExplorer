#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import patchwork
ConsumptionSeries <- function(Data,
                              param,
                              info,
                              group,
                              grouplabel,
                              ylab = "Consumption (1000t)",
                              facet = FALSE,
                              session) {
  
  # Select sample lines for overlay
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  info <- info %>%
    rename(series = ID)
  
  Filtered_data <- Data %>%
    filter(stringr::str_detect(Var, paste0("_(", paste(param, collapse = "|"), ")$"))) %>%
    mutate(
      target = stringr::word(Var, 1, sep = "_"),
      series = stringr::word(Var, 2, sep = "_")
    )
  
  if (nrow(Filtered_data) == 0) 
    stop("param not recognized")
  
  if (group == TRUE) {
    Filtered_data <- Filtered_data %>%
      group_by(Year, target, Sample_id) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(series = grouplabel)
    
    info <- bind_rows(
      info,
      tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE)
    )
  }
  
  listplot <- unique(Filtered_data$series) %>%
    purrr::map(function(.x) {
      quantiles <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        group_by(Year, series) %>%
        summarise(
          quantiles = list(stats::quantile(value, c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))),
          .groups = "drop"
        ) %>%
        unnest_wider(quantiles) %>%
        mutate(Year = as.numeric(Year))
      
      colnames(quantiles)[(ncol(quantiles)-6):ncol(quantiles)] <- 
        c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
      
      quantiles <- quantiles %>%
        left_join(info, by = "series")
      
      Data_total <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, Sample_id, series) %>%
        summarise(value = sum(value), .groups = "drop") %>%
        left_join(info, by = "series")
      
      Data_byprey <- Filtered_data %>%
        filter(series == .x) %>%
        group_by(Year, target, series) %>%
        summarise(value = mean(value), .groups = "drop") %>%
        left_join(info, by = "series")
      
      name <- info %>%
        filter(series == .x) %>%
        pull(FullName)
      
      p1 <- Quantiles_plot(quantiles, Data_total, selectedsamples, facet, ylab, session = session) +
        ggtitle(name)
      
      p2 <- Proportion_plot(Data_byprey, info = info, session = session)
      
      p <- p1 + p2  # patchwork layout
      return(p)
    })
  
  Names <- info %>%
    filter(series %in% param) %>%
    distinct(FullName) %>%
    pull(FullName)
  
  width <- session$clientData$output_Graphs_width
  bigtitle_size <- max(ceiling(width / 20), 40)
  
  wrap_plots(listplot, ncol = 1) +
    plot_annotation(
      title = paste("Consumption by", paste(Names, collapse = ", ")),
      theme = theme(
        text = element_text(size = bigtitle_size),
        plot.margin = margin(70, 10, 50, 10)
      )
    )
}