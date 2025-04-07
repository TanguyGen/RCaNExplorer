#' @import dplyr
#' @import tidyr
BiomassSeries <- function(Data,
                          param,
                          info,
                          plot_series = TRUE,
                          group,
                          grouplabel,
                          ylab = "Biomass (1000t)",
                          facet = TRUE,
                          session) {
  
  # Select 3 sample lines for consistent overlay
  selectedsamples <- sample(1:max(Data$Sample_id), size = 3)
  
  info <- info %>%
    rename(series = ID)
  
  Filtered_data <- Data %>%
    filter(Var %in% param) %>%
    rename(series = Var)
  
  if (nrow(Filtered_data) == 0) 
    stop("param not recognized")
  
  if (group == TRUE) {
    Filtered_data <- Filtered_data %>%
      group_by(Year, Sample_id) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(series = grouplabel)
    
    facet <- FALSE
    info <- rbind(info, tibble::tibble(series = grouplabel, FullName = grouplabel, Color = "#27548A", Biomass = FALSE))
  }
  
  quantiles <- Filtered_data %>%
    group_by(Year, series) %>%
    summarise(
      quantiles = list(stats::quantile(value, c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1))),
      .groups = "drop"
    ) %>%
    unnest_wider(quantiles) %>%
    mutate(Year = as.numeric(Year))
  
  colnames(quantiles)[(ncol(quantiles) - 6):ncol(quantiles)] <- c("q0", "q2.5", "q25", "q50", "q75", "q97.5", "q100")
  
  quantiles <- quantiles %>%
    left_join(info, by = "series")
  
  Filtered_data <- Filtered_data %>%
    left_join(info, by = "series")
  
  g <- Quantiles_plot(quantiles, Filtered_data, selectedsamples, facet = facet, ylab = ylab, session = session)
  
  return(g)
}