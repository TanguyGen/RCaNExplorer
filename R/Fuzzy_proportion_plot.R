#' Proportion Plot
#'
#' Creates a bar plot showing the proportion of the value of each targetted species relative to the total for each Year.
#' This function makes proportion plots for the functions ConsumptionSeries,ProductionSeries, BiomassSeries and FluxSeries.
#'
#' @param Data RCaNSample_long data-frame calculated from the RData RCaNSample.
#' @param info A data frame that contains additional metadata for each target. It should have the columns  `value` ,`series`, `Year`, `target`, and `Colour`.
#' @param session The Shiny session object, used for responsive plot sizing based on the UI.
#'
#' @import dplyr
#' @import ggplot2
#' @import purrr
#'
#' @return A ggplot object representing the proportion bar plot.
#'
#'
Fuzzy_proportion_plot <- function(Data, session) {
  width <- session$clientData$output_Plots_width # Get the width of the plot from the session to adjust text sizes accordingly
  axistitle_size <- max(ceiling(width / 60), 12)  # Adjust axis title size
  text_size <- max(ceiling(width / 80), 12)  # Adjust axis text size

  
  # Function to estimate Beta distribution from data in a given Year
  # and build the fuzzy coverage bands
  make_bands <- function(df) {
    # --- Estimate Beta(shape1 = a, shape2 = b) via method of moments ---
    m   <- mean(df$F)
    v   <- var(df$F)
    eps <- 1e-8
    t   <- (m * (1 - m)) / max(v, eps) - 1
    a   <- max(m * t, eps)             # avoid zero
    b   <- max((1 - m) * t, eps)
    
    # --- Build vertical bins along y in [0, 1] ---
    K  <- 1000                          # resolution (more bins = smoother gradient)
    y  <- seq(0, 1, length.out = K + 1)
    y0 <- head(y, -1)                  # lower edge of bin
    y1 <- tail(y, -1)                  # upper edge of bin
    ym <- 0.5 * (y0 + y1)              # midpoint of bin
    
    # --- Compute coverage probabilities from the Beta CDF ---
    Fy <- pbeta(ym, a, b)              # P(F <= y)
    
    # --- Return bottom and top coverage bands plus summary stats ---
    list(
      bottom = tibble(y0, y1, prob = 1 - Fy),        # "F part" below
      top    = tibble(y0, y1, prob = Fy),            # "M part" above
      mean_F = a / (a + b),                          # mean of Beta
      q05_95 = qbeta(c(0.05, 0.95), a, b)            # 5–95% interval
    )
  }
  
  # Apply to each Year separately
  bands_by_Year <- Data %>%
    group_by(Year) %>%                 # split by Year
    group_map(~ make_bands(.x), .keep = TRUE)
  
  # Collect bottom/top bands into long data frames with Year column
  bottom <- map2_dfr(bands_by_Year, unique(Data$Year),
                     ~ mutate(.x$bottom, Year = .y))
  top    <- map2_dfr(bands_by_Year, unique(Data$Year),
                     ~ mutate(.x$top, Year = .y))
  
  # --- Plot fuzzy bars per Year ---
  ggplot() +
    # Bottom rectangles
    geom_rect(data = bottom,
              aes(xmin = Year - 0.45, xmax = Year + 0.45,
                  ymin = y0, ymax = y1, alpha = prob, fill = "Fishing mortality"),
              color = NA) +
    # Top rectangles
    geom_rect(data = top,
              aes(xmin = Year - 0.45, xmax = Year + 0.45,
                  ymin = y0, ymax = y1, alpha = prob, fill = "Natural mortality"),
              color = NA) +
    scale_fill_manual(
      values = c("Natural mortality" = "#6c9a8b", "Fishing mortality" = "#e8998d" ),
      name   = "Mortality"  # legend title (or leave blank for none)
    ) +
    scale_alpha(range = c(0, 0.9), guide = "none") +
    coord_cartesian(ylim = c(0, 1), expand = FALSE) +
    scale_x_continuous(breaks = seq(min(Data$Year), max(Data$Year), by = 10)) +
    labs(y = "Proportion", x = "Year") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      panel.grid.major.x = element_line(
        colour = 'grey',
        linewidth = 0.25,
        linetype = 'dashed'
      ),
      strip.background = element_blank(),
      axis.title = element_text(size = axistitle_size),
      axis.text = element_text(size = text_size),
      legend.title = element_blank(),
      legend.text = element_text(size = text_size),
      aspect.ratio = 1/1
    )
  
}
