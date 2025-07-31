# tests/testthat/helper-global_var.R

# No need for `require(testthat)` — testthat loads itself

data.file <- system.file("tests/extdata", "CaNSample_mini.RData", package = "RCaNExplorer")
CaNsample <- load_CaNSample(data.file)
CaNsample_long <- transform_CaNSample(CaNsample)

Info_data <- read.csv(system.file("tests/extdata", "Info_table.csv", package = "RCaNExplorer"))

tested_components <- c("HerbZooplankton", "OmniZooplankton")