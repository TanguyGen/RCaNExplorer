#' Validate and Clean Metadata Data Frame
#'
#' This function takes a data frame assumed to be metadata from a CSV upload,
#' verifies the presence of required columns (`ID`, `FullName`, and `Colour` or `Color`),
#' replaces the `Color` column with `Colour` if needed,
#' checks for empty or missing values in required columns,
#' replaces all `NA`s with empty strings, and returns a cleaned data frame.
#' If the required columns are missing or all values in these columns are empty,
#' it returns an empty data frame and triggers a Shiny notification (requires shiny).
#'
#' @param df A data frame read from the user-uploaded CSV metadata file.
#' @return A cleaned data frame with columns `ID`, `FullName`, and `Colour`, 
#' or an empty data frame if validation fails.
#' @importFrom shiny showNotification

validate_metadata <- function(df) {
  # Accept 'Color' as alias for 'Colour'
  if ("Color" %in% colnames(df) && !"Colour" %in% colnames(df)) {
    df$Colour <- df$Color
    df$Color <- NULL
  }
  
  required_cols <- c("ID", "FullName", "Colour")
  
  # Check required columns exist
  if (!all(required_cols %in% colnames(df))) {
    shiny::showNotification("The CSV must contain columns: ID, FullName, and Colour (or Color).", type = "error")
    return(data.frame(ID = character(), FullName = character(), Colour = character(), stringsAsFactors = FALSE))
  }
  
  # Trim whitespace and standardize empty strings
  df[required_cols] <- lapply(df[required_cols], function(col) {
    col <- trimws(as.character(col))
    col[col == ""] <- NA
    col
  })
  
  # Check if all required columns are empty
  if (all(sapply(df[required_cols], function(col) all(is.na(col))))) {
    shiny::showNotification("The metadata file contains only empty values in required columns.", type = "error")
    return(data.frame(ID = character(), FullName = character(), Colour = character(), stringsAsFactors = FALSE))
  }
  
  # Replace all NAs with empty strings ("")
  df[required_cols] <- lapply(df[required_cols], function(col) {
    col[is.na(col)] <- ""
    col
  })
  
  df<-df|> select(ID,FullName,Colour)
  
  return(df)
}