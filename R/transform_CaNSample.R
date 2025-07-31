#' Transform a obj Object into more manipulable Format
#'
#' Simplifies a `obj` object by converting the MCMC matrix into a long-format tibble
#'
#' @param obj A CaNSample object with `mcmc` results and model structure.
#'
#' @return A tibble with columns: `Sample_id`, `Var`, `Year`, and `value`.
#' @keywords internal
#' 
transform_CaNSample <- function(obj) {
  m <- as.matrix(obj$mcmc)
  fluxes_def <- obj$CaNmod$fluxes_def |>
    dplyr::mutate(FluxName = paste0(From, "_", To))  # Create readable names
  
  tibble::as_tibble(m) |>
    dplyr::mutate(Sample_id = seq_len(nrow(m))) |>
    tidyr::pivot_longer(
      cols = -Sample_id,
      names_to = c("Var", "Year"),
      names_pattern = "(.*)\\[(.*)\\]",
      values_to = "value"
    ) |>
    dplyr::left_join(fluxes_def, by = c("Var" = "Flux")) |>
    dplyr::mutate(
      Var = ifelse(!is.na(FluxName), FluxName, Var)
    ) |>
    dplyr::select(Sample_id, Var, Year, value)
}
