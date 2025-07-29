#' is.CaNSample Function
#'
#' Verifies that an object claiming to be a `CaNSample` list has the required structure,
#' including the presence and format of the `CaNmod`, `mcmc`, and `covMat` components.
#' 
#' @param obj Returns `TRUE` if the structure is valid, `FALSE` otherwise.
#'
#' @return Returns `TRUE` if the structure is that of a CaNSample object. Otherwise, an error is thrown indicating
#'
#' @details
#' The function checks:
#' \itemize{
#'   \item That `obj` is a list.
#'   \item That it contains the elements: `CaNmod`, `mcmc`.
#'   \item That `mcmc` is a valid `mcmc.list` (from the \pkg{coda} package).
#'   \item That `CaNmod` is a list containing all required model components.
#' }
#'
#'#' @examples
#' \dontrun{
#' if (is.CaNSample(obj)) {
#'   message("Valid CaNSample object.")
#' } else {
#'   message("Invalid structure.")
#' }
#' }
#' @import coda
#' @export
#'

is.CaNSample <- function(obj) {

  if (!is.list(obj)) return(FALSE)
  
  # Check required top-level elements
  if (!all(c("CaNmod", "mcmc") %in% names(obj))){
    warning("Missing the element(s) CaNmod and/or mcmc")
    return(FALSE)
  } 
  if (!coda::is.mcmc.list(obj$mcmc)) {
    warning("The mcmc list does notan mcmc.list object. ")
    return(FALSE)
  } 
  
  # Check structure of CaNmod
  CaNmod <- obj$CaNmod
  
  if (!is.list(CaNmod)) {
    warning("CaNmod is empty or is not a list. ")
    return(FALSE)
  } 
  required_CaNmod_elements <- c(
    "components_param", "species", "fluxes_def", "ntstep"
  )
  if (!all(required_CaNmod_elements %in% names(CaNmod))) {
    warning("CaNmod miss the required elements components_param, species, fluxes_def, and/or ntstep.")
    return(FALSE)
  } 
  
  if (!all(c("Component", "Inside") %in% colnames(CaNmod$components_param))) {
    warning("CaNmod miss the required elements Component, and/or Inside.")
    return(FALSE)
  } 
  
  if (!all(c("Flux", "From", "To") %in% colnames(CaNmod$fluxes_def))){
    warning("CaNmod miss the required elements Flux, From, and/or To")
    return(FALSE)
  }
  
  
  return(TRUE)
}