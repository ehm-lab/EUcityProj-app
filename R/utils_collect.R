#' collect
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_collect <- function(query) {

  d <- sfarrow::read_sf_dataset(query)

  return(d)

}
