#' generate_search_keywords
#' utility function to generate search keywords
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
generate_search_keywords <- function(num_columns, filts) {

  search_keywords <- rep("", num_columns)
  search_keywords[2:6] <- c(
    glue("[\"{filts$adapt()}\"]"), glue("[\"{filts$range()}\"]"),
    glue("[\"{filts$ssp()}\"]"), glue("[\"{filts$sc()}\"]"),
    glue("[\"{filts$agegroup()}\"]")
  )
  search_keywords[1] <- switch(
    filts$lev_per(),
    "Warming level" = glue("[\"{filts$level()}\"]"),
    "Five-year periods" = glue("[\"{filts$period()}\"]")
  )

  return(search_keywords)
}
