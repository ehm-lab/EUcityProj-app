#' connect_arrow
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_connect_arrow <- function(lev_per, area) {

  if (lev_per=="Five-year periods"){

    data <- switch(area,
                   "City"    = arrow::open_dataset(sources = system.file("extdata/city_period", package = "ehmhipmapp"), partitioning = "agegroup"),
                   "Country" = arrow::open_dataset(sources = system.file("extdata/country_period", package = "ehmhipmapp"), partitioning = "agegroup"),
                   "Region"  = arrow::open_dataset(sources = system.file("extdata/region_period", package = "ehmhipmapp"), partitioning = "agegroup"),
                   default   = NULL)

  } else {

    data <- switch(area,
                   "City"    = arrow::open_dataset(sources = system.file("extdata/city_level", package = "ehmhipmapp"), partitioning = "agegroup"),
                   "Country" = arrow::open_dataset(sources = system.file("extdata/country_level", package = "ehmhipmapp"), partitioning = "agegroup"),
                   "Region"  = arrow::open_dataset(sources = system.file("extdata/region_level", package = "ehmhipmapp"), partitioning = "agegroup"),
                   default   = NULL)

  }

}
