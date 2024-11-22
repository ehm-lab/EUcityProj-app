#' filt
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_filt <- function(conn, lev_pe, are, perio, leve, adap, agegrou, ss, s, rang) {

  query <- conn
  #
  # DATA TYPE
  if (lev_pe=="Five-year periods"){

    query <- query %>% dplyr::filter(period==perio)

  } else if (lev_pe=="Warming level") {

    query <- query %>% dplyr::filter(level==leve)

  }

  # FILTERS
  query <- query %>%
    dplyr::filter(agegroup == agegrou,
                  ssp == ss,
                  sc == s,
                  range == rang,
                  adapt == adap) #%>% dplyr::mutate(an_est = round(an_est, 0))

  # # LOCATION
  # if (area == "Region" | any(locations %in% "All")) {
  #
  #   return(query)
  #
  # } else {
  #
  #   # add to country selector pattern
  #   pattern <- paste(locations, collapse = "|")
  #   query %>% dplyr::filter(base::grepl(pattern, country_name))
  #
  # }

}
