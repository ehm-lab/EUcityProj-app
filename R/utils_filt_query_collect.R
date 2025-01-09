#' filt_query_collect
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_filt_query_collect <- function(ds, lev_pe, perio, leve, adap, agegrou, ss, s, rang, fortable=F) {
  # build query
  if (lev_pe == "Ten-year periods") {
    ds <- ds %>% dplyr::filter(period %in% perio)
  } else if (lev_pe == "Warming level") {
    ds <- ds %>% dplyr::filter(level %in% leve)
  }
  ds <- ds %>%
    dplyr::filter(
      agegroup %in% agegrou,
      ssp %in% ss,
      sc %in% s,
      range %in% rang,
      adapt %in% adap
    )

  if (fortable) {

    dtrows <- nrow(ds)
    chunk_size <- min(dtrows, 2000000)
    n_chunks <- ceiling(dtrows / chunk_size)

    print(dtrows);
    print(chunk_size)
    print(n_chunks)

    incprog <- 0.9 / n_chunks

    chunk_list <- vector("list", n_chunks)

    for (chunk in seq_len(n_chunks)) {

      srow <- (chunk - 1) * chunk_size + 1
      erow <- min(chunk * chunk_size, dtrows)

      chunk_df <- ds %>%
        head(erow) %>%
        tail(erow - srow + 1) %>%
        # the whole point is collection in chunks ...
        dplyr::collect()
      # rounding and factor can also be done in each collection but it is more intense

      chunk_list[[chunk]] <- chunk_df
      incProgress(incprog, detail = paste0("Chunk ", chunk, " of ", n_chunks))
    }

    dtf <- data.table::rbindlist(chunk_list, use.names = TRUE, fill = TRUE)

    dtf[, setdiff(names(dtf), ordered_newnames) := NULL]
    setcolorder(dtf, intersect(ordered_newnames, names(dtf)))
    setnames(dtf, old = ordered_newnames, new = names(ordered_newnames), skip_absent = T)
    dtf[, (colnames(dtf)[sapply(dtf, is.numeric)]) := lapply(.SD, round, 2), .SDcols = sapply(dtf, is.numeric)]
    dtf[, (intersect(c("Age.group", "Country", "City"), names(dtf))) := lapply(.SD, as.factor), .SDcols = intersect(c("Age.group", "Country", "City"), names(dtf))]  # convert to factors

    return(
      list(dt = dtf, c_nms = colnames(dtf), c_num = ncol(dtf), r_num = nrow(dtf))
    )
  } else {
    # if the dataset contains geometry, use sfarrow to read
    return(
      sfarrow::read_sf_dataset(ds)
      )
  }
}
