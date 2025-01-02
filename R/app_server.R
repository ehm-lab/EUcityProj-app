#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import mapgl
#' @import DT
#' @import RColorBrewer
#' @import scico
#' @import glue
#' @importFrom utils head tail
#' @noRd
app_server <- function(input, output, session) {

  # MAP VIEW

  # initializes map inputs server module
  inputs_a <- mod_map_inputs_server("inpbmap")

  # establishes a reactive connection to the data source based on selected inputs
  map_data_conn <- reactive({
    req(inputs_a$area()) # ensures area input is provided
    utils_connect_arrow(
      lev_per = inputs_a$lev_per(),
      area = inputs_a$area()
    )
  })

  # builds a filtered query object based on user inputs
  map_data_query <- reactive({
    utils_filt(
      conn = map_data_conn(),
      lev_pe = inputs_a$lev_per(),
      are = inputs_a$area(),
      perio = inputs_a$period(),
      leve = inputs_a$level(),
      adap = inputs_a$adapt(),
      agegrou = inputs_a$agegroup(),
      ss = inputs_a$ssp(),
      s = inputs_a$sc(),
      rang = inputs_a$range()
    )
  })

  # collects data for rendering the map from the query
  map_data_coll <- reactive({
    utils_collect(query = map_data_query())
  })

  # generates a detailed label describing the current map scenario
  scene_label <- reactive({
    glue::glue(
      "<span style='color:red;'>{inputs_a$area()}</span> projection scenario for
      <span style='color:red;'>{names(range_ov[match(inputs_a$range(), range_ov)])}</span> temperature effects on age group
      <span style='color:red;'>'{names(agegroup_ov[match(inputs_a$agegroup(), agegroup_ov)])}'</span>.<br>
      At <span style='color:red;'>{tolower(inputs_a$lev_per())}</span>
      <span style='color:red;'>{ifelse(inputs_a$lev_per() == 'Warming level', paste0(inputs_a$level(), '&#176;C'), inputs_a$period())}</span>,
      <span style='color:red;'>{inputs_a$adapt()} adaptation</span> response, assuming
      <span style='color:red;'>SSP {inputs_a$ssp()}</span> and accounting for
      <span style='color:red;'>{names(sc_ov[match(inputs_a$sc(), sc_ov)])}</span> component(s)."
    )
  })

  # initializes the map server module with map data, labels, and inputs
  mod_map_server(
    "bmap",
    map_data_coll,
    inputs_a$area,
    inputs_a$outc,
    scelab = scene_label,
    inputs_a$opacity
  )

  # TABLE VIEW
  inputs_b <- mod_table_inputs_server("inpdt")

  # establishes a reactive connection to the data source based on selected inputs
  dt_data_conn <- reactive({
    req(inputs_b$area()) # ensures area input is provided
    utils_connect_arrow(
      lev_per = inputs_b$lev_per(),
      area = inputs_b$area()
    )
  })

  # prepares table data reactively based on filters
  dt_data <- reactive({
    req(input$tabs == "Table") # ensures the current tab is "Table"

    withProgress(message = "Preparing table data...", value = 0, {
      incProgress(0.1)

      query <- dt_data_conn()

      # DATA TYPE
      if (inputs_b$lev_per()=="Five-year periods"){

        query <- query %>% dplyr::filter(period==inputs_b$period())

      } else if (inputs_b$lev_per()=="Warming level") {

        query <- query %>% dplyr::filter(level==inputs_b$level())

      }

      dtcon <- query %>%
        dplyr::filter(agegroup %in% inputs_b$agegroup(),
                      ssp == inputs_b$ssp(),
                      sc == inputs_b$sc(),
                      range == inputs_b$range(),
                      adapt == inputs_b$adapt())

      dtrows <- nrow(dtcon)
      dtcols <- length(intersect(ordered_newnames, names(dtcon)))

      chunk_size <- 2000
      n_chunks <- ceiling(dtrows / chunk_size)

      # initialize a list to store data chunks
      chunk_list <- vector("list", n_chunks)

      sprog <- 0.1
      incprog <- 0.7 / n_chunks

      for (chk in seq_len(n_chunks)) {
        cat(chk,"\n")
        srow <- (chk - 1) * chunk_size + 1
        erow <- min(chk * chunk_size, dtrows)

        dt <- dtcon %>%
          head(erow) %>%
          tail(erow - srow + 1) %>%
          # sample ro sf_read here
          dplyr::collect() %>%
          select(any_of(ordered_newnames)) %>%
          mutate(across(where(is.numeric), \(x) round(x, 2))) %>%
          mutate(across(any_of(c("Age.group", "Country", "City")), as.factor))

        # store the chunk in the list
        chunk_list[[chk]] <- dt

        incProgress(incprog, detail = paste0(round((erow/dtrows)*100),"%"))
      }

      # combine all chunks into a single data frame
      dtf <- data.table::rbindlist(chunk_list, use.names = TRUE, fill = TRUE)

      cat("loaded:", nrow(dtf), "/n")

      return(
        list(dt = dtf, c_nms = colnames(dtf), c_num = ncol(dtf), r_num = nrow(dtf))
      )
    })
  })

  # initializes the table server module with map inputs and active tab
  mod_table_server(
    "table",
    dt_data
  )
}

