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

  # map inputs
  inputs_a <- mod_map_inputs_server("inpbmap")

  # map data
  map_data <- reactive({
    req(inputs_a$area())
    ds <- utils_connect_arrow(inputs_a$lev_per(), inputs_a$area())
    utils_filt_query_collect(
      ds,
      lev_pe  = inputs_a$lev_per(),
      perio   = inputs_a$period(),
      leve    = inputs_a$level(),
      adap    = inputs_a$adapt(),
      agegrou = inputs_a$agegroup(),
      ss      = inputs_a$ssp(),
      s       = inputs_a$sc(),
      rang    = inputs_a$range()
    )
  })

  # map module
  mod_map_server(
    "bmap",
    map_data,
    inputs_a$area,
    inputs_a$outc,
    # value box
    scelab = list(le_pe  = inputs_a$lev_per,
      lev=inputs_a$level, perio=inputs_a$period, adap=inputs_a$adapt,
      rang=inputs_a$range, ss=inputs_a$ssp, s=inputs_a$sc, agegr=inputs_a$agegroup
      ),
    inputs_a$opacity
  )
  # table inputs
  inputs_b <- mod_table_inputs_server("inpdt")

  # table data
  dt_data <- reactive({
    req(input$tabs == "Table")
    ds <- utils_connect_arrow(inputs_b$lev_per(), inputs_b$area())

    withProgress(message = "Preparing table data...", value = 0, {

    utils_filt_query_collect(
        ds,
        lev_pe  = inputs_b$lev_per(),
        perio   = inputs_b$period(),
        leve    = inputs_b$level(),
        adap    = inputs_b$adapt(),
        agegrou = inputs_b$agegroup(),
        ss      = inputs_b$ssp(),
        s       = inputs_b$sc(),
        rang    = inputs_b$range(),
        fortable = TRUE
      )
    })
  })

  mod_table_server("table", dt_data,
                   scelab = list(le_pe  = inputs_b$lev_per,
                                 lev=inputs_b$level, perio=inputs_b$period, adap=inputs_b$adapt,
                                 rang=inputs_b$range, ss=inputs_b$ssp, s=inputs_b$sc, agegr=inputs_b$agegroup
                   ))

}
