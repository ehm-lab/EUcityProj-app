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
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # MAP VIEW
  inputs_a <- mod_map_inputs_server("inpbmap")

  # guide from other app
  data_conn <- reactive({

    req(inputs_a$area())

    utils_connect_arrow(lev_per=inputs_a$lev_per(), area=inputs_a$area())

  })

  data_query <- reactive({
    utils_filt(conn=data_conn(),
               lev_pe=inputs_a$lev_per(), are=inputs_a$area(),
               perio=inputs_a$period(), leve=inputs_a$level(),
               adap= inputs_a$adapt(),
               agegrou=inputs_a$agegroup(), ss=inputs_a$ssp(),
               s=inputs_a$sc(), rang=inputs_a$range())
  })

  map_data_coll <- reactive({
    utils_collect(query=data_query())
  })



  scene_label <- reactive({

      s <- glue::glue(
    "<span style='color:red;'>{inputs_a$area()}</span> projection scenario for
    <span style='color:red;'>{names(range_ov[match(inputs_a$range(), range_ov)])}</span> temperature effects on age group
    <span style='color:red;'>'{names(agegroup_ov[match(inputs_a$agegroup(), agegroup_ov)])}'</span>.<br>
    At <span style='color:red;'>{tolower(inputs_a$lev_per())}</span>
    <span style='color:red;'>{ifelse(inputs_a$lev_per() == 'Warming level', paste0(inputs_a$level(), '℃'), inputs_a$period())}</span>,
    <span style='color:red;'>{inputs_a$adapt()} adaptation</span> response, assuming
    <span style='color:red;'>SSP {inputs_a$ssp()}</span> and accounting for a
    <span style='color:red;'>{names(sc_ov[match(inputs_a$sc(), sc_ov)])}</span> component."
      )

    return(s)

  })

  mod_map_server("bmap", map_data_coll, inputs_a$area, inputs_a$outc, scelab=scene_label, inputs_a$opacity)

  # TABLE VIEW
  mod_table_server("table")
}
