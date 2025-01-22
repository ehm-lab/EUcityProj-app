#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom utils write.csv
mod_table_ui <- function(id) {
  ns <- NS(id)

  # creates a layout with a download button and text output in one column, and a table in another
  tagList(
    uiOutput(ns("scenariolabel")),
      layout_column_wrap(
        width = NULL,
        style = css(grid_template_columns = "1fr 4fr", grid_column_gap = "5px"),
        card_body(
          downloadButton(ns("download_filtered"), "Download Table Data"),
          mod_table_inputs_ui("inpdt"),
          htmlOutput(ns("tb_usage"))
        ),
        card_body(
          div(
            style = "height: calc(100vh - 280px); overflow-y: auto;", # ensures scrollable table area
            shinycssloaders::withSpinner(
              DTOutput(ns("table"))
            )
          )
        )
      )
    )
}


#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, tbdata, scelab) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # server
    output$tb_usage <- renderText({
      HTML("<span style='display:inline;'> Use column boxes to filter, diamonds to sort, and show/hide button to show and hide table columns. After clicking download, please wait for your system's prompt before using the app. Hidden columns are included in the download. For the full source data please see <a href='https://doi.org/10.5281/zenodo.14004322'><img src='https://zenodo.org/badge/DOI/10.5281/zenodo.14004322.svg' alt='DOI'></a>
.</span>")
    })

    # renders the DataTable with appropriate configurations
    output$table <- renderDT({
      req(tbdata())

      if (any(tbdata() == "NoData")) {

        showModal(
          modalDialog(
            title = "No data",
            "Projected values for this scenario are unavailable. Please try a different selection or check the Information tab.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      dtf <- data.table::copy(tbdata()$dt)

      # Estimate (lower to higher) formatting
      for (grp in names(grpcols)) {
        dtf[,(grp) :=
              paste0(formatC(get(grpcols[[grp]][1]), format="f", big.mark=",", digits=2), " (",
                     formatC(get(grpcols[[grp]][2]), format="f", big.mark=",", digits=2), " to ",
                     formatC(get(grpcols[[grp]][3]), format="f", big.mark=",", digits=2), ")")]
        dtf[, (grpcols[[grp]][-1]):= NULL]
      }

      c_nms <- names(dtf)

      withProgress(message = glue("Loading table with {tbdata()$r_num} rows ..."), value = 0.9, {
        # trigger(TRUE)
        incProgress(0.1)

        hidden_cols <- which(c_nms %in% c(
          "Period","Level","Adaptation","Temp range","SSP","Sc","Age group",
          "Country code", "City code")) - 1

        dtt <- datatable(
          dtf,
          style = "auto",
          filter = "top",
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            pageLength = 50,
            lengthMenu = c(50, 100, 200, 500),
            autoWidth = TRUE,
            dom = 'Brtip',
            buttons = list(list(extend = 'colvis', text = "Show/Hide columns")),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(visible = FALSE, targets = hidden_cols)
            ),
            serverSide = TRUE
          )
        )

        dtt <- dtt %>% formatStyle(columns = c_nms, fontSize = '13px')

        dtt
      })
    })

    # dynamically renders the scenario label
    output$scenariolabel <- renderUI({
      utils_scenario_ui_boxes(scelab)
    })

    filtered_data <- reactive({
      req(tbdata())
      tbdata()$dt
    })

    # provides a download handler for filtered data
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0("app_projdata_download.csv")
      },
      content = function(file) {
        write.csv(tbdata()$dt, file, row.names = FALSE)
      }
    )

  })
}


## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
