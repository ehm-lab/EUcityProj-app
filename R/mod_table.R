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
      HTML("<span style='display:inline;'> Use column boxes to filter, diamonds to sort, and show/hide button to show and hide table columns. After clicking download, please wait for your system's prompt before using the app. Hidden columns are included in the download. For the full source data please see <a href='https://zenodo.org/records/14004322' target='_blank' style='display:inline;'>this link</a>.</span>")
    })

    # # triggers reactive updates when data changes - set up the necessary input
    # trigger <- reactiveVal(FALSE)

    # renders the DataTable with appropriate configurations
    output$table <- renderDT({
      req(tbdata())

      withProgress(message = glue("Loading table with {tbdata()$r_num} rows ..."), value = 0.9, {
        # trigger(TRUE)
        incProgress(0.1)

        hidden_cols <- which(tbdata()$c_nms %in% c(
          "Period","Level","Adaptation","Temp range","SSP","Sc","Age group",
          "Country code", "City code",  "AF(%)", "Rate(x10\u2075)",
          paste0(rep(c( "AF", "Rate"), each = 2), c("_low", "_high")))) - 1

        dtt <- datatable(
          tbdata()$dt,
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

        numeric_col_names <- names(tbdata()$dt)[sapply(tbdata()$dt, is.numeric)]

        dtt <- dtt %>%
          formatStyle(columns = tbdata()$c_nms, fontSize = '12px') %>%
          formatCurrency(columns = numeric_col_names, currency = "", interval = 3, mark = ",")

        cat("shown:", nrow(dtt), "/n")

        dtt
      })
    })

    # extracts filtered data for download
    filtered_data <- reactive({
      req(input$table_rows_all)
      tbdata()$dt[input$table_rows_all, ]
    })

    # provides a download handler for filtered data
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0("app_projdata_download.csv")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )

    # dynamically renders the scenario label
    output$scenariolabel <- renderUI({

      # can be copy pasted to the table tab
      levorper <- switch(scelab$le_pe(),
                         "Warming level" = "Global warming level",
                         "Ten-year periods" = "Period")
      levorper_val <- switch(scelab$le_pe(),
                             "Warming level" = paste0(scelab$lev(), "°C"),
                             "Ten-year periods"=scelab$perio())

      inc_scen <- paste0("Including ",switch(as.character(scelab$s()),
                                             "clim" = "climate change",
                                             "demo" = "demographic change",
                                             "full" = "climate and demographic change")," effects")

      layout_column_wrap(
        width = 1/4,
        div(
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
          p(strong(levorper)),
          p(levorper_val)
        ),
        div(
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
          p(strong("Temperature range:"),names(range_ov[match(scelab$rang(), range_ov)])),
          p(strong("Age group:"),names(agegroup_ov[match(scelab$agegr(), agegroup_ov)]))
        ),
        div(
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
          p(strong("Adaptation to heat:"), scelab$adap()),
          p(strong("Shared Socioeconomic Pathway:"), names(ssp_ov[match(scelab$ss(), ssp_ov)]))
        ),
        div(
          style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
          p(strong(inc_scen))
        )
      )
    })

  })
}


## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
