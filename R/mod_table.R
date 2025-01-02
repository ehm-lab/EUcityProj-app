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
  card(
    height = 500, full_screen = FALSE,
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
mod_table_server <- function(id, tbdata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # server
    output$tb_usage <- renderText({
      HTML("<span style='display:inline;'> Use column boxes, diamonds, and the visibility button to filter, sort, and hide table data. After clicking download, wait for your system's prompt before using the app. Hidden columns are included in the download. For the full source data please see <a href='https://zenodo.org/records/14004322' target='_blank' style='display:inline;'>this link</a></span>")
    })

    # # triggers reactive updates when data changes - set up the necessary input
    # trigger <- reactiveVal(FALSE)

    # renders the DataTable with appropriate configurations
    output$table <- renderDT({
      req(tbdata())

      withProgress(message = glue("Loading table with {tbdata()$r_num} rows ..."), value = 0.9, {
        # trigger(TRUE)
        incProgress(0.1)

        hidden_cols <- which(tbdata()$c_nms %in% c("Country.code", "City.code", "Excess.deaths", "AF(%)", "Rate(*10^6)",
                                                   paste0(rep(c("Exc.deaths", "AF", "Rate"), each = 2), c("_low", "_high")))) - 1

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
            dom = 'Bfrtip',
            buttons = list(list(extend = 'colvis', text = "Show/Hide columns")),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all"),
              list(visible = FALSE, targets = hidden_cols)
            ),
            serverSide = TRUE
          )
        )
        dtt <- dtt %>% formatStyle(columns = tbdata()$c_nms, fontSize = '12px')

        cat("shown:", nrow(dtt), "/n")

        dtt
      })
    })

    # do not delete this can be implemented
    # proxy <- dataTableProxy("table", session = session)
    #
    # # updates search keywords dynamically
    # observeEvent(trigger(), {
    #
    #
    #   search_keywords <- rep("", tbdata()$c_nums)
    #   search_keywords[2:6] <- c(
    #     glue("[\"{inputs_b$adapt()}\"]"), glue("[\"{inputs_b$range()}\"]"),
    #     glue("[\"{inputs_b$ssp()}\"]"), glue("[\"{inputs_b$sc()}\"]"),
    #     glue("[\"{inputs_b$agegroup()}\"]")
    #   )
    #   search_keywords[1] <- switch(
    #     inputs_b$lev_per(),
    #     "Warming level" = glue("[\"{inputs_b$level()}\"]"),
    #     "Five-year periods" = glue("[\"{inputs_b$period()}\"]")
    #   )
    #
    #   updateSearch(dataTableProxy("table", session = session), keywords = list(global = NULL, columns = search_keywords))
    #   trigger(FALSE)
    # })

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
  })
}


## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
