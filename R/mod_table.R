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
    fluidRow(
      column(width = 2,
        card(
          downloadButton(ns("download_filtered"), "Download Filtered Rows as CSV"),
          textOutput(ns("tb_usage"))
        )),
      column(width = 10,
        card(
          title = NULL,
          div(
            style="height: calc(100vh - 280px); overflow-y: auto;", # Adjust height as needed
            shinycssloaders::withSpinner(
              DTOutput(ns("table")))
            ))
        ))
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id, filts){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$tb_usage <- renderText({
      "Use column boxes, diamonds, and the visibility button to filter, sort, and hide table data. After clicking download, wait for your system's prompt before using the app. Hidden columns are included in the download."
    })

    tbdata <- reactive({

      withProgress(message = "Preparing table data...", value = 0, {

        ordered_newnames <- c("Level"="level","Period"="period","Adaptation"="adapt","Temp.range"="range",
                              "SSP"="ssp","Sc"="sc","Age.group"="agegroup",
                              "Country.code"="country_code", "Country"="country_name",
                              "City"="city_name","City.code"="city", "Region"="region",
                              "Excess.deaths"="an_est","AF(%)"="af_est","Rate(*10^6)"="rate_est","Cumulative"="cuman_est",
                              setNames(
                                paste0(rep(c("an","af","rate","cuman"), each=2),c("_low","_high")),
                                paste0(rep(c("Exc.deaths","AF","Rate","Cumulative"), each=2),c("_low","_high"))
                                ))

        incProgress(0.1, detail = "")

        dt <-
          utils_connect_arrow(lev_per=filts$lev_per(), area=filts$area()) %>%
          # slice_sample(prop=.1) %>%
          sfarrow::read_sf_dataset() %>% st_drop_geometry(.)  %>%
          select(any_of(ordered_newnames)) %>%
          mutate(across(where(is.numeric),\(x) round(x, 2))) %>%
          mutate(across(any_of(c("Age.group","Country", "City")),as.factor))

        c_nms <- colnames(dt)
        c_num <- ncol(dt)
        r_num <- nrow(dt)

        return(
          list(dt=dt, c_nms=c_nms, c_num=c_num, r_num=r_num)
          )
        })
      # works to cache data and reduce loading time
      }) %>% bindCache(filts$lev_per(), filts$area())

    trigger <- reactiveVal(FALSE)

    output$table <- renderDT({

      col_names <- tbdata()$c_nms
      r_num <- tbdata()$r_num
      dt <- tbdata()$dt

      withProgress(message = glue("Loading table with {r_num} rows ..."), value = 0.3, {

        trigger(TRUE)

        incProgress(0.6, detail = "") # Increment progress by 40%

        hidden_cols <- which(
          col_names %in% c("Country.code", "City.code", "Excess.deaths", "AF(%)", "Rate(*10^6)",
                           paste0(rep(c("Exc.deaths", "AF", "Rate"), each = 2), c("_low", "_high")))
        ) - 1 # Convert to 0-index

        dtt <- datatable(dt,
                         style = "auto",
                         filter = "top",
                         rownames = FALSE,
                         extensions = "Buttons",
                         options = list(
                           pageLength = 50,
                           lengthMenu = c(50, 100, 200, 500),
                           autoWidth = TRUE, # automatically adjust column width
                           dom = 'Bfrtip',      # simplify table controls
                           buttons = list(
                             list(extend = 'colvis',
                                  text = "Show/Hide columns")),
                           columnDefs = list(
                             list(className = 'dt-center', targets = "_all"), # center align
                             list(visible = FALSE, targets = hidden_cols)
                           ),
                           serverSide = TRUE
                         )
        )
        incProgress(0.7, detail = "")

        dtt <- dtt %>%
          formatStyle(
            columns = col_names,
            fontSize = '12px'
          )

        dtt
      })
    })

    proxy <- dataTableProxy("table", session = session)

    observeEvent(trigger(),{

        num_columns <- tbdata()$c_num
        search_keywords <- rep("", num_columns)

        search_keywords[2:6] <- c(glue("[\"{filts$adapt()}\"]"),glue("[\"{filts$range()}\"]"),
                                  glue("[\"{filts$ssp()}\"]"),glue("[\"{filts$sc()}\"]"),
                                  glue("[\"{filts$agegroup()}\"]"))

        search_keywords[1] <- switch(
          filts$lev_per(),
          "Warming level" = glue("[\"{filts$level()}\"]"),
          "Five-year periods" = glue("[\"{filts$period()}\"]")
        )

        updateSearch(proxy,
                     keywords=list(
                       global=NULL,
                       columns=search_keywords))

        trigger(FALSE)

    })

    # Reactive expression to get filtered data
    filtered_data <- reactive({
      req(input$table_rows_all)

      # use datatable automatic reactive input 'table_rows_all'
      tbdata()$dt[input$table_rows_all, ]
    })

    # Add a download handler for filtered rows
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0(filts$lev_per(), "_", filts$area(), ".csv")
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
