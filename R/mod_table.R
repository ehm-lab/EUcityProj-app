#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_ui <- function(id) {
  ns <- NS(id)
    fluidRow(
      column(width = 2,
        card(
          mod_table_inputs_ui(ns("table_inp")),
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
mod_table_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # period level and spatial resol, can add more
    inputs_table <- mod_table_inputs_server("table_inp")

    output$tb_usage <- renderText({
      "Table data can be filtered, sorted, hidden and downloaded using: the column boxes, the diamonds next to the column names and the visibility and CSV buttons."
    })

    tbdata <- reactive({

      ordered_newnames <- c("Level"="level","Period"="period","Adaptation"="adapt","Temp.range"="range",
                            "SSP"="ssp","Sc"="sc","Age.group"="agegroup",
                            "Country.code"="country_code", "Country"="country_name",
                            "City"="city_name","City.code"="city", "Region"="region",
                            "Excess.deaths"="an_est","AF(%)"="af_est","Rate(*10^6)"="rate_est","Cumulative"="cuman_est",
                            setNames(
                              paste0(rep(c("an","af","rate","cuman"), each=2),c("_low","_high")),
                              paste0(rep(c("Exc.deaths","AF","Rate","Cumulative"), each=2),c("_low","_high"))
                              ))

      dt <-
        utils_connect_arrow(lev_per=inputs_table$lev_per(), area=inputs_table$area()) %>%
        # SHOWING 10% OF DATA !
        slice_sample(prop=1) %>%
        sfarrow::read_sf_dataset() %>% st_drop_geometry(.)  %>%
        select(any_of(ordered_newnames)) %>%
        mutate(across(where(is.numeric),\(x) round(x, 2))) %>%
        mutate(across(any_of(c("Age.group","Country", "City")),as.factor))

      print(colnames(dt))

      return(dt)

      })


    # Track whether the filter has already been applied
    trigger <- reactiveVal(FALSE)

    output$table <- renderDT({

      trigger(TRUE)
      col_names <- colnames(tbdata())

      hidden_cols <- which(
        col_names %in% c("Country.code","City.code","Excess.deaths","AF(%)","Rate(*10^6)",
                         paste0(rep(c("Exc.deaths","AF","Rate"), each=2),c("_low","_high")))
        ) - 1 # Convert to 0-index

      dtt <-  datatable(tbdata(),
                  style = "auto",
                  filter = "top",
                  rownames = FALSE,
                  extensions="Buttons",
                  options = list(
                    pageLength = 50,
                    lengthMenu = c(50,100,200,500),
                    autoWidth = TRUE, # automatically adjust column width
                    dom = 'Bfrtip',      # simplify table controls
                    buttons = list(
                      list(extend='colvis',
                           text="Show/Hide columns")),
                    columnDefs = list(
                      list(className = 'dt-center', targets = "_all"), # center align
                      list(visible = FALSE, targets = hidden_cols)
                    ),
                    serverSide=TRUE
                  )
        ) %>%
      # reduce font size
          formatStyle(
            columns = colnames(tbdata()),
            fontSize = '12px'
          )

        dtt

      })

    proxy <- dataTableProxy("table", session = session)

    observeEvent(trigger(),{

      num_columns <- ncol(tbdata())
      search_keywords <- rep("", num_columns)
      if (inputs_table$lev_per()=="Warming level") {
        search_keywords[1:6] <- c("[\"1.5\"]", "[\"0%\"]", "[\"heat\"]", "[\"1\"]", "[\"full\"]", "[\"all\"]")
      } else if (inputs_table$lev_per()=="Five-year periods") {
        search_keywords[1:6] <- c("[\"2025\"]", "[\"0%\"]", "[\"heat\"]", "[\"1\"]", "[\"full\"]", "[\"all\"]")
      }

      updateSearch(proxy,
                   keywords=list(
                     global=NULL,
                     columns=search_keywords))

      trigger(FALSE)
    })

    # Reactive expression to get filtered data
    filtered_data <- reactive({
      req(input$table_rows_all) # Ensure table is rendered

      # Use `tbdata()` and the filtered rows
      tbdata()[input$table_rows_all, ]
    })

    # Add a download handler for filtered rows
    output$download_filtered <- downloadHandler(
      filename = function() {
        paste0(inputs_table$lev_per(), "_", inputs_table$area(), "_filtered.csv")
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
