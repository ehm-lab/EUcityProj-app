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
  useWaitress(color="lightblue", percent_color = "black")
    fluidRow(
      column(width = 2,
        card(
          title = "Inputs",
          mod_table_inputs_ui(ns("table_inp"))
        ),
        card(
          title="Using the table",
          textOutput(ns("tb_usage"))
        )),
      column(width = 10,
        card(
          title = NULL,
          div(
            style="height: calc(100vh - 280px); overflow-y: auto;", # Adjust height as needed
          DTOutput(ns("table"))
        )
          )))
}

#' table Server Functions
#'
#' @noRd
mod_table_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    waitress <- Waitress$new(theme = "overlay-percent", infinite=TRUE)

    # period level and spatial resol, can add more
    inputs_table <- mod_table_inputs_server("table_inp")

    output$tb_usage <- renderText({
      "Table data can be filtered, sorted, hidden and downloaded using: the column boxes, the diamonds next to the column names and the visibility and CSV buttons."
    })

    tbdata <- reactive({

      ordered_newnames <- c("Level"="level","Period"="period","Adaptation"="adapt","Temp.Range"="range",
                            "SSP"="ssp","Sc"="sc","Age group"="agegroup",
                            "Country code"="country_code", "Country"="country_name",
                            "City"="city_name","City code"="city", "Region"="region",
                            "AN"="an_est","AF(%)"="af_est","Rate(*10^6)"="rate_est","Cumulative"="cuman_est",
                            setNames(
                              paste0(rep(c("an","af","rate","cuman"), each=2),c("_low","_high")),
                              paste0(rep(c("AN","AF","Rate","Cumulative"), each=2),c("_low","_high"))
                              ))

      dt <-
        utils_connect_arrow(lev_per=inputs_table$lev_per(), area=inputs_table$area()) %>%
        # SHOWING 1% OF DATA !
        slice_sample(prop=0.1) %>%
        sfarrow::read_sf_dataset() %>% st_drop_geometry(.)  %>%
        select(any_of(ordered_newnames)) %>% mutate(across(where(is.numeric),\(x) round(x, 2)))

      return(dt)

      })

    output$table <- renderDT({

      waitress$start()

      col_names <- colnames(tbdata())
      hidden_cols <- which(
        col_names %in% c("Country code","City code","AN","AF(%)","Rate(*10^6)",
                         paste0(rep(c("AN","AF","Rate"), each=2),c("_low","_high")))
        ) - 1 # Convert to 0-index

      dtt <-  datatable(tbdata(),
                  style = "auto",
                  filter = "top",
                  rownames = FALSE,
                  extensions="Buttons",
                  options = list(
                    pageLength = 50,
                    lengthMenu = c(50,100,200,500),
                    # initComplete = JS(
                    #   "function(settings, json) {",
                    #   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                    #   "}"),
                    autoWidth = TRUE, # automatically adjust column width
                    dom = 'Bfrtip',      # simplify table controls
                    buttons = c('csv','colvis'),
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

        waitress$close()

        dtt

      })

  })
}

## To be copied in the UI
# mod_table_ui("table_1")

## To be copied in the server
# mod_table_server("table_1")
