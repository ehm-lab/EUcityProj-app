#' table_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table_inputs_ui <- function(id) {
  ns <- NS(id)

  # creates an accordion layout with panels for selecting inputs
  accordion(
    open = NULL, multiple = FALSE,
    # # panel for selecting view type and time-based options
    accordion_panel(
      "View by:",
      radioGroupButtons(
        ns("lev_per"),
        label=NULL,
        choices = c("Warming level", "Five-year periods"),
        selected = "Warming level",
        justified = TRUE
      ),
      radioGroupButtons(
        ns("area"),
        label = NULL,
        choices = c("City", "Country", "Region"),
        selected = "Country",
        justified = TRUE
     ),
     conditionalPanel(
       condition = sprintf("input['%s'] == 'Five-year periods'", ns("lev_per")),
       sliderTextInput(
         ns("period"),
         NULL,
         choices = period_ov,
         selected = period_ov[4],
         animate = FALSE,
         grid = TRUE
       )
     ),
     conditionalPanel(
       condition = sprintf("input['%s'] == 'Warming level'", ns("lev_per")),
       sliderTextInput(
         ns("level"),
         NULL,
         choices = names(level_ov),
         selected = names(level_ov)[1]
       )
     )
    ),
    accordion_panel(
      "Temperature effect, age group:",
    # panel for selecting temperature effects and outcomes
      radioGroupButtons(
        ns("range"),
        "Temperature:",
        range_ov,
        range_ov[2],
        size = "xs",
        justified = TRUE
      ),
    selectizeInput(
      ns("agegroup"),
      "Age group:",
      agegroup_ov,
      #multiple=TRUE,
      multiple=FALSE,
      selected = "all"
    )
    ),
    # panel for selecting scenarios
    accordion_panel(
      title = "Scenarios",
      sliderTextInput(
        ns("adapt"),
        "Adaptation:",
        adapt_ov,
        selected = "0%"
      ),
      radioGroupButtons(
        ns("ssp"),
        "SSP:",
        ssp_ov,
        ssp_ov[2],
        size = "xs",
        justified = TRUE
      ),
        radioGroupButtons(
          ns("sc"),
          "Including:",
          sc_ov,
          sc_ov[2],
          size = "xs",
          justified = TRUE
        )
    )
  )

}

#' table_inputs Server Functions
#'
#' @noRd
mod_table_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # enables named choices for the "level" slider
    rw_level <- allow_named_choices(
      inputId = "level",
      update_function = updateSliderTextInput,
      input = input,
      session = session,
      init_choices = level_ov,
      init_selected = level_ov[1]
    )

    # sets default values for inputs
    observe({
      updateSelectizeInput(session, "lev_per", selected = "Warming level")
      updateSelectizeInput(session, "area", selected = "City")
      updateSliderTextInput(session, "level", selected = names(level_ov)[1])
      updateRadioGroupButtons(session, "range", selected = range_ov[2])
      updateSelectizeInput(session, "agegroup", selected = "all")
      updateRadioGroupButtons(session, "ssp", selected = ssp_ov[1])
      updateRadioGroupButtons(session, "sc", selected = sc_ov[3])
    })

    # returns reactive inputs for use in other modules
    return(
      list(
        area = reactive(input$area),
        lev_per = reactive(input$lev_per),
        period = reactive(input$period),
        level = rw_level$read,
        range = reactive(input$range),
        adapt = reactive(input$adapt),
        agegroup = reactive(input$agegroup),
        ssp = reactive(input$ssp),
        sc = reactive(input$sc)
      )
    )
  })
}

## To be copied in the UI
# mod_table_inputs_ui("table_inputs_1")

## To be copied in the server
# mod_table_inputs_server("table_inputs_1")
