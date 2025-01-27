#' map_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_inputs_ui <- function(id) {
  ns <- NS(id)

  # creates an accordion layout with panels for selecting inputs
  accordion(
    open = NULL, multiple = FALSE, style="background-color:#FFF;",
    # panel for selecting view type and time-based options
    accordion_panel(
      "View by:",
      radioGroupButtons(
        ns("lev_per"),
        NULL,
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
          choices = names(period_ov),
          selected = names(period_ov)[1],
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
    # panel for selecting temperature effects and outcomes
    accordion_panel(
      title = "Temperature effect, age group, outcome summary",
      radioGroupButtons(
        ns("range"),
        "Effect of:",
        range_ov,
        range_ov[2],
        size = "xs",
        justified = TRUE
      ),
      selectizeInput(
        ns("agegroup"),
        "Age group:",
        agegroup_ov,
        selected = "all"
      ),
      selectizeInput(
        ns("outc"),
        "Outcome:",
        outcomes_ov,
        selected = "cuman"
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
        ssp_ov[1],
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
    ),
    # conditional panel for opacity control (non-city views only)
    conditionalPanel(
      condition = sprintf("input['%s'] != 'City'", ns("area")),
      sliderInput(
        ns("opacity"),
        "Fill Opacity",
        min = 0,
        max = 1,
        value = 0.7,
        ticks = FALSE,
        step = 0.1
      )
    )
  )
}


#' map_inputs Server Functions
#'
#' @noRd
mod_map_inputs_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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

    # enables named choices for the "period" slider
    rw_period <- allow_named_choices(
      inputId = "period",
      update_function = updateSliderTextInput,
      input = input,
      session = session,
      init_choices = period_ov,
      init_selected = period_ov[1]
    )

    # sets default values for inputs
    observe({
      updateSelectizeInput(session, "lev_per", selected = "Warming level")
      updateSelectizeInput(session, "area", selected = "City")
      updateSliderTextInput(session, "level", selected = names(level_ov)[1])
      updateSliderTextInput(session,"period", selected = names(period_ov)[1])
      updateRadioGroupButtons(session, "range", selected = range_ov[2])
      updateSelectizeInput(session, "agegroup", selected = "all")
      updateRadioGroupButtons(session, "ssp", selected = ssp_ov[1])
      updateRadioGroupButtons(session, "sc", selected = sc_ov[1])
      updateSelectizeInput(session, "outc", selected = "cuman")
    })

    # returns reactive inputs for use in other modules
    return(
      list(
        area = reactive(input$area),
        lev_per = reactive(input$lev_per),
        period = rw_period$read,
        level = rw_level$read,
        range = reactive(input$range),
        adapt = reactive(input$adapt),
        agegroup = reactive(input$agegroup),
        ssp = reactive(input$ssp),
        sc = reactive(input$sc),
        outc = reactive(input$outc),
        opacity = reactive(input$opacity)
      )
    )
  })
}


## To be copied in the UI
# mod_map_inputs_ui("map_inputs_1")

## To be copied in the server
# mod_map_inputs_server("map_inputs_1")
