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

    card( padding="50px",
      selectizeInput(ns("lev_per"), "View by:", choices = c("Warming level","Five-year periods")),
      selectizeInput(ns("area"), label=NULL, choices = c("City","Country","Region")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Five-year periods'", ns("lev_per")),
        sliderTextInput(ns("period"),"Period",choices=period_ov,animate = TRUE, grid = TRUE)
        ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'Warming level'", ns("lev_per")),
        sliderTextInput(ns("level"),"Warming level",choices=names(level_ov),selected=names(level_ov)[1])
      ),

      # accordion attempt
      accordion(
        open = F, multiple = F,
        accordion_panel(title="Temperature effect, age group, outcome summary",
          radioGroupButtons(ns("range"),"Effect of:",range_ov, range_ov[2], size = "xs",justified = F),
          selectizeInput(ns("agegroup"),"Age group:", agegroup_ov,  selected="all"),
          selectizeInput(ns("outc"),"Outcome:", outcomes_ov, selected="cuman")),
        accordion_panel(title="Scenarios",
          sliderTextInput(ns("adapt"),"Adaptation:", adapt_ov),
          radioGroupButtons(ns("ssp"),"SSP:",ssp_ov, ssp_ov[2], size = "xs",justified = F),
          radioGroupButtons(ns("sc"),"Component:",sc_ov, sc_ov[2], size = "xs",justified = FALSE)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] != 'City'", ns("area")),
          sliderInput(ns("opacity"), "Fill Opacity", min = 0, max = 1, value = 0.7, ticks=F, step = 0.1)
        )
      )
  )
}

#' map_inputs Server Functions
#'
#' @noRd
mod_map_inputs_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # ALLOW SLIDER TEXT INPUT NAMED CHOICES
    rw_level <- allow_named_choices(
      inputId = "level", #id of widget to extend
      update_function = updateSliderTextInput, #widget updater function
      input    = input,
      session  = session,
      init_choices  = level_ov,   #named choices, not only names
      init_selected = level_ov[1] #named select , not only name
    )

    # OUTPUTS
    return(
      list(
        area = reactive(input$area),
        lev_per = reactive(input$lev_per),
        period = reactive(input$period),
        level = rw_level$read, # reactive(input$level)
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
