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
      #sliderTextInput(ns("range"),"Temperature effect:", choices=names(range_ov), selected=names(range_ov)[2]),
      radioGroupButtons(ns("range"),"Temperature effect:",range_ov, range_ov[2], size = "xs",justified = TRUE),

      sliderTextInput(ns("adapt"),"Adaptation:", adapt_ov),

      #sliderTextInput(ns("ssp"),"SSP:", choices=names(ssp_ov), selected=names(ssp_ov)[1]),
      radioGroupButtons(ns("ssp"),"SSP:",ssp_ov, ssp_ov[2], size = "xs",justified = TRUE),

      #sliderTextInput(ns("sc"), "Projection component:", choices=names(sc_ov), selected = names(sc_ov)[3]),
      radioGroupButtons(ns("sc"),"Projection component:",sc_ov, sc_ov[2], size = "xs",justified = TRUE),

      selectizeInput(ns("agegroup"),"Age group:", agegroup_ov,  selected="all"),
      selectizeInput(ns("outc"),"Outcome:", outcomes_ov, selected="cuman"),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'City'", ns("area")),
        sliderInput(ns("opacity"), "Fill Opacity", min = 0, max = 1, value = 0.7, ticks=F, step = 0.1)
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
    # rw_range <-allow_named_choices(
    #   inputId = "range", #id of widget to extend
    #   update_function = updateSliderTextInput, #widget updater function
    #   input    = input,
    #   session  = session,
    #   init_choices  = range_ov,   #named choices, not only names
    #   init_selected = range_ov[2] #named select , not only name
    # )
    # rw_ssp <- allow_named_choices(
    #   inputId = "ssp", #id of widget to extend
    #   update_function = updateSliderTextInput, #widget updater function
    #   input    = input,
    #   session  = session,
    #   init_choices  = ssp_ov,   #named choices, not only names
    #   init_selected = ssp_ov[1] #named select , not only name
    # )
    # rw_sc <- allow_named_choices(
    #   inputId = "sc", #id of widget to extend
    #   update_function = updateSliderTextInput, #widget updater function
    #   input    = input,
    #   session  = session,
    #   init_choices  = sc_ov,   #named choices, not only names
    #   init_selected = sc_ov[3] #named select , not only name
    # )

    # OUTPUTS
    return(
      list(
        area = reactive(input$area),
        lev_per = reactive(input$lev_per),
        period = reactive(input$period),
        level = rw_level$read, # reactive(input$level)
        range = reactive(input$range), # rw_range$read,#
        adapt = reactive(input$adapt),
        agegroup = reactive(input$agegroup),
        ssp = reactive(input$ssp), #rw_ssp$read,#
        sc = reactive(input$sc), #rw_sc$read,#
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
