#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bslib
#' @import mapgl
#' @import arrow
#' @import sf
#' @import sfarrow
#' @rawNamespace import(dplyr, except=c(first, last, between))
#' @import data.table
#' @import htmltools
#' @import waiter
#' @import shinyWidgets
#' @import shinyjqui
#' @import glue
#' @import crayon
#' @noRd
#'
link_satrm <- tags$a(shiny::icon("temperature-high"), "Article", href = "https://www.thelancet.com/journals/lanplh/article/PIIS2542-5196(22)00138-3/fulltext", target = "_blank")
link_ehm <- tags$a(shiny::icon("people-group"), "EHM-Lab", href = "https://www.lshtm.ac.uk/research/centres-projects-groups/ehm-lab", target = "_blank")
link_ehmres <- tags$a(shiny::icon("magnifying-glass"), "EHM-Research", href= "https://www.lshtm.ac.uk/research/centres-projects-groups/ehm-lab#research", target = "_blank")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fillable(
    gap="5px",
    card("Health impact projection update", padding=c(0), fill = FALSE,
         height="60px", min_height="60px",max_height="60px"),
    navset_card_underline(
      nav_panel(
        title="Map",
        mod_map_ui("bmap"),
        jqui_resizable(
          absolutePanel(
                        id = "controls", class = "float-panel",
                        fixed = TRUE, draggable = TRUE,
                        # for reproducible positioning 2/3 specified
                        top = "28%", bottom = "auto", height = 300,
                        left = 65, right = "auto",  width = 420,
                        mod_map_inputs_ui("inpbmap")
          )
        )
      ),
      nav_panel(title="Table",mod_table_ui("table")),
      nav_panel(title="Research"),
      nav_panel(title="Help"),
      nav_spacer(),
      nav_menu(title = "Links", nav_item(link_satrm), nav_item(link_ehm), nav_item(link_ehmres))
    )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ehmhipmapp"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css") # add this line to include the CSS
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
