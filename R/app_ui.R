#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import bslib
#' @import pkgload
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
    # loads external resources, such as CSS or JS files
    golem_add_external_resources(),
    page_fillable(
      gap = "5px",
      # creates a banner with fixed dimensions, placeholder for title
      card(
        "IN-DEVELOPMENT ! - banner title tbd",
        padding = c(0),
        fill = FALSE,
        height = "60px",
        min_height = "60px",
        max_height = "60px"
      ),
      # main navigation with tabs
      navset_card_underline(
        id = "tabs",
        # tab for displaying the map, uses modular UI
        nav_panel(
          title = "Map",
          mod_map_ui("bmap")
        ),
        # tab for displaying a table, uses modular UI
        nav_panel(
          title = "Table",
          mod_table_ui("table")
        ),
        # placeholder tab for research content
        nav_panel(title = "Research"),
        # placeholder tab for help or documentation
        nav_panel(title = "Help"),
        # adds space for layout balance
        nav_spacer(),
        # menu for external links, items should be defined elsewhere
        nav_menu(
          title = "Links",
          nav_item(link_satrm),
          nav_item(link_ehm),
          nav_item(link_ehmres)
        )
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
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
      app_title = "vistemphip"
    ),
    # add this line to include the CSS
    # tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
}
