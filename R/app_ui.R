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
#' @import markdown
#' @noRd
#'
link_satrm <- tags$a(shiny::icon("temperature-high"), "Article", href = "https://www.nature.com/articles/s41591-024-03452-2", target = "_blank")
link_ehm <- tags$a(shiny::icon("people-group"), "EHM-Lab", href = "https://www.lshtm.ac.uk/research/centres-projects-groups/ehm-lab", target = "_blank")
link_ehmres <- tags$a(shiny::icon("magnifying-glass"), "EHM-Research", href= "https://www.lshtm.ac.uk/research/centres-projects-groups/ehm-lab#research", target = "_blank")

app_ui <- function(request) {
  tagList(
    # loads external resources, such as CSS or JS files
    golem_add_external_resources(),
    page_fillable(
      gap = "5px",
      tags$div(
        style = "
    background-color: #FFF;
    margin: 0;
    padding: 4px 8px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    font-weight: bold;
    border: 1px solid #dcdcdc;
    border-radius: 8px;
  ",
        p(
          style = "
      margin: 0;
      padding: 0;
      font-size:1.1em;
      line-height: 1.5em;
    ",
          "Visualizing future heat and cold related mortality across Europe"
        )
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
        # placeholder tab for research content, help or documentation
        nav_panel(title = "Information",
                  includeMarkdown("inst/app/www/info.md")
                  ),
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
    # add this line to include the CSS - somehow automatically applied
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  )
}
