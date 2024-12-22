#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_map_ui <- function(id) {
  ns <- NS(id)

  # creates a card layout with inputs on the left and the map on the right
  card(
    height = 500, full_screen = FALSE,
    # displays the scenario label dynamically
    card_header(uiOutput(ns("scenariolabel"))),
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 3fr", grid_column_gap = "5px"),
      card_body(
        class = "p-0",
        mod_map_inputs_ui("inpbmap") # map input controls
      ),
      card_body(
        class = "p-0",
        shinycssloaders::withSpinner(
          maplibreOutput(ns("bmap"), height = "75vh"), type = 4 # map output with loading spinner
        )
      )
    )
  )
}


#' map Server Functions
#'
#' @importFrom stats quantile setNames
#'
#' @noRd
mod_map_server <- function(id, mapdata, spat, outc, scelab, opacity) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # initializes the map with a default style and zoom level
    output$bmap <- renderMaplibre({
      maplibre(
        style = carto_style("voyager"),
        center = c(10, 49.7),
        zoom = 3
      )
    })

    observe({
      if (nrow(mapdata()) == 0) {
        # handles cases where no data is available
        maplibre_proxy("bmap") %>%
          # layer_id, js-modify, object
          set_paint_property("fill_polys", "fill-opacity", 0) %>%
          set_paint_property("fill_circs", "circle-opacity", 0) %>%
          clear_legend()

        showModal(
          modalDialog(
            title = "No data",
            "Projected values for this scenario are unavailable. Please try a different selection or check the Help and Research pages for more information.",
            easyClose = TRUE,
            footer = NULL
          )
        )
        return()
      }

      # processes map data for rendering
      md <- mapdata()

      # defines column mappings based on selected outcome
      vnames <- c("est", "low", "high")
      suffixes <- paste0("_", vnames)
      vlabcols <- switch(
        outc(),
        "cuman" = c(list(leg_title = "<b>Cumulative excess deaths</b>"), setNames(paste0("cuman", suffixes), vnames)),
        "an" = c(list(leg_title = "<b>Excess deaths</b>"), setNames(paste0("an", suffixes), vnames)),
        "af" = c(list(leg_title = "<b>Attributable fraction (%)</b>"), setNames(paste0("af", suffixes), vnames)),
        "rate" = c(list(leg_title = "<b>Excess death rate (x10<sup>6</sup>)</b>"), setNames(paste0("rate", suffixes), vnames))
      )

      # retrieves values for visualization and defines color palettes
      v <- st_drop_geometry(md)[[vlabcols$est]]
      pal <- if (any(md[["range"]] %in% "heat")) {
        RColorBrewer::brewer.pal(5, "OrRd")
      } else if (any(md[["range"]] %in% "cold")) {
        RColorBrewer::brewer.pal(5, "PuBu")
      } else {
        rev(scico::scico(5, palette = "tokyo"))
      }

      # calculates legend labels and breaks
      quants <- quantile(v, seq(0, 1, 0.2), names = FALSE)
      pbreaks <- quants[-c(1, 6)]
      plabs <- paste(
        paste0("<b>", round(quants[-6], 2), "</b>"),
        paste0("<b>", round(quants[-1], 2), "</b>"),
        sep = " to "
      )

      # adds hover and popup information to map data
      md$popup <- glue::glue(
        "<b>{vlabcols$leg_title}: {round(md[[vlabcols$est]], 2)}</b><br>
         <b>95% eCI: </b>{round(md[[vlabcols$low]], 2)} to {round(md[[vlabcols$high]], 2)}"
      )
      md$hover <- if ("city_name" %in% names(md)) {
        glue::glue("<b>{md$city_name}</b>")
      } else if ("country_name" %in% names(md)) {
        glue::glue("<b>{md$country_name}</b>")
      } else {
        glue::glue("<b>{md$region}</b>")
      }

      # renders polygons or circles on the map based on spatial resolution
      if (spat() != "City") {
        maplibre_proxy("bmap") %>%
          clear_layer("fill_polys") %>%
          clear_layer("fill_circs") %>%
          add_fill_layer(
            id = "fill_polys",
            source = md,
            fill_outline_color = "darkgrey",
            before_id = "building",
            tooltip = "hover",
            popup = "popup",
            fill_color = step_expr(
              column = vlabcols$est,
              base = pal[1],
              stops = pal[2:5],
              values = pbreaks,
              na_color = "white"
            )
          ) %>%
          add_legend(
            type = "categorical",
            legend_title = vlabcols$leg_title,
            values = plabs,
            colors = pal,
            position = "top-right"
          )
      } else {
        maplibre_proxy("bmap") %>%
          clear_layer("fill_polys") %>%
          clear_layer("fill_circs") %>%
          add_circle_layer(
            id = "fill_circs",
            source = md,
            before_id = "building",
            tooltip = "hover",
            popup = "popup",
            circle_radius = 6,
            circle_stroke_color = "black",
            circle_stroke_opacity = .6,
            circle_stroke_width = .8,
            circle_color = step_expr(
              column = vlabcols$est,
              base = pal[1],
              stops = pal[2:5],
              values = pbreaks,
              na_color = "white"
            )
          ) %>%
          add_legend(
            type = "categorical",
            legend_title = vlabcols$leg_title,
            values = plabs,
            colors = pal,
            position = "top-right"
          )
      }
    }) %>% bindEvent(mapdata())

    # updates opacity of map layers based on input
    observe({
      maplibre_proxy("bmap") %>%
        set_paint_property("fill_polys", "fill-opacity", opacity()) %>%
        set_paint_property("fill_circs", "circle-opacity", opacity())
    }) %>% bindEvent(opacity())

    # dynamically renders the scenario label
    output$scenariolabel <- renderUI({
      HTML(scelab())
    })
  })
}


## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
