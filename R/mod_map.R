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
  card(
    height = 500, full_screen = FALSE,
    card_header(uiOutput(ns("scenariolabel"))),
    card_body(
      class = "p-0", # removes padding
      shinycssloaders::withSpinner(
        maplibreOutput(ns("bmap"), height = "75vh"), type=4)
    )
  )
}

#' map Server Functions
#'
#' @noRd
mod_map_server <- function(id, mapdata, spat, outc, scelab, opacity){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$bmap <- renderMaplibre({

      maplibre(style = carto_style("voyager"),
               center = c(11,48), # or the other way around
               zoom = 3) |>
        # can add_line_layer() for some outline layers controllable by map button before_id="building"
        add_layers_control(position = "bottom-left", layers=NULL, collapsible = TRUE)

    })

  observe({

    req(nrow(mapdata()) > 0)
    # cannot get the detailed warning to work
    # , {
    #   showModal(
    #     modalDialog(
    #       title="No data",
    #       "Projected values for this scenario were not calculated, please make a different selection and explore the 'Impossible scenarios' section in the Help tab.", easyClose=TRUE, footer = NULL)
    #     )
    #   return()
    # })

    # all the colours and palette etc
    # RETRIEVE DATA
    md <- mapdata()



    vnames <- c("est", "low", "high")
    suffixes <- paste0("_",vnames)

    # SET VALUE COLUMN probably shorter as switch that reutnrs named list
    vlabcols <- switch(outc(),
                     "cuman" = c(list(leg_title = "<b>Cumulative excess deaths</b>"),
                                 setNames(paste0("cuman", suffixes), vnames)),
                     "an" = c(list(leg_title = "<b>Attributable number</b>"),
                              setNames(paste0("an", suffixes), vnames)),
                     "af" = c(list(leg_title = "<b>Attributable fraction (%)</b>"),
                              setNames(paste0("af", suffixes), vnames)),
                     "rate" = c(list(leg_title = "<b>Excess death rate (x10⁶)</b>"),
                                setNames(paste0("rate", suffixes), vnames))
    )
    # to use results, index vlabcols e.g. vlabcols$est -> "cuman_est"

    # RESULTS VALUES
    v <- st_drop_geometry(md)[[vlabcols$est]]

        # COLOR PALETTES
    if (any(md[["range"]] %in% c("heat"))) {
      pal <- RColorBrewer::brewer.pal(5, "OrRd")
    } else if (any(md[["range"]] %in% c("cold"))) {
      pal <- RColorBrewer::brewer.pal(5,"PuBu")
    } else if (any(md[["range"]] %in% c("tot"))) {
      #pal <- RColorBrewer::brewer.pal(5,"GrPu")
      pal <- scico::scico(5, palette = "tokyo")
    }

    # LEGEND LABELS
    quants <- quantile(v, seq(0,1,0.2),names=FALSE)
    pbreaks <- quants[-c(1,6)]
    plabs <- paste(paste0("<b>",round(quants[-6],2),"</b>"), paste0("<b>",round(quants[-1],2),"</b>"),sep=" to ")

    # TOOLTIPS + POPUPS -> ADD TO MAPDATA
    # hover
    md$popup <- glue::glue(
      "<b>{vlabcols$leg_title}: {round(md[[vlabcols$est]],2)}</b><br>
        <b>95% eCI: </b>{round(md[[vlabcols$low]],2)} to {round(md[[vlabcols$high]],2)}"
    )

    if (all(c("city_name","country_name")%in%names(md))) {
      md$hover <- glue::glue("<b>{md$city_name}</b>")
    } else if (c("country_name")%in%names(md)) {
      md$hover <- glue::glue("<b>{md$country_name}</b>")
    } else if ("region"%in%names(md)) {
      md$hover <- glue::glue("<b>{md$region}</b>")
    }


    if (spat()!="City") {

      maplibre_proxy("bmap") %>%
        clear_layer("fill_polys") %>%
        clear_layer("fill_circs") %>%
        add_fill_layer(id="fill_polys",
                       source=md,
                       fill_outline_color = "darkgrey",
                       before_id="building",
                       tooltip="hover",popup="popup",
                       fill_color = step_expr(
                         column = vlabcols$est,
                         base = pal[1],
                         stops = pal[2:5],
                         values = pbreaks,
                         na_color = "white")) %>%
        add_legend(
          type = "categorical",
          legend_title=vlabcols$leg_title,
          values = plabs,
          colors = pal,
          position = "top-right"
        )


    } else {

      maplibre_proxy("bmap") %>%
        clear_layer("fill_circs") %>%
        clear_layer("fill_polys") %>%
        add_circle_layer(id="fill_circs",
                         source=md,
                         before_id = "building",
                         tooltip="hover",popup="popup",
                         circle_color=step_expr(
                           column = vlabcols$est,
                           base = pal[1],
                           stops = pal[2:5],
                           values = pbreaks,
                           na_color = "white")) %>%
        add_legend(
          type = "categorical",
          legend_title=vlabcols$leg_title,
          values = plabs,
          colors = pal,
          position = "top-right"
        )
    }
  })

  # CHANGES OPACITY OF FILLED POLYGONS
  observe({
    maplibre_proxy("bmap") |>
      # layer_id, js-modify, object
      set_paint_property("fill_polys", "fill-opacity", opacity()) |>
      set_paint_property("fill_circs","circle-color",opacity())
  }) %>% bindEvent(opacity())

  output$scenariolabel <- renderUI({

    HTML(scelab())

  })

})}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
