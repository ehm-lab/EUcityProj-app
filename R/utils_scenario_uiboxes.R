#' scenario_uiboxes
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
utils_scenario_ui_boxes <- function(scelab){

  # can be copy pasted to the table tab
  levorper <- switch(scelab$le_pe(),
                     "Warming level" = "Global warming level",
                     "Five-year periods" = "Period")
  levorper_val <- switch(scelab$le_pe(),
                         "Warming level" = paste0(scelab$lev(), "°C"),
                         "Five-year periods"=scelab$perio())

  inc_scen <- paste0("Including ",switch(as.character(scelab$s()),
                                         "clim" = "climate change",
                                         "demo" = "demographic change",
                                         "full" = "climate and demographic change")," effects")

  layout_column_wrap(
    width = 1/4,
    div(
      style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
      p(strong(levorper)),
      p(levorper_val)
    ),
    div(
      style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
      p(strong("Temperature range:"),names(range_ov[match(scelab$rang(), range_ov)])),
      p(strong("Age group:"),names(agegroup_ov[match(scelab$agegr(), agegroup_ov)]))
    ),
    div(
      style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
      p(strong("Adaptation to heat:"), scelab$adap()),
      p(strong("Shared Socioeconomic Pathway:"), names(ssp_ov[match(scelab$ss(), ssp_ov)]))
    ),
    div(
      style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9; line-height:1; font-size:0.9em;",
      p(strong(inc_scen))
    )
  )

}
