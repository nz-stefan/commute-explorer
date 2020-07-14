################################################################################
# Generic UI helper functions
#
# Author: Stefan Schliebs
# Created: 2019-02-21 09:40:56
################################################################################


# Material card -----------------------------------------------------------

material_card <- function(..., header = NULL, bgcolor = "white") {
  div(
    class = "card",
    header, 
    div(class = "card-content", ..., style = sprintf("background-color: %s", bgcolor))
  )
}



# Tile header -------------------------------------------------------------

tile_header <- function(left_ui, right_ui, left_ui_width = 7) {
  right_ui_width = 12 - left_ui_width
  fluidRow(
    column(width = left_ui_width, left_ui),
    if (right_ui_width > 0)
      column(width = right_ui_width, right_ui, style = "margin-top: -7px; padding-right: 7px;")
  )
}



# Simple info box ----------------------------------------------------------

simple_infobox <- function(value, title, color = "orange") {
  div(
    class = paste("simple-infobox-default", paste0("simple-infobox-", color)),
    div(value, class = "simple-infobox-value"),
    div(title, class = "simple-infobox-title")
  )
}
