################################################################################
# UI of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:34:50
################################################################################


htmlTemplate(
  filename = "www/index.html",
  blog_counter = count_icon_ui("total_commuters_counter", icon = "icon-map", icon_text = "Total Commuters"),
  author_counter = count_icon_ui("total_work_at_home_counter", icon = "icon-desktop", icon_text = "Work At Home"),
  word_counter = count_icon_ui("total_public_counter", icon = "icon-hotairballoon", icon_text = "Public Transport"),
  page_counter = count_icon_ui("total_walk_or_bike_counter", icon = "icon-bike", icon_text = "Walkers or Bikers"),
  # insights_charts = "TODO", #insights_charts_ui("insights_charts"),
  commute_explorer = commute_explorer_ui("commute_explorer"),
  last_refresh = uiOutput("last_refresh")
  # topics_graph = "TODO" #topics_graph_ui("topics_graph"),
)
