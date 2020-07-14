################################################################################
# Server logic of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-13 09:21:18
################################################################################


server <- function(input, output, session) {
  
  # load data model
  d_data_model <- reactive({ readRDS("data/data-model.rds") })
  
  # add server logic for count icons
  callModule(count_icon, "total_commuters_counter", d_data_model, "total_commuters")
  callModule(count_icon, "total_work_at_home_counter", d_data_model, "total_work_at_home")
  callModule(count_icon, "total_public_counter", d_data_model, "total_public")
  callModule(count_icon, "total_walk_or_bike_counter", d_data_model, "total_walk_or_bike")

  # add server logic for the commute explorer
  callModule(commute_explorer, "commute_explorer", d_data_model)
  
  output$last_refresh <- renderUI({
    last_refresh_formatted <- strftime(d_data_model()$last_refresh, format = "%d %b %Y")
    HTML(glue("Last data refresh occurred on <strong>{ last_refresh_formatted }</strong>."))
  })
}