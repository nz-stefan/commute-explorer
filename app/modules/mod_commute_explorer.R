################################################################################
# Shiny module commute_explorer
#
# Author: Stefan Schliebs
# Created: 2020-06-26 10:59:40 UTC
################################################################################


# Module constants --------------------------------------------------------

ECHARTS_THEME <- "auritus"

COLOR_BLUE <- "#00a2eb"
COLOR_GREEN <- "#adb514"
COLOR_ORANGE <- "#fd9f02"
COLOR_PINK <- "#ce2c78"
COLOR_RED <- "#d32d05"
COLOR_PURPLE <- "#7522b8"



# Module UI ---------------------------------------------------------------

commute_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    fluidRow(
      column(
        width = 3, 
        uiOutput(ns("box_work_at_home"))
      ),
      column(
        width = 3,
        uiOutput(ns("box_commute_car"))
      ),
      column(
        width = 3,
        uiOutput(ns("box_commute_public"))
      ),
      column(
        width = 3,
        uiOutput(ns("box_commute_walk_or_jog"))
      )
    ),
    fluidRow(
      column(
        width = 4,
        material_card(
          tile_header(
            uiOutput(ns("charts_headline")),
            NULL,
            # div(
            #   style = "text-align: right; margin-top: 2px;",
            #   uiOutput(ns("total_commuters"))
            #   # prettyToggle(
            #   #   inputId = ns("from_to"),
            #   #   label_on = "HOME", label_off = "WORK",
            #   #   value = TRUE,
            #   #   inline = TRUE, shape = "round", fill = FALSE, plain = TRUE, width = "100%", bigger = TRUE,
            #   #   icon_on = icon("home"), icon_off = icon("building")
            #   # )
            # ),
            # pickerInput(ns("from_to"), label = NULL, choices = c(`home` = "home", `work` = "work")),
            left_ui_width = 12
          ),
          echarts4rOutput(ns("chart_work_at_home"), height = "125px"),
          echarts4rOutput(ns("chart_commute_car"), height = "125px"),
          echarts4rOutput(ns("chart_commute_public"), height = "125px"),
          echarts4rOutput(ns("chart_commute_walk_or_jog"), height = "125px"),
          bgcolor = "#262626"
        )
      ),
      column(
        width = 8,
        material_card(
          tile_header(
            uiOutput(ns("map_headline")),
            pickerInput(ns("region"), label = NULL, choices = NULL, width = "100%"),
            left_ui_width = 8
          ),
          div(
            style = "margin: 0 -14px -14px -14px",
            leafletOutput(ns("map"), height = "514px"),
            absolutePanel(
              prettyRadioButtons(
                inputId = ns("from_to"),
                choices = c(OUTBOUND = "home", INBOUND = "work"),
                selected = "home",
                label = NULL,
                width = "auto",
                animation = "jelly"
              ),
              top = "75px", right = "15px"
            )
          ),
          bgcolor = "#262626"
        )
      )
    )
  )
}



# Module logic ------------------------------------------------------------

commute_explorer <- function(input, output, session, d_data_model) {
  ns <- session$ns

  # Unpack data model -------------------------------------------------------

  d_commute <- reactive(d_data_model()$d_commute)
  shapes <- reactive(d_data_model()$shapes)
  

  
  # Data aggregations -------------------------------------------------------

  # commute data filtered by the selected region
  d_filtered <- reactive({
    req(d_commute(), input$region)
    
    # filter shapes to only show selected region
    if (input$region != "All Regions") {
      d_commute() %>% 
        filter(commute_to_region == input$region | commute_from_region == input$region)
    } else {
      d_commute()
    }
  })
  
  # aggregated commute numbers by home or work SA  
  d_aggregated_area <- reactive({
    req(d_filtered())
    
    if (selected_from_to() == "home") {
      d <- 
        d_filtered() %>%
        select(-starts_with("commute_from"), -starts_with("commute_to"), area = commute_from)
    } else {
      d <- 
        d_filtered() %>%
        select(-starts_with("commute_from"), -starts_with("commute_to"), area = commute_to)
    }
    
    d %>%
      select(-commute_other, -commute_all, -commute_bus, -commute_train, -commute_ferry) %>%
      gather(var, value, -area) %>% 
      group_by(var, area) %>%
      summarise(value = sum(value, na.rm = TRUE)) %>% 
      group_by(area) %>% 
      mutate(ratios = value / sum(value, na.rm = TRUE)) %>% 
      ungroup()
  })
  
  

  # Populate region field ---------------------------------------------------

  observe({
    req(d_commute())
    
    choices <- c("All Regions", unique(d_commute()$commute_from_region) %>% sort())
    default_region <- "Auckland Region"
    
    updatePickerInput(session, "region", choices = choices, selected = default_region)
  })
  
  selected_region <- reactive({
    req(input$region)
    center_map(TRUE)
    selected_area(NULL)
    input$region
  })
  
  selected_from_to <- reactiveVal("home")
  
  observeEvent(input$from_to, {
    center_map(FALSE)
    selected_from_to(input$from_to)
    # if (input$from_to) selected_from_to("home") else selected_from_to("work")
    update_selected_area(isolate(map_shape_click()$id))
  })

  

  # Map ---------------------------------------------------------------------

  center_map <- reactiveVal(TRUE)
  
  output$map <- renderLeaflet({
    # show an empty map centered on Auckland
    # we leave the rendering of the polygons to the observe function
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lat = -36.848461, lng = 174.763336, zoom = 7)
  })
  
  observe({
    req(selected_region(), shapes(), d_filtered())
    
    # decide whether to show home or work regions
    if (isolate(selected_from_to()) == "home") {
      areas <- unique(c(d_filtered()$commute_from_code))
      polygon_color <- "#ffffbb"
    } else {
      areas <- unique(c(d_filtered()$commute_to_code))
      polygon_color <- "#ccffcc"
    }
    
    # filter shapes to only show selected region
    if (selected_region() != "All Regions") {
      d <- shapes()[shapes()$SA22018_V1 %in% areas,]
    } else {
      d <- shapes()
    }
    
    # prepare a nice tooltip label
    labels <- 
      d %>% 
      as_tibble() %>% 
      select(area = SA22018__1) %>% 
      left_join(d_aggregated_area(), by = "area") %>% 
      mutate(ratios = round(ratios * 100)) %>% 
      pivot_wider(names_from = var, values_from = c(value, ratios)) %>%
      mutate(
        label = glue(
          # if(selected_from_to() == "home") "<strong>Commuting from</strong>" else "<strong>Commuting into</strong>", 
          "<strong>{area}</strong><br>",
          "<table width='100%'>",
          "<tr><td>Bike:</td><td style='text-align: right; padding-left: 10px'>{value_commute_bicycle}</td><td style='text-align: right; padding-left: 10px'>{ratios_commute_bicycle}%</td></tr>",
          "<tr><td>Car:</td><td style='text-align: right; padding-left: 10px'>{value_commute_car}</td><td style='text-align: right; padding-left: 10px'>{ratios_commute_car}%</td></tr>",
          "<tr><td>Public Transport:</td><td style='text-align: right; padding-left: 10px'>{value_commute_public}</td><td style='text-align: right; padding-left: 10px'>{ratios_commute_public}%</td></tr>",
          "<tr><td>Walk or Jog</td><td style='text-align: right; padding-left: 10px'>{value_commute_walk_or_jog}</td><td style='text-align: right; padding-left: 10px'>{ratios_commute_walk_or_jog}%</td></tr>",
          "<tr><td>Work at Home</td><td style='text-align: right; padding-left: 10px'>{value_work_at_home}</td><td style='text-align: right; padding-left: 10px'>{ratios_work_at_home}%</td></tr>",
          "</table>"
        )
      ) %>% 
      pull(label) %>% 
      lapply(HTML)   # wrap all labels with HTML() to treat label text as formatted HTML
    
    # update the map
    m <- 
      leafletProxy("map", data = d) %>% 
      clearShapes() %>% 
      clearControls() %>% 
      addPolygons(
        color = polygon_color, weight = 1, smoothFactor = 0.5,
        opacity = 0.5, fillOpacity = 0.05,
        highlightOptions = highlightOptions(color = "white", weight = 2, fillOpacity = 0.2, bringToFront = TRUE),
        label = labels, labelOptions = labelOptions(className = "leaflet-tooltip", opacity = 0.7),
        layerId = ~SA22018__1
      )
    
    # recenter the map so all polygons fit into map boundaries
    if (isolate(center_map())) {
      # compute bounding box of shapes subset
      bounds <- sf::st_bbox(d) %>% as.numeric()
      m <- m %>% flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4]) 
      center_map(FALSE)
    }
    
    isolate(draw_map_highlighting(m))
  })
  


  # Map clicks --------------------------------------------------------------

  draw_map_highlighting <- function(map_proxy) {
    # if no area is selected then we don't need any highlighting and exit early
    if (is.null(selected_area())) return()

    # decide whether to use home or work places to select from
    if (selected_from_to() == "home") {
      d_commuter_areas <- 
        d_filtered() %>% 
        filter(commute_from == selected_area()$area) %>% 
        select(area = commute_to, commute_all)
    } else {
      d_commuter_areas <- 
        d_filtered() %>% 
        filter(commute_to == selected_area()$area) %>% 
        select(area = commute_from, commute_all)
    }
    
    # determine areas to highlight in the map
    areas <- d_commuter_areas %>% distinct(area) %>% pull()
    
    # remove clicked area from list of highlighted shapes, we show the clicked area with a different highlight
    areas <- setdiff(areas, selected_area()$area)
    
    # identify shape that was clicked on
    highlight_shape <- shapes()[shapes()$SA22018__1 == selected_area()$area,]
    
    # filter shapes to only show selected region
    d <- shapes()[shapes()$SA22018__1 %in% areas,]
    
    # add commuter stats into selected shapes
    d_complete <- merge(d, d_commuter_areas, by.x = "SA22018__1", by.y = "area")
    
    # define a palette to colorise areas according to the number of commuters
    pal <- colorBin("YlOrRd", domain = range(d_commuter_areas$commute_all, na.rm = TRUE))
    
    # update map and highlight the selected shape
    map_proxy %>% 
      removeShape("highlight") %>%
      removeShape(paste0("highlighted_", highlighted_areas())) %>%
      clearControls() %>% 
      addMapPane("highlight", zIndex = 401) %>%
      addPolygons(
        data = highlight_shape, color = "#ce2c78", weight = 1, fillOpacity = 0.75,
        options = pathOptions(pane = "highlight", clickable = FALSE, layerId = "highlight")
      ) %>% 
      addPolygons(
        data = d_complete,
        color = ~pal(commute_all), weight = 1, 
        fillColor = ~pal(commute_all), fillOpacity = 0.5,
        options = pathOptions(pane = "highlight", clickable = FALSE),
        layerId = ~paste0("highlighted_", SA22018__1)
      ) %>% 
      addLegend(
        data = d_complete,
        pal = pal, values = ~pal(commute_all), 
        opacity = 0.7, title = "# commuters",
        position = "bottomright"
      )
    
    # keep a record of the highlighted areas, so we can delete them later from the map
    highlighted_areas(areas)
  }
  
  selected_area <- reactiveVal(NULL)
  highlighted_areas <- reactiveVal(NULL)
  
  map_click <- reactiveVal()
  observeEvent(input$map_click, map_click(input$map_click))

  map_shape_click <-reactiveVal() 
  observeEvent(input$map_shape_click, map_shape_click(input$map_shape_click))
  
  update_selected_area <- function(id) {
    if (is.null(id)) return()
    
    d <- 
      isolate(d_aggregated_area()) %>% 
      filter(area == id) %>% 
      select(area, var, ratios) %>% 
      spread(var, ratios)
    
    # If we switched regions between map clicks then `d` can be an empty dataframe
    # which breaks downstream processes. We prevent this by clearing the selected
    # area instead of passing the zero-row dataframe.
    if (nrow(d) == 0) d <- NULL
    
    selected_area(d)
    
    return()
  }
  
  # capture clicks into map
  observeEvent(map_click(), {
    # we capture all clicks in the map, but we are only interested in clicks on shapes
    # event <- input$map_shape_click
    event <- map_shape_click()

    # if no area is selected and user did not click on a shape (click somewhere else on the map)
    # then we return early.
    if (is.null(selected_area()) && is.null(event)) return()
    
    # we only update selected_area() if the clicked shape is different from the previous click event
    # if the previous click event is identical with the captured event, then either no shape was clicked
    # on (e.g. click somewhere else on the map) or the same shape was clicked on again. in this case we
    # clear the selection, i.e. we "unclick".
    if (is.null(selected_area()) || selected_area()$area != event$id) {
      update_selected_area(event$id)      
      draw_map_highlighting(leafletProxy("map"))
    } else {
      # clear selection, this will tell the chart to plot data for the entire region
      selected_area(NULL)
      map_shape_click(NULL)

      # remove highlighted shapes from map
      leafletProxy("map") %>% 
        removeShape(paste0("highlighted_", highlighted_areas())) %>% 
        removeShape("highlight") %>% 
        clearControls()
    }
  })

  

  # Map headline ------------------------------------------------------------

  output$map_headline <- renderUI({
    req(input$region, d_filtered())
    
    n_commuters <- sum(d_filtered()$commute_all, na.rm = TRUE)

    if (! is.null(selected_area()) && selected_from_to() == "home") {
      tagList(
        h4(selected_area()$area, class = "tile-headline"),
        h5("Outbound Commuters from Place of Residence", class = "tile-subheadline")
      )
    } else if (! is.null(selected_area()) && selected_from_to() == "work") {
      tagList(
        h4(selected_area()$area, class = "tile-headline"),
        h5("Inbound Commuters to Place of Work", class = "tile-subheadline")
      )
    } else if (is.null(selected_area()) && selected_from_to() == "home") {
      tagList(
        h4("Places of Residence in Region", class = "tile-headline"),
        h5(input$region, class = "tile-subheadline")
      )
    } else if (is.null(selected_area()) && selected_from_to() == "work") {
      tagList(
        h4("Places of Work for Residents in Region", class = "tile-headline"),
        h5(input$region, class = "tile-subheadline")
      )
    }
  })

  
  
  # Chart headline ----------------------------------------------------------

  output$charts_headline <- renderUI({
    tagList(
      h4("Mode of Travel", class = "tile-headline"),
      if (selected_from_to() == "home") {
        h5("Histogram of Fraction of Outbound Commuters", class = "tile-subheadline")
      } else {
        h5("Histogram of Fraction of Inbound Commuters", class = "tile-subheadline")
      }
    )
  })
  
  counter_js_func <- function(id) {
    glue(
      "sleep(500).then(() => {{",
      "  var number = $('#{id}').data('countto');",
      "  $('#{id}').countTo({{from: 0, to: number, speed: 1000, refreshInterval: 30}});",
      "}}); "
    )
  }
  
  # output$total_commuters <- renderUI({
  #   req(d_filtered())
  #   if (! is.null(selected_area())) {
  #     if (selected_from_to() == "home") 
  #       d <- d_filtered() %>% rename(commute = commute_from)
  #     else 
  #       d <- d_filtered() %>% rename(commute = commute_to)
  #     n_commuters <- sum(d %>% filter(commute %in% selected_area()$area) %>% pull(commute_all), na.rm = TRUE) 
  #   } else {
  #     n_commuters <- sum(d_filtered()$commute_all, na.rm = TRUE)
  #   }
  #   
  #   tagList(
  #     h4(id = ns("commuters"), class = "tile-headline", `data-countto` = n_commuters, style = "font-size: 16px; margin-top: -2px;"),
  #     h5("Commuters", class = "tile-subheadline"),
  #     shinyjs::runjs(counter_js_func(ns("commuters")))
  #   )
  # })
  
  

  # Charts ------------------------------------------------------------------

  d_histogram <- reactive({
    req(d_aggregated_area())

    d_aggregated_area() %>% 
      group_by(var) %>% 
      nest() %>% 
      mutate(
        histogram = map(data, function(d) {
          if (all(is.na(d$ratios))) return(tibble(bin = c(0, 1), value = c(0, 0)))
          
          h <- hist(d$ratios, breaks = seq(0, 1, length.out = 21), plot = FALSE)
          tibble(bin = h$breaks[-1] - 0.025, value = h$counts)
        })
      ) %>% 
      select(-data) %>% 
      unnest(histogram) %>% 
      ungroup()
  })
  
  histogram_tooltip_js <- function(mode_of_travel, from_to) {
    mot <- stringr::str_to_title(mode_of_travel)
    if (from_to == "home") {
      direction = "From"
      destination = "Places of Residence"
    } else {
      direction = "Into"
      destination = "Places of Work"
    }
    
    htmlwidgets::JS(glue(
      "function(params, ticket, callback) {{",
      # "  console.log(ticket); console.log(params);",
      "  var ratio_from = Math.round(100 * (params[0].value[0] - 0.025));",
      "  var ratio_to = Math.round(ratio_from + 5);",
      "  return('<strong>' + ratio_from + '%&ndash;' + ratio_to + '%</strong> {mot}<br>{direction} <strong>' + params[0].value[1] + '</strong> {destination}')",
      # "  return('In <strong>' + params.value[1] + '</strong> Places of Residence<br><strong>' + ratio_from + '%&ndash;' + ratio_to + '%</strong> commute by car');",
      "}}"
    ))
  }
  
  make_chart <- function(d, selected_val, bar_color, chart_title) {
    if (!is.null(selected_val)) {
      # browser()
      p <- 
        d %>% 
        mutate(
          diff_to_selected = abs(bin - selected_val),
          selected_bin = diff_to_selected == min(diff_to_selected),
          selected_value = ifelse(selected_bin, value, 0)
        ) %>% 
        select(bin, value, selected_value) %>% 
        gather(key, val, -bin) %>%
        group_by(key) %>% 
        e_chart(bin, dispose = FALSE) %>% 
        e_bar(val, legend = FALSE, stack = "grp") %>% 
        e_color(color = c("#aaa", bar_color))
    } else {
      p <- 
        d %>% 
        mutate(selected_value = 0) %>% 
        select(bin, value, selected_value) %>% 
        gather(key, val, -bin) %>% 
        group_by(key) %>% 
        e_chart(bin, dispose = FALSE) %>% 
        e_bar(val, legend = FALSE, stack = "grp") %>% 
        e_color(color = c(bar_color, "#aaa"))
    }
    
    p %>% 
      e_y_axis(show = FALSE) %>%
      e_x_axis(formatter = e_axis_formatter("percent"), min = 0, max = 1) %>%
      e_grid(top = "30px", left = "10px", right = "25px", bottom = "35px") %>% 
      e_title(subtext = chart_title, left = "center", subtextStyle = list(fontFamily = "Roboto Condensed")) %>% 
      e_tooltip(
        trigger = "axis",
        formatter = histogram_tooltip_js(chart_title, selected_from_to()),
        confine = TRUE,
        textStyle = list(fontFamily = "Roboto Condensed", fontSize = 12)
      ) %>% 
      e_theme(ECHARTS_THEME)
  }
  
  output$chart_work_at_home <- renderEcharts4r({
    req(d_histogram())
    
    d_histogram() %>% 
      filter(var == "work_at_home") %>% 
      make_chart(selected_area()$work_at_home, COLOR_BLUE, "Work At Home")
  })

  output$chart_commute_car <- renderEcharts4r({
    req(d_histogram())
    
    d_histogram() %>% 
      filter(var == "commute_car") %>% 
      make_chart(selected_area()$commute_car, COLOR_ORANGE, "Commute By Car")
  })

  output$chart_commute_public <- renderEcharts4r({
    req(d_histogram())
    
    d_histogram() %>% 
      filter(var == "commute_public") %>% 
      make_chart(selected_area()$commute_public, COLOR_RED, "Commute By Public Transport")
  })

  output$chart_commute_walk_or_jog <- renderEcharts4r({
    req(d_histogram())
    
    d_histogram() %>% 
      filter(var == "commute_walk_or_jog") %>% 
      make_chart(selected_area()$commute_walk_or_jog, COLOR_PURPLE, "Walk Or Jog")
  })

  

  # Infoboxes ---------------------------------------------------------------

  output$box_work_at_home <- renderUI({
    req(d_filtered())
    if (! is.null(selected_area())) {
      if (selected_from_to() == "home") 
        d <- d_filtered() %>% rename(commute = commute_from)
      else 
        d <- d_filtered() %>% rename(commute = commute_to)
      n_commuters <- sum(d %>% filter(commute %in% selected_area()$area) %>% pull(work_at_home), na.rm = TRUE) 
    } else {
      n_commuters <- sum(d_filtered()$work_at_home, na.rm = TRUE)
    }
    
    tagList(
      simple_infobox(
        div("0", id = ns("value_work_at_home"), `data-countto` = n_commuters),
        "WORK AT HOME", color = "blue"
      ),
      shinyjs::runjs(counter_js_func(ns("value_work_at_home")))
    )
  })

  output$box_commute_car <- renderUI({
    req(d_filtered())
    if (! is.null(selected_area())) {
      if (selected_from_to() == "home") 
        d <- d_filtered() %>% rename(commute = commute_from)
      else 
        d <- d_filtered() %>% rename(commute = commute_to)
      n_commuters <- sum(d %>% filter(commute %in% selected_area()$area) %>% pull(commute_car), na.rm = TRUE) 
    } else {
      n_commuters <- sum(d_filtered()$commute_car, na.rm = TRUE)
    }
    
    tagList(
      simple_infobox(
        div("0", id = ns("value_commute_car"), `data-countto` = n_commuters),
        "COMMUTE BY CAR", color = "orange"
      ),
      shinyjs::runjs(counter_js_func(ns("value_commute_car")))
    )
  })
  
  output$box_commute_public <- renderUI({
    req(d_filtered())
    if (! is.null(selected_area())) {
      if (selected_from_to() == "home") 
        d <- d_filtered() %>% rename(commute = commute_from)
      else 
        d <- d_filtered() %>% rename(commute = commute_to)
      n_commuters <- sum(d %>% filter(commute %in% selected_area()$area) %>% pull(commute_public), na.rm = TRUE) 
    } else {
      n_commuters <- sum(d_filtered()$commute_public, na.rm = TRUE)
    }
    
    tagList(
      simple_infobox(
        div("0", id = ns("value_commute_public"), `data-countto` = n_commuters),
        "COMMUTE BY PUBLIC TRANSPORT", 
        color = "red"
      ),
      shinyjs::runjs(counter_js_func(ns("value_commute_public")))
    )
  })

  output$box_commute_walk_or_jog <- renderUI({
    req(d_filtered())
    if (! is.null(selected_area())) {
      if (selected_from_to() == "home") 
        d <- d_filtered() %>% rename(commute = commute_from)
      else 
        d <- d_filtered() %>% rename(commute = commute_to)
      n_commuters <- sum(d %>% filter(commute %in% selected_area()$area) %>% pull(commute_walk_or_jog), na.rm = TRUE) 
    } else {
      n_commuters <- sum(d_filtered()$commute_walk_or_jog, na.rm = TRUE)
    }
    
    tagList(
      simple_infobox(
        div("0", id = ns("value_commute_walk_or_jog"), `data-countto` = n_commuters),
        "WALK OR JOG", 
        color = "purple"
      ),
      shinyjs::runjs(counter_js_func(ns("value_commute_walk_or_jog")))
    )
  })
}  
