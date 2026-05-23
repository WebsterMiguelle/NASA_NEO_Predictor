# ==========================================
# 1. SETUP & LIBRARIES
# ==========================================
library(shiny)
library(bslib)
library(tidyverse)
library(randomForest)
library(plotly) 
library(bsicons) 

# Load Apraxia's Backend Files
rf_model <- readRDS("asteroid_rf_model.rds")
clean_data <- readRDS("clean_asteroids.rds")

# ==========================================
# 2. USER INTERFACE (UI)
# ==========================================
ui <- page_navbar(
  title = "NASA NEO Threat Assessment",
  
  # Allow the light/dark toggle to work
  theme = bs_theme(), 
  
  # INJECT THE TOGGLE
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode")),
  
  # --- TAB 1: Global Database (The Big Picture) ---
  nav_panel("Global Database",
            
            # The Top Row: Summary Value Boxes
            layout_column_wrap(
              width = 1/3, 
              value_box(
                title = "Total NEOs Tracked",
                value = textOutput("box_total_neos"),
                showcase = bsicons::bs_icon("globe"),
                theme = "primary" 
              ),
              value_box(
                title = "Confirmed Hazards",
                value = textOutput("box_hazards"), 
                showcase = bsicons::bs_icon("exclamation-triangle"),
                theme = "danger" 
              ),
              value_box(
                title = "Avg. Max Diameter",
                value = textOutput("box_avg_size"),
                showcase = bsicons::bs_icon("arrows-angle-expand"),
                theme = "info" 
              )
            ),
            
            # The Bottom Grid: 4 Global Analytics Charts (With Info Tooltips!)
            layout_column_wrap(
              width = 1/2, 
              
              card(
                card_header(tagList("Orbit Class Distribution ", tooltip(bsicons::bs_icon("info-circle"), "Shows the classification of asteroid orbits. Notice which classes, like Apollos (APO), historically contain the most hazardous objects due to their Earth-crossing paths."))), 
                plotlyOutput("eda_bar_chart")
              ),
              
              card(
                card_header(tagList("Threat Matrix: Size vs. Proximity ", tooltip(bsicons::bs_icon("info-circle"), "Plots asteroid size against miss distance. Objects in the top-left (closest to Earth and largest in diameter) represent the greatest theoretical threat."))), 
                plotlyOutput("eda_scatter_chart")
              ),
              
              card(
                card_header(tagList("Velocity Distribution ", tooltip(bsicons::bs_icon("info-circle"), "Compares the speed (Mean Motion) of safe vs. hazardous asteroids. Faster objects are harder to deflect and carry more kinetic energy upon impact."))), 
                plotlyOutput("eda_histogram")
              ),
              
              card(
                card_header(tagList("Magnitude (Brightness) Analysis ", tooltip(bsicons::bs_icon("info-circle"), "In astronomy, a lower magnitude means a brighter object, which usually indicates a larger physical size. Notice how confirmed hazards skew towards lower (brighter) magnitudes."))), 
                plotlyOutput("eda_boxplot")
              )
            )
  ),
  
  # --- TAB 2: The Hazard Scanner (Machine Learning) ---
  nav_panel("Hazard Scanner",
            layout_sidebar(
              
              # THE SIDEBAR (Now with Tooltips!)
              sidebar = sidebar(
                title = "Asteroid Parameters",
                
                accordion(
                  open = "Physical Traits", 
                  
                  accordion_panel("Physical Traits", icon = bsicons::bs_icon("boxes"),
                                  sliderInput("diameter_max_m", 
                                              label = tagList("Max Diameter (m) ", tooltip(bsicons::bs_icon("info-circle"), "The estimated maximum diameter in meters.")), 
                                              min = 10, max = 5000, value = 250),
                                  sliderInput("magnitude", 
                                              label = tagList("Absolute Magnitude (H) ", tooltip(bsicons::bs_icon("info-circle"), "In astronomy, magnitude is a measure of how bright a celestial object is. The lower the magnitude, the brighter the object.")), 
                                              min = 10, max = 30, value = 20)
                  ),
                  
                  accordion_panel("Orbital Geometry", icon = bsicons::bs_icon("bullseye"),
                                  sliderInput("eccentricity", 
                                              label = tagList("Eccentricity (0 to 1) ", tooltip(bsicons::bs_icon("info-circle"), "Measures how much an asteroid or astronomical object’s orbit deviates from a perfect circle.")), 
                                              min = 0, max = 1, value = 0.38, step = 0.01),
                                  sliderInput("inclination", 
                                              label = tagList("Inclination (deg) ", tooltip(bsicons::bs_icon("info-circle"), "The tilt of the asteroid’s orbit relative to the reference plane.")), 
                                              min = 0, max = 90, value = 13.2),
                                  sliderInput("perihelion_argument", 
                                              label = tagList("Perihelion Arg (deg) ", tooltip(bsicons::bs_icon("info-circle"), "The orientation of an elliptical orbit in space. The angular distance between the orbit's ascending node and its perihelion.")), 
                                              min = 0, max = 360, value = 180)
                  ),
                  
                  accordion_panel("Distances & Speed", icon = bsicons::bs_icon("speedometer2"),
                                  sliderInput("min_orbit_intersection", 
                                              label = tagList("Min Orbit Intersect (AU) ", tooltip(bsicons::bs_icon("info-circle"), "The distance between the closest points of the osculating orbits of two bodies. In this case, the distance between the closest points of an asteroid and Earth.")), 
                                              min = 0, max = 0.1, value = 0.05, step = 0.001),
                                  sliderInput("perihelion_distance", 
                                              label = tagList("Perihelion Dist (AU) ", tooltip(bsicons::bs_icon("info-circle"), "A celestial body’s closest orbital distance from the sun.")), 
                                              min = 0, max = 1.5, value = 0.8),
                                  sliderInput("semi_major_axis", 
                                              label = tagList("Semi-Major Axis (AU) ", tooltip(bsicons::bs_icon("info-circle"), "Average distance from the asteroid to the Sun.")), 
                                              min = 0.5, max = 3.0, value = 1.5),
                                  sliderInput("mean_motion", 
                                              label = tagList("Mean Motion (deg/day) ", tooltip(bsicons::bs_icon("info-circle"), "The angular speed required to complete one orbit.")), 
                                              min = 0.1, max = 2.0, value = 0.5)
                  )
                ),
                
                hr(), 
                # THE BUTTON UPGRADE: Added bold text, full width, and a shadow!
                actionButton("scan_btn", "SCAN FOR THREAT", class = "btn btn-danger btn-lg w-100 fw-bold shadow")
              ),
              
              # THE MAIN SCREEN
              card(
                card_header("System Output"),
                uiOutput("threat_alert") 
              ),
              card(
                card_header("Orbital Radar"),
                plotlyOutput("radar_plot") 
              )
            )
  ),
  
  # --- TAB 3: Explanatory Tab ---
  nav_panel("Mission Briefing", 
            card(
              h3("About This Dashboard"),
              p("Claiyax will paste the research and data dictionary here.")
            )
  ),
  
  # --- TAB 4: Data Source ---
  nav_panel("Database Link",
            card(
              p("Data sourced from NASA's Near-Earth Object Web Service via Kaggle.")
            )
  )
)

# ==========================================
# 3. SERVER LOGIC (The Brains)
# ==========================================
server <- function(input, output, session) {
  
  # --- DYNAMIC THEME DETECTOR ---
  t_colors <- reactive({
    if (is.null(input$dark_mode) || input$dark_mode == "dark") {
      list(text = "white", grid = "#444444")
    } else {
      list(text = "#222222", grid = "#cccccc")
    }
  })
  
  # ---------------------------------------------------------
  # SERVER: GLOBAL DATABASE TAB
  # ---------------------------------------------------------
  
  output$box_total_neos <- renderText({ format(nrow(clean_data), big.mark = ",") })
  
  output$box_hazards <- renderText({ format(sum(clean_data$potentially_hazardous == "TRUE"), big.mark = ",") })
  
  output$box_avg_size <- renderText({ paste0(round(mean(clean_data$diameter_max_m, na.rm = TRUE), 1), " m") })
  
  # 1. Bar Chart (Reactive Colors applied!)
  output$eda_bar_chart <- renderPlotly({
    plot_ly(clean_data, x = ~orbit_class_type, color = ~potentially_hazardous, 
            colors = c("#00e676", "#ff4d4d"), type = "histogram",
            hovertemplate = "<b>Orbit Class:</b> %{x}<br><b>Count:</b> %{y}<extra></extra>") |>
      layout(barmode = "stack",
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Orbit Class", gridcolor = t_colors()$grid),
             yaxis = list(title = "Asteroid Count", gridcolor = t_colors()$grid))
  })
  
  # 2. Scatter Chart (Reactive Colors applied!)
  output$eda_scatter_chart <- renderPlotly({
    plot_ly(clean_data, x = ~min_orbit_intersection, y = ~diameter_max_m, 
            color = ~potentially_hazardous, colors = c("#00e676", "#ff4d4d"),
            type = "scatter", mode = "markers", marker = list(opacity = 0.6),
            hoverinfo = "text",
            text = ~paste("<b>Hazard Status:</b>", potentially_hazardous,
                          "<br><b>Proximity:</b>", min_orbit_intersection, "AU",
                          "<br><b>Max Diameter:</b>", diameter_max_m, "m",
                          "<br><b>Speed:</b>", mean_motion, "deg/day")) |>
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Min Orbit Intersect (AU)", gridcolor = t_colors()$grid),
             yaxis = list(title = "Max Diameter (m)", gridcolor = t_colors()$grid))
  })

  # 3. Velocity Overlay (Reactive Colors applied!)
  output$eda_histogram <- renderPlotly({
    plot_ly(clean_data, x = ~mean_motion, color = ~potentially_hazardous, 
            colors = c("#00e676", "#ff4d4d"), type = "histogram", opacity = 0.7,
            hovertemplate = "<b>Speed:</b> %{x} deg/day<br><b>Count:</b> %{y}<extra></extra>") |>
      layout(barmode = "overlay", 
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Mean Motion (Speed)", gridcolor = t_colors()$grid),
             yaxis = list(title = "Frequency", gridcolor = t_colors()$grid))
  })

  # 4. Statistical Boxplot (Reactive Colors applied!)
  output$eda_boxplot <- renderPlotly({
    plot_ly(clean_data, x = ~potentially_hazardous, y = ~magnitude, 
            color = ~potentially_hazardous, colors = c("#00e676", "#ff4d4d"), type = "box") |>
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Hazard Status", gridcolor = t_colors()$grid),
             yaxis = list(title = "Absolute Magnitude (Brightness)", gridcolor = t_colors()$grid),
             showlegend = FALSE)
  })

  
  # ---------------------------------------------------------
  # SERVER: HAZARD SCANNER TAB
  # ---------------------------------------------------------
  
  observeEvent(input$scan_btn, {
    
    user_asteroid <- clean_data[1, ]
    
    user_asteroid$min_orbit_intersection <- as.numeric(input$min_orbit_intersection)
    user_asteroid$perihelion_distance <- as.numeric(input$perihelion_distance)
    user_asteroid$inclination <- as.numeric(input$inclination)
    user_asteroid$magnitude <- as.numeric(input$magnitude)
    user_asteroid$diameter_max_m <- as.numeric(input$diameter_max_m)
    user_asteroid$perihelion_argument <- as.numeric(input$perihelion_argument)
    user_asteroid$mean_motion <- as.numeric(input$mean_motion)
    user_asteroid$eccentricity <- as.numeric(input$eccentricity)
    user_asteroid$semi_major_axis <- as.numeric(input$semi_major_axis)
    
    prediction_matrix <- predict(rf_model, user_asteroid, type = "prob")
    hazard_prob <- round(prediction_matrix[1, "TRUE"] * 100, 1)
    
    is_hazardous <- hazard_prob >= 50
    
    # RENDER THE ALERT (Reactive Bootstrap alerts!)
    output$threat_alert <- renderUI({
      if(is_hazardous) {
        div(class = "alert alert-danger text-center shadow-sm", style = "padding: 20px; border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: bold;", "⚠️ HAZARDOUS OBJECT DETECTED"),
            div(style = "font-size: 24px; margin-top: 10px;", paste("Threat Probability:", hazard_prob, "%")))
      } else {
        div(class = "alert alert-success text-center shadow-sm", style = "padding: 20px; border-radius: 10px;",
            div(style = "font-size: 32px; font-weight: bold;", "✅ ORBIT CLEAR. NO THREAT."),
            div(style = "font-size: 24px; margin-top: 10px;", paste("Threat Probability:", hazard_prob, "%")))
      }
    })

    # RENDER THE RADAR (Reactive Colors applied!)
    output$radar_plot <- renderPlotly({
      miss_dist <- as.numeric(input$min_orbit_intersection)
      diameter <- as.numeric(input$diameter_max_m)
      speed <- as.numeric(input$mean_motion)
      
      ast_color <- ifelse(is_hazardous, "#ff4d4d", "#00e676")
      ast_status <- ifelse(is_hazardous, "HAZARD", "SAFE")
      
      radar_zoom <- max(miss_dist, 0.1) + 0.05
      scaled_size <- 10 + (sqrt(diameter) * 0.8)
      tail_length <- speed * 0.03
      
      plot_ly() |>
        add_paths(x = c(miss_dist + tail_length, miss_dist), y = c(tail_length, 0), line = list(color = ast_color, dash = "dot", width = 2), name = "Trajectory", hoverinfo = "none") |>
        add_markers(x = 0, y = 0, marker = list(size = 30, color = "#4DA8DA"), name = "Earth", hoverinfo = "text", text = "Earth (0 AU)") |>
        add_markers(x = miss_dist, y = 0, marker = list(size = scaled_size, color = ast_color, opacity = 0.3, symbol = "circle"), name = "Asteroid Size", hoverinfo = "none") |>
        add_markers(x = miss_dist, y = 0, marker = list(size = 10, color = ast_color, symbol = "cross", line = list(width = 2)), name = "Asteroid", hoverinfo = "text", 
                    text = paste("Asteroid", "\nMiss Dist:", miss_dist, "AU", "\nDiameter:", diameter, "m", "\nStatus:", ast_status)) |>
        layout(showlegend = FALSE, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
               font = list(color = t_colors()$text),
               xaxis = list(title = "Distance (AU)", range = c(-radar_zoom, radar_zoom), zeroline = FALSE, gridcolor = t_colors()$grid),
               yaxis = list(title = "", showticklabels = FALSE, range = c(-radar_zoom, radar_zoom), zeroline = FALSE, gridcolor = t_colors()$grid),
               shapes = list(list(type = 'circle', xref = 'x', yref = 'y', x0 = -0.05, y0 = -0.05, x1 = 0.05, y1 = 0.05, line = list(color = '#ffaa00', dash = 'dash'))))
    })
  })
}

# ==========================================
# 4. RUN THE APP
# ==========================================
shinyApp(ui = ui, server = server)