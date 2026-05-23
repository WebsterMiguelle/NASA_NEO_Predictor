# ==========================================

library(shiny)
library(bslib)
library(tidyverse)
library(randomForest)
library(plotly) 
library(bsicons) 
library(shinythemes)

addResourcePath(prefix = "img", directoryPath = "www")

rf_model <- readRDS("asteroid_rf_model.rds")
clean_data <- readRDS("clean_asteroids.rds")

ui <- page_navbar(
  title = "☄️NASA NEO Threat Assessment",
  fillable = FALSE,
  
theme = bs_theme(
    bootswatch = "cerulean",
    "navbar-bg" = "#0a3c91",   
    "navbar-light-color" = "#ffffff"  
  ),
 
  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode")),
  

  nav_panel("Global Database",

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
                theme = "orange" 
              )
            ),

            layout_column_wrap(
              width = 1/2, 
              
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                tagList("Orbit Class Distribution ", tooltip(bsicons::bs_icon("info-circle"), "Shows the classification of asteroid orbits. Notice which classes, like Apollos (APO), historically contain the most hazardous objects due to their Earth-crossing paths."))), 
                plotlyOutput("eda_bar_chart")
              ),
              
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                  tagList("Threat Matrix: Size vs. Proximity ", tooltip(bsicons::bs_icon("info-circle"), "Plots asteroid size against miss distance. Objects in the top-left (closest to Earth and largest in diameter) represent the greatest theoretical threat."))), 
                plotlyOutput("eda_scatter_chart")
              ),
              
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                tagList("Velocity Distribution ", tooltip(bsicons::bs_icon("info-circle"), "Compares the speed (Mean Motion) of safe vs. hazardous asteroids. Faster objects are harder to deflect and carry more kinetic energy upon impact."))), 
                plotlyOutput("eda_histogram")
              ),
              
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                tagList("Magnitude (Brightness) Analysis ", tooltip(bsicons::bs_icon("info-circle"), "In astronomy, a lower magnitude means a brighter object, which usually indicates a larger physical size. Notice how confirmed hazards skew towards lower (brighter) magnitudes."))), 
                plotlyOutput("eda_boxplot")
              )
            )
  ),
  
  nav_panel("Hazard Predictor",
            layout_sidebar(

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
                actionButton("scan_btn", "SCAN FOR THREAT", class = "btn btn-danger btn-lg w-100 fw-bold shadow")
              ),
              
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                tagList("System Output")),
                uiOutput("threat_alert") 
            
              ),
              card(
                card_header(style = "background-color: #F96E5B; color: white;", class = "fw-bold",
                tagList("Orbital Radar")),
                plotlyOutput("radar_plot") 
              )
            )
  ),
  
  nav_panel("About This", 
            
            card(
              class = "shadow-sm mb-4 border-0",
              card_header("About This Project", class = "bg-primary text-white fw-bold fs-5"),
              card_body(
                p(class = "lead", "INFORMATION GOES HERE ARN"),
                
                div(
                  tags$a(href = "https://www.kaggle.com/datasets/itszubi/nasa-asteroid-tracker-dataset?resource=download", target = "_blank", class = "btn btn-primary fw-bold", 
                         bsicons::bs_icon("database"), " View Dataset")
                )
              )
            ),
            layout_columns(
              col_widths = c(4, 8), 
              
              layout_column_wrap(
                width = 1,

                card(
                  class = "border-0 border-start border-danger border-5 shadow-sm",
                  card_header("Model Performance", class = "h4 fw-bold border-0 bg-transparent"),
                  card_body(
                    layout_column_wrap(
                      width = 1/3,
                      div(class = "text-center", h6("Accuracy", class = "text-muted fw-bold"), h3("99.5%", class = "fw-bold")),
                      div(class = "text-center", h6("F1 Score", class = "text-muted fw-bold"), h3("99.7%", class = "fw-bold")),
                      div(class = "text-center", h6("Kappa", class = "text-muted fw-bold"), h3("0.98", class = "fw-bold"))
                    ),
                    
                    br(), 
                    
                    layout_column_wrap(
                      width = 1/2,
                      div(class = "text-center", h6("Sensitivity (Recall)", class = "text-muted fw-bold"), h3("99.3%", class = "fw-bold")),
                      div(class = "text-center", h6("Specificity", class = "text-muted fw-bold"), h3("100%", class = "fw-bold"))
                    ),
                    
                    hr(), 
                    
                    div(
                      class = "small text-muted",
                      strong("Metrics explanation:", class = "text-body"), br(),
                      strong("Accuracy:", class = "text-body"), " Overall correctness of predictions", br(),
                      strong("F1 Score:", class = "text-body"), " Harmonic mean of precision and recall", br(),
                      strong("Kappa:", class = "text-body"), " Accuracy normalized at the baseline of random chance", br(),
                      strong("Sensitivity:", class = "text-body"), " True positive rate", br(),
                      strong("Specificity:", class = "text-body"), " True negative rate", br(),
                      br(),
                      "Based on 5-fold cross-validation of the training data"
                    )
                  )
                ),
                
                card(
                  card_header("The Threat Engine (Random Forest)", class = "bg-danger text-white"),
                  markdown("
Our backend is powered by a **Random Forest Classification** algorithm. 

During training, the model autonomously learned the core physics of orbital mechanics. It determined that **Minimum Orbit Intersection (Proximity)** is the ultimate gatekeeper. If an asteroid's orbit does not physically cross Earth's path, the hazard probability drops to zero, regardless of size or speed. 

Secondary features like **Maximum Diameter** and **Mean Motion** act as critical tie-breakers to calculate the exact Threat Probability of objects that *do* enter our orbital neighborhood.
                  ")
                )
              ),
              
              card(
                card_header("Data Dictionary", class = "bg-info text-white"),
                class = "overflow-auto h-100", 
                markdown("
**IDENTIFIERS & TRAITS**
* **designation**: A unique serial number to identify the specific asteroid.
* **orbit_id**: Orbit solution identifier; in most cases, the JPL solution number.
* **magnitude**: A measure of how bright a celestial object is. The lower the magnitude, the brighter the object.
* **diameter_min_m**: The estimated minimum diameter in meters.
* **diameter_max_m**: The estimated maximum diameter in meters.

**ORBITAL GEOMETRY & MOVEMENT**
* **orbit_class_type**: Abbreviated orbit classification type.
    * *APO (Apollos)*: Cross Earth's path at their closest point to the Sun. They spend most of their time farther out but intersect Earth's path periodically.
    * *AMO (Amors)*: Orbits strictly outside Earth's but inside Mars's. They approach Earth's neighborhood but do not cross our orbit.
    * *ATE (Atens)*: Overall orbit is smaller than Earth's, but they cross Earth's orbit at their farthest point from the Sun.
* **eccentricity**: Measures how much an asteroid's orbit deviates from a perfect circle.
* **inclination**: The tilt of the asteroid’s orbit relative to the reference plane.
* **semi_major_axis**: Average distance from the asteroid to the Sun.
* **perihelion_distance**: A celestial body’s closest orbital distance from the sun.
* **aphelion_distance**: A celestial body’s farthest orbital distance from the sun.
* **perihelion_argument**: The angular distance between the orbit's ascending node and its perihelion.
* **ascending_node_longitude**: The angle from the reference direction to the direction of the ascending node.
* **mean_anomaly**: Position of the asteroid in its orbit at a specific time.
* **mean_motion**: The angular speed required to complete one orbit.
* **orbital_period**: The time an astronomical object takes to revolve around the sun.

**OBSERVATION & PROXIMITY**
* **min_orbit_intersection**: The distance between the closest points of the asteroid and Earth.
* **jupiter_tisserand**: A mathematical value relative to Jupiter used to classify orbits.
* **epoch**: A moment in time used as a reference point for some time-varying quantity.
* **data_arc_days**: The period of time elapsed between the first and most recent observations.
* **observation_used**: How many times the asteroid was observed.
* **orbit_uncertainty**: The quantification of doubt regarding an object's exact location and trajectory.
                ")
              )
            ),
            
            hr(class = "mt-5 mb-4 border-secondary"),
            
            div(class = "text-center mb-4",
                h3("Project Makers", class = "fw-bold"),
            ),
            
            layout_column_wrap(
              width = 1/3,
              
              card(
                class = "text-center shadow-sm p-0 border-0",
                
                div(class = "text-white p-4", style = "background-color: #800020;",
                    h5("Arndria Basco", class = "fw-bold mb-0"),
                    div("BSCS Student")
                ),
                
                card_body(
                  div(class = "mb-3 mt-2",tags$img(src = "img/arn_photo.png", style = "width: 150px; height: 150px; border-radius: 50%; object-fit: cover; border: 3px solid #800020;")),
                  div("College of Information and Computing", class = "fw-bold"),
                  div("BS Computer Science Major in Data Science"),
                  div("University of Southeastern Philippines, Obrero Campus", class = "small text-muted mt-2")
                )
              ),

              card(
                class = "text-center shadow-sm p-0 border-0", 
                
                div(class = "text-white p-4", style = "background-color: #5D3FD3;",
                    h5("Webster Miguelle D. Isidor", class = "fw-bold mb-0"),
                    div("BSCS Student")
                ),
                
                card_body(
                  div(class = "mb-3 mt-2",tags$img(src = "img/webster_photo.png", style = "width: 150px; height: 150px; border-radius: 50%; object-fit: cover; border: 3px solid #5D3FD3;")),
                  div("College of Information and Computing", class = "fw-bold"),
                  div("BS Computer Science Major in Data Science"),
                  div("wmdisidor01202401034@usep.edu.ph", class = "small text-muted mt-1"),
                  div("University of Southeastern Philippines, Obrero Campus", class = "small text-muted mt-2")
                )
              ),
              

              card(
                class = "text-center shadow-sm p-0 border-0",
                
                div(class = "text-white p-4", style = "background-color: #db8282;",
                    h5("Fe Aubrey Oledan", class = "fw-bold mb-0"),
                    div("BSCS Student")
                ),
                
                card_body(
                  div(class = "mb-3 mt-2",tags$img(src = "img/baubbie_photo.png", style = "width: 150px; height: 150px; border-radius: 50%; object-fit: cover; border: 3px solid #db8282;")),
                  div("College of Information and Computing", class = "fw-bold"),
                  div("BS Computer Science Major in Data Science"),
                  div("University of Southeastern Philippines, Obrero Campus", class = "small text-muted mt-2")
                )
              )
            )
  )
)


server <- function(input, output, session) {
  
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
  
  output$eda_bar_chart <- renderPlotly({
    plot_ly(clean_data, x = ~orbit_class_type, color = ~potentially_hazardous, 
            colors = c("#74b9ff", "#e17055"), type = "histogram",
            name = ~ifelse(potentially_hazardous == "TRUE", "Unsafe", "Safe"),
            hovertemplate = "<b>Orbit Class:</b> %{x}<br><b>Count:</b> %{y}<extra></extra>") |>
      layout(barmode = "stack",
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Orbit Class", gridcolor = t_colors()$grid),
             yaxis = list(title = "Asteroid Count", gridcolor = t_colors()$grid))
  })
  
  output$eda_scatter_chart <- renderPlotly({
    plot_ly(clean_data, x = ~min_orbit_intersection, y = ~diameter_max_m, 
            color = ~potentially_hazardous, colors = c("#74b9ff", "#e17055"),
            name = ~ifelse(potentially_hazardous == "TRUE", "Unsafe", "Safe"),
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

  output$eda_histogram <- renderPlotly({
    plot_ly(clean_data, x = ~mean_motion, color = ~potentially_hazardous, 
            colors = c("#74b9ff", "#e17055"), type = "histogram", opacity = 0.7,
            name = ~ifelse(potentially_hazardous == "TRUE", "Unsafe", "Safe"),
            hovertemplate = "<b>Speed:</b> %{x} deg/day<br><b>Count:</b> %{y}<extra></extra>") |>
      layout(barmode = "overlay", 
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Mean Motion (Speed)", gridcolor = t_colors()$grid),
             yaxis = list(title = "Frequency", gridcolor = t_colors()$grid))
  })

  output$eda_boxplot <- renderPlotly({
    plot_ly(clean_data, x = ~potentially_hazardous, y = ~magnitude, color = ~potentially_hazardous,
            colors = c("#74b9ff", "#e17055"),
            type = "box") |>
      layout(paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)', 
             font = list(color = t_colors()$text),
             xaxis = list(title = "Hazard Status", gridcolor = t_colors()$grid,
             tickvals = c("FALSE", "TRUE"),
             ticktext = c("Safe", "Unsafe")),
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
    
    # RENDER THE ALERT (CSS-Free via Bootstrap)
    output$threat_alert <- renderUI({
      if(is_hazardous) {
        div(class = "alert alert-danger text-center shadow-sm p-4 rounded-3",
            div(class = "fs-2 fw-bold", "⚠️ HAZARDOUS OBJECT DETECTED"),
            div(class = "fs-4 mt-3", paste("Threat Probability:", hazard_prob, "%")))
      } else {
        div(class = "alert alert-success text-center shadow-sm p-4 rounded-3",
            div(class = "fs-2 fw-bold", "✅ ORBIT CLEAR. NO THREAT."),
            div(class = "fs-4 mt-3", paste("Threat Probability:", hazard_prob, "%")))
      }
    })

    # RENDER THE RADAR
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