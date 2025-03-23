library(shiny)
library(DT)
library(leaflet)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# Initial sample data
lexical_data <- data.frame(
  language = character(),
  base_form = character(),
  variant = character(),
  location = character(),
  translation = character(),
  stringsAsFactors = FALSE
)

coordinates <- data.frame(
  location = character(),
  latitude = numeric(),
  longitude = numeric(),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Lexical Data and Location Mapping"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data Import/Export"),
      fileInput("lexical_file", "Import Lexical Data (CSV)"),
      fileInput("coord_file", "Import Coordinates Data (CSV)"),
      downloadButton("download_lexical", "Download Lexical Data"),
      downloadButton("download_coord", "Download Coordinates Data"),
      hr(),
      h4("Location Selection"),
      tags$p("Click on map to select a location when adding new entries."),
      verbatimTextOutput("click_info"),
      textInput("new_location_name", "Location Name:"),
      actionButton("add_location", "Add This Location", icon = icon("plus")),
      hr(),
      h4("Visualization Settings"),
      selectInput("base_form_select", "Select Base Form to Visualize:", choices = NULL),
      selectInput("color_by", "Color Points By:", choices = c("Language", "Variant")),
      checkboxInput("show_labels", "Show Labels on Map", value = TRUE)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Map Visualization", 
                 leafletOutput("map", height = "500px"),
                 br(),
                 h4("Legend"),
                 uiOutput("legend_ui")
        ),
        tabPanel("Lexical Data", 
                 h4("Lexical Data Table (Editable)"),
                 DTOutput("lexical_table"),
                 actionButton("add_lexical_row", "Add Row"),
                 actionButton("delete_lexical_row", "Delete Selected Row(s)")
        ),
        tabPanel("Coordinates Data", 
                 h4("Coordinates Data Table (Editable)"),
                 DTOutput("coord_table"),
                 actionButton("add_coord_row", "Add Row"),
                 actionButton("delete_coord_row", "Delete Selected Row(s)")
        ),
        tabPanel("Add Location From Map",
                 h4("Interactive Location Selection"),
                 p("Click anywhere on the map to select a location. Fill in the details below and submit to add it to both tables."),
                 leafletOutput("selection_map", height = "400px"),
                 br(),
                 fluidRow(
                   column(6, 
                          h4("Location Details"),
                          textInput("map_location_name", "Location Name:"),
                          numericInput("map_latitude", "Latitude:", value = NULL),
                          numericInput("map_longitude", "Longitude:", value = NULL)
                   ),
                   column(6,
                          h4("Lexical Details"),
                          textInput("map_language", "Language:"),
                          textInput("map_base_form", "Base Form:"),
                          textInput("map_variant", "Variant:"),
                          textInput("map_translation", "Translation:")
                   )
                 ),
                 actionButton("add_from_map", "Add This Entry", icon = icon("plus"))
        ),
        tabPanel("Data Summary", 
                 h4("Summary Statistics"),
                 verbatimTextOutput("summary_stats"),
                 h4("Variants by Location"),
                 plotOutput("variants_plot", height = "400px")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values for both tables and map clicks
  values <- reactiveValues(
    lexical_data = lexical_data,
    coordinates = coordinates,
    click_lat = NULL,
    click_lng = NULL,
    click_zoom = NULL
  )
  
  # Import lexical data
  observeEvent(input$lexical_file, {
    file <- input$lexical_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    if (ext == "csv") {
      values$lexical_data <- read.csv(file$datapath, stringsAsFactors = FALSE)
      updateSelectInput(session, "base_form_select", 
                        choices = c("All", unique(values$lexical_data$base_form)))
    } else {
      showNotification("Please upload a CSV file", type = "error")
    }
  })
  
  # Import coordinates data
  observeEvent(input$coord_file, {
    file <- input$coord_file
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    if (ext == "csv") {
      values$coordinates <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else {
      showNotification("Please upload a CSV file", type = "error")
    }
  })
  
  # Lexical data table
  output$lexical_table <- renderDT({
    DT::datatable(values$lexical_data, 
                  editable = TRUE,
                  selection = 'multiple',
                  options = list(pageLength = 10, 
                                 scrollX = TRUE))
  })
  
  # Edit lexical data
  observeEvent(input$lexical_table_cell_edit, {
    info <- input$lexical_table_cell_edit
    values$lexical_data[info$row, info$col] <- info$value
    
    # Update base form select input
    updateSelectInput(session, "base_form_select", 
                      choices = c("All", unique(values$lexical_data$base_form)))
  })
  
  # Add row to lexical data
  observeEvent(input$add_lexical_row, {
    new_row <- data.frame(
      language = "",
      base_form = "",
      variant = "",
      location = "",
      translation = "",
      stringsAsFactors = FALSE
    )
    values$lexical_data <- rbind(values$lexical_data, new_row)
  })
  
  # Delete selected rows from lexical data
  observeEvent(input$delete_lexical_row, {
    if (!is.null(input$lexical_table_rows_selected)) {
      values$lexical_data <- values$lexical_data[-input$lexical_table_rows_selected, ]
    }
  })
  
  # Coordinates data table
  output$coord_table <- renderDT({
    DT::datatable(values$coordinates, 
                  editable = TRUE,
                  selection = 'multiple',
                  options = list(pageLength = 10, 
                                 scrollX = TRUE))
  })
  
  # Edit coordinates data
  observeEvent(input$coord_table_cell_edit, {
    info <- input$coord_table_cell_edit
    values$coordinates[info$row, info$col] <- info$value
  })
  
  # Add row to coordinates data
  observeEvent(input$add_coord_row, {
    new_row <- data.frame(
      location = "",
      latitude = 0,
      longitude = 0,
      stringsAsFactors = FALSE
    )
    values$coordinates <- rbind(values$coordinates, new_row)
  })
  
  # Delete selected rows from coordinates data
  observeEvent(input$delete_coord_row, {
    if (!is.null(input$coord_table_rows_selected)) {
      values$coordinates <- values$coordinates[-input$coord_table_rows_selected, ]
    }
  })
  
  # Download lexical data
  output$download_lexical <- downloadHandler(
    filename = function() {
      paste("lexical_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$lexical_data, file, row.names = FALSE)
    }
  )
  
  # Download coordinates data
  output$download_coord <- downloadHandler(
    filename = function() {
      paste("coordinates_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$coordinates, file, row.names = FALSE)
    }
  )
  
  # Capture map clicks for main map location selection
  observeEvent(input$map_click, {
    click <- input$map_click
    values$click_lat <- click$lat
    values$click_lng <- click$lng
    values$click_zoom <- input$map_zoom
    
    # Update the location name input with a default value (can be changed by user)
    location_name_suggestion <- paste0("Loc_", round(click$lat, 2), "_", round(click$lng, 2))
    updateTextInput(session, "new_location_name", value = location_name_suggestion)
  })
  
  # Display click information
  output$click_info <- renderText({
    if (is.null(values$click_lat) || is.null(values$click_lng)) {
      return("Click on map to select a location")
    }
    
    paste0("Selected coordinates:\nLatitude: ", round(values$click_lat, 6), 
           "\nLongitude: ", round(values$click_lng, 6))
  })
  
  # Add clicked location to coordinates table
  observeEvent(input$add_location, {
    req(values$click_lat, values$click_lng, input$new_location_name)
    
    # Check if location name already exists
    if (input$new_location_name %in% values$coordinates$location) {
      showNotification("Location name already exists. Please choose a different name.", type = "warning")
      return()
    }
    
    new_location <- data.frame(
      location = input$new_location_name,
      latitude = values$click_lat,
      longitude = values$click_lng,
      stringsAsFactors = FALSE
    )
    
    values$coordinates <- rbind(values$coordinates, new_location)
    showNotification(paste("Added location:", input$new_location_name), type = "message")
    
    # Reset input
    updateTextInput(session, "new_location_name", value = "")
  })
  
  # Selection map for the dedicated tab
  output$selection_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Capture clicks on the selection map
  observeEvent(input$selection_map_click, {
    click <- input$selection_map_click
    
    # Update input fields with the clicked coordinates
    updateNumericInput(session, "map_latitude", value = click$lat)
    updateNumericInput(session, "map_longitude", value = click$lng)
    
    # Generate a suggested location name
    suggested_name <- paste0("Loc_", round(click$lat, 2), "_", round(click$lng, 2))
    updateTextInput(session, "map_location_name", value = suggested_name)
  })
  
  # Add entry from map tab
  observeEvent(input$add_from_map, {
    req(input$map_location_name, input$map_latitude, input$map_longitude)
    
    # Check if location name already exists in coordinates
    if (input$map_location_name %in% values$coordinates$location) {
      showNotification("Location name already exists. Please choose a different name.", type = "warning")
      return()
    }
    
    # Add to coordinates table
    new_coord <- data.frame(
      location = input$map_location_name,
      latitude = input$map_latitude,
      longitude = input$map_longitude,
      stringsAsFactors = FALSE
    )
    values$coordinates <- rbind(values$coordinates, new_coord)
    
    # Add to lexical table if lexical data is provided
    if (!is.null(input$map_language) && input$map_language != "" &&
        !is.null(input$map_base_form) && input$map_base_form != "" &&
        !is.null(input$map_variant) && input$map_variant != "") {
      
      new_lexical <- data.frame(
        language = input$map_language,
        base_form = input$map_base_form,
        variant = input$map_variant,
        location = input$map_location_name,
        translation = input$map_translation,
        stringsAsFactors = FALSE
      )
      values$lexical_data <- rbind(values$lexical_data, new_lexical)
      
      # Update base form select input
      updateSelectInput(session, "base_form_select", 
                        choices = c("All", unique(values$lexical_data$base_form)))
    }
    
    showNotification(paste("Added entry for location:", input$map_location_name), type = "message")
    
    # Reset inputs
    updateTextInput(session, "map_location_name", value = "")
    updateNumericInput(session, "map_latitude", value = NULL)
    updateNumericInput(session, "map_longitude", value = NULL)
    updateTextInput(session, "map_language", value = "")
    updateTextInput(session, "map_base_form", value = "")
    updateTextInput(session, "map_variant", value = "")
    updateTextInput(session, "map_translation", value = "")
  })
  
  # Filter data for visualization
  filtered_data <- reactive({
    # Join lexical data with coordinates
    joined_data <- left_join(values$lexical_data, values$coordinates, by = "location")
    
    # Filter by selected base form
    if (!is.null(input$base_form_select) && input$base_form_select != "All") {
      joined_data <- joined_data %>%
        filter(base_form == input$base_form_select)
    }
    
    # Remove entries without coordinates
    joined_data %>% 
      filter(!is.na(latitude) & !is.na(longitude))
  })
  
  # Generate color palette
  color_data <- reactive({
    data <- filtered_data()
    
    # Determine coloring variable
    if (input$color_by == "Language") {
      color_var <- data$language
      color_title <- "Language"
    } else {
      color_var <- data$variant
      color_title <- "Variant"
    }
    
    # Create color palette
    unique_values <- unique(color_var)
    n_colors <- length(unique_values)
    pal <- colorFactor(
      palette = if(n_colors <= 12) {
        brewer.pal(max(3, n_colors), "Set3")
      } else {
        colorRampPalette(brewer.pal(12, "Set3"))(n_colors)
      },
      domain = unique_values
    )
    
    list(
      color_var = color_var,
      unique_values = unique_values,
      pal = pal,
      color_title = color_title
    )
  })
  
  # Map output
  output$map <- renderLeaflet({
    data <- filtered_data()
    
    # Base map
    map <- leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
    
    # If we have data, add markers
    if (nrow(data) > 0) {
      color_info <- color_data()
      pal <- color_info$pal
      color_var <- color_info$color_var
      
      map <- map %>%
        addCircleMarkers(
          data = data,
          ~longitude, ~latitude,
          color = ~pal(color_var),
          radius = 8,
          stroke = TRUE,
          fillOpacity = 0.8,
          popup = ~paste(
            "<strong>Location:</strong>", location, "<br>",
            "<strong>Language:</strong>", language, "<br>",
            "<strong>Base Form:</strong>", base_form, "<br>",
            "<strong>Variant:</strong>", variant, "<br>",
            "<strong>Translation:</strong>", translation
          )
        ) %>%
        fitBounds(
          min(data$longitude, na.rm = TRUE) - 1,
          min(data$latitude, na.rm = TRUE) - 1,
          max(data$longitude, na.rm = TRUE) + 1,
          max(data$latitude, na.rm = TRUE) + 1
        )
      
      # Add labels if requested
      if (input$show_labels) {
        map <- map %>%
          addLabelOnlyMarkers(
            data = data,
            ~longitude, ~latitude,
            label = ~location,
            labelOptions = labelOptions(
              noHide = TRUE,
              direction = "top",
              textOnly = TRUE,
              style = list(
                "color" = "black",
                "font-weight" = "bold",
                "font-size" = "12px"
              )
            )
          )
      }
    }
    
    return(map)
  })
  
  # Legend UI
  output$legend_ui <- renderUI({
    color_info <- color_data()
    unique_values <- color_info$unique_values
    pal <- color_info$pal
    color_title <- color_info$color_title
    
    if (length(unique_values) == 0) {
      return(NULL)
    }
    
    # Create legend
    tags$div(
      tags$h5(paste("Color by:", color_title)),
      tags$table(
        lapply(1:length(unique_values), function(i) {
          value <- unique_values[i]
          color <- pal(value)
          tags$tr(
            tags$td(
              tags$div(
                style = paste0(
                  "width: 20px; height: 20px; background-color: ", 
                  color, 
                  "; display: inline-block; margin-right: 5px;"
                )
              )
            ),
            tags$td(value)
          )
        })
      )
    )
  })
  
  # Summary statistics
  output$summary_stats <- renderPrint({
    lexical <- values$lexical_data
    coords <- values$coordinates
    
    if (nrow(lexical) == 0 || nrow(coords) == 0) {
      return("Please upload data to see summary statistics")
    }
    
    # Count number of locations with coordinates
    locations_with_coords <- length(intersect(lexical$location, coords$location))
    
    # Count by language and base_form
    by_language <- table(lexical$language)
    by_base_form <- table(lexical$base_form)
    
    cat("Total lexical entries:", nrow(lexical), "\n")
    cat("Total locations with coordinates:", locations_with_coords, "\n")
    cat("Total locations without coordinates:", 
        length(setdiff(lexical$location, coords$location)), "\n\n")
    
    cat("Entries by language:\n")
    print(by_language)
    
    cat("\nEntries by base form:\n")
    print(by_base_form)
  })
  
  # Variants by location plot
  output$variants_plot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No data available for plotting", cex = 1.5)
      return()
    }
    
    if (input$base_form_select == "All" || is.null(input$base_form_select)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Please select a specific base form to see variant distribution", cex = 1.5)
      return()
    }
    
    # Prepare data for plotting
    plot_data <- data %>%
      count(location, variant) %>%
      group_by(location) %>%
      mutate(total = sum(n)) %>%
      ungroup() %>%
      complete(location, variant, fill = list(n = 0)) %>%
      mutate(
        location = factor(location, levels = rev(sort(unique(location)))),
        variant = factor(variant, levels = sort(unique(variant)))
      )
    
    # Create barplot
    par(mar = c(5, 10, 4, 2) + 0.1)
    
    # Define colors for variants
    n_variants <- length(unique(plot_data$variant))
    colors <- colorRampPalette(brewer.pal(min(9, n_variants), "Set1"))(n_variants)
    
    # Create barplot
    barplot(
      xtabs(n ~ location + variant, data = plot_data),
      beside = TRUE,
      horiz = TRUE,
      col = colors,
      las = 1,
      xlab = "Count",
      main = paste("Variant distribution for", input$base_form_select)
    )
    
    # Add legend
    legend("topright", 
           legend = levels(plot_data$variant),
           fill = colors,
           title = "Variants",
           cex = 0.8)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
