# ---- Heatmap tab: Settings UI ----
output$heatmap_settings_ui <- renderUI({
  req(data(), input$id_col)
  
  df <- isolate(data())
  cols <- names(df)
  
  # Filter to numeric or convertible-to-numeric columns for VALUES ONLY
  numeric_cols <- c()
  na_counts <- list()
  
  for(col in cols) {
    col_data <- df[[col]]
    
    if(is.numeric(col_data)) {
      numeric_cols <- c(numeric_cols, col)
      na_counts[[col]] <- sum(is.na(col_data))
    } else {
      converted <- suppressWarnings(as.numeric(col_data))
      non_na_count <- sum(!is.na(converted))
      
      if(non_na_count > 0) {
        numeric_cols <- c(numeric_cols, col)
        na_counts[[col]] <- sum(is.na(converted))
      }
    }
  }
  
  # Need at least one numeric column for heatmap values
  if(length(numeric_cols) < 1) {
    return(
      div(
        class = "info-box",
        style = "background-color: #fff3cd; border-left-color: #ffc107;",
        p(icon("exclamation-triangle"), "No numeric columns found. Please ensure your data has at least one numeric column to display in the heatmap.")
      )
    )
  }
  
  tree_obj <- tryCatch(tree_data(), error = function(e) NULL)
  
  # Get current settings
  current_value_cols <- isolate(input$heatmap_value_cols)
  
  # Smart default: all numeric columns for values
  if(is.null(current_value_cols)) {
    current_value_cols <- numeric_cols
  } else {
    # Filter out any non-numeric columns that might have been selected before
    current_value_cols <- current_value_cols[current_value_cols %in% numeric_cols]
  }
  
  # Build adaptive NA filtering message (only for SELECTED columns)
  info_messages <- tagList()
  has_nas <- FALSE
  
  # Only show NA info for currently selected value columns
  selected_cols <- if(!is.null(current_value_cols)) current_value_cols else numeric_cols
  
  for(col in selected_cols) {
    if(col %in% names(na_counts) && na_counts[[col]] > 0) {
      has_nas <- TRUE
      info_messages <- tagList(
        info_messages,
        div(
          class = "help-text",
          style = "color: #856404; margin-bottom: 0.5rem;",
          icon("info-circle"),
          sprintf(" Column '%s': %d NA/non-numeric value(s) will display as 'X' in heatmap", col, na_counts[[col]])
        )
      )
    }
  }
  
  tagList(
    # NA filtering info box - only shown if selected columns have NAs
    if(has_nas) {
      div(
        class = "info-box",
        style = "background-color: #fff3cd; border-left-color: #ffc107; margin-bottom: 1rem;",
        p(tags$strong("Value Filtering:")),
        info_messages
      )
    },
    
    card(
      card_header("Column Selection"),
      card_body(
        
        selectizeInput(
          "heatmap_value_cols",
          "Value Columns (numeric fields for heatmap)",
          choices = numeric_cols,
          selected = current_value_cols,
          multiple = TRUE,
          options = list(
            placeholder = 'Select numeric columns for heatmap',
            plugins = list('remove_button')
          )
        ),
        div(class = "help-text",
            "Only numeric columns are shown. Non-numeric or NA values will display as 'X' in the heatmap")
      )
    ),
    
    card(
      card_header("Dataset Label"),
      card_body(
        textInput(
          "heatmap_dataset_label",
          NULL,
          value = isolate(input$heatmap_dataset_label) %||% "heatmap",
          placeholder = "Enter dataset label"
        ),
        div(class = "help-text",
            "Label for this heatmap dataset")
      )
    ),

    # Tree options card
    card(
      card_header("Tree Options"),
      card_body(
        if(!is.null(tree_obj) && !is.null(tree_obj$tree)) {
          tagList(
            div(
              class = "info-box",
              style = "background-color: #d4edda; border-left-color: #28a745; margin-bottom: 1rem;",
              p(
                icon("check-circle"),
                tags$strong(" Tree loaded")
              )
            ),
            checkboxInput(
              "heatmap_add_tree",
              "Add FIELD_TREE to heatmap (creates a tree above the heatmap fields)",
              value = isolate(input$heatmap_add_tree) %||%
                (!is.null(tree_obj) && !is.null(tree_obj$tree))
            ),
            div(class = "help-text",
                "When enabled, the tree will be displayed above the heatmap fields in iTOL. The fields will be ordered based on the tree structure.")
          )
        } else {
          div(
            class = "info-box",
            style = "background-color: #fff3cd; border-left-color: #ffc107;",
            p(
              icon("info-circle"),
              " No tree uploaded. Upload a tree in the sidebar to enable tree-based ordering."
            )
          )
        }
      )
    ),
    
    card(
      card_header("Color Settings"),
      card_body(
        radioGroupButtons(
          "heatmap_color_mode",
          "Color Mode",
          choices = c("ColorBrewer" = "ColorBrewer", 
                      "Manual" = "Manual"),
          selected = isolate(input$heatmap_color_mode) %||% "ColorBrewer",
          justified = FALSE,
          size = "xs",
          status = "primary",
          width = "100%",
          individual = TRUE
        ),
        
        conditionalPanel(
          condition = "input.heatmap_color_mode == 'ColorBrewer'",
          selectInput(
            "heatmap_brewer_palette",
            "Select Palette",
            choices = c(
              "Select a palette" = "",
              get_brewer_palettes()$Sequential,
              get_brewer_palettes()$Diverging
            ),
            selected = isolate(input$heatmap_brewer_palette) %||% "RdYlBu",
            width = "200px"
          ),
          div(class = "help-text",
              "Choose a ColorBrewer palette for the heatmap."),
          
          checkboxInput(
            "heatmap_reverse_palette",
            "Reverse palette colors",
            value = isolate(input$heatmap_reverse_palette) %||% FALSE
          )
        ),
        
        conditionalPanel(
          condition = "input.heatmap_color_mode == 'Manual'",
          
          colourInput(
            "heatmap_color_max",
            "Maximum Value Color (high values)",
            value = isolate(input$heatmap_color_max) %||% "#FF3D3D",
            showColour = "both",
            palette = "square",
            returnName = FALSE
          ),
          
          checkboxInput(
            "heatmap_use_mid_color",
            "Use midpoint color (3-color gradient)",
            value = isolate(input$heatmap_use_mid_color) %||% FALSE
          ),
          
          conditionalPanel(
            condition = "input.heatmap_use_mid_color",
            colourInput(
              "heatmap_color_mid",
              "Midpoint Value Color",
              value = isolate(input$heatmap_color_mid) %||% "#ffff00",
              showColour = "both",
              palette = "square",
              returnName = FALSE
            )
          ),
          
          colourInput(
            "heatmap_color_min",
            "Minimum Value Color (low values)",
            value = isolate(input$heatmap_color_min) %||% "#0099FF",
            showColour = "both",
            palette = "square",
            returnName = FALSE
          )
        ),
        
        tags$hr(),
        
        colourInput(
          "heatmap_color_nan",
          "Color for Missing Values (NA)",
          value = isolate(input$heatmap_color_nan) %||% "#FFFFFF",
          showColour = "both",
          palette = "square",
          returnName = FALSE
        ),
        
        tags$hr(),
        
        conditionalPanel(
          condition = "input.heatmap_color_mode == 'ColorBrewer'",
          tags$details(
            tags$summary(
              style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0;",
              "View ColorBrewer Palette Reference"
            ),
            plotOutput("brewer_plot_heatmap", height = "600px")
          )
        )
      )
    ),
    
    card(
      card_header("Display Options"),
      card_body(
        numericInput(
          "heatmap_strip_width",
          "Cell Width",
          value = isolate(input$heatmap_strip_width) %||% 30,
          min = 5,
          max = 200,
          step = 1,
          width = "150px"
        ),
        div(class = "help-text",
            "Width of individual heatmap cells in pixels"),
        
        checkboxInput(
          "heatmap_auto_legend",
          "Automatically create legend",
          value = isolate(input$heatmap_auto_legend) %||% TRUE
        ),
        
        tags$hr(),
        
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            tags$h6(style = "color: #2C5F8D;", "Border Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                "heatmap_border_width",
                "Border Width",
                value = isolate(input$heatmap_border_width) %||% 0,
                min = 0,
                max = 10,
                step = 0.5,
                width = "150px"
              ),
              div(class = "help-text",
                  "Width of border around cells (0 = no border)")
            ),
            
            conditionalPanel(
              condition = "input.heatmap_border_width > 0",
              div(
                style = "margin-bottom: 1rem;",
                colourInput(
                  "heatmap_border_color",
                  "Border Color",
                  value = isolate(input$heatmap_border_color) %||% "#000000",
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              )
            ),
            
            tags$hr(),
            
            tags$h6(style = "color: #2C5F8D;", "Label Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                "heatmap_label_size_factor",
                "Label Size Factor",
                value = isolate(input$heatmap_label_size_factor) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                "heatmap_label_rotation",
                "Label Rotation (degrees)",
                value = isolate(input$heatmap_label_rotation) %||% 0,
                min = -180,
                max = 180,
                step = 1,
                width = "150px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                "heatmap_label_shift",
                "Label Shift",
                value = isolate(input$heatmap_label_shift) %||% 0,
                min = -200,
                max = 200,
                step = 1,
                width = "150px"
              )
            ),
            
            tags$hr(),
            
            tags$h6(style = "color: #2C5F8D;", "Spacing"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                "heatmap_margin",
                "Left Margin",
                value = isolate(input$heatmap_margin) %||% 0,
                min = -200,
                max = 200,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing to next dataset (can be negative)")
            )
          )
        )
      )
    )
  )
})

# ---- Generate heatmap output ----
heatmap_output <- reactive({
  req(data(), input$id_col, input$heatmap_value_cols)
  
  if(length(input$heatmap_value_cols) < 1) return(NULL)
  
  df <- data()
  id_col <- input$id_col  # Use ID column from sidebar
  value_cols <- input$heatmap_value_cols

  # Get tree object safely (optional)
  tree_obj <- tryCatch(tree_data(), error = function(e) NULL)

  # Get settings
  heatmap_dataset_label <- input$heatmap_dataset_label %||% "heatmap"
  heatmap_color_mode <- input$heatmap_color_mode %||% "ColorBrewer"
  heatmap_add_tree <- input$heatmap_add_tree %||% FALSE

  # Determine colors based on mode
  if(heatmap_color_mode == "ColorBrewer") {
    brewer_pal <- input$heatmap_brewer_palette %||% "RdYlBu"
    reverse_palette <- input$heatmap_reverse_palette %||% FALSE
    
    pal_colors <- suppressWarnings(brewer.pal(3, brewer_pal))
    
    if(reverse_palette) {
      pal_colors <- rev(pal_colors)
    }
    
    heatmap_color_min <- pal_colors[1]
    heatmap_color_mid <- pal_colors[2]
    heatmap_color_max <- pal_colors[3]
    heatmap_use_mid_color <- TRUE
  } else {
    heatmap_color_min <- input$heatmap_color_min %||% "#0099FF"
    heatmap_color_max <- input$heatmap_color_max %||% "#FF3D3D"
    heatmap_color_mid <- input$heatmap_color_mid %||% "#ffff00"
    heatmap_use_mid_color <- input$heatmap_use_mid_color %||% FALSE
  }
  
  heatmap_color_nan <- input$heatmap_color_nan %||% "#FFFFFF"
  heatmap_strip_width <- input$heatmap_strip_width %||% 30
  heatmap_auto_legend <- input$heatmap_auto_legend %||% TRUE
  heatmap_border_width <- input$heatmap_border_width %||% 0
  heatmap_border_color <- input$heatmap_border_color %||% "#000000"
  heatmap_label_size_factor <- input$heatmap_label_size_factor %||% 1
  heatmap_label_rotation <- input$heatmap_label_rotation %||% 0
  heatmap_label_shift <- input$heatmap_label_shift %||% 0
  heatmap_margin <- input$heatmap_margin %||% 0
  
  # Build iTOL DATASET_HEATMAP format
  content <- c("DATASET_HEATMAP")
  content <- c(content, "SEPARATOR TAB")
  content <- c(content, paste("DATASET_LABEL", heatmap_dataset_label, sep = "\t"))
  content <- c(content, paste("COLOR", heatmap_color_max, sep = "\t"))
  content <- c(content, "")
  
  # Get original column names
  original_name_map <- attr(df, "original_colnames")
  if(!is.null(original_name_map)) {
    original_cols <- original_name_map[value_cols]
  } else {
    original_cols <- value_cols
  }

  # Field labels using original sample names
  content <- c(content, paste("FIELD_LABELS", paste(original_cols, collapse = "\t"), sep = "\t"))
  content <- c(content, "")
  
  # Add FIELD_TREE if tree is loaded and user wants it
  if(!is.null(tree_obj) && !is.null(tree_obj$tree) && heatmap_add_tree) {
    field_tree <- gsub("[\\n\\r\\s]", "", tree_obj$tree_text)
    content <- c(content, paste("FIELD_TREE", field_tree, sep = "\t"))
    content <- c(content, "SHOW_TREE\t1")
    content <- c(content, "")
  }
  
  # Color settings
  content <- c(content, paste("COLOR_MAX", heatmap_color_max, sep = "\t"))
  content <- c(content, paste("COLOR_MIN", heatmap_color_min, sep = "\t"))
  if(heatmap_use_mid_color) {
    content <- c(content, "USE_MID_COLOR\t1")
    content <- c(content, paste("COLOR_MID", heatmap_color_mid, sep = "\t"))
  }
  content <- c(content, paste("COLOR_NAN", heatmap_color_nan, sep = "\t"))
  content <- c(content, "")
  
  # Display settings
  content <- c(content, paste("STRIP_WIDTH", heatmap_strip_width, sep = "\t"))
  content <- c(content, paste("AUTO_LEGEND", if(heatmap_auto_legend) "1" else "0", sep = "\t"))
  content <- c(content, "")
  
  # Border settings
  if(heatmap_border_width > 0) {
    content <- c(content, paste("BORDER_WIDTH", heatmap_border_width, sep = "\t"))
    content <- c(content, paste("BORDER_COLOR", heatmap_border_color, sep = "\t"))
    content <- c(content, "")
  }
  
  # Label settings
  content <- c(content, paste("SIZE_FACTOR", heatmap_label_size_factor, sep = "\t"))
  content <- c(content, paste("LABEL_ROTATION", heatmap_label_rotation, sep = "\t"))
  content <- c(content, paste("LABEL_SHIFT", heatmap_label_shift, sep = "\t"))
  content <- c(content, "")
  
  # Margin
  if(heatmap_margin != 0) {
    content <- c(content, paste("MARGIN", heatmap_margin, sep = "\t"))
    content <- c(content, "")
  }
  
  content <- c(content, "DATA")
  
  # Data: ID followed by numeric values for each field
  for(i in 1:nrow(df)) {
    id <- as.character(df[[id_col]][i])
    
    # Get values for each field, converting to numeric if needed
    values <- sapply(value_cols, function(col) {
      # Check if column exists
      if(!col %in% names(df)) {
        return("X")
      }
      
      val <- df[[col]][i]
      
      # Convert to numeric if not already
      if(!is.numeric(val)) {
        val <- suppressWarnings(as.numeric(val))
      }
      
      # Return "X" for NA values, otherwise the numeric value
      if(length(val) == 0 || is.na(val)) {
        return("X")
      } else {
        return(as.character(val))
      }
    })
    
    content <- c(content, paste(c(id, values), collapse = "\t"))
  }
  
  return(paste(content, collapse = "\n"))
})

# ---- Heatmap download card ----
output$heatmap_download_card <- renderUI({
  req(heatmap_output())
  
  card(
    card_header("Download Heatmap Annotation"),
    card_body(
      centered_download_button(
        "download_heatmap",
        "Download Heatmap File"
      )
    )
  )
})

# ---- ColorBrewer plot for heatmap tab ----
output$brewer_plot_heatmap <- renderPlot({
  par(mfrow = c(2, 1), mar = c(1, 10, 3, 2))
  
  display.brewer.all(type = "seq")
  title("Sequential Palettes", cex.main = 1.2, font.main = 2)
  
  display.brewer.all(type = "div")
  title("Diverging Palettes", cex.main = 1.2, font.main = 2)
  
}, res = 96, height = 600)

# Heatmap download handler
output$download_heatmap <- downloadHandler(
  filename = function() {
    paste0(input$dataset_label, "_heatmap.txt")
  },
  content = function(file) {
    writeLines(heatmap_output(), file)
  }
)