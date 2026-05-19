
# ---------- Server ----------

server <- function(input, output, session) {
  
  # ---- Reactive to detect Excel sheets ----
  excel_sheets_list <- reactive({
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    
    if (ext == "xlsx") {
      sheets <- excel_sheets(input$file$datapath)
      if (length(sheets) > 1) {
        return(sheets)
      }
    }
    return(NULL)
  })

  # ---- Show modal for sheet selection ----
  observeEvent(excel_sheets_list(), {
    sheets <- excel_sheets_list()
    
    if(!is.null(sheets)) {
      showModal(
        modalDialog(
          title = "Select Excel Sheet",
          size = "m",
          easyClose = FALSE,
          
          div(
            class = "info-box",
            p(icon("info-circle"), 
              "This Excel file contains multiple sheets. Please select which sheet to import.")
          ),
          
          selectInput(
            "excel_sheet",
            "Choose a sheet:",
            choices = sheets,
            selected = sheets[1]
          ),
          
          footer = tagList(
            actionButton("confirm_sheet", "Load Sheet", class = "btn-success")
          )
        )
      )
    }
  })

  # ---- Close modal when sheet is confirmed ----
  observeEvent(input$confirm_sheet, {
    removeModal()
    # Trigger data reload
    updateNumericInput(session, "trigger_reload", value = runif(1))
  })
  
  # Hidden trigger for reload (add to UI later)
  # This is handled internally, no UI change needed
  
  # ---- Data reactive with sheet selection ----
  data <- reactive({
    req(input$file)
    
    # For multi-sheet Excel files, wait for sheet selection
    ext <- tolower(tools::file_ext(input$file$name))
    if (ext == "xlsx") {
      sheets <- excel_sheets(input$file$datapath)
      if (length(sheets) > 1) {
        req(input$excel_sheet, input$confirm_sheet)
      }
    }
    
    file <- input$file$datapath
    
    tryCatch({
      
      df <- if (ext == "tsv") {
        read_delim(file, delim = "\t", trim_ws = TRUE, show_col_types = FALSE)
      } else if (ext == "csv") {
        read_csv(file, show_col_types = FALSE)
      } else if (ext == "xlsx") {
        # Check number of sheets
        sheets <- excel_sheets(file)
        
        if (length(sheets) > 1) {
          req(input$excel_sheet)
          read_excel(file, sheet = input$excel_sheet)
        } else {
          # Single sheet - read directly
          read_excel(file)
        }
      } else {
        stop("Unsupported file format")
      }
      
      # ---------------- VALIDATION ----------------
      
      # Validate that data was loaded
      if (is.null(df) || nrow(df) == 0) {
        showNotification(
          "Error: File appears to be empty or could not be read",
          type = "error",
          duration = 5
        )
        return(NULL)
      }
      
      # Check if parsing likely failed
      if (ncol(df) == 1) {
        showNotification(
          "Warning: Only one column detected. File may not be properly delimited",
          type = "warning",
          duration = 7
        )
      }
      
      # Check for duplicate column names (before sanitization)
      if (any(duplicated(names(df)))) {
        showNotification(
          "Warning: Duplicate column names detected. They will be made unique automatically",
          type = "warning",
          duration = 7
        )
      }
      
      # ---------------- SANITIZATION ----------------
      
      # Sanitize column names
      names(df) <- sapply(names(df), sanitize_colname)
      
      # Ensure uniqueness after sanitization
      if (any(duplicated(names(df)))) {
        names(df) <- make.unique(names(df), sep = "_")
      }
      
      return(df)
      
    }, error = function(e) {
      showNotification(
        paste("Error loading file:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
  })

  # ---- Data table output ----
  output$table <- renderDT({
    req(data())
    
    datatable(
      data(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv')
      ),
      class = 'cell-border stripe',
      rownames = FALSE
    )
  })

  # ---- Column selection card ----
  output$column_selection_card <- renderUI({
    req(data())
    cols <- names(data())
    
    card(
      card_header("Column Selection"),
      card_body(
        selectInput(
          "id_col",
          "ID Column",
          choices = cols,
          selected = cols[1]
        ),
        div(class = "help-text",
            "Select the column containing unique identifiers"),
        
        tags$br(),
        
        selectizeInput(
          "data_cols",
          "Columns to Visualize/Annotate",
          choices = cols,
          multiple = TRUE,
          options = list(
            placeholder = 'Select one or more columns',
            plugins = list('remove_button')
          )
        )
      )
    )
  })
  
  # ---- ID column validation ----
  observeEvent(list(data(), input$id_col), {
    req(data(), input$id_col)
    
    df <- data()
    
    if (any(is.na(df[[input$id_col]]) | df[[input$id_col]] == "")) {
      showNotification(
        "Warning: ID column contains missing or empty values",
        type = "warning",
        duration = 5
      )
    }
    
    if (any(duplicated(df[[input$id_col]]))) {
      showNotification(
        "Warning: ID column contains duplicate values. This may cause issues in iTOL",
        type = "warning",
        duration = 6
      )
    }
  }, ignoreInit = TRUE)

  # ---- Dataset label card ----
  output$dataset_label_card <- renderUI({
    req(data())
    
    card(
      card_header("Dataset Label"),
      card_body(
        textInput(
          "dataset_label", 
          NULL,
          value = "My Dataset",
          placeholder = "Enter a descriptive name"
        ),
        div(class = "help-text",
            "This label will appear in iTOL annotations")
      )
    )
  })

  # ---- Tree upload card ----
  output$tree_upload_card <- renderUI({
    card(
      card_header("Tree File (Optional)"),
      card_body(
        fileInput(
          "tree_file",
          NULL,
          accept = c(".nwk", ".newick", ".tree", ".tre", ".txt"),
          buttonLabel = "Browse...",
          placeholder = "No tree file selected"
        ),
        div(class = "help-text",
            "Upload a Newick tree to validate ID matching")
      )
    )
  })

  # ---- Tree data reactive ----
  tree_data <- reactive({
    req(input$tree_file)
    tree <- read_newick_tree(input$tree_file$datapath)
    
    if(!is.null(tree$error)) {
      showNotification(
        tree$error,
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    return(tree)
  })

  # ---- Tree info display ----
  output$tree_info_card <- renderUI({
    req(tree_data())
    
    tree <- tree_data()
    df <- data()
    
    # Calculate match statistics if data is loaded
    match_stats <- NULL
    if(!is.null(df) && !is.null(input$id_col)) {
      data_ids <- unique(as.character(df[[input$id_col]]))
      data_ids <- data_ids[!is.na(data_ids) & data_ids != ""]
      tree_ids <- tree$tip_labels
      
      missing_in_tree <- setdiff(data_ids, tree_ids)
      missing_in_data <- setdiff(tree_ids, data_ids)
      match_count <- sum(data_ids %in% tree_ids)
      match_pct <- if(length(data_ids) > 0) {
        round(100 * match_count / length(data_ids), 1)
      } else {
        0
      }
      
      match_stats <- list(
        data_ids = data_ids,
        tree_ids = tree_ids,
        missing_in_tree = missing_in_tree,
        missing_in_data = missing_in_data,
        match_count = match_count,
        match_pct = match_pct
      )
    }
    
    card(
      card_header("Tree Information"),
      card_body(
        # Basic tree info
        div(
          style = "font-size: 0.9rem;",
          p(
            icon("tree"), 
            tags$strong("Tip labels:"), 
            tree$n_tips
          ),
          if(tree$n_nodes > 0) {
            p(
              icon("project-diagram"), 
              tags$strong("Internal nodes:"), 
              tree$n_nodes
            )
          }
        ),
        
        # Match statistics (if data loaded)
        if(!is.null(match_stats)) {
          tagList(
            
            # Match percentage display
            div(
              class = if(match_stats$match_pct >= 90) "info-box" else "info-box",
              style = if(match_stats$match_pct >= 90) {
                "margin: 0.5rem 0;"
              } else {
                "margin: 0.5rem 0; background-color: #fff3cd; border-left-color: #ffc107;"
              },
              p(
                style = "font-weight: 600; margin-bottom: 0.25rem;",
                icon(if(match_stats$match_pct >= 90) "check-circle" else "exclamation-triangle"),
                sprintf(" ID Match: %s%%", match_stats$match_pct)
              ),
              p(
                style = "margin: 0; font-size: 0.85rem;",
                sprintf("%d of %d IDs match tree tip labels", 
                        match_stats$match_count, 
                        length(match_stats$data_ids))
              )
            ),
            
            # IDs in data but not in tree
            if(length(match_stats$missing_in_tree) > 0) {
              tags$details(
                tags$summary(
                  style = "cursor: pointer; color: #E74C3C; font-weight: 600; margin: 0.5rem 0;",
                  sprintf("⚠ %d ID(s) in data NOT in tree", length(match_stats$missing_in_tree))
                ),
                tags$pre(
                  style = "max-height: 150px; overflow-y: auto; background-color: #fff5f5; padding: 0.5rem; margin-top: 0.5rem; font-size: 0.75rem; border: 1px solid #feb2b2; border-radius: 0.25rem;",
                  paste(match_stats$missing_in_tree, collapse = "\n")
                )
              )
            },
            
            # IDs in tree but not in data
            if(length(match_stats$missing_in_data) > 0) {
              tags$details(
                tags$summary(
                  style = "cursor: pointer; color: #F39C12; font-weight: 600; margin: 0.5rem 0;",
                  sprintf("ℹ %d tree tip(s) NOT in data", length(match_stats$missing_in_data))
                ),
                tags$pre(
                  style = "max-height: 150px; overflow-y: auto; background-color: #fffbf0; padding: 0.5rem; margin-top: 0.5rem; font-size: 0.75rem; border: 1px solid #ffd97d; border-radius: 0.25rem;",
                  paste(match_stats$missing_in_data, collapse = "\n")
                )
              )
            }
          )
        } else {
          div(
            class = "help-text",
            style = "margin-top: 0.5rem;",
            "Load data and select an ID column to see matching statistics"
          )
        }
      )
    )
  })
    # ---- Label column selection UI ----
  output$label_column_selection <- renderUI({
    req(data())
    req(input$data_cols)
    cols <- names(data())
    
    tagList(
      selectInput(
        "old_label_col",
        "ID Column",
        choices = cols,
        selected = input$id_col,
        width = "200px"
      ),
      div(class = "help-text",
          "Column containing the labels currently/originally in your tree"),
      
      tags$br(),
      
      selectInput(
        "new_label_col",
        "New Tip Label Column",
        choices = cols,
        selected = if(length(cols) > 1) cols[2] else cols[1],
        width = "200px"
      ),
      div(class = "help-text",
          "Column containing the new tip labels to use")
    )
  })

  # ---- Symbol tab: Column settings UI ----
  output$symbol_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    brewer_pals <- get_brewer_palettes()
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_data <- df[[col]]
      
      # Determine if column is numeric (same logic as bar chart tab)
      is_numeric_col <- is.numeric(col_data)
      if(!is_numeric_col) {
        # Try converting to numeric
        converted <- suppressWarnings(as.numeric(col_data))
        non_na_count <- sum(!is.na(converted))
        # If at least one value converts successfully, treat as numeric
        is_numeric_col <- non_na_count > 0
      }
      
      # Get unique values (after standardization and NA removal)
      if(is_numeric_col) {
        # For numeric columns, convert and remove NAs
        if(!is.numeric(col_data)) {
          col_data <- suppressWarnings(as.numeric(col_data))
        }
        col_values <- unique(col_data[!is.na(col_data)])
        col_values <- sort(col_values)  # Sort numeric values
      } else {
        # For qualitative columns, standardize and remove NAs
        col_values <- unique(sapply(as.character(col_data), standardize_value))
        col_values <- col_values[!is.na(col_values)]
      }
      
      # Build palette choices based on column type
      if(is_numeric_col) {
        # Sequential palettes only for numeric data
        palette_choices <- c("Select a palette" = "", brewer_pals$Sequential)
        default_palette <- "Blues"
      } else {
        # Qualitative palettes only for categorical data
        palette_choices <- c("Select a palette" = "", brewer_pals$Qualitative)
        default_palette <- "Set1"
      }
      
      # Get current settings
      current_color_mode <- isolate(input[[paste0("color_mode_", col)]])
      current_brewer_pal <- isolate(input[[paste0("brewer_palette_", col)]])
      current_symbol_mode <- isolate(input[[paste0("symbol_mode_", col)]])
      current_auto_symbol <- isolate(input[[paste0("auto_symbol_", col)]])
      
      if(is.null(current_color_mode)) {
        current_color_mode <- "ColorBrewer" 
      }
      if(is.null(current_brewer_pal)) current_brewer_pal <- default_palette
      if(is.null(current_symbol_mode)) current_symbol_mode <- "Auto"
      if(is.null(current_auto_symbol)) current_auto_symbol <- 1
      
      # Create accordion item with advanced settings in side-by-side layout
      accordion_panel(
        title = paste0(col, if(is_numeric_col) " (Numeric)" else " (Categorical)"),
        value = paste0("panel_", idx),
        
        # Info about column type
        div(
          class = "info-box",
          style = "margin-bottom: 1rem; font-size: 0.85rem;",
          p(
            icon(if(is_numeric_col) "hashtag" else "font"),
            if(is_numeric_col) {
              sprintf("Numeric column with %d unique values (NA values filtered)", length(col_values))
            } else {
              sprintf("Categorical column with %d unique values (NA values filtered)", length(col_values))
            }
          )
        ),
        
        # Color mode selection
        radioGroupButtons(
          paste0("color_mode_", col),
          "Color Mode",
          choices = c("ColorBrewer" = "ColorBrewer", 
                      "Manual" = "Manual",
                      "Hue Scale" = "Hue Scale"),
          selected = current_color_mode,
          justified = FALSE,
          size = "xs",
          status = "primary",
          width = "100%",
          individual = TRUE
        ),
        
        # ColorBrewer palette selector
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'ColorBrewer'", col),
          selectInput(
            paste0("brewer_palette_", col),
            if(is_numeric_col) "Select Sequential Palette" else "Select Qualitative Palette",
            choices = palette_choices,
            selected = current_brewer_pal,
            width = "200px"
          ),
          div(class = "help-text",
              if(is_numeric_col) {
                "Sequential palettes are recommended for numeric data"
              } else {
                "Qualitative palettes are recommended for categorical data"
              }
          )
        ),
        
        tags$hr(),
        
        # Symbol mode selection
        radioGroupButtons(
          paste0("symbol_mode_", col),
          "Symbol Mode",
          choices = c("Auto" = "Auto", "Manual" = "Manual"),
          selected = current_symbol_mode,
          justified = FALSE,
          size = "xs",
          status = "primary",
          width = "75%",
          individual = TRUE
        ),
        
        # Auto symbol selector
        conditionalPanel(
          condition = sprintf("input['symbol_mode_%s'] == 'Auto'", col),
          selectInput(
            paste0("auto_symbol_", col),
            "Symbol for All Values",
            choices = symbol_names,
            selected = current_auto_symbol,
            width = "200px"
          )
        ),
        
        tags$hr(),
        
        # Fill/empty option for symbols
        checkboxInput(
          paste0("symbol_filled_", col),
          "Fill symbols (uncheck for outline only)",
          value = isolate(input[[paste0("symbol_filled_", col)]]) %||% TRUE
        ),
        
        # Manual configuration
        conditionalPanel(
          condition = sprintf("input['color_mode_%s'] == 'Manual' || input['symbol_mode_%s'] == 'Manual'", col, col),
          tags$h6("Configure Individual Values"),
          lapply(col_values, function(val) {
            val_display <- if(is_numeric_col) as.character(val) else val
            val_id <- safe_id(paste(col, val_display, sep = "_"))
            
            current_color <- isolate(input[[paste0("color_", val_id)]])
            current_symbol <- isolate(input[[paste0("symbol_", val_id)]])
            
            if(is.null(current_color)) current_color <- "#3498DB"
            if(is.null(current_symbol)) current_symbol <- 1
            
            div(
              class = "value-config",
              div(class = "value-label", val_display),
              conditionalPanel(
                condition = sprintf("input['color_mode_%s'] == 'Manual'", col),
                colourInput(
                  paste0("color_", val_id),
                  NULL,
                  value = current_color,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              conditionalPanel(
                condition = sprintf("input['symbol_mode_%s'] == 'Manual'", col),
                selectInput(
                  paste0("symbol_", val_id),
                  NULL,
                  choices = symbol_names,
                  selected = current_symbol,
                  width = "120px"
                )
              )
            )
          })
        ),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          # Summary (clickable header) - CLOSED by default
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          # Content (hidden by default)
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("symbol_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("symbol_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("symbol_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("symbol_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("symbol_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Scale factor for legend symbols")
            ),
            
            tags$hr(),

            tags$h6(style = "color: #2C5F8D;", "Symbol Settings"),

            # Maximum symbol size
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_max_size_", col),
                "Maximum Symbol Size",
                value = isolate(input[[paste0("symbol_max_size_", col)]]) %||% 14,
                min = 1,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Largest symbol will be displayed with this size (in pixels)")
            ),
            
            # Gradient fill
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_gradient_", col),
                "Use gradient fill (instead of solid color)",
                value = isolate(input[[paste0("symbol_gradient_", col)]]) %||% FALSE
              )
            ),
            
            # Symbol spacing (for external symbols)
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_spacing_", col),
                "Symbol Column Spacing",
                value = isolate(input[[paste0("symbol_spacing_", col)]]) %||% 10,
                min = 0,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing between symbol columns (only for external symbols)")
            ),

            # Symbol position
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("symbol_position_", col),
                "Symbol Position",
                value = isolate(input[[paste0("symbol_position_", col)]]) %||% -1,
                min = -10,
                max = 1,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Position: 0-1 = on branch (0=start, 0.5=middle, 1=end), negative = external column (-1=first, -2=second, etc.)")
            ),
            
            tags$hr(),
            
            # Label settings
            tags$h6(style = "color: #2C5F8D; margin-top = 1rem;", "Label Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("symbol_show_labels_", col),
                "Show dataset label",
                value = isolate(input[[paste0("symbol_show_labels_", col)]]) %||% FALSE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['symbol_show_labels_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("symbol_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_rotation_", col),
                  "Label Rotation (degrees)",
                  value = isolate(input[[paste0("symbol_label_rotation_", col)]]) %||% 0,
                  min = -180,
                  max = 180,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("symbol_label_shift_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("symbol_label_shift_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("symbol_label_align_to_tree_", col),
                  "Align label to tree circle (circular mode only)",
                  value = isolate(input[[paste0("symbol_label_align_to_tree_", col)]]) %||% FALSE
                )
              )
            )
          )
        )
      )
    })
    
    # Return accordion
    accordion(
      id = "column_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })
    
  # ---- ColorBrewer plot for symbol tab ----
  output$brewer_plot_symbol <- renderPlot({
    # Create layout for two separate sections
    par(mfrow = c(2, 1), mar = c(1, 10, 3, 2))
    
    # Sequential palettes
    display.brewer.all(type = "seq")
    title("Sequential Palettes (for Numeric Data)", cex.main = 1.2, font.main = 2)
    
    # Qualitative palettes
    display.brewer.all(type = "qual")
    title("Qualitative Palettes (for Categorical Data)", cex.main = 1.2, font.main = 2)
    
  }, res = 96, height = 600)
  
# ---- Generate symbol outputs ----
symbol_outputs <- reactive({
  req(data(), input$id_col, input$data_cols)
  
  df <- data()
  output_list <- list()
  
  for(col in input$data_cols) {
    col_data <- df[[col]]
    
    # Determine if column is numeric (same logic as bar chart and UI)
    is_numeric_col <- is.numeric(col_data)
    if(!is_numeric_col) {
      converted <- suppressWarnings(as.numeric(col_data))
      non_na_count <- sum(!is.na(converted))
      is_numeric_col <- non_na_count > 0
    }
    
    # Get unique values with NA filtering
    if(is_numeric_col) {
      if(!is.numeric(col_data)) {
        col_data_clean <- suppressWarnings(as.numeric(col_data))
      } else {
        col_data_clean <- col_data
      }
      col_values <- unique(col_data_clean[!is.na(col_data_clean)])
      col_values <- sort(col_values)
    } else {
      col_values <- unique(sapply(as.character(col_data), standardize_value))
      col_values <- col_values[!is.na(col_values)]
    }
    
    # Get color and symbol settings
    color_mode <- input[[paste0("color_mode_", col)]]
    symbol_mode <- input[[paste0("symbol_mode_", col)]]
    symbol_filled <- input[[paste0("symbol_filled_", col)]]
    
    if(is.null(color_mode)) color_mode <- "ColorBrewer"
    if(is.null(symbol_mode)) symbol_mode <- "Auto"
    if(is.null(symbol_filled)) symbol_filled <- TRUE
    
    # Generate colors
    if(color_mode == "ColorBrewer") {
      brewer_pal <- input[[paste0("brewer_palette_", col)]]
      if(is.null(brewer_pal)) brewer_pal <- if(is_numeric_col) "Blues" else "Set1"
      n_colors <- max(3, min(length(col_values), 12))
      colors <- suppressWarnings(brewer.pal(n_colors, brewer_pal))
      if(length(col_values) > length(colors)) {
        colors <- colorRampPalette(colors)(length(col_values))
      }
      color_map <- setNames(colors[1:length(col_values)], as.character(col_values))
    } else if(color_mode == "Manual") {
      color_map <- setNames(
        sapply(col_values, function(val) {
          val_display <- if(is_numeric_col) as.character(val) else val
          val_id <- safe_id(paste(col, val_display, sep = "_"))
          color <- input[[paste0("color_", val_id)]]
          if(is.null(color)) "#3498DB" else color
        }),
        as.character(col_values)
      )
    } else {  # Hue Scale
      colors <- hue_pal()(length(col_values))
      color_map <- setNames(colors, as.character(col_values))
    }
    
    # Generate symbols
    if(symbol_mode == "Auto") {
      auto_symbol <- input[[paste0("auto_symbol_", col)]]
      if(is.null(auto_symbol)) auto_symbol <- 1
      symbol_map <- setNames(rep(auto_symbol, length(col_values)), as.character(col_values))
    } else {  # Manual
      symbol_map <- setNames(
        sapply(col_values, function(val) {
          val_display <- if(is_numeric_col) as.character(val) else val
          val_id <- safe_id(paste(col, val_display, sep = "_"))
          symbol <- input[[paste0("symbol_", val_id)]]
          if(is.null(symbol)) 2 else symbol
        }),
        as.character(col_values)
      )
    }
    
    # Get advanced settings
    symbol_max_size <- input[[paste0("symbol_max_size_", col)]] %||% 14
    symbol_gradient <- input[[paste0("symbol_gradient_", col)]] %||% FALSE
    symbol_spacing <- input[[paste0("symbol_spacing_", col)]] %||% 10
    symbol_legend_title <- input[[paste0("symbol_legend_title_", col)]] %||% col
    symbol_legend_visible <- input[[paste0("symbol_legend_visible_", col)]] %||% TRUE
    symbol_legend_horizontal <- input[[paste0("symbol_legend_horizontal_", col)]] %||% FALSE
    symbol_legend_scale <- input[[paste0("symbol_legend_scale_", col)]] %||% 1
    symbol_show_labels <- input[[paste0("symbol_show_labels_", col)]] %||% FALSE
    symbol_label_size_factor <- input[[paste0("symbol_label_size_factor_", col)]] %||% 1
    symbol_label_rotation <- input[[paste0("symbol_label_rotation_", col)]] %||% 0
    symbol_label_shift <- input[[paste0("symbol_label_shift_", col)]] %||% 0
    symbol_label_align_to_tree <- input[[paste0("symbol_label_align_to_tree_", col)]] %||% FALSE
    symbol_position <- input[[paste0("symbol_position_", col)]] %||% -1

    # Build iTOL DATASET_SYMBOL format
    content <- c("DATASET_SYMBOL")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = "\t"))
    content <- c(content, paste("COLOR", "#2C5F8D", sep = "\t"))
    content <- c(content, "")
    
    # Legend settings (with advanced options)
    content <- c(content, paste("LEGEND_TITLE", symbol_legend_title, sep = "\t"))
    content <- c(content, paste("LEGEND_SCALE", symbol_legend_scale, sep = "\t"))
    content <- c(content, paste("LEGEND_VISIBLE", if(symbol_legend_visible) "1" else "0", sep = "\t"))
    if(symbol_legend_horizontal) {
      content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = "\t"))
    }
    content <- c(content, paste("LEGEND_SHAPES", paste(symbol_map, collapse = "\t"), sep = "\t"))
    content <- c(content, paste("LEGEND_COLORS", paste(color_map, collapse = "\t"), sep = "\t"))
    content <- c(content, paste("LEGEND_LABELS", paste(names(color_map), collapse = "\t"), sep = "\t"))
    content <- c(content, "")
    
    # Symbol display settings (with advanced options)
    content <- c(content, paste("MAXIMUM_SIZE", symbol_max_size, sep = "\t"))
    content <- c(content, paste("GRADIENT_FILL", if(symbol_gradient) "1" else "0", sep = "\t"))
    content <- c(content, paste("SYMBOL_SPACING", symbol_spacing, sep = "\t"))
    content <- c(content, "")
    
    # Label settings (with advanced options)
    content <- c(content, paste("SHOW_LABELS", if(symbol_show_labels) "1" else "0", sep = "\t"))
    content <- c(content, paste("LABEL_SIZE_FACTOR", symbol_label_size_factor, sep = "\t"))
    content <- c(content, paste("LABEL_ROTATION", symbol_label_rotation, sep = "\t"))
    content <- c(content, paste("LABEL_SHIFT", symbol_label_shift, sep = "\t"))
    content <- c(content, paste("LABEL_ALIGN_TO_TREE", if(symbol_label_align_to_tree) "1" else "0", sep = "\t"))
    content <- c(content, "")
    content <- c(content, "DATA")
    
    # Data format: ID, symbol, size, color, fill, position, label
    for(i in 1:nrow(df)) {
      id <- as.character(df[[input$id_col]][i])
      
      # Get value and handle numeric vs categorical appropriately
      if(is_numeric_col) {
        if(!is.numeric(col_data)) {
          val <- suppressWarnings(as.numeric(df[[col]][i]))
        } else {
          val <- col_data[i]
        }
        val_display <- if(!is.na(val)) as.character(val) else NA_character_
        val_key <- val
      } else {
        val <- standardize_value(df[[col]][i])
        val_display <- val
        val_key <- val
      }
      
      # Skip NA values
      if(!is.na(val_key)) {
        val_key_char <- as.character(val_key)
        symbol <- symbol_map[val_key_char]
        color <- color_map[val_key_char]
        fill_value <- if(symbol_filled) "1" else "0"
        content <- c(content, paste(id, symbol, "1", color, fill_value, symbol_position, val_display, sep = "\t"))
      }
    }
    
    output_list[[col]] <- paste(content, collapse = "\n")
  }
  
  return(output_list)
})
  
  # ---- Symbol download card ----
  output$symbol_download_card <- renderUI({
    req(symbol_outputs())
    content_list <- symbol_outputs()
    
    card(
      card_header("Download Symbol Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_symbol_single", 
            "Download Symbol File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_symbols_zip",
              "Download All Symbol Files (ZIP)",
              icon_name = "file-zipper"
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text-center",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                centered_download_button(
                  paste0("download_symbol_", safe_id(name)),
                  paste0(name, ".txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })

    # ---- Binary tab: Column settings UI ----
  output$binary_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]  # Filter out NA values

      # Get current settings
      current_binary_shape <- isolate(input[[paste0("binary_shape_", col)]])
      current_binary_color <- isolate(input[[paste0("binary_color_", col)]])
      current_binary_filled <- isolate(input[[paste0("binary_filled_", col)]])
      
      if(is.null(current_binary_shape)) current_binary_shape <- 2
      if(is.null(current_binary_color)) current_binary_color <- "#3498DB"
      if(is.null(current_binary_filled)) current_binary_filled <- TRUE
      
      # Create accordion item
      accordion_panel(
        title = col,
        value = paste0("binary_panel_", idx),
        
        # Shape selection
        selectInput(
          paste0("binary_shape_", col),
          "Shape",
          choices = symbol_names,
          selected = current_binary_shape,
          width = "200px"
        ),
        
        # Color selection
        colourInput(
          paste0("binary_color_", col),
          "Color",
          value = current_binary_color,
          showColour = "both",
          palette = "square",
          returnName = FALSE
        ),
        
        # Filled/empty option
        div(
          style = "margin-bottom: 1rem;",
          checkboxInput(
            paste0("binary_filled_", col),
            "Show only filled shapes (hide empty shapes)",
            value = current_binary_filled
          )
        ),
        
        tags$hr(),
        
        # Value selection mode
        radioButtons(
          paste0("binary_mode_", col),
          "Value Selection Mode",
          choices = c(
            "Include specific values (show presence)" = "include",
            "Exclude specific values (show absence)" = "exclude",
            "All values as separate fields" = "all"
          ),
          selected = isolate(input[[paste0("binary_mode_", col)]]) %||% "all"
        ),
        
        # Value selection (conditional)
        conditionalPanel(
          condition = sprintf("input['binary_mode_%s'] != 'all'", col),
          checkboxGroupInput(
            paste0("binary_values_", col),
            "Select Values",
            choices = col_values,
            selected = isolate(input[[paste0("binary_values_", col)]]) %||% col_values[1],
            width = "200px"
          )
        ),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          # Summary (clickable header) - CLOSED by default
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          # Content (hidden by default)
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("binary_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("binary_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("binary_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("binary_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("binary_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              )
            ),
            
            tags$hr(),

            tags$h6(style = "color: #2C5F8D;", "Symbol Settings"),

            # Symbol height factor
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_height_factor_", col),
                "Symbol Height Factor",
                value = isolate(input[[paste0("binary_height_factor_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Multiplication factor for symbol height (values <1 decrease, >1 increase)")
            ),
            
            # Symbol spacing
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_symbol_spacing_", col),
                "Symbol Spacing",
                value = isolate(input[[paste0("binary_symbol_spacing_", col)]]) %||% 10,
                min = 0,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing between individual binary levels when there's more than one")
            ),
            
            # Margin
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("binary_margin_", col),
                "Left Margin",
                value = isolate(input[[paste0("binary_margin_", col)]]) %||% 0,
                min = -100,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Increase/decrease spacing to next dataset (can be negative for overlapping)")
            ),
            
            tags$hr(),
            
            # Grid settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Grid Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_horizontal_grid_", col),
                "Show horizontal grid",
                value = isolate(input[[paste0("binary_horizontal_grid_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_vertical_grid_", col),
                "Show vertical grid",
                value = isolate(input[[paste0("binary_vertical_grid_", col)]]) %||% FALSE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['binary_horizontal_grid_%s'] || input['binary_vertical_grid_%s']", col, col),
              
              div(
                style = "margin-bottom: 1rem;",
                colourInput(
                  paste0("binary_grid_color_", col),
                  "Grid Line Color",
                  value = isolate(input[[paste0("binary_grid_color_", col)]]) %||% "#0000ff",
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_grid_width_", col),
                  "Grid Line Width",
                  value = isolate(input[[paste0("binary_grid_width_", col)]]) %||% 0.6,
                  min = 0.1,
                  max = 10,
                  step = 0.1,
                  width = "150px"
                )
              )
            ),
            
            tags$hr(),
            
            # Label settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Label Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_show_labels_", col),
                "Show field labels",
                value = isolate(input[[paste0("binary_show_labels_", col)]]) %||% TRUE
              )
            ),
            
            conditionalPanel(
              condition = sprintf("input['binary_show_labels_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("binary_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_rotation_", col),
                  "Label Rotation (degrees)",
                  value = isolate(input[[paste0("binary_label_rotation_", col)]]) %||% 0,
                  min = -180,
                  max = 180,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("binary_label_shift_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("binary_label_shift_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("binary_label_align_to_tree_", col),
                  "Align labels to tree circle (circular mode only)",
                  value = isolate(input[[paste0("binary_label_align_to_tree_", col)]]) %||% FALSE
                )
              )
            ),
            
            tags$hr(),
            
            # Additional options
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Additional Options"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_dashed_lines_", col),
                "Show dashed lines to leaf labels",
                value = isolate(input[[paste0("binary_dashed_lines_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("binary_align_to_labels_", col),
                "Align symbols to end of leaf labels",
                value = isolate(input[[paste0("binary_align_to_labels_", col)]]) %||% FALSE
              ),
              div(class = "help-text",
                  "Individual dataset fields will not be aligned to each other")
            )
          )
        )
      )
    })
    
    # Return accordion
    accordion(
      id = "binary_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })

  # ---- Generate binary outputs ----
  binary_outputs <- reactive({
    # Validate all requirements
    req(data(), input$id_col, input$data_cols)
    req(length(input$data_cols) > 0)
    
    # Only isolate the data frame, not the inputs
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      # Remove NA values from col_values
      col_values <- col_values[!is.na(col_values)]
      
      if(length(col_values) == 0) next  # Skip columns with no valid values
      
      # Get settings WITHOUT isolation - we want these to be reactive
      binary_mode <- input[[paste0("binary_mode_", col)]]
      binary_shape <- input[[paste0("binary_shape_", col)]]
      binary_color <- input[[paste0("binary_color_", col)]]
      binary_filled <- input[[paste0("binary_filled_", col)]]
      selected_values <- input[[paste0("binary_values_", col)]]
      
      # Get advanced settings
      binary_height_factor <- input[[paste0("binary_height_factor_", col)]] %||% 1
      binary_symbol_spacing <- input[[paste0("binary_symbol_spacing_", col)]] %||% 10
      binary_margin <- input[[paste0("binary_margin_", col)]] %||% 0
      binary_horizontal_grid <- input[[paste0("binary_horizontal_grid_", col)]] %||% FALSE
      binary_vertical_grid <- input[[paste0("binary_vertical_grid_", col)]] %||% FALSE
      binary_grid_color <- input[[paste0("binary_grid_color_", col)]] %||% "#0000ff"
      binary_grid_width <- input[[paste0("binary_grid_width_", col)]] %||% 0.6
      binary_legend_title <- input[[paste0("binary_legend_title_", col)]] %||% col
      binary_legend_visible <- input[[paste0("binary_legend_visible_", col)]] %||% TRUE
      binary_legend_horizontal <- input[[paste0("binary_legend_horizontal_", col)]] %||% FALSE
      binary_legend_scale <- input[[paste0("binary_legend_scale_", col)]] %||% 1
      binary_show_labels <- input[[paste0("binary_show_labels_", col)]] %||% TRUE
      binary_label_size_factor <- input[[paste0("binary_label_size_factor_", col)]] %||% 1
      binary_label_rotation <- input[[paste0("binary_label_rotation_", col)]] %||% 0
      binary_label_shift <- input[[paste0("binary_label_shift_", col)]] %||% 0
      binary_label_align_to_tree <- input[[paste0("binary_label_align_to_tree_", col)]] %||% FALSE
      binary_dashed_lines <- input[[paste0("binary_dashed_lines_", col)]] %||% FALSE
      binary_align_to_labels <- input[[paste0("binary_align_to_labels_", col)]] %||% FALSE

      if(is.null(binary_mode)) binary_mode <- "all"
      if(is.null(binary_shape)) binary_shape <- 2
      if(is.null(binary_color)) binary_color <- "#3498DB"
      if(is.null(binary_filled)) binary_filled <- TRUE
      if(is.null(selected_values) && binary_mode != "all") selected_values <- col_values[1]
      
      # Determine which values to include
      if(binary_mode == "all") {
        fields <- col_values
      } else if(binary_mode == "include") {
        fields <- selected_values
      } else {  # exclude
        fields <- setdiff(col_values, selected_values)
      }
      
      # Build iTOL DATASET_BINARY format
      content <- c("DATASET_BINARY")
      content <- c(content, "SEPARATOR TAB")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col, "binary"), sep = "\t"))
      content <- c(content, paste("COLOR", binary_color, sep = "\t"))
      content <- c(content, "")
      
      # Field configuration
      field_shapes <- rep(binary_shape, length(fields))
      content <- c(content, paste("FIELD_SHAPES", paste(field_shapes, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("FIELD_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
      
      # Field colors (one per field)
      field_colors <- rep(binary_color, length(fields))
      content <- c(content, paste("FIELD_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
      
      content <- c(content, "")
      
      # Legend (use advanced settings)
      content <- c(content, paste("LEGEND_TITLE", binary_legend_title, sep = "\t"))
      content <- c(content, paste("LEGEND_SCALE", binary_legend_scale, sep = "\t"))
      content <- c(content, paste("LEGEND_VISIBLE", if(binary_legend_visible) "1" else "0", sep = "\t"))
      if(binary_legend_horizontal) {
        content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = "\t"))
      }
      content <- c(content, paste("LEGEND_SHAPES", paste(field_shapes, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = "\t"), sep = "\t"))
      content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = "\t"), sep = "\t"))
      content <- c(content, "")

      # Advanced display settings
      content <- c(content, paste("HEIGHT_FACTOR", binary_height_factor, sep = "\t"))
      content <- c(content, paste("SYMBOL_SPACING", binary_symbol_spacing, sep = "\t"))
      content <- c(content, paste("MARGIN", binary_margin, sep = "\t"))
      content <- c(content, "")

      # Grid settings
      content <- c(content, paste("HORIZONTAL_GRID", if(binary_horizontal_grid) "1" else "0", sep = "\t"))
      content <- c(content, paste("VERTICAL_GRID", if(binary_vertical_grid) "1" else "0", sep = "\t"))
      if(binary_horizontal_grid || binary_vertical_grid) {
        content <- c(content, paste("GRID_COLOR", binary_grid_color, sep = "\t"))
        content <- c(content, paste("GRID_WIDTH", binary_grid_width, sep = "\t"))
      }
      content <- c(content, "")

      # Label settings
      content <- c(content, paste("SHOW_LABELS", if(binary_show_labels) "1" else "0", sep = "\t"))
      content <- c(content, paste("SIZE_FACTOR", binary_label_size_factor, sep = "\t"))
      content <- c(content, paste("LABEL_ROTATION", binary_label_rotation, sep = "\t"))
      content <- c(content, paste("LABEL_SHIFT", binary_label_shift, sep = "\t"))
      content <- c(content, paste("LABEL_ALIGN_TO_TREE", if(binary_label_align_to_tree) "1" else "0", sep = "\t"))
      content <- c(content, "")

      # Additional options (only add if enabled)
      if(binary_dashed_lines) {
        content <- c(content, paste("DASHED_LINES", "1", sep = "\t"))
      }
      if(binary_align_to_labels) {
        content <- c(content, paste("ALIGN_TO_LABELS", "1", sep = "\t"))
      }
      content <- c(content, "")

      content <- c(content, "DATA")
      
      # Data: ID followed by binary values (1, 0, or -1)
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- standardize_value(df[[col]][i])
        
        # Create binary vector with proper NA handling
        binary_vec <- sapply(fields, function(field) {
          # Check if val is NA first
          if(is.na(val)) {
            if(binary_filled) {
              return(-1)  # Omit shape for NA values
            } else {
              return(0)  # Empty shape for NA values
            }
          } else if(val == field) {
            return(1)  # Filled shape
          } else if(binary_filled) {
            return(-1)  # Omit shape
          } else {
            return(0)  # Empty shape
          }
        })
        
        content <- c(content, paste(c(id, binary_vec), collapse = "\t"))
      }
      
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })
  
  # ---- Binary download card ----
  output$binary_download_card <- renderUI({
    req(binary_outputs())
    content_list <- binary_outputs()
    
    card(
      card_header("Download Binary Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_binary_single", 
            "Download Binary File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_binary_zip",
              "Download All Binary Files (ZIP)",
              icon_name = "file-zipper"
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text-center",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                centered_download_button(
                  paste0("download_binary_", safe_id(name)),
                  paste0(name, "_binary.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })



  # ---- Bar chart tab: Column settings UI ----
  output$bar_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    na_counts <- list()
    
    for(col in input$data_cols) {
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
    
    if(length(numeric_cols) == 0) {
      return(
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107;",
          p(icon("exclamation-triangle"), "No numeric columns selected. Please select at least one numeric column to generate bar charts.")
        )
      )
    }
    
    # Show info about NA filtering if any columns have NAs
    info_messages <- tagList()
    for(col in numeric_cols) {
      if(na_counts[[col]] > 0) {
        info_messages <- tagList(
          info_messages,
          div(
            class = "help-text",
            style = "color: #856404; margin-bottom: 0.5rem;",
            icon("info-circle"),
            sprintf(" Column '%s': %d NA/non-numeric value(s) will be filtered out", col, na_counts[[col]])
          )
        )
      }
    }
    
    # Create accordion for each numeric column
    accordion_items <- lapply(seq_along(numeric_cols), function(idx) {
      col <- numeric_cols[idx]
      
      # Get current settings
      current_bar_color <- isolate(input[[paste0("bar_color_", col)]])
      if(is.null(current_bar_color)) current_bar_color <- "#2C5F8D"
      
      # Create accordion item
      accordion_panel(
        title = col,
        value = paste0("bar_panel_", idx),
        
        # Dataset label
        div(
          style = "margin-bottom: 1rem;",
          textInput(
            paste0("bar_label_", col),
            "Dataset Label",
            value = isolate(input[[paste0("bar_label_", col)]]) %||% col,
            width = "250px"
          ),
          div(class = "help-text",
              "Label used in the legend table")
        ),
        
        # Color selection
        div(
          style = "margin-bottom: 1rem;",
          colourInput(
            paste0("bar_color_", col),
            "Bar Color",
            value = current_bar_color,
            showColour = "both",
            palette = "square",
            returnName = FALSE
          )
        ),
        
        # Scale lines configuration
        div(
          style = "margin-bottom: 1rem;",
          textInput(
            paste0("bar_scale_", col),
            "Scale Lines (comma-separated values)",
            value = isolate(input[[paste0("bar_scale_", col)]]) %||% "",
            placeholder = "e.g., 10,50,100"
          ),
          div(class = "help-text",
              "Optional: Specify values where scale lines will be drawn")
        ),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("bar_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("bar_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("bar_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              )
            ),
            
            tags$hr(),
            
            # Bar dimensions and spacing
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Dimensions & Spacing"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_width_", col),
                "Maximum Bar Width",
                value = isolate(input[[paste0("bar_width_", col)]]) %||% 1000,
                min = 50,
                max = 5000,
                step = 50,
                width = "150px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_margin_", col),
                "Left Margin",
                value = isolate(input[[paste0("bar_margin_", col)]]) %||% 0,
                min = -200,
                max = 200,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Spacing to next dataset (can be negative for overlapping)")
            ),
            
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_log_scale_", col),
                "Use logarithmic scale",
                value = isolate(input[[paste0("bar_log_scale_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_dashed_lines_", col),
                "Show dashed lines to leaf labels",
                value = isolate(input[[paste0("bar_dashed_lines_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_height_factor_", col),
                "Bar Height Factor",
                value = isolate(input[[paste0("bar_height_factor_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Multiplication factor for bar height")
            ),
            
            tags$hr(),

            # Value display settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Value Display"),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("bar_show_value_", col),
                "Display individual values above bars",
                value = isolate(input[[paste0("bar_show_value_", col)]]) %||% TRUE
              )
            ),
            
            
            conditionalPanel(
              condition = sprintf("input['bar_show_value_%s']", col),
              
              div(
                style = "margin-bottom: 1rem;",
                selectInput(
                  paste0("bar_label_position_", col),
                  "Label Position",
                  choices = c(
                    "Outside Right" = "outside-right",
                    "Outside Left" = "outside-left",
                    "Left" = "left",
                    "Center" = "center",
                    "Right" = "right",
                    "Dataset Center" = "dataset-center"
                  ),
                  selected = isolate(input[[paste0("bar_label_position_", col)]]) %||% "left",
                  width = "200px"
                )
              ),
          
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_shift_x_", col),
                  "Label Horizontal Shift",
                  value = isolate(input[[paste0("bar_label_shift_x_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_shift_y_", col),
                  "Label Vertical Shift",
                  value = isolate(input[[paste0("bar_label_shift_y_", col)]]) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  paste0("bar_label_auto_color_", col),
                  "Automatic label color (white/black based on bar darkness)",
                  value = isolate(input[[paste0("bar_label_auto_color_", col)]]) %||% TRUE
                )
              ),
              
              conditionalPanel(
                condition = sprintf("!input['bar_label_auto_color_%s']", col),
                
                div(
                  style = "margin-bottom: 1rem;",
                  colourInput(
                    paste0("bar_label_color_", col),
                    "Value Label Color",
                    value = isolate(input[[paste0("bar_label_color_", col)]]) %||% "#0000ff",
                    showColour = "both",
                    palette = "square",
                    returnName = FALSE
                  )
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  paste0("bar_label_size_factor_", col),
                  "Label Size Factor",
                  value = isolate(input[[paste0("bar_label_size_factor_", col)]]) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              )
            ),
            
            tags$hr(),
            
            # Bar positioning
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Positioning"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_shift_", col),
                "Bar Vertical Shift",
                value = isolate(input[[paste0("bar_shift_", col)]]) %||% 0,
                min = -100,
                max = 100,
                step = 1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Move all bars up/down by a fixed amount")
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_zero_", col),
                "Bar Zero Point",
                value = isolate(input[[paste0("bar_zero_", col)]]) %||% 0,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Starting point for bars. Values smaller than this will extend left")
            ),
            
            tags$hr(),
            
            # Border settings
            tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Border"),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("bar_border_width_", col),
                "Border Width",
                value = isolate(input[[paste0("bar_border_width_", col)]]) %||% 0,
                min = 0,
                max = 10,
                step = 0.5,
                width = "150px"
              ),
              div(class = "help-text",
                  "Width of border around bars (0 = no border)")
            ),
            
            conditionalPanel(
              condition = sprintf("input['bar_border_width_%s'] > 0", col),
              
              div(
                style = "margin-bottom: 0;",
                colourInput(
                  paste0("bar_border_color_", col),
                  "Border Color",
                  value = isolate(input[[paste0("bar_border_color_", col)]]) %||% "#0000ff",
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              )
            )
          )
        )
      )
    })
    
    # Return accordion with NA info
    tagList(
      if(length(info_messages) > 0) {
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107; margin-bottom: 1rem;",
          p(tags$strong("Value Filtering:")),
          info_messages
        )
      },
      accordion(
        id = "bar_accordion",
        multiple = TRUE,
        !!!accordion_items
      )
    )
  })

  # ---- Generate bar chart outputs ----
  bar_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    for(col in input$data_cols) {
      col_data <- df[[col]]
      
      if(is.numeric(col_data)) {
        numeric_cols <- c(numeric_cols, col)
      } else {
        converted <- suppressWarnings(as.numeric(col_data))
        if(sum(!is.na(converted)) > 0) {
          numeric_cols <- c(numeric_cols, col)
        }
      }
    }
    
    if(length(numeric_cols) == 0) return(NULL)
    
    for(col in numeric_cols) {
      # Get basic settings
      bar_color <- input[[paste0("bar_color_", col)]] %||% "#2C5F8D"
      bar_scale <- input[[paste0("bar_scale_", col)]] %||% ""
      
      # Get advanced settings
      bar_width <- input[[paste0("bar_width_", col)]] %||% 1000
      bar_margin <- input[[paste0("bar_margin_", col)]] %||% 0
      bar_height_factor <- input[[paste0("bar_height_factor_", col)]] %||% 1
      bar_shift <- input[[paste0("bar_shift_", col)]] %||% 0
      bar_zero <- input[[paste0("bar_zero_", col)]] %||% 0
      bar_border_width <- input[[paste0("bar_border_width_", col)]] %||% 0
      bar_border_color <- input[[paste0("bar_border_color_", col)]] %||% "#0000ff"
      bar_log_scale <- input[[paste0("bar_log_scale_", col)]] %||% FALSE
      bar_dashed_lines <- input[[paste0("bar_dashed_lines_", col)]] %||% FALSE
      bar_show_value <- input[[paste0("bar_show_value_", col)]] %||% TRUE
      bar_label_position <- input[[paste0("bar_label_position_", col)]] %||% "outside-right"
      bar_label_shift_x <- input[[paste0("bar_label_shift_x_", col)]] %||% 0
      bar_label_shift_y <- input[[paste0("bar_label_shift_y_", col)]] %||% 0
      bar_label_auto_color <- input[[paste0("bar_label_auto_color_", col)]] %||% TRUE
      bar_label_color <- input[[paste0("bar_label_color_", col)]] %||% "#0000ff"
      bar_label_size_factor <- input[[paste0("bar_label_size_factor_", col)]] %||% 1
      bar_legend_title <- input[[paste0("bar_legend_title_", col)]] %||% col
      bar_legend_scale <- input[[paste0("bar_legend_scale_", col)]] %||% 1
      
      # Build iTOL DATASET_SIMPLEBAR format
      content <- c("DATASET_SIMPLEBAR")
      content <- c(content, "SEPARATOR COMMA")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col), sep = ","))
      content <- c(content, paste("COLOR", bar_color, sep = ","))
      content <- c(content, "")
      
      # Scale lines
      if(bar_scale != "" && !is.na(bar_scale)) {
        scale_values <- trimws(unlist(strsplit(bar_scale, ",")))
        scale_line <- paste(scale_values, collapse = ",")
        content <- c(content, paste("DATASET_SCALE", scale_line, sep = ","))
      }
      content <- c(content, "")
      
      # Legend settings
      content <- c(content, paste("LEGEND_TITLE", bar_legend_title, sep = ","))
      content <- c(content, paste("LEGEND_SCALE", bar_legend_scale, sep = ","))
      content <- c(content, paste("LEGEND_SHAPES", "1", sep = ","))
      content <- c(content, paste("LEGEND_COLORS", bar_color, sep = ","))
      content <- c(content, paste("LEGEND_LABELS", col, sep = ","))
      content <- c(content, "")
      
      # Advanced display settings
      content <- c(content, paste("WIDTH", bar_width, sep = ","))
      content <- c(content, paste("MARGIN", bar_margin, sep = ","))
      if(bar_log_scale) {
        content <- c(content, "LOG_SCALE,1")
      }
      if(bar_dashed_lines) {
        content <- c(content, "DASHED_LINES,1")
      }
      content <- c(content, paste("HEIGHT_FACTOR", bar_height_factor, sep = ","))
      content <- c(content, "")
      
      # Value display settings
      content <- c(content, paste("SHOW_VALUE", if(bar_show_value) "1" else "0", sep = ","))
      
      if(bar_show_value) {
        content <- c(content, paste("LABEL_POSITION", bar_label_position, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_X", bar_label_shift_x, sep = ","))
        content <- c(content, paste("LABEL_SHIFT_Y", bar_label_shift_y, sep = ","))
        
        if(bar_label_auto_color) {
          content <- c(content, "LABEL_AUTO_COLOR,1")
        } else {
          content <- c(content, "LABEL_AUTO_COLOR,0")
          content <- c(content, paste("BAR_LABEL_COLOR", bar_label_color, sep = ","))
        }
        
        content <- c(content, paste("LABEL_SIZE_FACTOR", bar_label_size_factor, sep = ","))
      }
      content <- c(content, "")
      
      # Bar positioning
      content <- c(content, paste("BAR_SHIFT", bar_shift, sep = ","))
      content <- c(content, paste("BAR_ZERO", bar_zero, sep = ","))
      content <- c(content, "")
      
      # Border settings
      if(bar_border_width > 0) {
        content <- c(content, paste("BORDER_WIDTH", bar_border_width, sep = ","))
        content <- c(content, paste("BORDER_COLOR", bar_border_color, sep = ","))
      }
      content <- c(content, "")
      
      content <- c(content, "DATA")
      
      # Get column data and convert to numeric if needed
      col_data <- df[[col]]
      if(!is.numeric(col_data)) {
        col_data <- suppressWarnings(as.numeric(col_data))
      }
      
      # Data: ID followed by numeric value - only include valid numerics
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- col_data[i]
        
        if(!is.na(val)) {
          content <- c(content, paste(id, val, sep = ","))
        }
      }
          
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })
    # ---- Bar chart download card ----
  output$bar_download_card <- renderUI({
    req(bar_outputs())
    content_list <- bar_outputs()
    
    if(length(content_list) == 0) return(NULL)
    
    card(
      card_header("Download Bar Chart Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_bar_single", 
            "Download Bar Chart File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_bar_zip",
              "Download All Bar Chart Files (ZIP)",
              icon_name = "file-zipper"
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text-center",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                centered_download_button(
                  paste0("download_bar_", safe_id(name)),
                  paste0(name, "_bar.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })

  # ---- Style tab: Column settings UI ----
  output$style_column_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Create accordion for each column
    accordion_items <- lapply(seq_along(input$data_cols), function(idx) {
      col <- input$data_cols[idx]
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]
      
      accordion_panel(
        title = col,
        value = paste0("style_panel_", idx),
        
        div(
          class = "info-box",
          style = "margin-bottom: 1rem; font-size: 0.85rem;",
          p(
            icon("info-circle"),
            sprintf("Configure label styles for %d unique values", length(col_values))
          )
        ),
        
        # Manual configuration for each value
        tags$h6("Configure Label Styles"),
        lapply(col_values, function(val) {
          val_id <- safe_id(paste(col, val, sep = "_"))
          
          current_color <- isolate(input[[paste0("style_color_", val_id)]])
          current_style <- isolate(input[[paste0("style_font_", val_id)]])
          current_bg <- isolate(input[[paste0("style_bg_", val_id)]])
          current_size <- isolate(input[[paste0("style_size_", val_id)]])
          
          if(is.null(current_color)) current_color <- "#0000ff"
          if(is.null(current_style)) current_style <- "normal"
          if(is.null(current_bg)) current_bg <- "#FFFFFF00"
          if(is.null(current_size)) current_size <- 1
          
          div(
            style = "border: 1px solid #dee2e6; padding: 1rem; margin-bottom: 1rem; border-radius: 0.25rem; background-color: #f8f9fa;",
            tags$h6(style = "color: #2C5F8D; margin-top: 0;", val),
            
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem; margin-bottom: 0.5rem;",
              
              div(
                colourInput(
                  paste0("style_color_", val_id),
                  "Label Color",
                  value = current_color,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE
                )
              ),
              
              div(
                selectInput(
                  paste0("style_font_", val_id),
                  "Font Style",
                  choices = c(
                    "Normal" = "normal",
                    "Bold" = "bold",
                    "Italic" = "italic",
                    "Bold Italic" = "bold-italic"
                  ),
                  selected = current_style,
                  width = "200px"
                )
              )
            ),
            
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 1rem;",
              
              div(
                colourInput(
                  paste0("style_bg_", val_id),
                  "Background Color (optional)",
                  value = current_bg,
                  showColour = "both",
                  palette = "square",
                  returnName = FALSE,
                  allowTransparent = TRUE
                ),
                div(class = "help-text", "Leave transparent for no background")
              ),
              
              div(
                numericInput(
                  paste0("style_size_", val_id),
                  "Size Factor",
                  value = current_size,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "200px"
                ),
                div(class = "help-text", "Relative to global font size")
              )
            )
          )
        }),
        
        tags$hr(),
        
        # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
            icon("cog"),
            "Advanced iTOL Settings"
          ),
          
          div(
            style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
            
            # Legend settings
            tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
            
            div(
              style = "margin-bottom: 1rem;",
              textInput(
                paste0("style_legend_title_", col),
                "Legend Title",
                value = isolate(input[[paste0("style_legend_title_", col)]]) %||% col,
                width = "250px"
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("style_legend_visible_", col),
                "Show legend initially",
                value = isolate(input[[paste0("style_legend_visible_", col)]]) %||% TRUE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              checkboxInput(
                paste0("style_legend_horizontal_", col),
                "Horizontal legend layout",
                value = isolate(input[[paste0("style_legend_horizontal_", col)]]) %||% FALSE
              )
            ),
            
            div(
              style = "margin-bottom: 1rem;",
              numericInput(
                paste0("style_legend_scale_", col),
                "Legend Scale Factor",
                value = isolate(input[[paste0("style_legend_scale_", col)]]) %||% 1,
                min = 0.1,
                max = 5,
                step = 0.1,
                width = "150px"
              ),
              div(class = "help-text",
                  "Scale factor for legend symbols")
            )
          )
        )
      )
    })
    
    accordion(
      id = "style_accordion",
      multiple = TRUE,
      !!!accordion_items
    )
  })

  # ---- Generate style outputs ----
  style_outputs <- reactive({
    req(data(), input$id_col, input$data_cols)
    
    df <- data()
    output_list <- list()
    
    for(col in input$data_cols) {
      col_values <- unique(sapply(as.character(df[[col]]), standardize_value))
      col_values <- col_values[!is.na(col_values)]
      
      if(length(col_values) == 0) next
      
      # Build iTOL DATASET_STYLE format
      content <- c("DATASET_STYLE")
      content <- c(content, "SEPARATOR COMMA")
      content <- c(content, paste("DATASET_LABEL", paste(input$dataset_label, "-", col, "style"), sep = ","))
      content <- c(content, paste("COLOR", "#0000ff", sep = ","))
      content <- c(content, "")
      
      # Get legend colors
      legend_colors <- sapply(col_values, function(val) {
        val_id <- safe_id(paste(col, val, sep = "_"))
        input[[paste0("style_color_", val_id)]] %||% "#0000ff"
      })
      
      # Get advanced legend settings
      style_legend_title <- input[[paste0("style_legend_title_", col)]] %||% col
      style_legend_visible <- input[[paste0("style_legend_visible_", col)]] %||% TRUE
      style_legend_horizontal <- input[[paste0("style_legend_horizontal_", col)]] %||% FALSE
      style_legend_scale <- input[[paste0("style_legend_scale_", col)]] %||% 1
      
      # Add legend
      content <- c(content, paste("LEGEND_TITLE", style_legend_title, sep = ","))
      content <- c(content, paste("LEGEND_SCALE", style_legend_scale, sep = ","))
      content <- c(content, paste("LEGEND_VISIBLE", if(style_legend_visible) "1" else "0", sep = ","))
      if(style_legend_horizontal) {
        content <- c(content, paste("LEGEND_HORIZONTAL", "1", sep = ","))
      }
      content <- c(content, paste("LEGEND_SHAPES", paste(rep("1", length(col_values)), collapse = ","), sep = ","))
      content <- c(content, paste("LEGEND_COLORS", paste(legend_colors, collapse = ","), sep = ","))
      content <- c(content, paste("LEGEND_LABELS", paste(col_values, collapse = ","), sep = ","))
      content <- c(content, "")
      
      content <- c(content, "DATA")
      
      # For each row, check if value matches and apply styling
      for(i in 1:nrow(df)) {
        id <- as.character(df[[input$id_col]][i])
        val <- standardize_value(df[[col]][i])
        
        if(!is.na(val) && val %in% col_values) {
          val_id <- safe_id(paste(col, val, sep = "_"))
          
          color <- input[[paste0("style_color_", val_id)]] %||% "#0000ff"
          font_style <- input[[paste0("style_font_", val_id)]] %||% "normal"
          bg_color <- input[[paste0("style_bg_", val_id)]] %||% "#FFFFFF00"
          size_factor <- input[[paste0("style_size_", val_id)]] %||% 1
          
          # Format: ID,TYPE,WHAT,COLOR,WIDTH_OR_SIZE_FACTOR,STYLE,BACKGROUND_COLOR
          line_parts <- c(
            id,
            "label",
            "node",
            color,
            as.character(size_factor),
            font_style
          )
          
          # Add background color only if specified
          if(!is.null(bg_color) && bg_color != "" && bg_color != "transparent") {
            line_parts <- c(line_parts, bg_color)
          }
          
          content <- c(content, paste(line_parts, collapse = ","))
        }
      }
      
      output_list[[col]] <- paste(content, collapse = "\n")
    }
    
    return(output_list)
  })

  # ---- Style download card ----
  output$style_download_card <- renderUI({
    req(style_outputs())
    content_list <- style_outputs()
    
    if(length(content_list) == 0) return(NULL)
    
    card(
      card_header("Download Label Style Annotations"),
      card_body(
        if(length(content_list) == 1) {
          centered_download_button(
            "download_style_single", 
            "Download Style File"
          )
        } else {
          tagList(
            centered_download_button(
              "download_style_zip",
              "Download All Style Files (ZIP)",
              icon_name = "file-zipper"
            ),
            tags$br(),
            tags$br(),
            div(class = "help-text-center",
                "Or download each annotation file separately:"),
            tags$br(),
            lapply(names(content_list), function(name) {
              tags$div(
                style = "margin-bottom: 0.5rem;",
                centered_download_button(
                  paste0("download_style_", safe_id(name)),
                  paste0(name, "_style.txt"),
                  class = "btn-primary btn-sm"
                )
              )
            })
          )
        }
      )
    )
  })

  # Single style file
  output$download_style_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_style.txt")
    },
    content = function(file) {
      content_list <- style_outputs()
      writeLines(content_list[[1]], file)
    }
  )

  # All style files as ZIP
  output$download_style_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_style_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- style_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_style.txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      unlink(temp_files)
    }
  )

  # Individual style files
  observe({
    req(input$data_cols)
    
    tryCatch({
      content_list <- style_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_style_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_style.txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      NULL
    })
  })

  # ---- Generate metadata output ----
  metadata_output <- reactive({
    req(data(), input$id_col, input$data_cols)
    df <- data()
    
    content <- c("METADATA")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, paste("FIELD_LABELS", paste(input$data_cols, collapse = "\t"), sep = "\t"))
    content <- c(content, "")
    content <- c(content, "DATA")
    
    for(i in 1:nrow(df)) {
      id <- as.character(df[[input$id_col]][i])
      values <- sapply(input$data_cols, function(col) as.character(df[[col]][i]))
      content <- c(content, paste(c(id, values), collapse = "\t"))
    }
    
    paste(content, collapse = "\n")
  })
  
  # ---- Metadata preview ----
  output$metadata_preview_ui <- renderUI({
    req(input$data_cols)
    req(metadata_output())
    
    card(
      card_header("Preview: metadata.txt"),
      card_body(
        tags$pre(
          style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
          metadata_output()
        )
      )
    )
  })
  
  # ---- Metadata download card ----
  output$metadata_download_card <- renderUI({
    req(metadata_output())
    
    card(
      card_header("Download Metadata"),
      card_body(
        centered_download_button(
          "download_metadata",
          "Download metadata.txt"
        )
      )
    )
  })
  
  # ---- Generate labels output ----
  labels_output <- reactive({
    req(data(), input$old_label_col, input$new_label_col)
    df <- data()
    
    content <- c("LABELS")
    content <- c(content, "SEPARATOR TAB")
    content <- c(content, "DATA")
    
    for(i in 1:nrow(df)) {
      old_label <- as.character(df[[input$old_label_col]][i])
      new_label <- as.character(df[[input$new_label_col]][i])
      content <- c(content, paste(old_label, new_label, sep = "\t"))
    }
    
    paste(content, collapse = "\n")
  })
  
    # ---- Labels preview ----
  output$labels_preview_ui <- renderUI({
    req(labels_output())
    
    card(
      card_header("Preview: labels.txt"),
      card_body(
        tags$pre(
          style = "max-height: 400px; overflow-y: auto; background-color: #f8f9fa; padding: 1rem; border-radius: 0.25rem; border: 1px solid #dee2e6;",
          labels_output()
        )
      )
    )
  })
  
  # ---- Labels download card ----
  output$labels_download_card <- renderUI({
    req(labels_output())
    
    card(
      card_header("Download Labels"),
      card_body(
        centered_download_button(
          "download_labels",
          "Download labels.txt"
        )
      )
    )
  })
  
  # ---- Download handlers ----
  
  # Single symbol file
  output$download_symbol_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_symbol.txt")
    },
    content = function(file) {
      content_list <- symbol_outputs()
      writeLines(content_list[[1]], file)
    }
  )
  
  # All symbol files as ZIP
  output$download_symbols_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_symbols_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- symbol_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, ".txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      unlink(temp_files)
    }
  )
  
  # Individual symbol files
  observe({
    req(input$data_cols)
    
    tryCatch({
      content_list <- symbol_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_symbol_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, ".txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      NULL
    })
  })


    # Single binary file
  output$download_binary_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_binary.txt")
    },
    content = function(file) {
      content_list <- binary_outputs()
      writeLines(content_list[[1]], file)
    }
  )
  
  # All binary files as ZIP
  output$download_binary_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_binary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- binary_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_binary.txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      unlink(temp_files)
    }
  )
  
  # Individual binary files
  observe({
    # Add requirement and validation
    req(input$data_cols)
    
    # Use try-catch to prevent errors
    tryCatch({
      content_list <- binary_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_binary_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_binary.txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      # Silently handle errors during initial load
      NULL
    })
  })

    # Single bar chart file
  output$download_bar_single <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_bar.txt")
    },
    content = function(file) {
      content_list <- bar_outputs()
      writeLines(content_list[[1]], file)
    }
  )
  
  # All bar chart files as ZIP
  output$download_bar_zip <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_bar_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      content_list <- bar_outputs()
      temp_dir <- tempdir()
      temp_files <- c()
      
      for(name in names(content_list)) {
        temp_file <- file.path(temp_dir, paste0(name, "_bar.txt"))
        writeLines(content_list[[name]], temp_file)
        temp_files <- c(temp_files, temp_file)
      }
      
      zip::zip(
        zipfile = file,
        files = basename(temp_files),
        root = temp_dir,
        mode = "cherry-pick"
      )
      
      unlink(temp_files)
    }
  )
  
  # Individual bar chart files
  observe({
    req(input$data_cols)
    
    tryCatch({
      content_list <- bar_outputs()
      req(content_list)
      
      if(length(content_list) > 1) {
        lapply(names(content_list), function(name) {
          local({
            my_name <- name
            output[[paste0("download_bar_", safe_id(my_name))]] <- downloadHandler(
              filename = function() {
                paste0(my_name, "_bar.txt")
              },
              content = function(file) {
                writeLines(content_list[[my_name]], file)
              }
            )
          })
        })
      }
    }, error = function(e) {
      NULL
    })
  })

  # ---- Multi-bar tab: Settings UI ----
  output$multibar_settings_ui <- renderUI({
    req(input$data_cols)
    
    df <- isolate(data())
    
    # Filter to numeric or convertible-to-numeric columns
    numeric_cols <- c()
    na_counts <- list()
    
    for(col in input$data_cols) {
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
    
    if(length(numeric_cols) == 0) {
      return(
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107;",
          p(icon("exclamation-triangle"), "No numeric columns selected. Please select at least one numeric column to generate multi-value bar charts.")
        )
      )
    }
    
    # Show info about NA filtering if any columns have NAs
    info_messages <- tagList()
    has_nas <- FALSE
    for(col in numeric_cols) {
      if(na_counts[[col]] > 0) {
        has_nas <- TRUE
        info_messages <- tagList(
          info_messages,
          div(
            class = "help-text",
            style = "color: #856404; margin-bottom: 0.5rem;",
            icon("info-circle"),
            sprintf(" Column '%s': %d NA/non-numeric value(s) will be filtered out", col, na_counts[[col]])
          )
        )
      }
    }
    
    # Get current settings
    current_selected <- isolate(input$multibar_fields)
    current_layout <- isolate(input$multibar_layout)
    current_show_value <- isolate(input$multibar_show_value)
    current_label_position <- isolate(input$multibar_label_position)
    current_auto_color <- isolate(input$multibar_auto_color)
    current_label_color <- isolate(input$multibar_label_color)
    
    if(is.null(current_selected)) current_selected <- numeric_cols[1:min(3, length(numeric_cols))]
    if(is.null(current_layout)) current_layout <- "stacked"
    if(is.null(current_show_value)) current_show_value <- TRUE
    if(is.null(current_label_position)) current_label_position <- "left"
    if(is.null(current_auto_color)) current_auto_color <- TRUE
    if(is.null(current_label_color)) current_label_color <- "#000000"
    
    tagList(
      # NA filtering info box
      if(has_nas) {
        div(
          class = "info-box",
          style = "background-color: #fff3cd; border-left-color: #ffc107; margin-bottom: 1rem;",
          p(tags$strong("Value Filtering:")),
          info_messages
        )
      },
      
      card(
        card_header("Field Selection"),
        card_body(
          selectizeInput(
            "multibar_fields",
            "Select Numeric Columns to Display",
            choices = numeric_cols,
            selected = current_selected,
            multiple = TRUE,
            options = list(
              placeholder = 'Select 2 or more columns',
              plugins = list('remove_button')
            )
          ),
          div(class = "help-text",
              "Select multiple numeric columns to display as a multi-value bar chart")
        )
      ),
      
      card(
        card_header("Dataset Label"),
        card_body(
          textInput(
            "multibar_dataset_label",
            NULL,
            value = isolate(input$multibar_dataset_label) %||% "multibar",
            placeholder = "Enter dataset label"
          ),
          div(class = "help-text",
              "Label for this multi-value bar chart dataset")
        )
      ),
      
      card(
        card_header("Field Colors"),
        card_body(
          uiOutput("multibar_color_inputs")
        )
      ),
      
      card(
        card_header("Display Options"),
        card_body(
          # Bar layout mode selection
          radioButtons(
            "multibar_layout",
            "Bar Layout Mode",
            choices = c(
              "Stacked (default - values stacked vertically)" = "stacked",
              "Aligned (fields displayed side-by-side)" = "aligned",
              "Side Stacked (fields next to each other, slightly offset)" = "side_stacked"
            ),
            selected = current_layout
          ),
          
          div(class = "help-text",
              "Choose how multiple fields are displayed in the bar chart"),
          
          tags$hr(),
          
          checkboxInput(
            "multibar_na_to_zero",
            "Convert missing values (NA) to 0.0",
            value = isolate(input$multibar_na_to_zero) %||% TRUE
          ),
          div(class = "help-text",
              style = "margin-top: -0.5rem; margin-bottom: 1rem;",
              "When checked, missing values will be displayed as 0. When unchecked, samples with any missing values will be excluded."),
              
          tags$hr(),

          textInput(
            "multibar_scale",
            "Scale Lines (comma-separated values)",
            value = isolate(input$multibar_scale) %||% "",
            placeholder = "e.g., 10,50,100"
          ),
          div(class = "help-text",
              "Optional: Specify values where scale lines will be drawn"),
          
          tags$hr(),
          
          # ---- COLLAPSIBLE ADVANCED SETTINGS SECTION ----
          tags$details(
            tags$summary(
              style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
              icon("cog"),
              "Advanced iTOL Settings"
            ),
            
            div(
              style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
              
              # Legend settings
              tags$h6(style = "color: #2C5F8D;", "Legend Settings"),
              
              div(
                style = "margin-bottom: 1rem;",
                textInput(
                  "multibar_legend_title",
                  "Legend Title",
                  value = isolate(input$multibar_legend_title) %||% "Multi-bar Legend",
                  width = "250px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_legend_scale",
                  "Legend Scale Factor",
                  value = isolate(input$multibar_legend_scale) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              tags$hr(),
              
              # Bar dimensions and spacing
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Dimensions & Spacing"),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_width",
                  "Maximum Bar Width",
                  value = isolate(input$multibar_width) %||% 1000,
                  min = 50,
                  max = 5000,
                  step = 50,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_margin",
                  "Left Margin",
                  value = isolate(input$multibar_margin) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Spacing to next dataset (can be negative for overlapping)")
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "multibar_log_scale",
                  "Use logarithmic scale",
                  value = isolate(input$multibar_log_scale) %||% FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "multibar_dashed_lines",
                  "Show dashed lines to leaf labels",
                  value = isolate(input$multibar_dashed_lines) %||% FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_height_factor",
                  "Bar Height Factor",
                  value = isolate(input$multibar_height_factor) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Multiplication factor for bar height")
              ),
              
              tags$hr(),

              # Value display settings
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Value Display"),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "multibar_show_value",
                  "Display individual values inside bars",
                  value = FALSE
                )
              ),
              
              conditionalPanel(
                condition = "input.multibar_show_value",
                
                div(
                  style = "margin-bottom: 1rem;",
                  selectInput(
                    "multibar_label_position",
                    "Label Position",
                    choices = c(
                      "Left" = "left",
                      "Center" = "center",
                      "Right" = "right"
                    ),
                    selected = current_label_position,
                    width = "200px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_shift_x",
                    "Label Horizontal Shift",
                    value = isolate(input$multibar_label_shift_x) %||% 0,
                    min = -200,
                    max = 200,
                    step = 1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_shift_y",
                    "Label Vertical Shift",
                    value = isolate(input$multibar_label_shift_y) %||% 0,
                    min = -200,
                    max = 200,
                    step = 1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "multibar_label_size_factor",
                    "Label Size Factor",
                    value = isolate(input$multibar_label_size_factor) %||% 1,
                    min = 0.1,
                    max = 5,
                    step = 0.1,
                    width = "150px"
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "multibar_auto_color",
                    "Automatic label color (white/black based on bar darkness)",
                    value = current_auto_color
                  )
                ),
                
                conditionalPanel(
                  condition = "!input.multibar_auto_color",
                  
                  div(
                    style = "margin-bottom: 1rem;",
                    colourInput(
                      "multibar_label_color",
                      "Value Label Color",
                      value = current_label_color,
                      showColour = "both",
                      palette = "square",
                      returnName = FALSE
                    )
                  )
                )
              ),
              
              tags$hr(),
              
              # Bar positioning
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Positioning"),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_shift",
                  "Bar Vertical Shift",
                  value = isolate(input$multibar_shift) %||% 0,
                  min = -100,
                  max = 100,
                  step = 1,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Move all bars up/down by a fixed amount")
              ),
              
              tags$hr(),
              
              # Border settings
              tags$h6(style = "color: #2C5F8D; margin-top: 1rem;", "Bar Border"),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "multibar_border_width",
                  "Border Width",
                  value = isolate(input$multibar_border_width) %||% 0,
                  min = 0,
                  max = 10,
                  step = 0.5,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Width of border around bars (0 = no border)")
              ),
              
              conditionalPanel(
                condition = "input.multibar_border_width > 0",
                
                div(
                  style = "margin-bottom: 0;",
                  colourInput(
                    "multibar_border_color",
                    "Border Color",
                    value = isolate(input$multibar_border_color) %||% "#0000ff",
                    showColour = "both",
                    palette = "square",
                    returnName = FALSE
                  )
                )
              )
            )
          )
        )
      )
    )
    })

  # ---- Multi-bar color inputs ----
  output$multibar_color_inputs <- renderUI({
    req(input$multibar_fields)
    
    if(length(input$multibar_fields) == 0) {
      return(
        div(class = "help-text", "Select fields to configure colors")
      )
    }
    
    # Default colors
    default_colors <- c("#ff5757ff", "#86fc86ff", "#5858ffff", "#ff9bffff", "#7affffff", "#ffff88ff")
    
    tagList(
      lapply(seq_along(input$multibar_fields), function(idx) {
        field <- input$multibar_fields[idx]
        default_color <- default_colors[((idx - 1) %% length(default_colors)) + 1]
        current_color <- isolate(input[[paste0("multibar_field_color_", safe_id(field))]])
        if(is.null(current_color)) current_color <- default_color
        
        div(
          class = "value-config",
          div(class = "value-label", field),
          colourInput(
            paste0("multibar_field_color_", safe_id(field)),
            NULL,
            value = current_color,
            showColour = "both",
            palette = "square",
            returnName = FALSE
          )
        )
      })
    )
  })

# ---- Generate multi-bar outputs ----
multibar_output <- reactive({
  req(data(), input$id_col, input$multibar_fields)
  
  if(length(input$multibar_fields) < 1) return(NULL)
  
  df <- data()
  fields <- input$multibar_fields
  
  # Get basic settings
  multibar_scale <- input$multibar_scale %||% ""
  multibar_na_to_zero <- input$multibar_na_to_zero %||% TRUE
  multibar_layout <- input$multibar_layout %||% "stacked"
  
  # Get advanced settings
  multibar_width <- input$multibar_width %||% 1000
  multibar_margin <- input$multibar_margin %||% 0
  multibar_log_scale <- input$multibar_log_scale %||% FALSE
  multibar_dashed_lines <- input$multibar_dashed_lines %||% FALSE
  multibar_height_factor <- input$multibar_height_factor %||% 1
  multibar_show_value <- input$multibar_show_value %||% TRUE
  multibar_label_position <- input$multibar_label_position %||% "left"
  multibar_label_shift_x <- input$multibar_label_shift_x %||% 0
  multibar_label_shift_y <- input$multibar_label_shift_y %||% 0
  multibar_label_size_factor <- input$multibar_label_size_factor %||% 1
  multibar_auto_color <- input$multibar_auto_color %||% TRUE
  multibar_label_color <- input$multibar_label_color %||% "#0000ff"
  multibar_shift <- input$multibar_shift %||% 0
  multibar_border_width <- input$multibar_border_width %||% 0
  multibar_border_color <- input$multibar_border_color %||% "#0000ff"
  
  # Build iTOL DATASET_MULTIBAR format
  content <- c("DATASET_MULTIBAR")
  content <- c(content, "SEPARATOR COMMA")
  
  # Get dataset label
  multibar_dataset_label <- input$multibar_dataset_label %||% "multibar"
  content <- c(content, paste("DATASET_LABEL", multibar_dataset_label, sep = ","))
  content <- c(content, paste("COLOR", "#2C5F8D", sep = ","))
  content <- c(content, "")
  
  # Get field colors
  field_colors <- sapply(fields, function(field) {
    color <- input[[paste0("multibar_field_color_", safe_id(field))]]
    if(is.null(color)) "#3498DB" else color
  })
  
  content <- c(content, paste("FIELD_COLORS", paste(field_colors, collapse = ","), sep = ","))
  content <- c(content, paste("FIELD_LABELS", paste(fields, collapse = ","), sep = ","))
  content <- c(content, "")
  
  # Add scale lines if specified
  if(multibar_scale != "" && !is.na(multibar_scale)) {
    scale_values <- trimws(unlist(strsplit(multibar_scale, ",")))
    scale_line <- paste(scale_values, collapse = ",")
    content <- c(content, paste("DATASET_SCALE", scale_line, sep = ","))
  }
  content <- c(content, "")
  
  # Legend settings
  multibar_legend_title <- input$multibar_legend_title %||% "Multi-bar Legend"
  multibar_legend_scale <- input$multibar_legend_scale %||% 1

  content <- c(content, paste("LEGEND_TITLE", multibar_legend_title, sep = ","))
  content <- c(content, paste("LEGEND_SCALE", multibar_legend_scale, sep = ","))
  content <- c(content, paste("LEGEND_SHAPES", paste(rep("1", length(fields)), collapse = ","), sep = ","))
  content <- c(content, paste("LEGEND_COLORS", paste(field_colors, collapse = ","), sep = ","))
  content <- c(content, paste("LEGEND_LABELS", paste(fields, collapse = ","), sep = ","))
  content <- c(content, "")
  
  # Advanced display settings
  content <- c(content, paste("WIDTH", multibar_width, sep = ","))
  content <- c(content, paste("MARGIN", multibar_margin, sep = ","))
  if(multibar_log_scale) {
    content <- c(content, "LOG_SCALE,1")
  }
  if(multibar_dashed_lines) {
    content <- c(content, "DASHED_LINES,1")
  }
  content <- c(content, paste("HEIGHT_FACTOR", multibar_height_factor, sep = ","))
  content <- c(content, "")
  
  # Layout mode
  if(multibar_layout == "aligned") {
    content <- c(content, "ALIGN_FIELDS,1")
  } else if(multibar_layout == "side_stacked") {
    content <- c(content, "ALIGN_FIELDS,0")
    content <- c(content, "SIDE_STACKED,1")
  } else {  # stacked (default)
    content <- c(content, "ALIGN_FIELDS,0")
  }
  content <- c(content, "")
  
  # Border settings
  if(multibar_border_width > 0) {
    content <- c(content, paste("BORDER_WIDTH", multibar_border_width, sep = ","))
    content <- c(content, paste("BORDER_COLOR", multibar_border_color, sep = ","))
  }
  content <- c(content, "")
  
  # Value display settings
  content <- c(content, paste("SHOW_VALUE", if(multibar_show_value) "1" else "0", sep = ","))
  if(multibar_show_value) {
    content <- c(content, paste("LABEL_POSITION", multibar_label_position, sep = ","))
    content <- c(content, paste("LABEL_SHIFT_X", multibar_label_shift_x, sep = ","))
    content <- c(content, paste("LABEL_SHIFT_Y", multibar_label_shift_y, sep = ","))
    content <- c(content, paste("LABEL_SIZE_FACTOR", multibar_label_size_factor, sep = ","))
    
    if(multibar_auto_color) {
      content <- c(content, "LABEL_AUTO_COLOR,1")
    } else {
      content <- c(content, "LABEL_AUTO_COLOR,0")
      content <- c(content, paste("BAR_LABEL_COLOR", multibar_label_color, sep = ","))
    }
  }
  content <- c(content, "")
  
  # Bar positioning
  content <- c(content, paste("BAR_SHIFT", multibar_shift, sep = ","))
  content <- c(content, "")
  
  content <- c(content, "SHOW_LABELS,0")
  content <- c(content, "")
  content <- c(content, "DATA")
  
  # Data: ID followed by multiple numeric values
  for(i in 1:nrow(df)) {
    id <- as.character(df[[input$id_col]][i])
    
    # Get values for each field, converting to numeric if needed
    values <- sapply(fields, function(field) {
      col_data <- df[[field]]
      if(!is.numeric(col_data)) {
        col_data <- suppressWarnings(as.numeric(col_data))
      }
      val <- col_data[i]
      
      # Handle NA based on user preference
      if(!is.na(val)) {
        return(as.character(val))
      } else {
        if(multibar_na_to_zero) {
          return("0.0")
        } else {
          return(NA_character_)
        }
      }
    })
    
    # Include row based on NA handling mode
    if(multibar_na_to_zero) {
      # Always include row (NAs converted to 0)
      content <- c(content, paste(c(id, values), collapse = ","))
    } else {
      # Only include row if at least one value is non-NA
      if(any(!is.na(values))) {
        values[is.na(values)] <- ""
        content <- c(content, paste(c(id, values), collapse = ","))
      }
    }
  }
  
  return(paste(content, collapse = "\n"))
})
  
  # ---- Multi-bar download card ----
  output$multibar_download_card <- renderUI({
    req(multibar_output())
    
    card(
      card_header("Download Multi-Value Bar Chart"),
      card_body(
        centered_download_button(
          "download_multibar",
          "Download Multi-Bar Chart File"
        )
      )
    )
  })

  # ---- Heatmap tab: Settings UI ----
  output$heatmap_settings_ui <- renderUI({
    req(data())
    
    df <- isolate(data())
    cols <- names(df)
    
    # Get current settings
    current_id_col <- isolate(input$heatmap_id_col)
    current_value_cols <- isolate(input$heatmap_value_cols)
    
    if(is.null(current_id_col)) current_id_col <- cols[1]
    if(is.null(current_value_cols)) current_value_cols <- cols[-1]
    
    tagList(
      card(
        card_header("Column Selection"),
        card_body(
          selectInput(
            "heatmap_id_col",
            "ID Column",
            choices = cols,
            selected = current_id_col,
            width = "250px"
          ),
          div(class = "help-text",
              "Column containing the identifiers (e.g., sample names)"),
          
          tags$br(),
          
          selectizeInput(
            "heatmap_value_cols",
            "Value Columns (fields for heatmap)",
            choices = cols,
            selected = current_value_cols,
            multiple = TRUE,
            options = list(
              placeholder = 'Select columns with numeric values',
              plugins = list('remove_button')
            )
          ),
          div(class = "help-text",
              "Columns containing numeric values to display as heatmap cells")
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
      
      card(
        card_header("Color Settings"),
        card_body(
          colourInput(
            "heatmap_color_min",
            "Minimum Value Color",
            value = isolate(input$heatmap_color_min) %||% "#0099FF",
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
            "heatmap_color_max",
            "Maximum Value Color",
            value = isolate(input$heatmap_color_max) %||% "#FF3D3D",
            showColour = "both",
            palette = "square",
            returnName = FALSE
          ),
          
          colourInput(
            "heatmap_color_nan",
            "Color for Missing Values (NA)",
            value = isolate(input$heatmap_color_nan) %||% "#000000",
            showColour = "both",
            palette = "square",
            returnName = FALSE
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
          
          # Advanced settings
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
                    value = isolate(input$heatmap_border_color) %||% "#0099FF",
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
    req(data(), input$heatmap_id_col, input$heatmap_value_cols)
    
    if(length(input$heatmap_value_cols) < 1) return(NULL)
    
    df <- data()
    id_col <- input$heatmap_id_col
    value_cols <- input$heatmap_value_cols
    
    # Get settings
    heatmap_dataset_label <- input$heatmap_dataset_label %||% "heatmap"
    heatmap_color_min <- input$heatmap_color_min %||% "#0099FF"
    heatmap_color_max <- input$heatmap_color_max %||% "#FF3D3D"
    heatmap_color_mid <- input$heatmap_color_mid %||% "#ffff00"
    heatmap_use_mid_color <- input$heatmap_use_mid_color %||% FALSE
    heatmap_color_nan <- input$heatmap_color_nan %||% "#000000"
    heatmap_strip_width <- input$heatmap_strip_width %||% 30
    heatmap_auto_legend <- input$heatmap_auto_legend %||% TRUE
    heatmap_border_width <- input$heatmap_border_width %||% 0
    heatmap_border_color <- input$heatmap_border_color %||% "#0000ff"
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
    
    # Field labels
    content <- c(content, paste("FIELD_LABELS", paste(value_cols, collapse = "\t"), sep = "\t"))
    content <- c(content, "")
    
    # Color settings
    content <- c(content, paste("COLOR_MIN", heatmap_color_min, sep = "\t"))
    content <- c(content, paste("COLOR_MAX", heatmap_color_max, sep = "\t"))
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
        val <- df[[col]][i]
        
        # Convert to numeric if not already
        if(!is.numeric(val)) {
          val <- suppressWarnings(as.numeric(val))
        }
        
        # Return "X" for NA values, otherwise the numeric value
        if(is.na(val)) {
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

  # Heatmap download
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_heatmap.txt")
    },
    content = function(file) {
      writeLines(heatmap_output(), file)
    }
  )
  # ---- Multi-bar download handler ----
  output$download_multibar <- downloadHandler(
    filename = function() {
      paste0(input$dataset_label, "_multibar.txt")
    },
    content = function(file) {
      writeLines(multibar_output(), file)
    }
  )
      
  # Metadata download
  output$download_metadata <- downloadHandler(
    filename = function() {
      "metadata.txt"
    },
    content = function(file) {
      writeLines(metadata_output(), file)
    }
  )
  
  # Labels download
  output$download_labels <- downloadHandler(
    filename = function() {
      "labels.txt"
    },
    content = function(file) {
      writeLines(labels_output(), file)
    }
  )
}