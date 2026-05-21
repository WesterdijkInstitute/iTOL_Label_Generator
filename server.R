
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
      # Store original column names before sanitization
      original_colnames <- names(df)

      # Sanitize column names
      names(df) <- sapply(names(df), sanitize_colname)

      # Ensure uniqueness after sanitization
      if (any(duplicated(names(df)))) {
        names(df) <- make.unique(names(df), sep = "_")
      }

      # Store mapping as an attribute
      attr(df, "original_colnames") <- setNames(original_colnames, names(df))

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
   
  source("tabs/symbols.R", local = TRUE)
  source("tabs/binary.R", local = TRUE)
  source("tabs/bar_chart.R", local = TRUE)
  source("tabs/multi_bar_chart.R", local = TRUE)
  source("tabs/heatmap.R", local = TRUE)
  source("tabs/alignment.R", local = TRUE)
  source("tabs/styles.R", local = TRUE)
  source("tabs/metadata.R", local = TRUE)
  source("tabs/labels.R", local = TRUE)
  
}