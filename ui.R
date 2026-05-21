# ---------- UI ----------

ui <- page_sidebar(
  
  # Theme configuration - using only system fonts
  theme = bs_theme(
    version = 5,
    preset = "flatly",
    primary = "#2C5F8D",
    secondary = "#5A7A9B",
    success = "#2C5F8D",
    info = "#5DADE2",
    warning = "#F39C12",
    danger = "#E74C3C",
    base_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
    heading_font = "system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif",
    font_scale = 0.95
  ),
  
  # Custom CSS for enhanced academic styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Application title
  title = "iTOL Label Generator",
  
  # Sidebar
  sidebar = sidebar(
    width = 450,
    
    # Header with logo
    div(class = "logo-container",
        tags$img(src = "https://wi.knaw.nl/images/westerdijk-logo.png", 
                 alt = "Westerdijk Institute Logo"),
        tags$h4("iTOL Label Generator")
    ),
    
    # File upload section
    card(
      card_header("Upload Data"),
      card_body(
        fileInput(
          "file", 
          NULL,
          accept = c(".tsv", ".csv", ".xlsx"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        div(class = "help-text",
            "Supported formats: TSV, CSV, XLSX")
      )
    ),
    
    # Column selection (shown after file upload)
    uiOutput("column_selection_card"),
    
    # Dataset label
    uiOutput("dataset_label_card"),

    # Tree file upload (optional)
    uiOutput("tree_upload_card"),

    # Tree information display  
    uiOutput("tree_info_card")
  ),
  
      # Main panel with tabs
    navset_card_tab(
      id = "main_tabs",
      
      # Data preview tab
      nav_panel(
        "Data Preview",
        icon = icon("table"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Preview your uploaded data. Verify that all columns are correctly loaded.")
            ),
            DTOutput("table")
          )
        )
      ),
      
      # Symbol Annotations tab
      nav_panel(
        "Symbol Annotations",
        icon = icon("shapes"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_SYMBOL annotations. Configure colors and symbols for each metadata column.")
            ),
            
            uiOutput("symbol_column_settings_ui"),
            
            tags$details(
              tags$summary(
                style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 1rem 0 0.5rem 0;",
                "View ColorBrewer Palette Reference"
              ),
              plotOutput("brewer_plot_symbol", height = "600px")
            ),
            
            tags$hr(),
            
            uiOutput("symbol_download_card")
          )
        )
      ),

      # Binary Set tab
      nav_panel(
        "Binary Set",
        icon = icon("chart-simple"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_BINARY annotations. Configure binary presence/absence patterns for each metadata column.")
            ),
            
            uiOutput("binary_column_settings_ui"),
            
            tags$hr(),
            
            uiOutput("binary_download_card")
          )
        )
      ),
      
      # Simple Bar Chart tab
      nav_panel(
        "Simple Bar Chart",
        icon = icon("chart-bar"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_SIMPLEBAR annotations. Display numeric values as bars outside the tree.")
            ),
            
            uiOutput("bar_column_settings_ui"),
            
            tags$hr(),
            
            uiOutput("bar_download_card")
          )
        )
      ),

      # Multi-Value Bar Chart tab
      nav_panel(
        "Multi-Value Bar Chart",
        icon = icon("chart-column"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_MULTIBAR annotations. Display multiple numeric values as stacked or aligned bar charts.")
            ),
            
            uiOutput("multibar_settings_ui"),
            
            tags$hr(),
            
            uiOutput("multibar_download_card")
          )
        )
      ),

      # Heatmap tab
      nav_panel(
        "Heatmap",
        icon = icon("table-cells"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_HEATMAP annotations. Upload a matrix with IDs defined in chosen ID column and sample/field names as headers.")
            ),
            
            uiOutput("heatmap_settings_ui"),
            
            tags$hr(),
            
            uiOutput("heatmap_download_card")
          )
        )
      ),
      
      # Label Styles tab
      nav_panel(
        "Label Styles",
        icon = icon("palette"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate DATASET_STYLE annotations. Customize label colors and styles for specific taxa.")
            ),
            
            uiOutput("style_column_settings_ui"),
            
            tags$hr(),
            
            uiOutput("style_download_card")
          )
        )
      ),

      # Metadata tab
      nav_panel(
        "Metadata",
        icon = icon("database"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate METADATA annotations. Selected columns will be included as metadata fields in iTOL.")
            ),
            
            uiOutput("metadata_preview_ui"),
            
            tags$hr(),
            
            uiOutput("metadata_download_card")
          )
        )
      ),
      
      # Change Labels tab
      nav_panel(
        "Change Labels",
        icon = icon("tags"),
        card_body(
          div(class = "scrollable-tab-content",
            div(class = "info-box",
                p(icon("info-circle"), "Generate LABELS annotation file. Replace tree labels with new values from your data.")
            ),
            
            card(
              card_header("Label Configuration"),
              card_body(
                uiOutput("label_column_selection")
              )
            ),
            
            uiOutput("labels_preview_ui"),
            
            tags$hr(),
            
            uiOutput("labels_download_card")
          )
        )
      )
    )
  )

