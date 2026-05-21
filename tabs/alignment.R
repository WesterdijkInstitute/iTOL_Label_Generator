# ---- Alignment tab: Settings UI ----
output$alignment_settings_ui <- renderUI({
  req(data(), input$id_col)
  
  tagList(
    card(
      card_header("Upload Alignment File"),
      card_body(
        fileInput(
          "alignment_file",
          NULL,
          accept = c(".fasta", ".fa", ".fna", ".faa", ".txt"),
          buttonLabel = "Browse...",
          placeholder = "No alignment file selected"
        ),
        div(class = "help-text",
            "Upload a FASTA format alignment file. Sequence IDs should match your ID column.")
      )
    ),
    
    # Only show settings if alignment is loaded
    conditionalPanel(
      condition = "output.alignment_loaded",
      
      card(
        card_header("Dataset Label"),
        card_body(
          textInput(
            "alignment_dataset_label",
            NULL,
            value = isolate(input$alignment_dataset_label) %||% "alignment",
            placeholder = "Enter dataset label"
          ),
          div(class = "help-text",
              "Label for this alignment dataset")
        )
      ),
      
      card(
        card_header("Alignment Type"),
        card_body(
            selectInput(
            "alignment_type",
            NULL,
            choices = c(
                "Protein (amino acids)" = "aa",
                "DNA" = "dna"
            ),
            selected = isolate(input$alignment_type) %||% "aa",
            width = "200px"
            ),
            div(class = "help-text",
                "Select the type of sequences in your alignment file")
            )
        ),
      card(
        card_header("Display Options"),
        card_body(
          # Color scheme selection
          tags$h6("Color Scheme"),
          
          radioButtons(
            "alignment_color_scheme",
            NULL,
            choices = c(
              "None (no coloring)" = "none",
              "Clustal (by amino acid properties)" = "clustal",
              "Zappo (by physico-chemical properties)" = "zappo",
              "Taylor (by residue type)" = "taylor",
              "Hydrophobicity" = "hphob",
              "Helix propensity" = "helix",
              "Strand propensity" = "strand",
              "Turn propensity" = "turn",
              "Buried index" = "buried"
            ),
            selected = isolate(input$alignment_color_scheme) %||% "clustal"
          ),
          
          tags$hr(),
          
          numericInput(
            "alignment_start_pos",
            "Start Position (residue number)",
            value = isolate(input$alignment_start_pos) %||% 1,
            min = 1,
            step = 1,
            width = "150px"
          ),
          div(class = "help-text",
              "First residue of alignment to display"),
          
          numericInput(
            "alignment_end_pos",
            "End Position (residue number)",
            value = isolate(input$alignment_end_pos) %||% 100,
            min = 1,
            step = 1,
            width = "150px"
          ),
          div(class = "help-text",
              "Last residue of alignment to display (max ~4000)"),
          
          tags$hr(),
          
          tags$details(
            tags$summary(
              style = "cursor: pointer; font-weight: 600; color: #2C5F8D; margin: 0.5rem 0; display: flex; align-items: center; gap: 0.5rem;",
              icon("cog"),
              "Advanced iTOL Settings"
            ),
            
            div(
              style = "padding: 1rem; background-color: #f8f9fa; border-radius: 0.25rem; margin-top: 0.5rem; border: 1px solid #dee2e6;",
              
              tags$h6(style = "color: #2C5F8D;", "Highlighting Options"),
              
              div(
                style = "margin-bottom: 1rem;",
                radioButtons(
                  "alignment_highlight_type",
                  "Highlight Type",
                  choices = c(
                    "None" = "none",
                    "Consensus" = "consensus",
                    "Reference sequences" = "reference"
                  ),
                  selected = isolate(input$alignment_highlight_type) %||% "none"
                )
              ),
              
              conditionalPanel(
                condition = "input.alignment_highlight_type == 'reference'",
                div(
                  style = "margin-bottom: 1rem;",
                  textAreaInput(
                    "alignment_references",
                    "Reference Sequence IDs (comma-separated)",
                    value = isolate(input$alignment_references) %||% "",
                    placeholder = "seq1,seq2,seq3",
                    rows = 2,
                    width = "100%"
                  ),
                  div(class = "help-text",
                      "IDs must match sequence IDs in your FASTA file")
                ),
                
                tags$hr(),
                
                tags$h6(style = "color: #2C5F8D;", "Reference Box Styling"),
                
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "alignment_mark_references",
                    "Mark reference sequences with a box",
                    value = isolate(input$alignment_mark_references) %||% TRUE
                  )
                ),
                
                conditionalPanel(
                  condition = "input.alignment_mark_references",
                  div(
                    style = "margin-bottom: 1rem;",
                    numericInput(
                      "alignment_ref_box_border_width",
                      "Reference Box Border Width",
                      value = isolate(input$alignment_ref_box_border_width) %||% 1,
                      min = 0,
                      max = 10,
                      step = 0.5,
                      width = "150px"
                    )
                  ),
                  
                  div(
                    style = "margin-bottom: 1rem;",
                    colourInput(
                      "alignment_ref_box_border_color",
                      "Reference Box Border Color",
                      value = isolate(input$alignment_ref_box_border_color) %||% "#ff0000",
                      showColour = "both"
                    )
                  ),
                  
                  div(
                    style = "margin-bottom: 1rem;",
                    colourInput(
                      "alignment_ref_box_fill_color",
                      "Reference Box Fill Color",
                      value = isolate(input$alignment_ref_box_fill_color) %||% "#aaaaaa",
                      showColour = "both"
                    )
                  )
                ),
                
                tags$hr()
              ),
              
              conditionalPanel(
                condition = "input.alignment_highlight_type != 'none'",
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "alignment_highlight_disagreements",
                    "Highlight disagreements (non-matching residues)",
                    value = isolate(input$alignment_highlight_disagreements) %||% FALSE
                  )
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "alignment_colored_dots",
                    "Color dots based on underlying residue",
                    value = isolate(input$alignment_colored_dots) %||% TRUE
                  )
                ),
                
                tags$hr()
              ),
              
              tags$h6(style = "color: #2C5F8D;", "Consensus Settings"),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "alignment_display_consensus",
                  "Display consensus sequence below alignment",
                  value = isolate(input$alignment_display_consensus) %||% TRUE
                )
              ),
              
              conditionalPanel(
                condition = "input.alignment_display_consensus",
                div(
                  style = "margin-bottom: 1rem;",
                  numericInput(
                    "alignment_consensus_threshold",
                    "Consensus Threshold (%)",
                    value = isolate(input$alignment_consensus_threshold) %||% 50,
                    min = 0,
                    max = 100,
                    step = 5,
                    width = "150px"
                  ),
                  div(class = "help-text",
                      "Minimum percentage for residue to appear in consensus")
                ),
                
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "alignment_ignore_gaps",
                    "Ignore gaps when calculating consensus",
                    value = isolate(input$alignment_ignore_gaps) %||% TRUE
                  )
                )
              ),
              
              tags$hr(),
              
              tags$h6(style = "color: #2C5F8D;", "Conservation Graph"),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "alignment_display_conservation",
                  "Display conservation graph below alignment",
                  value = isolate(input$alignment_display_conservation) %||% TRUE
                )
              ),
              
              conditionalPanel(
                condition = "input.alignment_display_conservation",
                div(
                  style = "margin-bottom: 1rem;",
                  checkboxInput(
                    "alignment_color_graph",
                    "Color conservation bars (green >95%, orange >30%, red <30%)",
                    value = isolate(input$alignment_color_graph) %||% TRUE
                  )
                )
              ),
              
              tags$hr(),
              
              tags$h6(style = "color: #2C5F8D;", "Visual Settings"),
              
              div(
                style = "margin-bottom: 1rem;",
                checkboxInput(
                  "alignment_inverse_gaps",
                  "Inverse gap coloring (make gaps stand out)",
                  value = isolate(input$alignment_inverse_gaps) %||% FALSE
                )
              ),
              
              div(
                style = "margin-bottom: 1rem;",
                numericInput(
                  "alignment_size_factor",
                  "Font Size Factor",
                  value = isolate(input$alignment_size_factor) %||% 1,
                  min = 0.1,
                  max = 5,
                  step = 0.1,
                  width = "150px"
                )
              ),
              
              div(
                style = "margin-bottom: 0;",
                numericInput(
                  "alignment_margin",
                  "Left Margin",
                  value = isolate(input$alignment_margin) %||% 0,
                  min = -200,
                  max = 200,
                  step = 1,
                  width = "150px"
                ),
                div(class = "help-text",
                    "Spacing to next dataset")
              )
            )
          )
        )
      )
    )
  )
})

# ---- Alignment loaded flag ----
output$alignment_loaded <- reactive({
  !is.null(alignment_data())
})
outputOptions(output, "alignment_loaded", suspendWhenHidden = FALSE)

# ---- Alignment data reactive ----
alignment_data <- reactive({
  req(input$alignment_file, input$alignment_type)
  
  tryCatch({
    # Read FASTA file based on alignment type
    seqs <- switch(
      input$alignment_type,
      "aa" = readAAStringSet(input$alignment_file$datapath, format = "fasta"),
      "dna" = readDNAStringSet(input$alignment_file$datapath, format = "fasta")
    )
    
    # Get sequence names and validate
    seq_names <- names(seqs)
    seq_lengths <- width(seqs)
    
    # Check for alignment consistency
    if(length(unique(seq_lengths)) > 1) {
      showNotification(
        "Warning: Sequences have different lengths. This may not be a proper alignment.",
        type = "warning",
        duration = 7
      )
    }
    
    # Store alignment info
    return(list(
      sequences = seqs,
      names = seq_names,
      length = seq_lengths[1],
      n_seqs = length(seqs),
      raw_text = paste(readLines(input$alignment_file$datapath, warn = FALSE), collapse = "\n"),
      error = NULL
    ))
    
  }, error = function(e) {
    showNotification(
      paste("Error reading alignment file:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })
})

# ---- Generate alignment output ----
alignment_output <- reactive({
  req(alignment_data(), data(), input$id_col)
  
  aln <- alignment_data()
  df <- data()
  
  # Get settings
  alignment_dataset_label <- input$alignment_dataset_label %||% "alignment"
  alignment_start_pos <- input$alignment_start_pos %||% 1
  alignment_end_pos <- input$alignment_end_pos %||% 100
  
  # Color scheme
  alignment_color_scheme <- input$alignment_color_scheme %||% "clustal"
  
  # Advanced settings
  alignment_highlight_type <- input$alignment_highlight_type %||% "none"
  alignment_references <- input$alignment_references %||% ""
  alignment_mark_references <- input$alignment_mark_references %||% FALSE
  alignment_ref_box_border_width <- input$alignment_ref_box_border_width %||% 1
  alignment_ref_box_border_color <- input$alignment_ref_box_border_color %||% "#ff0000"
  alignment_ref_box_fill_color <- input$alignment_ref_box_fill_color %||% "#aaaaaa"
  alignment_highlight_disagreements <- input$alignment_highlight_disagreements %||% FALSE
  alignment_colored_dots <- input$alignment_colored_dots %||% TRUE
  alignment_display_consensus <- input$alignment_display_consensus %||% TRUE
  alignment_consensus_threshold <- input$alignment_consensus_threshold %||% 50
  alignment_ignore_gaps <- input$alignment_ignore_gaps %||% TRUE
  alignment_display_conservation <- input$alignment_display_conservation %||% TRUE
  alignment_color_graph <- input$alignment_color_graph %||% TRUE
  alignment_inverse_gaps <- input$alignment_inverse_gaps %||% FALSE
  alignment_size_factor <- input$alignment_size_factor %||% 1
  alignment_margin <- input$alignment_margin %||% 0
  
  # Build iTOL DATASET_ALIGNMENT format
  content <- c("DATASET_ALIGNMENT")
  content <- c(content, "SEPARATOR COMMA")
  content <- c(content, paste("DATASET_LABEL", alignment_dataset_label, sep = ","))
  content <- c(content, paste("COLOR", "#ff0000", sep = ","))
  content <- c(content, "")
  
  # Color scheme
  content <- c(content, paste("COLOR_SCHEME", alignment_color_scheme, sep = ","))
  content <- c(content, "")
  
  # Position range
  content <- c(content, paste("START_POSITION", alignment_start_pos, sep = ","))
  content <- c(content, paste("END_POSITION", alignment_end_pos, sep = ","))
  content <- c(content, "")
  
  # Highlighting options
  if(alignment_highlight_type != "none") {
    content <- c(content, paste("HIGHLIGHT_TYPE", alignment_highlight_type, sep = ","))
    
    if(alignment_highlight_type == "reference" && alignment_references != "") {
      ref_list <- trimws(unlist(strsplit(alignment_references, ",")))
      content <- c(content, paste("HIGHLIGHT_REFERENCES", paste(ref_list, collapse = ","), sep = ","))
      
      # Reference box styling
      if(alignment_mark_references) {
        content <- c(content, "MARK_REFERENCES,1")
        content <- c(content, paste("REFERENCE_BOX_BORDER_WIDTH", alignment_ref_box_border_width, sep = ","))
        content <- c(content, paste("REFERENCE_BOX_BORDER_COLOR", alignment_ref_box_border_color, sep = ","))
        content <- c(content, paste("REFERENCE_BOX_FILL_COLOR", alignment_ref_box_fill_color, sep = ","))
      }
    }
    
    if(alignment_highlight_disagreements) {
      content <- c(content, "HIGHLIGHT_DISAGREEMENTS,1")
    }
    
    if(alignment_colored_dots) {
      content <- c(content, "COLORED_DOTS,1")
    }
    
    content <- c(content, "")
  }
  
  # Consensus settings
  if(alignment_display_consensus) {
    content <- c(content, "DISPLAY_CONSENSUS,1")
    content <- c(content, paste("CONSENSUS_THRESHOLD", alignment_consensus_threshold, sep = ","))
    
    if(alignment_ignore_gaps) {
      content <- c(content, "IGNORE_GAPS,1")
    }
    
    content <- c(content, "")
  }
  

    # Conservation graph - DISPLAY_CONSERVATION, set to 0 or 1
    if(alignment_display_conservation) {
    content <- c(content, "DISPLAY_CONSERVATION,1")
    
    if(alignment_color_graph) {
        content <- c(content, "COLOR_GRAPH,1")
    }
    } else {
    content <- c(content, "DISPLAY_CONSERVATION,0")
    }
  
  # Display settings
  if(alignment_inverse_gaps) {
    content <- c(content, "INVERSE_GAPS,1")
  }
  
  content <- c(content, paste("SIZE_FACTOR", alignment_size_factor, sep = ","))
  content <- c(content, paste("MARGIN", alignment_margin, sep = ","))
  content <- c(content, "")
  
  # Add the alignment data
  content <- c(content, "DATA")
  content <- c(content, "")
  content <- c(content, aln$raw_text)
  
  return(paste(content, collapse = "\n"))
})

# ---- Alignment download card ----
output$alignment_download_card <- renderUI({
  req(alignment_output())
  
  card(
    card_header("Download Alignment Annotation"),
    card_body(
      centered_download_button(
        "download_alignment",
        "Download Alignment File"
      )
    )
  )
})

# Alignment download handler
output$download_alignment <- downloadHandler(
  filename = function() {
    paste0(input$dataset_label, "_alignment.txt")
  },
  content = function(file) {
    writeLines(alignment_output(), file)
  }
)