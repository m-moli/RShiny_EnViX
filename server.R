
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

########################################
# Project: application R Shiny pour des analyses en enrichissement fonctionnel - UE5 Analyses bioinformatiques -3 
# Author: Miquel MOLI GONZALEZ
# Affiliation: master 2.1 BIMS, Université de Rouen Normandie
# Date: 09-10-2025
# Description: premier rendu - EnViX - Enrichment Visual eXploration
# Usage: l'application peut être exécutée en appuyant sur le boutton 'Run App' en haut de l'éditeur. 
# Plus d'informations sur https://github.com/m-moli/RShiny_EnViX.git
########################################


############################################
# ========= Logique du serveur =========== #
############################################

server <- function(input, output) {

  # ---- Upload le fihcier CSV avec les obnnes colonnes ---- #
  # ---- Cette fonction s'assure que le fichier est au ----- #
  # ---- bon format et contient les bonnes colonnes. ------- #
  # ---- Pour l'instant, les 6 colonnes du fichier donné --- #
  # ---- en exemple sont nécessaires.  --------------------- #
   
  inputData <- reactive({
    
    inputFile <- input$inputGeneFile
    if (is.null(inputFile)) return(NULL)
    
    # Vérifier extension .csv 
    
    if (tools::file_ext(inputFile$name) != "csv") {
      showNotification("Le fichier doit être au format .csv", type = "error")
      return(NULL)
    }
    
    # Lecture du fichier
    
    df <- read.csv(inputFile$datapath, header = TRUE, sep = ';')
    
    # Colonnes obligatoires
    
    required_columns <- c("GeneName", "ID", "baseMean", "log2FC", "pval", "padj")
    
    # Vérification des colonnes
    
    if (!all(required_columns %in% colnames(df))) {
      missing_columns <- setdiff(required_columns, colnames(df))
      showNotification(
        paste("Colonnes manquantes :", paste(missing_columns, collapse = ", ")),
        type = "error"
      )
      return(NULL)
    }
    
    return(df)
  })
  
  # --- Tableau d'entrée contenant tous les gènes --- #
  
  output$gene_table_all <- DT::renderDataTable({
    
    DT::datatable(
      
      extensions = "Buttons",
      inputData(),
      options = list(scrollX = TRUE,
                     scrollY = "250px"),
      selection = "multiple",
      width = "100%"
    )
  })
  
  # ---- Variable réactive pour les gènes sélectionnés ----- #
  # ---- Elle permettra de stocker les gènes sélectionnés  - #
  # ---- à partir du tableau  ------------------------------ #
  
  selected_genes <- reactiveVal(character(0))
  
  # ---- Mettre à jour quand la variable selected_genes ---- #
  # ---- quand l'utilisateur sélectionne dans le tableau --- #
  
  observeEvent(
    input$gene_table_all_rows_selected, { # gene_table_all_rows_selected est automatiquement créée par DT
      data <- inputData()
      selected_genes(data$GeneName[input$gene_table_all_rows_selected])
    }
  )
  
  # ---- Bouton pour désélectionner à la fois tous les gènes ---- #
  # ---- évite de devoir déselectionner un par un les gènes  ---- #
  
  observeEvent(
    input$clear_selected_genes, {
      DT::selectRows(DT::dataTableProxy("gene_table_all"), NULL)  # désélectionner les lignes du tableau de gènes
      selected_genes(character(0))                            # vider la sélection (variable selected_genes)
    }
  )
  
  # ---- Tableau contenant uniquement les gènes significatifs --- #
  # ---- Permet à l'utilisateur de regrouper les gènes ---------- #
  # ---- significatifs en un seul tableau (téléchargeable) ------ #
  
  output$gene_table_sig <- DT::renderDataTable({
    
    data <- inputData()
    if (is.null(data)) return(NULL)
    
    # Filtrer les gènes significatifs
    
    sig_genes <- abs(data$log2FC) >= input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
    
    # Créer le tableau des gènes significatifs
    
    DT::datatable(
      data[sig_genes, , drop = FALSE],
      options = list(scrollX = TRUE, scrollY = "250px"),
      selection = "multiple",
      width = "100%"
    )
  })
  
  # ---- Tableau contenant uniquement les gènes sélectionnés par l'utilisateur ---- #
  # ---- à partir du tableau contenant tous les gènes ----------------------------- #
  
  output$gene_table_selected <- DT::renderDataTable({
    
    data <- inputData()
    if (is.null(data)) return(NULL)

    # Créer le tableau des gènes sélectionnés
    
    DT::datatable(
      data[data$GeneName %in% selected_genes(), , drop = FALSE], # selected_genes a déjà été créée auparavant
      options = list(scrollX = TRUE, scrollY = "250px"),
      selection = "multiple",
      width = "100%"
    )
  })
  
  # ---- Tracer le Volcano Plot interactif ---- #

  output$volcano_plot <- renderPlotly({
    
    data <- inputData()
    if (is.null(data)) return(NULL)
    
    yvals <- -log10(data$padj)
    sig_up <- data$log2FC >= input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
    sig_down <- data$log2FC <= -input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
    
    # Couleurs et labels
    
    colors <- rep("grey", nrow(data)) # les gènes non significatifs seront gris
    colors[sig_up] <- "darkviolet"
    colors[sig_down] <- "violet"
    
    significant_labels <- rep("", nrow(data)) # n'afficher que les noms des gènes significatifs
    if (input$display_significant_labels) {
      significant_labels[sig_up | sig_down] <- data$GeneName[sig_up | sig_down]
    }
    
    # Gènes sélectionnés dans le tableau
    
    sel_idx <- which(data$GeneName %in% selected_genes())
    if (length(sel_idx) > 0) {
      colors[sel_idx] <- "red"
      significant_labels[sel_idx] <- data$GeneName[sel_idx]
    }
    
    # Tracer le volcanoPlot
    # Il est automatiquement téléchargeable avec plotly au format PNG
    # (Pas besoin d'un bouton d'action)
    
    plot_ly(
      x = ~data$log2FC,
      y = ~yvals,
      type = "scatter",
      mode = "markers+text",
      marker = list(color = if (input$color_by_significant_genes) colors else "grey", size = 10),
      text = significant_labels,
      hoverinfo = "text",
      hovertext = paste0( # affichage du gène quand le curseur passe sur un point du volcanoplot
        "Gène: ", data$GeneName, "<br>",
        "Log2FC: ", data$log2FC, "<br>",
        "padj: ", data$padj
      ),
      textposition = "top center") %>%
      layout(
        title = input$volcano_title,
        xaxis = list(title = "log2(Fold Change)"),
        yaxis = list(title = "-log10(padj)"),
        shapes = if (input$display_threshold_lines) { # affichage intéractif des seuils choisis pour log2FC et adjpval 
          list(
            list(type = "line", x0 = -input$log2FCslider, x1 = -input$log2FCslider,
                 y0 = 0, y1 = max(yvals, na.rm = TRUE), line = list(dash = "dash", color = "purple")),
            list(type = "line", x0 = input$log2FCslider, x1 = input$log2FCslider,
                 y0 = 0, y1 = max(yvals, na.rm = TRUE), line = list(dash = "dash", color = "purple")),
            list(type = "line", x0 = min(data$log2FC, na.rm = TRUE), x1 = max(data$log2FC, na.rm = TRUE),
                 y0 = input$pvalueslider, y1 = input$pvalueslider, line = list(dash = "dash", color = "purple"))
          )
        }
      )
  })

  # ---- Téléchargements ---- #
  
  # Télécharger le(s) tableau(x) de gènes
  
  output$download_gene_table <- downloadHandler(
    
    filename = function() {
      paste0("table_des_genes", ".csv")
    },
    content = function(file) {
      
      # Vérifie quel onglet du tableau est actif pour le télécharger
      
      active_tab <- input$gene_table_tabs
      
      # Choisir l'onglet correspondant
      
      if (active_tab == "Tous les gènes") {
        data_to_download <- inputData()
      } else if (active_tab == "Gènes significatifs") {
        data <- inputData()
        sig_genes <- abs(data$log2FC) >= input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
        data_to_download <- data[sig_genes, , drop = FALSE]
      } else if (active_tab == "Gènes sélectionnés") {
        data <- inputData()
        data_to_download <- data[data$GeneName %in% selected_genes(), drop = FALSE]
      }
      
      write.csv(data_to_download, file, row.names = FALSE)
      
    }
  )
}