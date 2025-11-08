
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
# Usage: l'application peut être exécutée en appuyant sur le boutton 'Run App' en haut de l'éditeur. Un README et un dépôt github seront créés pour le prochain rendu.
########################################

library(shiny)
library(DT)


# Définir la logique du server

# Ce module permet à l'utilisateur de télécharger un jeu de données mtcars au format .CSV (dataset R intégré)
# Ce jeu de données est donné à titre d'exemple, et sera remplacé dans un second temps par les résultats de l'analyse en enrichissement obtenus

server <- function(input, output) {
  
  inputData <- reactive({
    inputFile <- input$file1
    if (is.null(inputFile)) {
      return(NULL)
    }
    read.csv(inputFile$datapath, header = TRUE, sep=';')
  })
  
  output$gene_table <- DT::renderDataTable({
    DT::datatable(
      inputData(),
      options = list(scrollX = TRUE,
                     scrollY = "250px"),
      width = "100%"
    )
  })

  output$volcano_plot <- renderPlotly({
    
    data <- inputData()
    
    yvals <- -log10(data$padj)
    sig_up <- data$log2FC >= input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
    sig_down <- data$log2FC <= -input$log2FCslider & data$padj <= 10^(-input$pvalueslider)
    
    # Récupérer les labels des gènes significatifs pour l'affichage du volcano plot
    
    significant_labels <- if (input$display_significant_labels) {
      ifelse(sig_up | sig_down, data$GeneName, "")
    } else {
      rep("", nrow(data))
    }
    
    # Définir les couleurs des gènes significatifs
    colors <- rep("grey", nrow(data))
    colors[sig_up] <- "violet"
    colors[sig_down] <- "pink"
    
    plot_ly(
      x = ~data$log2FC,
      y = ~yvals,
      type = "scatter",
      mode = "markers+text",
      marker = list(color = if (input$color_by_significant_genes) colors else "grey", size = 10),
      text = significant_labels,
      hovertext = data$GeneName,
      textposition = "top center",
      hoverinfo = "text+x+y"
    ) %>%
      layout(
        title = "Volcano Plot",
        xaxis = list(title = "log2(Fold Change)"),
        yaxis = list(title = "-log10(padj)"),
        shapes = if (input$display_threshold_lines) {
          
          list(
            # Lignes verticales
            list(type = "line", 
                 x0 = -input$log2FCslider, 
                 x1 = -input$log2FCslider,
                 y0 = 0, y1 = max(yvals, na.rm = TRUE), 
                 line = list(dash = "dash", color = "purple")),
            
            list(type = "line", 
                 x0 = input$log2FCslider, 
                 x1 = input$log2FCslider,
                 y0 = 0, 
                 y1 = max(yvals, na.rm = TRUE), 
                 line = list(dash = "dash", color = "purple")),
            
            # Ligne horizontale
            list(type = "line", 
                 x0 = min(data$log2FC, na.rm = TRUE), 
                 x1 = max(data$log2FC, na.rm = TRUE),
                 y0 = input$pvalueslider, 
                 y1 = input$pvalueslider,
                 line = list(dash = "dash", color = "purple"))
          )
        }
      )
    })

  # -- Téléchargements --
  
  output$downloadData <- downloadHandler(
    filename = "example.csv",
    content = function(file) {
      write.csv(mtcars, file)
    }
  )
  
  output$download_volcano <- downloadHandler(
    filename = function() {
      paste0("volcano-plot", ".pdf")
    },
    
    content = function(file) {
      ggplot2::ggsave(file, reactive_volcano(), device = "pdf", width = 10, height = 5, units = "in")
    }
  )
}