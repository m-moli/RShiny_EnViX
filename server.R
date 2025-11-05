
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
  
  # -- Volcano plot --
  
  volcano_plot <- reactive({
    data <- inputData()
    if (is.null(data)) return(NULL)
    
    ggplot(data, aes(x = data$log2FC, y = -log10(data$padj))) +
      geom_point(alpha = 0.6, color = "grey") +
      geom_vline(xintercept = c(-input$log2FCslider, input$log2FCslider), 
                 linetype = "dashed", color = "purple") +
      geom_hline(yintercept = input$pvalueslider, 
                 linetype = "dashed", color = "purple") +
      labs(
        x = "log2(Fold Change)",
        y = "-log10(padj)",
        title = "Volcano Plot"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$volcano_plot <- renderPlot({
    volcano_plot()
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
      paste0("volcano-plot-", Sys.Date(), ".pdf")
    },
    
    content = function(file) {
      ggsave(file, reactive_volcano(), device = "pdf", width = 10, height = 5, units = "in")
    }
  )
}