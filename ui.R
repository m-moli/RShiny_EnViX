#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


# Packages utilisés
library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(fresh)
library(shinyBS)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(ggrepel)


# Couleur de la thématique
main_theme = create_theme(
  adminlte_color(
    light_blue = "#8806ce",
    red = "violet"
  )
)


# Définir UI pour l'application
ui <- dashboardPage(
  
  # Entête de l'application
  header <- dashboardHeader(
    title = "EnViX",
    tags$li(
      class = "dropdown",
      actionLink("info_button", icon("info-circle"), label = NULL)
    )
  ),
  
  # Menu latéral
  dashboardSidebar(
    
    sidebarMenu(
      
      # Bouton d'accueil
      
      menuItem(
        "Accueil", 
        tabName = "Home", 
        icon = icon("home")
      ),
      
      # Bouton pour parcourir et déposer un fichier
      
      fileInput(
        inputId = "file1",
        label = HTML("<strong style='color: white ;'><i class='fa-regular fa-file'></i> Choisissez un fichier CSV :</strong>"),
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné",
        accept = ".csv",
        multiple = FALSE
      ),
      
      # Bouton pour le choix d'un organisme
      
      selectInput( 
        inputId = "select", 
        label = HTML("<strong style='color: white ;'><i class='fa-solid fa-dna'></i> Choisissez un organisme :</strong>"),
        list("Homo sapiens" = "1A", "Mus musculus" = "1B", "Arabidopsis thaliana" = "1C") 
      ), 
      
      # Caractéristique 1 ("Whole data inspection", par exemple)
      
      menuItem(
        "Inspection générale", 
        tabName = "feature1", 
        icon = icon("th")),
      
      # Caractéristique 2
      
      menuItem(
        "Caractéristique 2", 
        tabName = "feature2", 
        icon = icon("th")),
      
      # Caractéristique 3
      
      menuItem(
        "Caractéristique 3", 
        tabName = "feature3", 
        icon = icon("th")),
      
      # Caractéristique 4
      
      menuItem(
        "Caractéristique 4", 
        tabName = "feature4", 
        icon = icon("th"))
    )
  ),
  
  # Corps de l'application
  
  dashboardBody(
    
    use_theme(main_theme),
    
    # Bouton d'information (avec le package shinyBS), sans nécessité de passer par server.R
    
    bsModal("info_modal", "À propos", "info_button", size = "medium",
            p("Bienvenue sur cette app Shiny fabuleuse :)"),
            tags$ul(
              tags$li("Il s'agit du premier rendu, soyez indulgent.e.s !"),
              tags$li("Auteur: Miquel MOLI GONZALEZ"),
              tags$li("Contact: miquel.moli-gonzalez@univ-rouen.fr"),
              tags$li("Plus: plus d'informations seront ajoutées si nécessaire")
            )
    ),
    
    # Définir dans chaque caractéristique le contenu à afficher
    
    tabItems(
      
      # Caractéristique Acceuil, présentation du projet et explications des autres caractéristiques
      
      tabItem(tabName= "Home",
              tags$div(
                style = "display: flex; align-items: center;",
                tags$img(src = "logo.png", height = "50px", style = "margin-right: 15px;"),
                tags$h1("EnViX - Enrichment Visual eXploration")
              ),
              h3("Bienvenue sur EnViX, l'appli qui donne envie :)"),
              tags$ul(
                tags$li("L'objectif de ce projet est de construire une application Shiny pour des analyses en enrichissement fonctionnel."),
                tags$li("Ce projet s'inscrit dans le cadre du Master de Bioinformatique, Modélisation et Statistiques de l'Université de Rouen Normandie."),
                tags$li("Puisque les statistiques peuvent être un sujet polémique, notamment en bio, l'objectif de cette application est de rester transparent. 
                    Cet onglet pourra alors contenir les explications sur le principe des méthodes d'analyse en enrichissement fonctionnel (ORA), (GSEA), ainsi que tous les tests statistiques associés."),
              ),
              
              fluidRow(
                box(
                  title = "Tutoriel rapide", status = "danger", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Cette boîte pourrait contenir une vidéo qui fait le tour rapide des caractéristiques principales de l'application."),
                  width = 6
                ),
                
                box(
                  title = "Qu'est-ce que la Gene Set Enrichment Analysis (GSEA) ?", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Expliquer ici le principe de cette méthode et les tests statistiques associés."),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Qu'est-ce que la Over Representation Analysis (ORA) ?", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Expliquer ici le principe de cette méthode et les tests statistiques associés."),
                  width = 6
                ),
                
                box(
                  title = "Correction des p-values ?", status = "warning", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Les p-values... Mystérieuses, mais influencent l'interprétation du biologiste... Il faudra expliquer ici les différentes méthodes de correction utilisées dans l'application."),
                  width = 6
                )
              )
      ),

      tabItem(tabName = "feature1",
              
              h2("Inspection générale des données"),
              
              # Utilisation de sidebarLayout
              fluidPage(
                
                sidebarLayout(
          
                # Side Panel contenant les contrôles
                sidebarPanel(
                width = 3, # Largeur de la barre latérale
                  
                  # Sliders pour les paramètres |log2FC| et p-value
                  
                  h4("Choisissez les paramètres"),
                  
                  chooseSliderSkin(skin = "Shiny", color = "purple"),
      
                  sliderInput("log2FCslider", 
                              "seuil |log2FC|", 
                              min = 0, 
                              max = 5, 
                              value = 2.5, 
                              step = 0.5),
                  
                  sliderInput("pvalueslider", 
                              "seuil p-value", 
                              min = 0, 
                              max = 5, 
                              value = 2.5, 
                              step = 0.1),
                
                # Personnalisation affichage du volcanoplot
                
                h4("Personnalisez le Volcano Plot"),
                
                # Checkbox pour colorier les gènes 'significatifs'
                
                checkboxInput("color_by_significant_genes",
                              "Colorier les gènes significatifs",
                              TRUE),
            
                
                # Checkbox pour afficher les seuils (|log2FC| et adjpval) sur le VolcanoPlot
                
                checkboxInput("display_threshold_lines",
                              "Afficher les seuils choisis sur le graphe",
                              TRUE),
                
                # Checkbox pour afficher les labels des gènes significatifs
                
                checkboxInput("display_significant_labels",
                              "Afficher les étiquettes des gènes significatifs",
                              TRUE),
                
                actionButton(
                  inputId = "clear_selected_genes",
                  label = "Désélectionner les gènes du tableau",
                  icon = icon("times-circle"),  # quelle icône choisir ?
                  style = "margin-bottom: 10px; width: 100%;")
                
                ),
                
                # Main Panel pour les graphiques et tables
                
                mainPanel(
                  
                  # Volcano Plot
                  fluidRow(
                    box(
                      title = "Volcano Plot", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 16,
                      plotlyOutput("volcano_plot",
                                 width = "100%",
                                 height = "60vh"),
                      
                      # Bouton pour télécharger le volcanoplot
                      
                      downloadButton("download_volcano", "Télécharger le plot PDF"),
                    ),
                  ),
                  
    
                  # Table des gènes
                  
                  fluidRow(
                    box(
                      title = "Table", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 16,
                      DT::dataTableOutput("gene_table", height = "25vh"),
                      
                      # Bouton pour télécharger le volcanoplot
                      
                      downloadButton("download_genetable", "Télécharger le tableau")
                    )
                  )
                )
              )
            )
      ),
      
      # Caractéristique 2
      tabItem(tabName = "feature2",
              h2("Caractéristique 2")
      ),
      
      # Caractéristique 3
      tabItem(tabName = "feature3",
              h2("Caractéristique 3")
      ),
      
      # Caractéristique 4
      tabItem(tabName = "feature4",
              h2("Caractéristique 4")
      )
    )
  )
)
