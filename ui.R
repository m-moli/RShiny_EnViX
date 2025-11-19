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
# Usage: l'application peut être exécutée en appuyant sur le boutton 'Run App' en haut de l'éditeur. 
# Plus d'informations sur https://github.com/m-moli/RShiny_EnViX.git
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

############################################
# ====== Couleur de la thématique ======== #
############################################

main_theme = create_theme(
  adminlte_color(
    light_blue = "#8806ce",
    red = "#d473d4"
  )
)

###############################################
# ====== Définir UI pour l'application ====== #
###############################################

ui <- dashboardPage(
  
  #################################
  # -- Entête de l'application -- #
  #################################
  
  header <- dashboardHeader(
    title = "EnViX",
    tags$li(
      class = "dropdown",
      actionLink("info_button", 
                 icon("info-circle"), 
                 label = NULL)
    )
  ),
  
  #######################
  # --  Menu latéral -- #
  #######################
  
  dashboardSidebar(
    
    sidebarMenu(
      
      # -- Bouton d'accueil -- #
      
      menuItem(
        "Accueil", 
        tabName = "Home", 
        icon = icon("home")
      ),
      
      # -- Bouton pour parcourir et déposer un fichier -- #
      
      fileInput(
        inputId = "inputGeneFile",
        label = HTML("<strong style='color: white ;'><i class='fa-regular fa-file'></i> Choisissez un fichier CSV :</strong>"),
        buttonLabel = "Parcourir...",
        placeholder = "Aucun fichier sélectionné",
        accept = ".csv",
        multiple = FALSE
      ),
      
      # -- Bouton pour le choix d'un organisme -- #
      
      selectInput( 
        inputId = "select", 
        label = HTML("<strong style='color: white ;'><i class='fa-solid fa-dna'></i> Choisissez un organisme :</strong>"),
        list("Homo sapiens" = "1A", "Mus musculus" = "1B", "Arabidopsis thaliana" = "1C") 
      ), 
      
      # -- Caractéristique 1 : Inspection générale des données -- #
      
      menuItem(
        "Inspection générale", 
        tabName = "feature1", 
        icon = icon("eye")),
      
      # -- Caractéristique 2 : Enrichissement termes GO -- #
      
      menuItem(
        "Termes GO", 
        tabName = "go_term_enrichment_feat", 
        icon = icon("sitemap")),
      
      # -- Caractéristique 3 : Enrichissement voies KEGG -- #
      
      menuItem(
        "Voies KEGG", 
        tabName = "kegg_pathway_enrichment_feat", 
        icon = icon("pie-chart")),
      
      # -- Caractéristique 4 : à définir -- #
      
      menuItem(
        "Caractéristique 4", 
        tabName = "feature4", 
        icon = icon("th"))
    )
  ),
  
  ################################
  # -- Corps de l'application -- #
  ################################ 
  
  dashboardBody(
    
    use_theme(main_theme),
    
    # -- Bouton d'information sur l'application (avec le package shinyBS) -- #
    # -- sans nécessité de passer par server.R                            -- #
    
    bsModal("info_modal", "À propos", "info_button", size = "medium",
            p("Bienvenue sur cette app Shiny fabuleuse :)"),
            tags$ul(
              tags$li("Il s'agit du premier rendu, soyez indulgent.e.s !"),
              tags$li("Auteur: Miquel MOLI GONZALEZ"),
              tags$li("Contact: miquel.moli-gonzalez@univ-rouen.fr"),
              tags$li("Plus: plus d'informations seront ajoutées si nécessaire")
            )
    ),
    
    # -- Définir dans chaque caractéristique le contenu à afficher -- # 
    
    tabItems(
      
      # -- Caractéristique Acceuil, présentation du projet -- #
      # -- explication des fonctionalités de l'application -- #
      
      tabItem(tabName= "Home",
              tags$div(
                style = "display: flex; align-items: center;",
                tags$img(src = "logo.png", height = "50px", style = "margin-right: 15px;"),
                tags$h1("EnViX - Enrichment Visual eXploration")
              ),
              h3("Bienvenue sur EnViX, l'appli qui donne envie :)"),
              tags$ul(
                tags$li("L'objectif de ce projet est de construire une application
                        Shiny pour des analyses en enrichissement fonctionnel."),
                tags$li("Ce projet s'inscrit dans le cadre du Master de Bioinformatique, 
                        Modélisation et Statistiques de l'Université de Rouen Normandie."),
                tags$li("Puisque les statistiques peuvent être un sujet polémique, 
                        notamment en bio, l'objectif de cette application est de rester transparent. 
                        Cet onglet pourra alors contenir les explications sur le principe des
                        méthodes d'analyse en enrichissement fonctionnel (ORA), 
                        (GSEA), ainsi que tous les tests statistiques associés."),
              ),
              
              fluidRow(
                box(
                  title = "Tutoriel rapide", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Cette boîte pourrait contenir une vidéo qui fait le tour 
                    rapide des caractéristiques principales de l'application."),
                  width = 6
                ),
                
                box(
                  title = "Qu'est-ce que la Gene Set Enrichment Analysis (GSEA) ?", 
                  status = "primary", 
                  solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Expliquer ici le principe de cette méthode et les tests statistiques associés."),
                  width = 6
                )
              ),
              
              fluidRow(
                box(
                  title = "Qu'est-ce que la Over Representation Analysis (ORA) ?", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Expliquer ici le principe de cette méthode et les tests statistiques associés."),
                  width = 6
                ),
                
                box(
                  title = "Correction des p-values ?", status = "primary", solidHeader = TRUE,
                  collapsible = FALSE,
                  p("Les p-values... Mystérieuses, mais influencent l'interprétation du biologiste... 
                    Il faudra expliquer ici les différentes méthodes de correction utilisées dans l'application."),
                  width = 6
                )
              )
      ),
      
      # -- Caractéristique 1 : inspection générale des données -------------- #
      # -- affichage d'un volcanoplot et d'un tableau du fichier csv input -- #
      
      tabItem(tabName = "feature1",
              
              h2("Inspection générale des données"),
              
              # Utilisation de sidebarLayout pour avoir
              #les paramètre du volcanoplot à gauche et le volcanoplot à droite
              
              fluidPage(
                
                sidebarLayout(
          
                # Side Panel contenant les contrôles (paramètres) du volcanoplot
                  
                sidebarPanel(
                  
                  width = 3, # Largeur de la barre latérale
                  
                  # Sliders pour les paramètres |log2FC| et adjusted p-value 
                  
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
                
                div(style = "border: 2px solid purple; border-radius: 8px; padding: 10px; background-color: #f8f0ff; margin-bottom: 10px;",
                    h4("Personnalisez le Volcano Plot"),
                  
                  # Checkbox pour afficher les seuils (|log2FC| et adjpval) sur le VolcanoPlot
                  
                  checkboxInput("display_threshold_lines",
                                "Afficher les seuils choisis",
                                TRUE),
                  
                  # Checkbox pour colorier les gènes 'significatifs'
                  
                  checkboxInput("color_by_significant_genes",
                                "Colorier les gènes significatifs / sélectionnés",
                                TRUE),
                  
                  # Checkbox pour afficher les labels (noms) des gènes significatifs
                  
                  checkboxInput("display_significant_labels",
                                "Afficher les étiquettes des gènes significatifs",
                                FALSE),
                ),
                
                # Bouton d'action pour éliminer les gènes sélectionnés à partir du tableau
                
                actionButton(
                  inputId = "clear_selected_genes",
                  label = "Désélectionner les gènes du tableau",
                  icon = icon("times-circle"),  
                  style = "margin-bottom: 10px; width: 100%;"),
                
                # Zone de texte pour écrire le titre du volcanoplot
                
                textInput(
                  inputId = "volcano_title",
                  label = "Titre du Volcano Plot",
                  placeholder="Saisissez un titre...")

                ), 
                
                # Main Panel pour les graphiques et tables
                
                mainPanel(
                  
                  width = 9,
                  
                  # Volcano Plot
                  
                  fluidRow(
                    box(
                      title = "Volcano Plot", status = "primary", solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 16,
                      plotlyOutput("volcano_plot",
                                 width = "100%",
                                 height = "60vh"),
                      
                    ),
                  ),
                  )
                ),
                
                # Table des gènes
                # un onglet contenant tous les gènes (fichier csv input)
                # un onglet contenant les gènes significatifs (seuils choisis pour le volcanoplot)
                # un onglet contenant les gènes sélectionnés du premier onglet (sous-liste)
                
                fluidRow(
                  box(
                    title = "Table des gènes", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 16,
                    
                    div(
                      style = "display: flex; flex-direction: column; height: 40vh;", # hauteur totale du contenu
                      div(
                        tags$style(HTML("
                          table.dataTable tbody tr:nth-child(odd) {
                            background-color: #f3e8ff !important; # Encore un peu plus de violet pour les lignes impaires ;)
                          }
                          table.dataTable 
                        ")),
                        
                        style = "flex: 1; overflow-y: auto;",
                        tabsetPanel(
                          id = "gene_table_tabs", 
                          tabPanel("Tous les gènes", DT::dataTableOutput("gene_table_all", height = "25vh")),
                          tabPanel("Gènes significatifs", DT::dataTableOutput("gene_table_sig", height = "25vh")),
                          tabPanel("Gènes sélectionnés", DT::dataTableOutput("gene_table_selected", height = "25vh"))
                        )
                      ),
                      div(
                        style = "margin-top: auto; text-align: left;",
                        downloadButton("download_gene_table", "Télécharger le tableau (csv)")
                      )
                    
                    ) 
                  )
                )
              )
      ),
      
      # -- Caractéristique 2 -- #
      
      tabItem(
        tabName = "go_term_enrichment_feat",
        h2("Enrichissement termes GO")
      ),
      
      # -- Caractéristique 3 -- #
      
      tabItem(tabName = "kegg_pathway_enrichment_feat",
              # icon = icon("pie-chart", lib = "fa"),
              h2("Enrichissement voies KEGG")
      ),
      
      # -- Caractéristique 4 -- #
      
      tabItem(tabName = "feature4",
              h2("Caractéristique 4")
      )
    )
  )
)
