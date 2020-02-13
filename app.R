#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
data("iris")

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("Ma Super App",
        
        # Onglet histogramme
        tabPanel("Histo",
                 # Titre page
                 titlePanel("Histogramme Iris"),
                 
                 # Sidebar
                 sidebarLayout(
                     sidebarPanel(
                         h3("Histogramme avec choix de variable"),
                         selectInput("variable", label = "Choisir la variable à afficher pour l'histogramme", 
                                     choices = c("longueur sépales" = 1, "largeur sépales" = 2, "longueur pétales" = 3, "largeur pétales" = 4), 
                                     selected = 1),
                 ),
                 # Affichage du graphique
                 mainPanel(
                     textOutput("monTexte"),
                     plotOutput("histPlot")

                 )
                )
        ),
        
        # Onglet Affichage graphique interacti X et Y      
        tabPanel("Plot",
                 
            # Titre page
            titlePanel("Graphique interactif Iris"),
        
            sidebarLayout(
                sidebarPanel(
                    
                    h3("Plot Iris avec choix X et Y"),
                    selectInput('plotX', label = "choisir axe X plot:", 
                                choices = c("longueur sépales" = 1, "largeur sépales" = 2, "longueur pétales" = 3, "largeur pétales" = 4), 
                                selected = 1),
                    selectInput('plotY', label = "choisir axe Y plot:", 
                                choices = c("longueur sépales" = 1, "largeur sépales" = 2, "longueur pétales" = 3, "largeur pétales" = 4),
                                selected = 2),
                    
                    h3("Filtre"),
                    selectInput("filterVar", label = "Choisir la variable sur laquelle vous voulez filtrer le dataframe", 
                                choices = c("longueur sépales" = "Sepal.Length", "largeur sépales" = "Sepal.Width", "longueur pétales" = "Petal.Length", "largeur pétales" = "Petal.Width"), 
                                selected = "Sepal.Length"),
                    sliderInput("rangeVar", label = h3("Filtre"), min = 0, 
                                max = 10, 
                                value = c(0, 10)),
                    actionButton("action", label = "Afficher le graphique"),
            
                ),
        
                # Affichage du graph
                mainPanel(
                   plotOutput("irisPlot")
                )
            )
            ),
        
        #Onglet Affichage des données
        tabPanel("Dataset",
  
                     selectInput("filterDataset", label = "Choisir la variable sur laquelle vous voulez filtrer le dataframe", 
                     choices = c("tout","setosa", "versicolor", "virginica"), 
                     selected = "tout"),
                     h2("Le dataset Iris"),
                     tableOutput("dataset")
                         
                ),


        # Onglet moyenne
        tabPanel("Résumé moyenne",
                 
                 # Titre page
                 titlePanel("Calcul moyenne"),
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("moyVar", label = "Choisir la variable à afficher pour le clacul de la moyenne:", 
                                     choices = c("longueur sépales" = 1, "largeur sépales" = 2, "longueur pétales" = 3, "largeur pétales" = 4), 
                                     selected = 1),
                     ),
                     mainPanel(
                         h3("Moyenne:"),
                         textOutput("moyenne")
                         )
                         
                     )
                 )
    )
)

# Le serveur
server <- function(input, output) {
    
    # On récupère le nom des colonnes
    cols = colnames(iris)
    
    output$monTexte <- renderText({
        paste("Vous avez choisi", cols[as.numeric(input$variable)])
    })
    
    output$histPlot <- renderPlot({
        hist(iris[,as.numeric(input$variable)], 
             xlab = cols[as.numeric(input$variable)], 
             main = "Histogramme Iris", 
             col = "green")
    })
    

    # On fait notre affichage dans un eventReactive bouton de sorte que l'affichage ne se crée qu'à la demande de l'utilisateur
    valuesPlot<- eventReactive(input$action, {
        #On applique le filtre (en base R pour ne pas avoir de problème)
        iris <- iris[(iris[[input$filterVar]] >= input$rangeVar[1]) & (iris[[input$filterVar]] <= input$rangeVar[2]),]
        plot(x = iris[[as.numeric(input$plotX)]], y = iris[[as.numeric(input$plotY)]],
             col = iris$Species,
             xlab = cols[as.numeric(input$plotX)],
             ylab =  cols[as.numeric(input$plotY)])
    })

    # Et on affiche le graphe
    output$irisPlot <- renderPlot({
        # Penser à rajouter des parenthèses à la variable comme pour une fonction
        valuesPlot()
    })
    
    # Pour permettre le rendu de nos données library DT
    output$dataset = renderTable({
        if (input$filterDataset == "tout") {
            iris <- iris
        } else {
            iris <- iris[iris$Species == input$filterDataset,]
        }
    })
    
    # output$irisTable <- renderDataTable({
    #     iris
    # })
    
    # Calcul de la moyenne de chaque variable
    output$moyenne <- renderText({
        paste("La moyenne de la variable", cols[as.numeric(input$moyVar)] ,"est", mean(iris[[as.numeric(input$moyVar)]]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
