## ui.R

library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(
              sidebarMenu(
                menuItem("Recommend by Genre", tabName = "genres", icon = icon("film")),
                menuItem("Recommend by Rating", tabName = "rating", icon = icon("star"))
              )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                # First tab content
                tabItem(tabName = "genres",
                  fluidRow(
                      box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                          selectInput(inputId = "dataset",
                                      label = "Choose a genre",
                                      choices = c("Action", "Adventure", "Animation", 
                                                     "Children's", "Comedy", "Crime",
                                                     "Documentary", "Drama", "Fantasy",
                                                     "Film-Noir", "Horror", "Musical", 
                                                     "Mystery", "Romance", "Sci-Fi", 
                                                     "Thriller", "War", "Western")
                          ),
                      )
                  ),
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                          actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results1")
                      )
                  )
                ),
                
                # Second tab content
                tabItem(tabName = "rating",
                  fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible (at least 10 is recommended)", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                  fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results2")
                    )
                  )
                )
                
                
              )
          )
    )
) 