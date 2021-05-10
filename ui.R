##############################################################################
# ui.R
##############################################################################

library(shiny)
library(shinydashboard)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
        skin = "blue",
        dashboardHeader(title = "Movie Recommender"),
        
        dashboardSidebar(
            sidebarMenu(
                menuItem("MOVIES BY GENRE", tabName = "genres", icon = icon("ticket-alt")),
                menuItem("MOVIES BY RATING", tabName = "rating", icon = icon("grin-stars"))
            )
        ),
        
        dashboardBody(includeCSS("css/movies.css"),
                      tabItems(
                          # TAB 1: RECOMMENDATIONS BY GENRE
                          tabItem(tabName = "genres",
                                  fluidRow(
                                      box(width = 12, title = "Step 1: Select Genre", status = "info", 
                                          solidHeader = TRUE, collapsible = FALSE,
                                          selectInput(inputId = "dataset",
                                                      label = "Select Genre",
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
                                              actionButton("btn1", "GO", class = "btn-secondary", icon = icon("search"))
                                          ),
                                          br(),
                                          tableOutput("results1")
                                      )
                                  )
                          ),
                          
                          # TAB 2: RECOMMENDATIONS BY RATINGS
                          tabItem(tabName = "rating",
                                  fluidRow(
                                      box(width = 12, title = "Step 1: Rate Movies", status = "info", solidHeader = TRUE,
                                          collapsible = TRUE,
                                          div(class = "rateitems",
                                              uiOutput('ratings')
                                          )
                                      )
                                  ),
                                  fluidRow(
                                      useShinyjs(),
                                      box(
                                          width = 12, status = "info", solidHeader = TRUE,
                                          title = "Step 2: Get Movies Recommended For You",
                                          br(),
                                          withBusyIndicatorUI(
                                              actionButton("btn2", "GO", class = "btn-primary", icon = icon("search"))
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