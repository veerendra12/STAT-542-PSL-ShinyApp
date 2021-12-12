## ui.R
library(shiny)
library(shinydashboard)
#vesingav library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(skin = "blue",
                  # Movie app title header
                  dashboardHeader(title = "Movie Recommender"),

                  # Side bar with two menu items - By Genre and By Rating
                  dashboardSidebar(
                    sidebarMenu(
                      id = "tabs",
                      menuItem("Recommend by Genre", tabName = "rec_genre", icon = icon("film")),
                      menuItem("Recommend by Rating", tabName = "rec_rate", icon = icon("star"))
                      )
                    ),
                  
                  dashboardBody(includeCSS("css/movies.css"),
                                tabItems(
                                  # recommender by genre
                                  tabItem(tabName = 'rec_genre',
                                          fluidRow(
                                            box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                                selectInput("user_genre", 'Choose a genre', genre_list)
                                                )
                                            ),
                                          fluidRow(
                                            useShinyjs(),
                                            box(
                                              width = 12, status = "info", solidHeader = TRUE,
                                              title = "Step 2: Discover movies you might like",
                                              br(),
                                              withBusyIndicatorUI(
                                                actionButton("genrebtn", "Click here to get your recommendations", class = "btn-warning")
                                                ),
                                              br(),
                                              tableOutput("rec_genre_results")
                                              )
                                            )
                                          ),
                                  # recommender by user ratings
                                  tabItem(tabName = 'rec_rate',
                                          fluidRow(
                                            box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                                div(id = 'user_rating',
                                                    class = "rateitems",
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
                                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                                                ),
                                              br(),
                                              tableOutput("results")
                                              )
                                            )
                                          )
                                  )
                                )
                  )
    ) 