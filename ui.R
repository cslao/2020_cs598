#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Template adapted from https://github.com/pspachtholz/BookRecommender/blob/master/ui.R
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
        
        dashboardSidebar(disable = TRUE),
        
        dashboardBody(includeCSS("css/movies.css"),
                      fluidRow(
                          column(6, radioButtons("recommendationType", "Recommend by: ",
                                       list("Genre", "Ratings"), inline=TRUE))
                      ),
                      fluidRow(
                          box(width = 12, title = textOutput("box1Title"), 
                              status = "info", solidHeader = TRUE, collapsible = TRUE,
                              div(class = "step1",
                                  uiOutput('step1')
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
                              textOutput("message"),
                              tableOutput("results")
                          )
                      )
        )
    )
) 
