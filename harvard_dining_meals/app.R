#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gsheet)
library(tidyverse)
library(ggplot2)
library(shinythemes)


huds <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1S3vFDul1PeB84Zd8G5ewe1ocZ6ezidQzAp5gn1eiHtQ/edit#gid=0') %>% 
  mutate(Date = as.Date(Date, "%m-%d-%Y")) %>% 
  filter(Date >= "2019-01-25")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Harvard University Dining Services Search Tool"),
   
   # Sidebar with a slider input for number of bins 
  tabsetPanel(
  tabPanel("Calendar", fluid = TRUE,
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         dateInput(inputId, label, value = NULL, min = NULL, max = NULL,
                   format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                   language = "en", width = NULL)
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
  ),
  tabPanel("Choose Meals", fluid = TRUE,
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot")
             )
           )
  )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

