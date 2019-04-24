# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# library(shinythemes)
# library(shiny)
# library(gsheet)
# library(tidyverse)
# library(ggplot2)


library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(gsheet)
library(DT)
library(shinydashboard)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "HUDS Dining"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Calendar", tabName = "calendar", icon = icon("th"))
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Controls",
                  selectInput("meal", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  dateInput("date", label = "Date Input", value = NULL, min = NULL, max = NULL,
                            format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                            language = "en", width = NULL)
                ),
                
                DTOutput("table")
                
              )),
      
      tabItem(tabName = "calendar",
              h2("Widgets tab content")
        )
      )
    )
      
      # Second tab content
      
    )




server <- function(input, output) {
  
  huds <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1S3vFDul1PeB84Zd8G5ewe1ocZ6ezidQzAp5gn1eiHtQ/edit#gid=0') %>%
    mutate(Date = as.Date(Date, "%m-%d-%Y")) %>%
    filter(Date >= "2019-01-25")
  
  output$table <- renderDT({
    huds %>% 
      filter(Date == input$date) %>%
      filter(Meal == input$meal) %>% 
      select(Food) 
  })
}

shinyApp(ui, server)


# 
# shinyApp(ui = ui, server = server)
# # 
# 

# 
# # Define UI for application that draws a histogram
# ui <- dashboardPage(
#    
#    # Application title
#    dashboardHeader(title = "HUDS"),
#    dashboardSidebar(
#      # There are a total of four main tabs, followed by a snapshot for a commodity. 
#      # I added interesting icons to match the topics of the tabs. 
#      sidebarMenu(
#        menuItem("Calendar", tabName = "dashboard", icon = icon("home")),
#        menuItem("Choose Meals", tabName = "graphs", icon = icon("align-left"))
#      )
#    ),
#    
#    
#    dashboardBody(
#      
#   tabItems(
#    
#    tabItem(tabName = "dashboard", selectInput("meal", "Meal:",
#                                               c("Breakfast",
#                                                 "Lunch",
#                                                 "Dinner")),
#            dateInput("date", label = "Date Input", value = NULL, min = NULL, max = NULL,
#                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
#                      language = "en", width = NULL), tableOutput("table")),
#           tabItem(tabName = "graphs", sliderInput("bins",
#                                            "Number of bins:",
#                                            min = 1,
#                                            max = 50,
#                                            value = 30))
#   )
# )
# )
#    
#            
#       
#    
# #    # Sidebar with a slider input for number of bins 
# #   tabsetPanel(
# #   tabPanel("Calendar", fluid = TRUE,
# #    sidebarLayout(
# #       sidebarPanel(
# #         
# #       ),
# #       
# #       
# #       # Show a plot of the generated distribution
# #       mainPanel(
# #          
# #       )
# #    )
# #   ),
# #   tabPanel("Choose Meals", fluid = TRUE,
# #            sidebarLayout(
# #              sidebarPanel(
# #                
# #              ),
# #              
# #              # Show a plot of the generated distribution
# #              mainPanel(
# #                tableOutput("distPlot")
# #              )
# #            )
# #   )
# # )
# # )
# # Define server logic required to draw a histogram

# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
