# #
# # This is a Shiny web application. You can run the application by clicking
# # the 'Run App' button above.
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/


library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(gsheet)
library(DT)
library(lubridate)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Harvard Dining"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("Common Meals of the Semester", tabName = "mealselect", icon = icon("school")),
      menuItem("Popular Meals by Weekday", tabName = "weekdayselect", icon = icon("clock")),
      menuItem("Meat Dishes", tabName = "meatselect", icon = icon("utensils"))
      
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              h1("Welcome to the Harvard University Dining Services (HUDS) Database"),
              h2("Summary"),
              h3("This dashboard serves a record keeper for Harvard's Undergraduate dining hall menus starting from
                the beginning of the Spring 2019 semester. You have the option to see past menus, todays menu and future menus on our calendar tab.
                Additional tabs include some cool trends and graphs that we have discovered from some close analysis of the menus."),
              h2("Background/Methodology"),
              h3("Some students at Harvard have mixed feelings about the dining hall system. Each dining hall, Annenberg (Freshman dining) and the upperclassemen houses offer
              the same menu options per day, with a few exceptions. While some students believe that HUDS provides a variety of options throughout the week, others complain of repitition."),
              h3("The objective of this investigation is to explore trends in the HUDS menu data while also serving as a record keeper for meals offered throughout. With the current 
              system, HUDS provides no archiving feature for meal options from previous days."),
              h3("In order to gain access to HUDS menu's, I simply needed a script that would scrape HUD's", a("website", href = "http://www.foodpro.huds.harvard.edu/foodpro/menu_items.asp?type=30&meal=1"), 
                 "each day. Harvard Open Data Project (HODP), a student organization on campus, has already developed", 
                 a("a scraper", href = "http://hodp.org/catalog/index.html?q=huds"), "that has collected information 
                since the beginning of the Spring 2019 semester.")
              ),
      
      tabItem(tabName = "calendar",
              h2("Harvard Dining Archive Calendar"),
              h4("Starting from the beginning of the spring semester (1/21/2019) to today, we have scraped every meal and its menu items. Please not that on Sundays
                breakfast is not served and there is only brunch and dinner. Brunch will fall under the Lunch tab for all Sundays. HUDs also shuts down ocasionally for special events 
                or school holidays/breaks which the table will show no data for."),
              fluidRow(
                box(
                  title = "Meal Selection",
                  selectInput("meal", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  h5(helpText("Select your meal time above.")),
                  dateInput("date", label = "Date Input", value = NULL, min = NULL, max = NULL,
                            format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                            language = "en", width = NULL),
                  h5(helpText("Select the desired date to view menu."))
                ),
                
                DTOutput("table", height = 300)
                
              )
        ), 
      
      tabItem(tabName = "meatselect",
              fluidRow(
                box(
                  title = "Meat Selection",
                  selectInput("meatType", "Meat:",
                              c("Beef" = "Beef",
                                "Chicken" = "Chicken",
                                "Pork" = "Pork")),
                  h5(helpText("Select your choice of meat."))
                ),
                
                plotOutput("meatselect", height = 500)
                
              )
          ),
      tabItem(tabName = "mealselect",
              fluidRow(
                box(
                  title = "Meal Selection",
                  selectInput("mealType", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  h5(helpText("Select your meal time."))
                ),
                
                plotOutput("mealselect", height = 500)
              )
        ),
        tabItem(tabName = "weekdayselect",
              fluidRow(
                box(
                  title = "Meal Selection",
                  
                  selectInput("weeks", "Day of Week:",
                              c("Sunday",
                                "Monday",
                                "Tuesday",
                                "Wednesday",
                                "Thursday",
                                "Friday",
                                "Saturday")),
                  h5(helpText("Select your desired day of the week.")),
                  selectInput("meal2", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  h5(helpText("Select your desired meal time."))
                ),
                
                plotOutput("weekselect", height = 500)
              )
        )
      
      
      )
    )
      
  )

server <- function(input, output) {
  
  huds <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1S3vFDul1PeB84Zd8G5ewe1ocZ6ezidQzAp5gn1eiHtQ/edit#gid=0') %>%
    mutate(Date = as.Date(Date, "%m-%d-%Y")) %>%
    filter(Date >= "2019-01-25") %>% 
    mutate(week = weekdays(as.Date(Date,'%Y-%m-%d'))) 
    
  huds_filter <- huds %>% 
    filter(!str_detect(Food, "Syrup|fruit|Fruit|Diced|Steamed|Beans|Cheese|Sauce|Rice|Salsa|Rolls|Bread|Baguette|Potatoes|Toast|Peas|Topping|Chips|Banana|Guacamole|Chopped|Lettuce|Hummus|Sugar|Loaf"))
  
  x <- huds
  output$table <- renderDT({
    x %>% 
      filter(Date == input$date) %>%
      filter(Meal == input$meal) %>% 
      select(Food) 
  })
    
    
    output$meatselect <- renderPlot({
        
        z <- huds_filter %>% 
          count(Food) %>% 
          filter(str_detect(Food, input$meatType)) %>% 
          arrange(desc(n)) %>% 
          slice(1:10)
        ggplot(data = z, aes(x= reorder(Food, -n), y = n)) +
          geom_bar(stat = "identity", fill = "firebrick1") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          labs(x = "Menu Item", y = "Number of Times Served") +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=14,face="bold"))
        
      
    })
    
    output$mealselect <- renderPlot({
      
      y <- huds_filter %>% 
        
        filter(str_detect(Meal, input$mealType)) %>% 
        count(Food) %>%
        arrange(desc(n)) %>% 
        slice(1:20)
      
      ggplot(data = y, aes(reorder(Food, -n), y = n)) +
        geom_bar(stat = "identity", fill = "firebrick1") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        labs(x = "Menu Item", y = "Number of Times Served")+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"))
      
      
      
    })
    
    output$weekselect <- renderPlot({
      
      aa <- huds_filter %>% 
      filter(str_detect(week, input$weeks)) %>% 
      filter(str_detect(Meal, input$meal2)) %>% 
      count(Food) %>%
      arrange(desc(n)) %>% 
      slice(1:10)
      ggplot(data = aa, aes(reorder(Food, -n), y = n)) +
        geom_bar(stat = "identity", fill = "firebrick1") +
        theme(axis.text.x = element_text(angle = 30, hjust = 1), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
          labs(x = "Menu Item", y = "Number of Times Served") + 
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"))
      
        
        
      
      
    })
           
           
    
}

shinyApp(ui, server)

