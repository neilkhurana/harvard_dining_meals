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

#Change theme to red, closer to Harvard Crimson theme colors
#Utilize the dashboard theme of shiny


ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Harvard Dining"),
  dashboardSidebar(
    
#Assign each tab a suitable name for its purpose
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Calendar", tabName = "calendar", icon = icon("calendar")),
      menuItem("Common Meals of the Semester", tabName = "mealselect", icon = icon("school")),
      menuItem("Popular Meals by Weekday", tabName = "weekdayselect", icon = icon("clock")),
      menuItem("Meat Dishes", tabName = "meatselect", icon = icon("utensils"))
      
    )
  ),


#Beginning of our body for each tab

  dashboardBody(
    
    # Boxes need to be put in a row (or column)
    
    tabItems(
      
      # First tab content including descriptions and sources
      
      tabItem(tabName = "home",
              h1("Welcome to the Harvard University Dining Services (HUDS) Search Tool"),
              h2("Summary"),
              h4("This dashboard serves a record keeper for Harvard's Undergraduate dining hall menu for the Spring 2019 semester.
                You have the option to select a menu of any date in our calendar tab.
                Additional tabs give you a breakdown of the frequently serves dishes of the entire semester, weekday trends and a categorization of the most frequent meat dishes."),
              h2("Background/Methodology"),
              h4("Some students at Harvard have mixed feelings about the dining hall system. Each dining hall, Annenberg (Freshman dining) and the upperclassmen houses offer
              the same menu options per day, with a few exceptions. While some students believe that HUDS provides a variety of options throughout the week/semester, others complain of repitition."),
              h4("The objective of this investigation was to explore trends in the HUDS menu data while also serving as a record keeper for meals offered. HUDS provides no
                 archiving feature for meal options beyond the current date."),
              h4("To access HUDS menus, I simply utilized a script that scrapes their", a("website", href = "http://www.foodpro.huds.harvard.edu/foodpro/menu_items.asp?type=30&meal=1"), 
                 "each day. Harvard Open Data Project (HODP), a student organization on campus, has developed", 
                 a("this", href = "http://hodp.org/catalog/index.html?q=huds"), "and their tool has been collecting information 
                from the beginning of the Spring 2019 semester."),
              h2("Contact"),
              h4("Feel free to reach out to me, Neil Khurana, at", a("neilkhurana@college.harvard.edu", href = "mailto:neilkhurana@college.harvard.edu"),". You can check 
                 out my",a("GitHub", href = "https://github.com/neilkhurana"),"and code", a("here", 
                  href = "https://github.com/neilkhurana/harvard_dining_meals"),".")
              ),
      
      # Another tab content with the purpose of archiving every Harvard meal from the beginning of the spring semester. 
      
      tabItem(tabName = "calendar",
              h2("Harvard Dining Archive Calendar"),
              h4("For the entirety of the spring semester, we have scraped each meal and its menu items. Please not that on Sundays,
                breakfast is not served (only brunch and dinner). Brunch falls under the Lunch tab for all Sundays. HUDs also shuts down ocasionally for special events 
                or school holidays/breaks and there will be no ouput for these days."),
              fluidRow(
                box(
                  title = "Meal Selection",
                  
                  #Users may select their meal time
                  
                  selectInput("meal", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  h5(helpText("Select your meal time above.")),
                  
                  #Users may pick a date on the calendar within the semester
                  
                  dateInput("date", label = "Date Input", value = "2019-05-10", min = "2019-01-28", max = "2019-05-10",
                            format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                            language = "en", width = NULL),
                  h5(helpText("Select the desired date to view menu."))
                ),
                
               #This is an output of everything on the menu for the selected meal and day
               
                 DTOutput("table", height = 300)
                
              )
        ), 
      
      tabItem(tabName = "meatselect",
              h2("Top 10 Meat Dishes"),
              h4("The most common meats served by HUDS are chicken, pork and meat. In this tab, you may view the top ten most frequently served dishes
                 containing your selected meat choice."),
             
              #Meat choices are made, only pork, chicken and beef
              
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
      
      
      
      #Tab descriptions are all at the top. 
      
      tabItem(tabName = "mealselect",
              h2("Commonly Served Meals"),
              h4("Students at Harvard often claim there is frequent repitition of dishes. Pick your meal below and see the top 20 most commonly
                 served dishes during that meal period. Please note that entrees served by HUDS daily such as fruits, ice cream, and sauce have been filtered out."),
              fluidRow(
                box(
                  
                  #Users may select a meal time from the dropdown and the most common items served during that meal time will be displayed
                  
                  title = "Meal Selection",
                  selectInput("mealType", "Meal:",
                              c("Breakfast",
                                "Lunch",
                                "Dinner")),
                  h5(helpText("Select your meal time."))
                ),
                
                plotOutput("mealselect", height = 600)
              )
        ),
      
      #Next tab is created 
      
        tabItem(tabName = "weekdayselect",
                
        #Descriptions and headings for weekday selection
                
                
                h2("Weekday Meals"),
                h4("Ever wonder if certain meals are prone to be served on a certain day of the week? Now's your chance to find out. Select the day of the week 
                   and meal time. Please note that entrees served by HUDS on a daily basis have been filtered out. There is also no breakfast served on Sundays and only
                   a brunch/lunch."),
              fluidRow(
                box(
                  title = "Meal Selection",
                  
                  #Users select the day of the week desired
                  
                  selectInput("weeks", "Day of Week:",
                              c("Monday",
                                "Tuesday",
                                "Wednesday",
                                "Thursday",
                                "Friday",
                                "Saturday",
                                "Sunday")),
                  h5(helpText("Select your desired day of the week.")),
                  
                  #Users select the meal they are interested in
                  
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
  
  #Our data is loaded from this google sheet. The HODP scraped dumps each days menu items into here
  
  huds <-gsheet2tbl('https://docs.google.com/spreadsheets/d/1S3vFDul1PeB84Zd8G5ewe1ocZ6ezidQzAp5gn1eiHtQ/edit#gid=0') %>%
    mutate(Date = as.Date(Date, "%m-%d-%Y")) %>%
    filter(Date >= "2019-01-25") %>% 
    mutate(week = weekdays(as.Date(Date,'%Y-%m-%d'))) 
  
  #We filter out menu items that contain these key words primarly because these items are served by HUDS everyday and their would be no variation in their frequency
    
  huds_filter <- huds %>% 
    
    #Please note that this filter code only works if places one long line and cannot be separated into a line break
    
    filter(!str_detect(Food, 
          "Syrup|fruit|Fruit|Diced|Steamed|Beans|Cheese|Sauce|Rice|Salsa|Rolls|Bread|Baguette|Cream|Potatoes|Toast|Peas|Topping|Chips|Banana|Guacamole|Chopped|Lettuce|Hummus|Sugar|Loaf|Blueberries|Apples|Bananas"))
  
  
  output$table <- renderDT({
    
    #For this tab that displays a data table of the menu items, we do not filter out anything.
    
    x <- huds
    x %>% 
      filter(Date == input$date) %>%
      filter(Meal == input$meal) %>% 
      select(Food) 
  })
    
    
    output$meatselect <- renderPlot({
        
        #Formatting for later use
      
        z <- huds_filter %>% 
          count(Food) %>% 
          filter(str_detect(Food, input$meatType)) %>% 
          arrange(desc(n)) %>% 
          slice(1:10)
      
        #Bar graph dsiplaying the top 10 meat items of the selected meat in descending order 
        
        ggplot(data = z, aes(x= reorder(Food, -n), y = n)) +
          geom_bar(stat = "identity", fill = "firebrick1") +
          theme(axis.text.x = element_text(angle = 60, hjust = 1), 
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank()) +
          labs(x = "Menu Item", y = "Freqency Served") +
          theme(axis.text=element_text(size=12),
                axis.title=element_text(size=15,face="bold"))
        
      
    })
    
    output$mealselect <- renderPlot({
      
      y <- huds_filter %>% 
        
      #formatted data dependent on user input
        
        filter(str_detect(Meal, input$mealType)) %>% 
        count(Food) %>%
        arrange(desc(n)) %>% 
        
      #Top 20 most frequent are used for this purpose of most common meals of entire semester
        
        slice(1:20)
      
      #Bar graph displaying top 20 meal items by meal for entire semester
      
      ggplot(data = y, aes(reorder(Food, -n), y = n)) +
        
        #Bar color selection 
        
        geom_bar(stat = "identity", fill = "firebrick1") +
        
        #Title of each item is at an angle so it fits in space
        
        theme(axis.text.x = element_text(angle = 70, hjust = 1), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        
        #Title/axis names and font/size are selected
        
        labs(x = "Menu Item", y = "Frequency Served")+
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15,face="bold"))
      
      
      
    })
    
    output$weekselect <- renderPlot({
      
      #Formatted data for later use
      
      aa <- huds_filter %>% 
        
      #Appropriate filters in place reliant on user input
      
      filter(str_detect(week, input$weeks)) %>% 
      filter(str_detect(Meal, input$meal2)) %>% 
      count(Food) %>%
      arrange(desc(n)) %>% 
        
      #Top ten most frequent are used for this graph
        
      slice(1:10)
      
      #Bar graph where user has much more flexibility in choosing the meal and day of the week they would like to select
      
      ggplot(data = aa, aes(reorder(Food, -n), y = n)) +
      
      #Bars ordered by descending order
        
        geom_bar(stat = "identity", fill = "firebrick1") +
        
        #Title of each item is at an angle so it fits in space
        
        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()) +
        
        #Title names and font/size are selected
          
        labs(x = "Menu Item", y = "Frequency Served") + 
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=15,face="bold"))
      
        
        
      
      
    })
           
           
    
}

shinyApp(ui, server)

