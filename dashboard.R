library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(highcharter)

#setwd("~/Google Drive File Stream/My Drive/SI/DataScience/Side projects/SLC Housing Dashboard/Data/for_dashboard/")
#for Hua's environment
setwd("~/Google Sorenson Drive/SLC Housing Dashboard/Data/for_dashboard/")

project_con <- read_excel("Housing Database Combined Data.xlsx", sheet = "All Data 2")
MSA_unemployment <- read_excel("MSA-unemployment.xlsx", sheet = "DataByYear")

#### UI ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    br(),
    menuItem("Help",icon = icon("info-circle"))
    
  )
)

body <- dashboardBody(
  # Dashboard favicon and title
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "house.png"),
    tags$title("SLC Housing")
  ),
  
  tabItems(
    tabItem(
       tabName="welcome",
       fluidRow(
         box(title = "", status = "primary", width = 8, 
             img(src = "house.png",
                 height = 64,
                 width = 64
              ),
             h2("SLC Housing Dashboard"),
             h4("Dynamic Web-based Analytics for Salt Lake City Housing"),
             br(),
             h4("SLC Housing is a ",a(href = 'http://shiny.rstudio.com', 'Shiny'),"web application built on top of R for housing-related data analytics"),
             br(),
             h4(HTML('&copy'), ' 2017 by University of Utah Business School')
         ),
         uiOutput("projectBox"),
         uiOutput("companyBox"),
         uiOutput("houseBox")
       )
     ),
    tabItem(tabName = "dashboard",
            fluidRow(
              box(highchartOutput("plot1", height = 250)),
              box(plotOutput("plot2", height=250))
            ),
            fluidRow(
              tags$iframe(src = "http://slcgov.maps.arcgis.com/apps/PublicInformation/index.html?appid=f632417a8bd94d5eb04f1f4eea728ce6", seamless=NA, height = 400, width = "100%")
            )
    ),
    
    tabItem(tabName = "widgets",
            fluidRow(
              # A static infoBox
              infoBox("New Affordable Units", 10 * 2, icon = icon("credit-card")),
              # Dynamic infoBoxes
              infoBox("New Market Units", 10 * 2, icon = icon("credit-card")),
              infoBox("New Dashboards", 1, icon = icon("dashboard"))
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "SLC Housing"),
  sidebar,
  body
)



#### Server ####
server <- function(input, output) {
  project<- 200
  company<-120
  house<-3000
  
  output$projectBox <- renderUI({
    valueBox(
      project,
      "Projects in SLC Housing",
      icon = shiny::icon("database"),
      color = "green"
    )
  })
  
  output$companyBox <-renderUI({
    valueBox(company,
             "Company Profiles",
              icon = icon("users"),
              color = "purple")
  })
  
  output$houseBox <- renderUI({
    valueBox(
      house,
      "Housing Profiles",
      icon = icon("building"),
      color = "yellow"
    )
  })
  
  output$plot1<-renderHighchart({
    barchart<-hchart(project_con$`Type (PSH, Affordable, or Market`, 
                      colorByPoint=TRUE, name="Affordable vs Market in SLC's construction projects")
    print(barchart)
  })
  output$plot2<-renderPlot({
    linechart<-plot(x=2007:2017,y=MSA_unemployment$Aug,
                    xlab="Year", main="Unemployment rate in August 2007-2017")
    print(linechart)
  })
}

shinyApp(ui, server)
