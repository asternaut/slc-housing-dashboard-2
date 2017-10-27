library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)

setwd("~/Google Drive File Stream/My Drive/SI/DataScience/Side projects/SLC Housing Dashboard/Data/for_dashboard/")

project_con <- read_excel("Housing Database Combined Data.xlsx", sheet = "All Data")
MSA_unemployment <- read_excel("MSA-unemployment.xlsx", sheet = 1)

#### UI ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
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
