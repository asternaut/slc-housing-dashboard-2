library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(highcharter)


#setwd("~/")

setwd("/Users/suyash/Sorenson/SLC-Housing-Dashboard")



project_con <- read_excel("Housing Database Combined Data.xlsx", sheet = "All Data 2")
MSA_unemployment <- read_excel("MSA-unemployment.xlsx", sheet = "DataByYear")
Permit <- read_excel("Permit_adjusted.xlsx", sheet = "State Total")

#### UI ####
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Goals of Growing SLC", tabName = "goals", icon = icon("road")),
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

             h4(HTML('&copy'), ' 2017 by Sorenson Impact Center at the University of Utah')

         ),
         uiOutput("projectBox"),
         uiOutput("companyBox"),
         uiOutput("houseBox")
       )
     ),
    tabItem(tabName = "dashboard",
            fluidRow(
              box(highchartOutput("plot1", height = 250)),
              box(plotOutput("plot2", height=250)),
              box(highchartOutput("plot3", height = 400, width = 500)),
              box(highchartOutput("plot4", height = 400, width = 500))
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
  
  output$plot3<-renderHighchart({
      permit_plot1<-highchart() %>%
        hc_chart(type="column") %>%
      hc_title(text = "New Residential Units in 2017") %>%
      hc_yAxis(title = list(text = "Number of Units")) %>%
      hc_xAxis(categories = c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September")) %>%
      hc_plotOptions(column=list(datalabels = list(enabled = FALSE),
                                 stacking = "normal", enableMouseTracking=TRUE)) %>%
        
      hc_series(list(name="Single-Family", data=subset(Permit,
                    `Year of Date` == "2017")$`Single-Family Detached units`),
                list(name="Condo/Townhome", data=subset(Permit,
                    `Year of Date` == "2017")$`Condo/Townhome units`),
                list(name="Duplex/Twin Home", data=subset(Permit,
                    `Year of Date` == "2017")$`Duplex/Twin Home units`),
                list(name="Apartments", data=subset(Permit,
                    `Year of Date` == "2017")$`Apartments/ 3 or 4 Family units`
                    + subset(Permit, `Year of Date` == "2017")$`Apartments/ 5+ Families units`)
                ) %>%
    print(permit_plot1)
  })
  
  output$plot4<-renderHighchart({
    permit_plot2<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "Value of New Residential Buildings in 2017") %>%
      hc_yAxis(title = list(text = "Value of New Residential Buildings")) %>%
      hc_xAxis(categories = c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September")) %>%
      hc_plotOptions(column=list(datalabels = list(enabled = FALSE),
                                 stacking = "normal", enableMouseTracking=TRUE)) %>%
      hc_series(list(name="Single-Family", data=subset(Permit,
                    `Year of Date` == "2017")$`Single-Family Detached value`),
                list(name="Condo/Townhome", data=subset(Permit,
                    `Year of Date` == "2017")$`Condo/Townhome value`),
                list(name="Duplex/Twin Home", data=subset(Permit,
                    `Year of Date` == "2017")$`Duplex/Twin Home value`),
                list(name="Apartments", data=subset(Permit,
                    `Year of Date` == "2017")$`Apartments/ 3 or 4 Family value`
                     + subset(Permit, `Year of Date` == "2017")$`Apartments/ 5+ Families value`)
      ) %>%
      print(permit_plot2)
  })
}

shinyApp(ui, server)
