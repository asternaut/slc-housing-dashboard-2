library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(highcharter)
library(leaflet)
library(dplyr)
library(data.table)

#setwd("~/")

#setwd("/Users/suyash/Sorenson/SLC-Housing-Dashboard")




project_con <- read_excel("Data/Housing Database Combined Data.xlsx", sheet = "All Data 2")
MSA_unemployment <- read_excel("Data/MSA-unemployment.xlsx", sheet = "DataByYear")
Permit <- read_excel("Data/Permit_adjusted.xlsx", sheet = "State Total")
Multifamily<-fread("Data/new_multifamilywithgeo.csv")

pal <- colorFactor(c("navy", "red", "orange"), domain = Multifamily$`Type:  Affordable, Mixed or Market`)
Multifamily$`Type:  Affordable, Mixed or Market`<- factor(Multifamily$`Type:  Affordable, Mixed or Market`, 
                                                          levels = c("Affordable", "Market", "Mixed"), ordered = TRUE)

#### UI ####
fluidPage(theme = "test.css",
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("How did we get here?", tabName = "how", icon = icon("bar-chart")),
    menuItem("Goals of Growing SLC", tabName = "goals", icon = icon("road")),
    br(),
    menuItem("Help",icon = icon("info-circle"))
    
  )
),

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
              column(width=10,
                     h2("SLC Multi-Family: Affordable, Market and Mixed Units"),   
                     br(),
                     h4("Datasource from HAND"),
                  box(highchartOutput("plot1", height = 300), width=NULL)
              )
            ),
            br(),br(), br(),
            fluidRow(
              column(width=12,
                     h2("Salt Lake City Multifamily Interative Map"),   
                     br(),
                     h4("This map shows the multi-family units in Salt Lake City in three categories:",
                        br(),
                        "Datasource from HAND"),
              box(
                collapsible = TRUE,
                width = NULL,
                height = NULL,
                leafletOutput("multifamily_map")
              )
            )
            ),
            br(),br(), br(),
            fluidRow(
              column(width=12,
                     h2("Unemployment Rate in August 2007-2017"),
                     br(),
                     h4("Datasource from Bureau of Labor Statistics"),
                  box(highchartOutput("plot2", height=350), width=NULL)
              )
            ),
            br(),br(), br(),
            fluidRow(
              h2("Salt Lake City's New Residential Units in 2017", br(),
                 "Value of New Residential Buildings in 2017"),
              br(),
              h4("Datasource from Ivory Boyer database"),
              box(highchartOutput("plot3", height = 400)),
              box(highchartOutput("plot4", height = 400))
            ),
            fluidRow(
              tags$iframe(src = "http://slcgov.maps.arcgis.com/apps/PublicInformation/index.html?appid=f632417a8bd94d5eb04f1f4eea728ce6", seamless=NA, height = 400, width = "100%")
            ),
            fluidRow(
              h3("Median Home Value"),
              h4("This map shows the variation in the median home value in the Salt Lake City area across census tracts. Data is collected from the recent Census"),
              leafletOutput("home", height = 400, width = 1000),
              h3("Median Rent"),
              h4("This map shows the variation in the median gross rent in the Salt Lake City area across census trancts. Data is collected from the recent Census"),
              leafletOutput("rent", height = 400, width = 1000)
            )
            
    ),
    
    tabItem(tabName = "how",
            fluidRow(
              column(width=10,
                     h2("The growing disparity between wages and rental rates"),   
                     br(),
                     h4("A single person household in Salt Lake County has an 
                        Area Median Income (AMI) of $51,690; the AMI for a family of four is $73,800. 
                        The graph shows $470 average monthly affordable Affordable gap between affordable
                        rent for one-person household and 1Br average rent plus utilities, and $610 
                        average monthly affordable Affordable gap between affordable rent for four-person 
                        household and 3Br average rent plus utilities."), 
                     br(),
                     h4("Salt Lake City Average Rents vs Affordability (80% AMI): Datasource from CBRE 2016"),
                     box(highchartOutput("graph1", height = 500), width=NULL)
                     )
              )
            ),
    tabItem(tabName = "goals",
            fluidRow(
              column(width=10,
              
              tags$div(class = "header",
              h2("Goals of Growing Salt Lake City")),
              h3("Goal 1: Reform City Practices"),
              h3("Goal 2:Affordable Housing"),
              h3("Goal 3:Equitable and fair Housing"),
              
              withTags({
                div(class="header", checked=NA,
                    p("Want to check out the plan?" , a("Click Here",target="_blank",href="plan.pdf"))
                    
                )
              })
              )
            )
            
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

    barchart<-hchart(Multifamily$`Type:  Affordable, Mixed or Market`, 
                      colorByPoint=TRUE, name="Affordable, Market, and Mixed in SLC's Multifamily Units")
    print(barchart)
  })
  
  output$multifamily_map<- renderLeaflet({
    leaflet(Multifamily) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%  
      setView(-111.876183, 40.758701, zoom = 13) %>%
      addCircleMarkers(Multifamily$lon, Multifamily$lat, popup=Multifamily$`Project Name`,
                       weight = 4, radius=6,
                       color = ~pal(`Type:  Affordable, Mixed or Market`),
                       stroke = TRUE, fillOpacity = .6) %>%
      addLegend("bottomright", colors=c("navy", "red", "orange"), 
                labels= c("Affordable", "Market", "Mixed"), title="Multifamily Units in SLC")%>%
      print(multifamily_map)
  })
  
  output$plot2<-renderHighchart({
    unemployment_plot<-highchart() %>%
      hc_title(text = "MSA Unemployment rate in August 2007-2017") %>% 
      hc_xAxis(categories = c("2007", "2008", "2009", "2010", "2011", "2012",
                              "2013", "2014", "2015", "2016", "2017")) %>%
      hc_yAxis(title = list(text = "unemployment rate")) %>%
      hc_add_series_scatter(MSA_unemployment$Year, MSA_unemployment$Aug)%>%
    print(unemployment_plot)
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
  ##"how did we get here" output graphs
  output$graph1<-renderHighchart({
    affordability1<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text= "Salt Lake City Average Rents vs Affordability (80% AMI)") %>%
      hc_xAxis(categories = c("one-person household and 1Br average rent + utilities", 
                              "four-person household and 3Br average rent + utilities")) %>%

      hc_yAxis(labels=list(format= "${value}"))%>%
      hc_series(list(name ="Affordable rent", 
                     data=c(900, 1300), dataLabels=list(enabled=TRUE,format= "${point.y}")),
                list(name = "Average rent",
                     data=c(1370, 1910),dataLabels=list(enabled=TRUE,format= "${point.y}"))

      ) %>%
      print(affordability1)
  }
  )
  output$home <- renderLeaflet({
    home_map
  })
  
  output$rent <- renderLeaflet({
    rent_map
  })

}

shinyApp(ui, server)
