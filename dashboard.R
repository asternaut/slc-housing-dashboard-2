library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(highcharter)
library(leaflet)
library(dplyr)
library(data.table)
source("tidycensus.R")

library("treemap")
library("viridis") 

#setwd("~/")
#setwd("/Users/suyash/Sorenson/SLC-Housing-Dashboard")




project_con <- read_excel("Data/Housing Database Combined Data.xlsx", sheet = "All Data 2")
MSA_unemployment <- read_excel("Data/MSA-unemployment.xlsx", sheet = "DataByYear")
Permit <- read_excel("Data/Permit_adjusted.xlsx", sheet = "State Total")
Multifamily<-fread("Data/new_multifamilywithgeo.csv")

pal <- colorFactor(c("navy", "red", "orange"), domain = Multifamily$`Type:  Affordable, Mixed or Market`)
Multifamily$`Type:  Affordable, Mixed or Market`<- factor(Multifamily$`Type:  Affordable, Mixed or Market`, 
                                                          levels = c("Affordable", "Market", "Mixed"), ordered = TRUE)

x=c("Health & Social Services", "Manufacturing", "Public Administration", "Professional Services",
    "Hospitality", "Retail Trade", "Transportation & Warehousing", "Finance & Insurance",
    "Admin & Waste Services", "Wholesale Trade")
y=c(45240, 61074, 49764, 76908, 18096, 34684, 48256, 73138, 33930, 68614)
z=c("60% AMI", "81% AMI", "66% AMI", "102% AMI", "24% AMI", "46% AMI", "64% AMI", "97% AMI", "45% AMI", "91% AMI")
industry<-data.frame(x,y,z)
tm <- treemap(industry, index =c("z","x"),
              vSize = "y", vColor = "y",
              type = "value", palette = rev(viridis(10)),
              draw = FALSE)
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
              h2("Salt Lake City's New Residential Units in 2017", br(),
                 "Value of New Residential Buildings in 2017"),
              br(),
              h4("Datasource from Ivory Boyer database"),
              box(highchartOutput("plot3", height = 400)),
              box(highchartOutput("plot4", height = 400))
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
                     h2("Unemployment Rate in 2nd Quarter 2007-2017"),
                     br(),
                     h4("Datasource from Bureau of Labor Statistics"),
                  box(highchartOutput("plot2", height=350), width=NULL)
              )
            ),
            br(),br(), br(),
            fluidRow(
              column(width=10,
                     h2("SLC Multi-Family: Affordable, Market and Mixed Units"),   
                     br(),
                     h4("Datasource from HAND"),
                     box(highchartOutput("plot1", height = 300), width=NULL)
              )
            ),
            fluidRow(
              column(width = 12,
              h2("Zoning"),
              h4("How land is zoned has a signifcant effect on how the city develops, and ultimately the supply and demand of housing."),
              tags$iframe(src = "http://slcgov.maps.arcgis.com/apps/PublicInformation/index.html?appid=f632417a8bd94d5eb04f1f4eea728ce6", seamless=NA, height = 400, width = "90%")
            )),
            fluidRow(
              column(width = 12,
              h3("Median Home Value"),
              h4("This map shows the variation in the median home value in the Salt Lake City area across census tracts. Data is collected from the recent Census"),
              leafletOutput("home", height = 400, width = 1000),
              h3("Median Rent"),
              h4("This map shows the variation in the median gross rent in the Salt Lake City area across census trancts. Data is collected from the recent Census"),
              leafletOutput("rent", height = 400, width = 1000)
              )
            ),
            fluidRow(
              column(width = 12,
                     h2("Salt Lake City Average Rent by Neighborhood"),
                     br(),
                     h4("The graph shows the most expensive Salt Lake City neighborhoods to rent apartments are Sugar House, Central City, and Central City-Liberty Welss.
The cheapest Salt Lake City neignborhoods to rent apartments are Poplar Grove, Liberty Wells, and Rose Park.", br(), br(),
                        "Datasource from Rent Jungle"),
                     box(highchartOutput("plot5", height=500), width=NULL)
                
              )
            )
            
    ),
    
    tabItem(tabName = "how",
            fluidRow(
              column(width=12,
                     h2("Salt Lake City AMI and Affodable Housing"),   
                     br(),
                     h4(" We use Area Median Income to help us understand how income is related to 
                        housing affordability. Generally we focus on those who make less than 
                        60% (about $45K) as those who struggle to make housing payments. 
                        Affordable means spending no more than 30% of income towards housing costs."), 
                     br(),
                     h4("Salt Lake City MSA income levels: Datasource from HUD 2017"),
                     box(highchartOutput("graph1", height = 600), width=NULL)
                     )
              ),
            fluidRow(
              column(width=12,
                     h2("SLC's Average Annual Wages for the Top 10 Industries"),   
                     br(),
                     h4(" This treemap shows the Salt Lake City top 10 industries and the respective
                        average annual wages as well as the AMI percentages for each industry."), 
                     br(),
                     h4("Datasource from HUD 2017"),
                     box(highchartOutput("graph2", height = 600), width=NULL)
                     )
              ),
            fluidRow(
              column(width=10,
                     h2("The growing disparity between wages and rental rates"),   
                     br(),
                     h4("Are current Salt Lake City housing price/rent affordable? 
                        A single person household in Salt Lake County has an 
                        Area Median Income (AMI) of $51,690; the AMI for a family of four is $73,800. 
                        The graph shows $470 average monthly affordable Affordable gap between affordable
                        rent for one-person household and 1Br average rent plus utilities, and $610 
                        average monthly affordable Affordable gap between affordable rent for four-person 
                        household and 3Br average rent plus utilities."), 
                     br(),
                     h4("Salt Lake City Average Rents vs Affordability (80% AMI): Datasource from CBRE 2016"),
                     box(highchartOutput("graph3", height = 500), width=NULL)
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
      hc_chart(type="line")%>%
      hc_title(text = "MSA Unemployment rate in 2nd Quarter 2007-2017") %>% 
      hc_xAxis(categories = c("2007", "2008", "2009", "2010", "2011", "2012",
                              "2013", "2014", "2015", "2016", "2017")) %>%
      hc_yAxis(title = list(text = "unemployment rate")) %>%
      hc_series(list(name="August", data=MSA_unemployment$Aug),
                list(name="July", data=MSA_unemployment$Jul),
                list(name="June", data=MSA_unemployment$Jun),
                list(name="May", data=MSA_unemployment$May))%>%
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
  
  output$plot5<-renderHighchart({
    rent_plot<-highchart() %>%
      hc_chart(type="bar") %>%
      hc_title(text = "Salt Lake City Average Rent by Neighborhood") %>%
      hc_yAxis(title = list(text = "Rent in dollars"),
               labels=list(format= "${value}")) %>%
      hc_xAxis(categories = c("Sugar House", "Central City", "Central City-Liberty Welss", 
                              "Downtown", "Capitol Hill", "Fairpark", "Glendale", "East Central",
                              "People's Freeway", "Westpointe", "Greater Avenues", "Rose Park",
                              "Liberty Wells", "Poplar Grove")) %>%
      hc_series(list(name="Average rent",
                     data=c(1410, 1335, 1281, 1250, 1225, 1128, 1121, 1106, 1030, 978, 966, 847, 831, 789))
                )%>%
      print(rent_plot)
  }
  )
    
  ##"how did we get here" output graphs
  output$graph1<-renderHighchart({
    AMI_plot<-highchart() %>%
      hc_chart(type="bar") %>%
      hc_title(text = "Salt Lake City MSA Income Levels in 2017") %>%
      hc_yAxis(title = list(text = "Income in dollars")) %>%
      hc_xAxis(categories = c("1 person", "2 people", "3 people", 
                              "4 people", "5 people", "6 people",
                              "7 people", "8 people"),
               title = list(text = "Household sizes")) %>%
      hc_series(list(name="Extremely low income 30% AMI $", data=c(15850, 18100, 20350, 22600, 24450, 26250, 28050, 29850)),
                list(name="Very low income 50% AMI $", data=c(26400, 30200, 33950, 37700, 40750, 43750, 46750, 49800)),
                list(name="Moderately low income 60% AMI $", data=c(31680, 36240, 40740, 45240, 48900, 52500, 56100, 59760)),
                list(name="Low income 80% AMI $", data=c(42250, 48250, 54300, 60300, 65150, 69950, 74800, 79600)),
                list(name="100% AMI $", data=c(52800, 60400, 67900, 75400, 81500, 87500, 93500, 99600))
      )%>%
      print(AMI_plot)
  }
  )
  output$graph2<-renderHighchart({
  Industry_hc<-highchart(height = 500) %>% 
    hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                          layoutAlgorithm = "squarified",
                          name = "AMIdata") %>%
    hc_title(text = "AMI Percentage by Industry") %>% 
    hc_tooltip(pointFormat = "Average annual wage: ${point.value:.0f}<br>
               {point.name}")
  print(Industry_hc)
  }
  )
  output$graph3<-renderHighchart({
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
