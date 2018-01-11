library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(highcharter)
library(leaflet)
library(dplyr)
library(data.table)
library(purrr)
library(scales)
#source("tidycensus.R")

library("treemap")
library("viridis") 

#setwd("~/")
#setwd("/Users/suyash/Sorenson/SLC-Housing-Dashboard")




#project_con <- read_excel("Data/Housing Database Combined Data.xlsx", sheet = "All Data 2")
#MSA_unemployment <- read_excel("Data/MSA-unemployment.xlsx", sheet = "DataByYear")
permit17 <- read_excel("Data/SLC_new_units.xlsx", sheet = "Sheet1")
Multifamily<-fread("Data/new_multifamilywithgeo.csv")
neighborhoodRent<-read.csv("rentAve.csv")
historicalVacancy<-read.csv("vacancyHis.csv")
multi<-read_excel("Data/Multifamily.xlsx", sheet = "Multi-Family Listings" )


# multifamily address map 
pal <- colorFactor(c("navy", "red", "orange"), domain = Multifamily$`Type:  Affordable, Mixed or Market`)
Multifamily$`Type:  Affordable, Mixed or Market`<- factor(Multifamily$`Type:  Affordable, Mixed or Market`, 
                                                          levels = c("Affordable", "Market", "Mixed"), ordered = TRUE)
# industry ami and median income in "How"
industryChart<-read.csv("industryC.csv")
tm <- treemap(industryChart, index =c("ami","profession"),
              vSize = "income", vColor = "income",
              type = "value", palette = rev(viridis(10)),
              draw = FALSE)
# SL County home sale median price csv file:y value in "medianPrice2017_3rdQuarter.csv" is written from "weighedAve"
#countyMedian<-read_excel("Data/countyMedian.xlsx", sheet = "Sheet1")
#weighedAve <- sapply(split(countyMedian, countyMedian$`City `), function(x){weighted.mean(x$`2017 Median Price`, x$`Units Sold`)})
#m<-read.csv("medianPrice2017_3rdQuarter.csv", stringsAsFactors = FALSE)
#mds<-list_parse(m)
#names(mds)<-NULL

# SL City historical sale median price csv file: y value in "historical_median_3rdQuarter.csv" is written from "cityWAve"
#cityMedian<-read_excel("cityHisMedian.xlsx", sheet = "Sheet1")
#cityWAve <- sapply(split(cityMedian, cityMedian$year), function(x){weighted.mean(x$medianPrice, x$unitsSold)})
mh<-read.csv("historical_median_3rdQuarter.csv", stringsAsFactors = FALSE)
mhShort<-mh %>%
  filter (name!="2003"& name!="2004"& name!="2005"& name!="2006"& name!="2007") 
  mhds<-list_parse(mhShort)
  names(mhds)<-NULL

#### UI ####
fluidPage(
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Welcome", tabName = "welcome", icon = icon("home")),
    menuItem("How did we get here?", tabName = "how", icon = icon("bar-chart")),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Goals of Growing SLC", tabName = "goals", icon = icon("road")),
    br()
    
  )
),

body <- dashboardBody( 
  # Dashboard favicon and title
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "house.png"),
    tags$link(href = "dashboard.css", rel = "stylesheet"),
    tags$title("SLC Housing")
  ),
  
  tabItems(
    tabItem(
       tabName="welcome",
       fluidRow(class="welcomeBox",
         fluidRow(class="welcomeText",
         h1("SLC Housing Dashboard"),
         h4("Dynamic Web-based Analytics for Salt Lake City Housing")
         ),
         img(src='welcome_image.jpg',class="welcomeImage") 
       ),
       fluidRow(class="headerText",
                h4("SLC Housing is a ",a(href = 'http://shiny.rstudio.com', 'Shiny'),"web application built on top of R for housing-related data analytics")      
                ),
       fluidRow(
             br(),
             h4(HTML('&copy'), ' 2017 by Sorenson Impact Center at the University of Utah'),
         uiOutput("projectBox"),
         uiOutput("companyBox"),
         uiOutput("houseBox")
       )
     ),
    
    tabItem(tabName = "dashboard",
            fluidRow(
              h2("Salt Lake City Housing Stock by Owner vs Renter: 2014",
                 br(),"Age of Housing Stock: Salt Lake City in comparison with Salt Lake County"),
              br(),
              h4("Datasource from 2014 ACS and BBC Research and Consulting"),
              box(highchartOutput("plot10", height = 400)),
              box(highchartOutput("plot14", height = 500))
            ),
            br(),br(), br(),
            fluidRow(
              h2("Salt Lake City Housing Type by Tenure: 2014"),
              br(),
              h4("Datasource from BBC Housing Market Study 2016"),
              column(width=4, box(highchartOutput("plot11", height = 400), width=NULL)),
              column(width=4,box(highchartOutput("plot12", height = 450), width=NULL)),
              column(width=4,box(highchartOutput("plot13", height = 400), width=NULL))
            ),
            br(),br(), br(),
            fluidRow(
              h2("Salt Lake City's Multi-Family Units: Affordable, Market vs Mixed"),
              br(),
              h4("Datasource from HAND"),
              box(highchartOutput("plot1", height = 400), width=NULL)
            ),
            br(),br(), br(),
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City's New Residential Housing Stock in 2017")
              ),
              br(),
              p("Datasource from Ivory Boyer database"),
              box(highchartOutput("plot3", height = 450), width=NULL)
            ),
            br(),br(), br(),
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City's New Housing Stock: Single-Family vs Multi-Family")
              ),
              br(),
              p("Datasource from Ivory Boyer database and HAND"),
              box(highchartOutput("plot4", height = 400), width=NULL)
            ),
            
            br(),br(), br(),
            
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h2("Salt Lake City Multifamily Interative Map")
                     ),
                     br(),
                     p("This map shows the multi-family units in Salt Lake City in three categories:",
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
            #fluidRow(
              #column(width=12,
                     #h2("Unemployment Rate in 2nd Quarter 2007-2017"),
                     #br(),
                    #p("Datasource from Bureau of Labor Statistics"),
                  #box(highchartOutput("plot2", height=350), width=NULL)
              #)
            #),
            #br(),br(), br(),
            #fluidRow(
              #column(width=10,
                     #h2("SLC Multi-Family: Affordable, Market and Mixed Units"),   
                     #br(),
                     #p("Datasource from HAND"),
                     #box(highchartOutput("plot1", height = 300), width=NULL)
              #)
            #),
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("Average Rent by Neighborhood")
                     ),
                     br(),
                     p("The graph shows the most expensive Salt Lake City neighborhoods to rent apartments are Sugar House, Central City, and Central City-Liberty Welss.The least expensive neignborhoods are Poplar Grove, Liberty Wells, and Rose Park. The data come from Rent Jungle, which uses X methodology to estimate local rents."
                     )
              )
            ),
            fluidRow(
              column(width = 8,
                     box(highchartOutput("plot5", height=500), width=NULL)
              ),
              column(width = 4, wellPanel(
                selectInput("neighborhood_type", "Rent Burden Calculator",
                            neighborhoodRent$neighboarhood)
              ),
              textOutput('a_out')
              )
            ),
            br(),br(), br(),
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("Historical Vacancy Rates for Salt Lake City and Downtown")
                     ),
                     br(),
                     p("The graph shows Salt Lake City's most recent three-year vacancy rates. 
                        Data shown here is based on rentals with square footages between around 800 and 1000. 
                        CBRE collects and interprets data to offer their perspective on the trends of real estate market.", 
                        br(), br(),
                        "Datasource from CBRE, inc"),
                     box(highchartOutput("plot6", height=400), width=NULL)
              )
              ),
            br(),br(), br(),
      #      fluidRow(
      #        column(width = 12,
      #               fluidRow(class="headerText",
      #               h2("Salt Lake City Vacancy Rate by Submarket")
      #               ),
      #               br(),
      #               p("Along with the vacancy rate, the line represents the total of buildings in submarkets in Salt Lake City. 
      #                  Data shown here reflects the vacancy rates of big office buildings and commercial real estate property.", 
      #                  br(), br(),
      #                  "Datasource from Cushman & Wakefield"),
      #               box(highchartOutput("plot7", height=400), width=NULL)
      #        )
      #      ),
      #      br(),br(), br(),
      #      fluidRow(
      #        column(width = 12,
      #               fluidRow(class="headerText",
      #               h2("Salt Lake City 2017 3rd Quarter Sale Median Price in Comparison with Other Cities in the County")
      #               ),
      #               br(),
      #               p("The graph shows the 3rd quarter home sale median price in Salt Lake County. Please click on the Salt Lake City column to
      #                  get navigated to more details on median prices of different zipcodes in the city.", 
      #                  br(), br(),
      #                  "Datasource from The Salt Lake Tribune"),
      #               box(highchartOutput("plot8", height=500), width=NULL)
      #        )
      #      ),
      #      br(),br(), br(),
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("Salt Lake City Sale Median Price 3rd Quarter 2003 - 2017")
                     ),
                     br(),
                     p("The graph shows the historical trend of 3rd quarter home sale median price in Salt Lake City from 2003 to 2017. 
                        You are welcome to click on the columns of specific years to get navigated to detailed median prices of
                        different zipcodes areas in the same year", 
                        br(), br(),
                        "Datasource from The Salt Lake Tribune"),
                     box(highchartOutput("plot9", height=500), width=NULL)
              )
            )      
    ),
    
    tabItem(tabName = "how",
            fluidRow(class="getBox",
                     img(src='gethere.png',class="getImage") 
            ),
            br(),br(),
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h2("Salt Lake City AMI and Affodable Housing")
                     ),
                     br(),
                     p(" We use Area Median Income to help us understand how income is related to 
                        housing affordability. Generally we focus on those who make less than 
                        60% (about $45K) as those who struggle to make housing payments. 
                        Affordable means spending no more than 30% of income towards housing costs."), 
                     br(),
                     p("Salt Lake City MSA income levels: Datasource from HUD 2017"),
                     box(highchartOutput("graph1", height = 600), width=NULL)
                     )
              ),
            br(),br(), br(),
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h2("SLC's Average Annual Wages for the Top 10 Industries")
                     ),
                     br(),
                     p(" This treemap shows the Salt Lake City top 10 industries and the respective
                        average annual wages as well as the AMI percentages for each industry."), 
                     br(),
                     p("Datasource from HUD 2017"),
                     box(highchartOutput("graph2", height = 600), width=NULL)
                     )
              ),
            br(),br(), br(),
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                              h2("The growing disparity between wages and rental rates")
                     ),
                     br(),
                     p("Are current Salt Lake City housing price/rent affordable? 
                        A single person household in Salt Lake County has an 
                        Area Median Income (AMI) of $51,690; the AMI for a family of four is $73,800. 
                        The graph shows $470 average monthly affordable Affordable gap between affordable
                        rent for one-person household and 1Br average rent plus utilities, and $610 
                        average monthly affordable Affordable gap between affordable rent for four-person 
                        household and 3Br average rent plus utilities."), 
                     br(),
                     p("Salt Lake City Average Rents vs Affordability (80% AMI): Datasource from CBRE 2016"),
                     box(highchartOutput("graph3", height = 500), width=NULL)
                     )
            ),
    br(),br(), br(),
    fluidRow(
            fluidRow(class="headerText",
             h2("Wage Increase vs Home Sale Price Increase: 2011-2014", br(),
                "Wage Increase vs Rent Increase: 2011-2016")
            ),
             br(),
             p("Home sale prices increased 33% between 2011 and 2014, while homeowner wages increased only 8%. 
               This steep rise in prices has created a market in which most for-sale homes are only affordable 
                for those in the high-income bracket. The rent increase of 26% is adding great pressure for 
                renters who have only 4% wage increase."), 
             br(),
             p("Datasource from BBC Housing Market Study 2016"),
             box(highchartOutput("graph4", height = 400)),
             box(highchartOutput("graph5", height = 400))
    )
    ),
    tabItem(tabName = "goals",
            fluidRow(
              column(width=10,
              
              tags$div(class = "header",
              h2("Goals of Growing Salt Lake City")),
              fluidRow(class="headerText",
                h3("Goal 1: Reform City Practices")
              ),
              p("Current housing regulations were established in the 90s in response to the population
                decline that had started in previous decades. Since then, however, the population has grown
                at its fastest rate in about a century. This in conjunction with the increasing diversity of the
                population means that housing regulations are in real need of an update to accommodate
                the changing demographics of Salt Lake City. “This goal focuses on the need to increase the
                diversity of housing types and opportunities in the city by seeking policy reforms that can
                
                enhance the flexibility of the land-use code and create an efficient and predictable
                development process for community growth.”"),
              fluidRow(class="headerText",
                       h3("Goal 2:Affordable Housing"),
                       p("Salt Lake City is experiencing a housing boom. Many new residential units have been and
continue to be built by developers. While the number of residential units has increased so
have their prices but wages have not risen at the same rate. The result is that low and
middle income families are now having to spend more of their incomes on housing or make
tough decisions in order to find more affordable housing. This goal focuses on expanding
policies and creating initiatives that encourage the development of affordable housing now
and in the long run to aid low and middle income households.")
              ),
              fluidRow(class="headerText",
                       h3("Goal 3:Equitable and fair Housing"),
                       p("Despite the increase in residential units being built, the supply of houses in Salt Lake City is
still not quite up to the demand so competition is very high amongst renters. Such
                         competition gives rise to discriminatory housing practices against low income households
                         and protected classes such as disabled people. “Actively rooting out discrimination in
                         housing is not only a standard that Salt Lake City holds itself to, but it is also a requirement
                         under the U.S. Department of Housing and Urban Development (HUD) administrative ruling
                         of 2015, the Affirmatively Furthering Fair Housing rule (AFFH).” This goal focuses on
                         measures that will be taken to improve access to high opportunity neighborhoods and
                         eliminating housing discrimination, measures that will also be included in Salt Lake City’s
                         AFFH plan due in 2019.")
              ),
              
              
              
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
  
  # output$projectBox <- renderUI({
  #   valueBox(
  #     project,
  #     "Projects in SLC Housing",
  #     icon = shiny::icon("database"),
  #     color = "green"
  #   )
  # })
  # 
  # output$companyBox <-renderUI({
  #   valueBox(company,
  #            "Company Profiles",
  #            icon = icon("users"),
  #            color = "purple")
  # })
  # 
  # output$houseBox <- renderUI({
  #   valueBox(
  #     house,
  #     "Housing Profiles",
  #     icon = icon("building"),
  #     color = "yellow"
  #   )
  # })
  
  output$plot1<-renderHighchart({
    multifamily_plot<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "Multi-Family Units: Affordable, Market vs Mixed") %>%
      hc_xAxis(categories = c("Affordable", "Market", "Mixed")) %>%
      hc_yAxis(title = list(text = "Number of Units")) %>%
      hc_plotOptions(series = list(colorByPoint = TRUE)) %>%
      hc_series(list(name="Multifamily Units", data=c(subset(multi, `Project Name` == "total")$`Affordable Units`, 
                                                      subset(multi,`Project Name` == "total")$`Market Units`,
                                                      subset(multi,`Project Name` == "total")$`Mixed Units`)
      )) %>%
      print(multifamily_plot)
  })
  
  output$multifamily_map<- renderLeaflet({
    leaflet(Multifamily) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%  
      setView(-111.876183, 40.758701, zoom = 13) %>%
      addCircleMarkers(Multifamily$lon, Multifamily$lat, popup=paste(Multifamily$`Type:  Affordable, Mixed or Market`, "<br>", Multifamily$`Total Units`),
                       weight = 4, radius=~ifelse(`Total Units`<50, 5,(ifelse(`Total Units`<120, 9, 13))),
                       color = ~pal(`Type:  Affordable, Mixed or Market`),
                       stroke = TRUE, fillOpacity = .6) %>%
      addLegend("bottomright", colors=c("navy", "red", "orange"), 
                labels= c("Affordable", "Market", "Mixed"), title="Multifamily Units in SLC")%>%
      print(multifamily_map)
  })
  
#  output$plot2<-renderHighchart({
#    unemployment_plot<-highchart() %>%
#      hc_chart(type="line")%>%
#      hc_title(text = "MSA Unemployment rate in 2nd Quarter 2007-2017") %>% 
#      hc_xAxis(categories = c("2007", "2008", "2009", "2010", "2011", "2012",
#                              "2013", "2014", "2015", "2016", "2017")) %>%
#      hc_yAxis(title = list(text = "unemployment rate")) %>%
#      hc_series(list(name="August", data=MSA_unemployment$Aug),
#                list(name="July", data=MSA_unemployment$Jul),
#                list(name="June", data=MSA_unemployment$Jun),
#                list(name="May", data=MSA_unemployment$May))%>%
#      print(unemployment_plot)
#  })
  
  output$plot3<-renderHighchart({
    permit_all<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "New Residential Units in 2017") %>%
      hc_yAxis(title = list(text = "Number of Units")) %>%
      hc_xAxis(categories = c("January-March", "April-June",
                              "July-September", "October-December")) %>%
      hc_plotOptions(column=list(datalabels = list(enabled = FALSE),
                                 stacking = "normal", enableMouseTracking=TRUE)) %>%
      
      hc_series(list(name="Single-Family", data=permit17$`Single-family Units`),
                list(name="Duplexes and Twin Homes", data=permit17$`Duplexes and Twin Homes`),
                list(name="Condominiums / Townhomes", data=permit17$`Condominiums / Townhomes`),
                list(name="Apartments", data=permit17$`Apartments (3 or more units)`)
      )
    print(permit_all)
  })
  
  output$plot4<-renderHighchart({
    permit_plot2<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "New Residential Units in 2017: Single-Family vs Multi-Family Units") %>%
      hc_yAxis(title = list(text = "Number of Units")) %>%
      hc_xAxis(categories = c("January", "February", "March", "April", "May", "June",
                              "July", "August", "September")) %>%
      hc_series(list(name="Single-Family", data=subset(Permit,
                                                       `Year of Date` == "17")$`Single-Family Detached units`),
                list(name="Multi-Family", data=subset(Permit,
                                                      `Year of Date` == "17")$`Condo/Townhome units`)
      ) %>%
      print(permit_plot2)
      
  })
  
  output$plot5<-renderHighchart({
    rent_plot<-highchart() %>%
      hc_chart(type="bar") %>%
      hc_title(text = "Salt Lake City Average Rent by Neighborhood") %>%
      hc_yAxis(title = list(text = "Rent in dollars"),
               labels=list(format= "${value}")) %>%
      hc_xAxis(categories=neighborhoodRent$neighboarhood) %>%
      hc_series(list(name="Average rent",
                     data=neighborhoodRent$a_rent)
                )%>%
      print(rent_plot)
  }
  )
  
  output$plot6<-renderHighchart({
    historical_vacancy<-highchart() %>%
      hc_title(text= "Salt Lake County Historical Vacancy Rate") %>%
      hc_xAxis(categories = c("Salt Lake City", "Downtown")) %>%
      hc_yAxis(labels=list(format= "{value}%"))%>%
      hc_series(list(name="2014 vacancy rates", type="column",
                     data=c(subset(historicalVacancy, county=="Salt Lake City")$vacancy2014, 
                            subset(historicalVacancy,county=="Downtown")$vacancy2014)),
                list(name="2015 vacancy rates", type="column",
                     data=c(subset(historicalVacancy, county=="Salt Lake City")$vacancy2015,
                            subset(historicalVacancy, county=="Downtown")$vacancy2015)),
                list(name="2016 vacancy rates", type="column",
                     data=c(subset(historicalVacancy, county=="Salt Lake City")$vacancy2016,
                            subset(historicalVacancy, county=="Downtown")$vacancy2016))
      )%>%
      print(historical_vacancy)
  }
  )
  
#  output$plot7<-renderHighchart({
#    vacancy_submarket<-highchart() %>%
#      hc_title(text = "Salt Lake City Vacancy Rate by Submarket") %>% 
#      hc_xAxis(categories = c("CBD", "Periphery", "Northeast", "Northwest", "Central East", 
#                              "Central West", "Southeast", "Southwest")) %>%
#      hc_series(list(name="submarket vacancy rate", type="area",
#                     data=c(14.5, 17.4, 9.5, 14.0, 16.7, 9.2, 13.9, 7.8)),
#                list(name="total buildings", type= "line",
#                     data=c(57, 46, 55, 69, 135, 24, 95, 18))
#      )%>%
#      print(vacancy_submarket)
#  }
#  )
#  output$plot8<-renderHighchart({
#    median_sale<-highchart() %>%
#      hc_chart(type="column") %>%
#      hc_title(text="Salt Lake County 2017 3rd Quarter Sale Median Prices") %>%
#      hc_yAxis(labels=list(format="${value}")) %>%
#      hc_xAxis(type="category") %>%
#      hc_plotOptions(series = list(boderWidth = 0,
#                                   dataLabels = list(enabled = TRUE) )) %>%
#      hc_add_series(name="Weighed average sale median price", data=mds,
#                    colorByPoint=TRUE, colors=c('#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec',
#                                                '#7cb5ec','#7cb5ec','#7cb5ec', '#7cb5ec','#7cb5ec','#FF0000','#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec','#7cb5ec'))
#    
#    slc_drill<-read.csv("SLC2017.csv", stringsAsFactors = FALSE)
   # sandy_drill<-read.csv("Sandy2017.csv", stringsAsFactors = FALSE)
  #wjordan_drill<-read.csv("WJordan2017.csv", stringsAsFactors = FALSE)
  # holladay_drill<-read.csv("Holladay2017.csv", stringsAsFactors = FALSE)
  #  wValleyCity_drill<-read.csv("WestValleyCity2017.csv", stringsAsFactors = FALSE)
    
#    second_el_to_numeric <- function(ls){
#      map(ls, function(x){
#        x[[2]] <- as.numeric(x[[2]])
#        x
#      }) }
    
#    dsSLC2017 <- second_el_to_numeric(list_parse2(slc_drill))
  #  dsSandy2017<- second_el_to_numeric(list_parse2(sandy_drill))
  #  dsWJordan2017<-second_el_to_numeric(list_parse2(wjordan_drill))
  #  dsHolladay2017<-second_el_to_numeric(list_parse2(holladay_drill))
  #  dsWValleyCity2017<-second_el_to_numeric(list_parse2(wValleyCity_drill))
    
#    median_sale <- median_sale %>%
#      hc_drilldown(
#        allowPointDrilldown=TRUE, 
#        series=list(list(
#          id="salt lake city",
#          data= dsSLC2017,
#          name="Salt Lake City sale median prices"
#        )
      #,
      #  list(
      #    id="sandy",
      #    data=dsSandy2017,
      #    name="Sandy sale median price"
      # ),
      #  list(
      #    id="west jordan",
      #    data=dsWJordan2017,
      #    name="West Jordan sale median price"
      #  ),
      #  list(
      #    id="holladay",
      #    data=dsHolladay2017,
      #    name="Holladay sale median price"
      #  ),
      #  list(
      #    id="west valley city",
      #    data=dsWValleyCity2017,
      #    name="West Valley City sale median price"
      # )
#      )) 
#    print(median_sale)
#  }
#  )
  
  output$plot9<-renderHighchart({
    historical_median_sale<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text="Salt Lake City Sale Median Price 3rd Quarter 2003 - 2017") %>%
      hc_yAxis(labels=list(format="${value}")) %>%
      hc_xAxis(type="category") %>%
      hc_plotOptions(series = list(boderWidth = 0,
                                   dataLabels = list(enabled = TRUE))) %>%
      hc_add_series(name="SLC weighted average sale median prices", data=mhds,
                    colorByPoint=FALSE)
    
#    drill03<-read.csv("SLC2003.csv", stringsAsFactors = FALSE)
#    drill04<-read.csv("SLC2004.csv", stringsAsFactors = FALSE)
#    drill05<-read.csv("SLC2005.csv", stringsAsFactors = FALSE)
#    drill06<-read.csv("SLC2006.csv", stringsAsFactors = FALSE)
#    drill07<-read.csv("SLC2007.csv", stringsAsFactors = FALSE)
    drill08<-read.csv("SLC2008.csv", stringsAsFactors = FALSE)
    drill09<-read.csv("SLC2009.csv", stringsAsFactors = FALSE)
    drill10<-read.csv("SLC2010.csv", stringsAsFactors = FALSE)
    drill11<-read.csv("SLC2011.csv", stringsAsFactors = FALSE)
    drill12<-read.csv("SLC2012.csv", stringsAsFactors = FALSE)
    drill13<-read.csv("SLC2013.csv", stringsAsFactors = FALSE)
    drill14<-read.csv("SLC2014.csv", stringsAsFactors = FALSE)
    drill15<-read.csv("SLC2015.csv", stringsAsFactors = FALSE)
    drill16<-read.csv("SLC2016.csv", stringsAsFactors = FALSE)
    drill17<-read.csv("SLC2017.csv", stringsAsFactors = FALSE)
    
    second_el_to_numeric <- function(ls){
      map(ls, function(x){
        x[[2]] <- as.numeric(x[[2]])
        x
      }) }
    
#    dsSLC2003 <- second_el_to_numeric(list_parse2(drill03))
#    dsSLC2004 <- second_el_to_numeric(list_parse2(drill04))
#    dsSLC2005 <- second_el_to_numeric(list_parse2(drill05))
#    dsSLC2006 <- second_el_to_numeric(list_parse2(drill06))
#    dsSLC2007 <- second_el_to_numeric(list_parse2(drill07))
    dsSLC2008 <- second_el_to_numeric(list_parse2(drill08))
    dsSLC2009 <- second_el_to_numeric(list_parse2(drill09))
    dsSLC2010 <- second_el_to_numeric(list_parse2(drill10))
    dsSLC2011 <- second_el_to_numeric(list_parse2(drill11))
    dsSLC2012 <- second_el_to_numeric(list_parse2(drill12))
    dsSLC2013 <- second_el_to_numeric(list_parse2(drill13))
    dsSLC2014 <- second_el_to_numeric(list_parse2(drill14))
    dsSLC2015 <- second_el_to_numeric(list_parse2(drill15))
    dsSLC2016 <- second_el_to_numeric(list_parse2(drill16))
    dsSLC2017 <- second_el_to_numeric(list_parse2(drill17))
    
    historical_median_sale <- historical_median_sale %>%
      hc_drilldown(
        allowPointDrilldown=TRUE, 
        series=list(
#         list(
#          id="2003 year",
#          data= dsSLC2003,
#          name="sale median prices 2003",
#          color='#8bbc21'
#        ),
#        list(
#          id="2004 year",
#          data= dsSLC2004,
#          name="sale median prices 2004",
#          color='#8bbc21'
#        ),
#        list(
#          id="2005 year",
#          data= dsSLC2005,
#          name="sale median prices 2005",
#          color='#8bbc21'
#        ),
#        list(
#          id="2006 year",
#          data= dsSLC2006,
#          name="sale median prices 2006",
#          color='#8bbc21'
#        ),
#        list(
#          id="2007 year",
#          data= dsSLC2007,
#          name="sale median price 2007",
#          color='#8bbc21'
#        ),
        list(
          id="2008 year",
          data= dsSLC2008,
          name="sale median prices 2008",
          color='#8bbc21'
        ),
        list(
          id="2009 year",
          data= dsSLC2009,
          name="sale median price 2009",
          color='#8bbc21'
        ),
        list(
          id="2010 year",
          data= dsSLC2010,
          name="sale median prices 2010",
          color='#8bbc21'
        ),
        list(
          id="2011 year",
          data= dsSLC2011,
          name="sale median prices 2011",
          color='#8bbc21'
        ),
        list(
          id="2012 year",
          data= dsSLC2012,
          name="sale median prices 2012",
          color='#8bbc21'
        ),
        list(
          id="2013 year",
          data= dsSLC2013,
          name="sale median prices 2013",
          color='#8bbc21'
        ),
        list(
          id="2014 year",
          data= dsSLC2014,
          name="sale median prices 2014",
          color='#8bbc21'
        ),
        list(
          id="2015 year",
          data= dsSLC2015,
          name="sale median prices 2015",
          color='#8bbc21'
        ),
        list(
          id="2016 year",
          data= dsSLC2016,
          name="sale median prices 2016",
          color='#8bbc21'
        ),
        list(
          id="2017 year",
          data= dsSLC2017,
          name="sale median prices 2017",
          color='#8bbc21'
        )
        )
      ) 
    
    print(historical_median_sale)
  }
  )
  output$plot10<-renderHighchart({ 
    ownerRenter4<-highchart()%>%
      hc_chart(type="column")%>%
      hc_title(text="Salt Lake City Housing Stock by Owner vs Renter: 2014")%>%
      hc_xAxis(categories = c("All units", "Owners", "Renters")) %>%
      hc_series(list(name ="Housing stock units", 
                     data=c(81715, 34697, 41226), dataLabels=list(enabled=TRUE,format= "{point.y}")))%>%
      print(ownerRenter4)
  }
  )
  
  output$plot11<-renderHighchart({ 
    ownerRenter1<-highchart()%>%
      hc_chart(type="pie") %>%
      hc_title(text = "Salt Lake City: All Units") %>%
      hc_add_series_labels_values(labels = c("Attached: 10 or more units", "Attached: fewer than 10 units", "Single family detached"), 
                                  values =c(28, 23, 49), size=150, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of all units'))%>%
      print(ownerRenter1)
  }
  )
  
  output$plot12<-renderHighchart({ 
    ownerRenter2<-highchart()%>%
      hc_chart(type="pie") %>%
      hc_title(text = "Salt Lake City: Owners Units") %>%
      hc_plotOptions(series = list(showInLegend = TRUE)) %>% 
      hc_add_series_labels_values(labels = c("Attached: 10 or more units", "Attached: fewer than 10 units", "Single family detached"), 
                                  values =c(9, 8, 83), size=150, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of owners'))%>%
      print(ownerRenter2)
  }
  )
  output$plot13<-renderHighchart({ 
    ownerRenter3<-highchart()%>%
      hc_chart(type="pie") %>%
      hc_title(text = "Salt Lake City: Renters Units") %>%
      hc_add_series_labels_values(labels = c("Attached: 10 or more units", "Attached: fewer than 10 units", "Single family detached"), 
                                  values =c(44, 36, 20), size=150, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of renters'))%>%
      print(ownerRenter3)
  }
  )
  
  output$plot14<-renderHighchart({ 
    age<-highchart()%>%
      hc_chart(type="bar")%>%
      hc_title(text="Age of Housing Stock: Salt Lake City, 2014")%>%
      hc_xAxis(categories = c("Built 2000 or later", "Built 1980 to 1999", 
                              "Built 1960 to 1979", "Built 1940 to 1959", "Built 1939 or earlier")) %>%
      hc_yAxis(labels=list(format= "{value}%")) %>%
      hc_series(list(name ="Salt Lake City", 
                     data=c(9, 13, 23, 24, 32), dataLabels=list(enabled=TRUE,format= "{point.y}%")),
                list(name ="Salt Lake County", 
                     data=c(19, 29, 29, 14, 9), dataLabels=list(enabled=TRUE,format= "{point.y}%")))
    
    print(age)
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
  output$graph4<-renderHighchart({
    wageVsPrice<-highchart() %>%
    hc_chart(type="column") %>%
      hc_title(text = "Wage Increase vs Home Sale Price Increase: 2011-2014") %>%
      hc_yAxis(title = list(text = "increase in percentage"),
               labels=list(format= "{value}%")) %>%
      hc_xAxis(categories = c("Increase in homeowner wages", "Increase in home sale prices")) %>%
      hc_series(list(name="increase rate", data=c(8, 33),
                     colorByPoint=TRUE)) %>%
      hc_plotOptions(series = list(boderWidth = 0,
                                   dataLabels = list(enabled = TRUE, format="{y}%") )) %>%
      print(wageVsPrice)
  }
  )
  output$graph5<-renderHighchart({
    wageVsRent<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text = "Wage Increase vs Rent Increase: 2011-2016") %>%
      hc_yAxis(title = list(text = "increase in percentage"),
               labels=list(format= "{value}%")) %>%
      hc_xAxis(categories = c("Increase in renter wages", "Increase in rent prices")) %>%
      hc_series(list(name="increase rate", data=c(4, 26),
                     colorByPoint=TRUE)) %>%
      hc_plotOptions(series = list(boderWidth = 0,
                                   dataLabels = list(enabled = TRUE, format="{y}%") )) %>%
      print(wageVsRent)
  }
  )
  output$home <- renderLeaflet({
    home_map
  })
  
  output$rent <- renderLeaflet({
    rent_map
  })
  
  output$a_out <- renderText({
    paste0(" HUD defines cost-burdened families as those “who pay more than 30 percent of their income for housing” and “may have difficulty affording necessities such as food, clothing, transportation, and medical care.” Severe rent burden is defined as paying more than 50 percent of one’s income on rent. The median rent in ", as.character(input$neighborhood_type), " is ", 
           dollar(neighborhoodRent$a_rent[which(neighborhoodRent$neighboarhood==input$neighborhood_type)]), 
           ". Therefore, a household with income below ", dollar(as.numeric(neighborhoodRent$a_rent[which(neighborhoodRent$neighboarhood==input$neighborhood_type)]) * 12 / .3), " would be considered cost-burdened. Below ", dollar(as.numeric(neighborhoodRent$a_rent[which(neighborhoodRent$neighboarhood==input$neighborhood_type)]) * 12 / .5), " would be severely rent burdened.")
  })

}

shinyApp(ui, server)
