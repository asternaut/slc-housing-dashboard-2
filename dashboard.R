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
source("tidycensus.R")
library("treemap")
library("viridis") 
source("goals.R")

# read and prepare datasources for visulization ####
neighborhoodRent<-read.csv("Data/rentAve.csv")
historicalVacancy<-read.csv("Data/vacancyHis.csv")
incomeMed<-read.csv("Data/incomeMedian.csv")
multi<-read_excel("Data/Multifamily.xlsx", sheet = "Multi-Family Listings" )
constructionTrend<-read_xlsx("Data/yearly_construction_permit_total.xlsx")
# ownerVsRenter<-read.csv("Data/housingStock_ownerVsRenter.csv")
allUnitsRatio<-read.csv("Data/allUnits.csv")
ownersUnitsRatio<-read.csv("Data/ownersUnits.csv")
rentersUnitsRatio<-read.csv("Data/rentersUnits.csv")
houseAge <- read.csv("Data/houseAge.csv")
costBurden <- read.csv("Data/costBurden.csv")
incomeLevels <- read.csv("Data/incomeLevels.csv")
averageRentsVsAffordability <- read.csv("Data/averageRentsVsAffordability.csv")
wageIncreaseVsHomeSalePrice <- read.csv("Data/wageIncreaseVsHomeSalePrice.csv")
wageIncreaseVsRent <- read.csv("Data/wageIncreaseVsRent.csv")
# new housing units
permit17 <- read_excel("Data/SLC_new_units.xlsx", sheet = "Sheet1")
permitSinVsMul<- permit17 %>%
  select(`Duplexes and Twin Homes`, `Condominiums / Townhomes`, `Apartments (3 or more units)`) %>%
  mutate(multifamily=`Duplexes and Twin Homes`+`Condominiums / Townhomes`+`Apartments (3 or more units)`)
# industry ami and median income in "How"
industryChart<-read.csv("Data/industryC.csv")
tm <- treemap(industryChart, index =c("ami","profession"),
              vSize = "income", vColor = "income",
              type = "value", palette = rev(viridis(10)),
              draw = FALSE)
# SL City historical sale median price csv file: y value in "historical_median_3rdQuarter.csv" is written from "cityWAve"
cityMedian<-read_xlsx("Data/cityHisMedian.xlsx")
cityWAve <- sapply(split(cityMedian, cityMedian$year), function(x){weighted.mean(x$medianPrice, x$unitsSold)})
WAve <-data.frame(keyName=names(cityWAve), value=cityWAve, row.names=NULL)

# design webpage sidebar menuitems ####
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
# welcome page design ####
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
         img(src='SLC_housing_dashboard.png',class="welcomeImage") 
       ),
       
       br(),
       
       fluidRow(class="headerText",
                h2("Salt Lake City is experiencing a systemic housing crisis that has implications for every resident and business.")
       ),
       p("Resolving the crisis will require a community wide effort. Salt Lake City’s Housing and Neighborhood Development Division worked collaboratively to draft a plan to address the root causes of affordability, create long-term solutions for increasing needed housing supply, and expand opportunities throughout the City. The 5 year plan is called Growing SLC and was unanimously adopted by City Council in December 2017. This site is a critical component that provides data on housing market performance and progress towards fulfilling the objectives of the plan."),
       br(),
       fluidRow(class="footerBox",
                p(HTML(paste0(
                  'Daniel Hadley, Hua Jiang, & Suyash Thite | ', a(href = 'https://github.com/Sorenson-Impact/SLC-Housing-Dashboard', 'Code'),'!'))),
                img(src='si_desb_hand_logo.png',class="getImage"),
                br()
       )
     ),
    # create dashboard boxes ####
    tabItem(tabName = "dashboard",
            # dashboard header image ####
            fluidRow(class="getBox",
                     img(src='dashboard.png',class="getImage") 
            ),
            
            br(),br(),
            # insert texts for "growing pains" ####
            fluidRow(
              fluidRow(class="headerText",
              h1("Growing Pains & Housing Gains: A look at long-term housing affordability")
              ),
              p("Salt Lake City’s housing market has been experiencing a boom since the end of the Great Recession. However, even with outsized new construction rates, vacancy rates are at an all-time low (around 2%), driving up housing prices across the city.  The unprecedented growth in population supports a vibrant city in which many want to live and work, but it only currently serves those with high incomes."),
              p("The Growing SLC plan, unanimously adopted by the City Council in December 2017, aims to address the root causes of housing affordability, increase the much-needed housing supply, and expand opportunities for residents throughout the City."),
              p("The affordable housing crisis has implications for every Salt Lake City resident and business. Resolving this crisis requires considering different issues like high home prices and rental rates,the pace of wage increases, and the economic inequities in the market."),
              p("The following graphics illustrate the existing barriers to be addressed, and help inform solutions to Salt Lake City’s housing crisis.")
            ),
            # create 2 boxes for SLC housing stock makeup ####
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City Housing Stock Makeup")
              ),
              p("A majority of Salt Lake City’s housing stock was built before 1940, indicating greater chances that dilapidation, blight, and unsafe conditions may exist. In fact, nearly 1,000 units of the nearly 82,000 total units lack key facilities such as plumbing or complete kitchens.
"),
              p("A key challenge that is unique to this market is the unusual age and type of existing housing stock. To meet the affordability needs of the city's low-income renters (those earning $20,000 and less per year), 7,500 additional rental units are needed."),
              p("The graph below on the left shows the number of Salt Lake City housing stock units in 2014 by owner vs. renter. The graph below on the right shows the number of housing stock units by age in Salt Lake City."),
              #tableOutput(incomeMed),
              box(highchartOutput("plot10", height = 500)),
              box(highchartOutput("plot14", height = 500))
            ),
            br(),br(), br(),
            # Opportunity chart and texts ####
            fluidRow(
              fluidRow(class="headerText",
                       h1("Salt Lake City Opportunity Index")
              ),
                       p("Neighborhood-level data help define the need for services as well as measure access to
                        opportunity for the local population. Measuring and mapping the access to
                        various opportunities, we can begin to understand some of the empirical differences 
                        between neighborhoods. Once we have grasp on this, the City can begin making strategic 
                        investments to help expand the opportunities throughout the city."),
                        p("This map displays a 
                        variety of indicators that are important in guiding these investments, including employment rate, 
                         income level, household cost burden, rate of homeownership, and educational attainment."),
              box(opportunity_index_map,width = NULL)
            ),
            # 3 boxes for housing type by tenure charts ####
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City Housing Type by Tenure: 2014")
              ),
              p("About half of the housing is single-family detached, which consumes large lots and is generally unaffordable for many low-income households. The other half consists primarily of apartments, duplexes, and condos."),
              p("However, the vast majority of rental units (80%) has only two bedrooms, thus amplifying both the need for new units, but also increased affordability for families that are renting."),
              p("This graph shows the type of housing units by type, attached (more than 10 units), attached (fewer than 10 units) and single family detached."),
              column(width=4, box(highchartOutput("plot11", height = 400), width=NULL)),
              column(width=4,box(highchartOutput("plot12", height = 450), width=NULL)),
              column(width=4,box(highchartOutput("plot13", height = 400), width=NULL)),
              p("Datasource from BBC Housing Market Study 2016")
            ),
            br(),br(), br(),
            # create a box for Affordable ratio in comparison with all multifamily units ####
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City's Multi-Family Units: Affordable vs. Market Rate vs. Mixed")
              ),
              p("Salt Lake City has seen a market rate multifamily boom with rents at all-time highs and vacancy rates at historic lows. However, while the market rate apartment inventory continues to grow, affordable multi-family units have lost ground, even with the addition of new units."),
              box(highchartOutput("plot1", height = 600), width=NULL),
              p("Datasource from HAND and ACS 2016")
            ),
            br(),br(), br(),
            # create a box for SLC new residential housing stock ####
            fluidRow(
              fluidRow(class="headerText",
              h2("Salt Lake City's New Residential Housing Stock in 2017")
              ),
              p("Salt Lake City is experiencing tremendous residential growth with new homes and apartment buildings being constructed in all communities. Due to low vacancy rates and all-time high rental rates, the increase in housing costs is far outpacing incomes."),
              p("The graph shows the number and type of new residential units coming up in Salt Lake City in 2017."),
              box(highchartOutput("plot3", height = 450), width=NULL),
              p("Datasource from Ivory Boyer database")
            ),
            br(),br(), br(),
            # create a box for yearly construction trend ####
            fluidRow(
              p("The following chart shows the construction trend within most recent 5 years."),
              br(),
              box(highchartOutput("plot4", height = 400), width=NULL),
              p("Datasource from Ivory Boyer database and HAND")
            ),
            br(),br(), br(),
            # average rent chart box and affordability calculator box ####
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("Average Rent by Neighborhood")
                     ),
                     p("Many affordable units throughout the city are currently being leased at higher rental rates due to market demand. In the fastest growing areas of the city, such as Downtown and Sugarhouse, affordable units are being sold and converted to housing for residents with higher incomes."),
                     p("In Salt Lake City, nearly one half of the renters are cost burdened, and nearly one quarter are extremely cost-burdened (spend more than 50% of their income on rent)."),
                     p("The graph shows that the neighborhoods of Sugar House, Central City, and Central City-Liberty Wells are the most expensive apartment rentals. The least expensive neighborhoods are Poplar Grove, Liberty Wells, and Rose Park. The data comes from Rent Jungle, which uses data aggregation to estimate local rents.")
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

            # create a box for historical vacancy rates ####
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("Historical Vacancy Rates")
                     ),
                     p("With rental vacancy rates at historic lows, the city requires a larger supply of rentals to not only accommodate demand, but to address the needs of lower income renters. For lower income renters, it is important that the rental stock priced below $500 increases (either through market production, subsidy or both)."),
                     p("The graph below shows Salt Lake City's most recent three-year vacancy rates. Data shown here is based on rentals with square footages between approximately 800 SF and 1,000 SF. CBRE collects and interprets this data to offer their perspective on the trends of the real estate market."),
                     box(highchartOutput("plot6", height=400), width=NULL),
                     p("Datasource from CBRE, inc")
              )
              ),
            br(),br(), br(),
            # create a box for the comparison of home prices and median income ####
            fluidRow(
              column(width = 12,
                     fluidRow(class="headerText",
                     h2("The Growing Affordability Gap: Home Prices Vs. Income")
                     ),
                     p("Like many housing markets across the country, Salt Lake City has experienced substantial increases in home values since early 2012. By the end of 2014, the median sale price of $235,000 exceeded the 2007 peak median sale price of $223,751."),
                     p("Unfortunately, incomes have not risen at the same rate as housing prices."),
                     p("The graph shows the historical trend of 3rd quarter median sale prices in Salt Lake City from 2003 to 2017."),
                     box(highchartOutput("plot9", height=500), width=NULL),
                     p("Datasource from The Salt Lake Tribune")
              )
            ),
          br(),br(), br(),
          # create a box for cost burden chart ####
          fluidRow(
            column(width = 12,
                 fluidRow(class="headerText",
                  h2("Cost Burden: Salt Lake City")
                  ),
                 p("In addition to income, it is important to consider residents' housing expenses relative to their income. Residents spending 30 percent or more of their income on housing are said to be 'cost burdened'
                   and residents spending 50 percent or more of their income on housing are said to be 'severely cost burdened'."),
                 p("Nearly half (49%) of all renters (18,672 households) in Salt Lake City are cost burdened. Twenty-three percent of renters are severely cost burdened. Owners are far less likely to be cost-burdened:
                   in Salt Lake City 22 percent of owners (7,599 households) are cost burdened and 8 percent are severely cost burdened."),
                 p("The graph displays housing costs as a percentage of monthly income for Salt Lake City households."),
                 box(highchartOutput("plot15", height=400), width=NULL),
                 p("Datasource from 2014 ACS and BBC Research & Consulting")
           )
         )
    ),
    # design "how did we get here" page boxes ####
    tabItem(tabName = "how",
          # get header for "how" webpage ####  
          fluidRow(class="getBox",
                     img(src='how.png',class="getImage") 
            ),
            br(),br(),
          # insert introductory texts and snapshot ####  
          fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h1("Salt Lake City Has a Unique Population, and It's Growing Quickly.")
                     ),
                     p("Salt Lake City is also experiencing a housing crisis where affordable housing is becoming more scarce. Challenges in housing residents will have widespread implications for every resident and business."),
                     p("In the face of these challenges, Salt Lake City Housing and Neighborhood Development Division (HAND) sees the opportunity to find meaningful and lasting solutions that can bring stability to residents by providing housing that is safe, secure and affordable."),
                     p("The Growing SLC Five Year Plan is a response to these challenges and proposes a fundamental shift to how housing is prioritized in the city. This site provides data on the housing market performance and progress towards fulfilling the objectives of the Growing SLC Plan."),
                     fluidRow(class="headerText",
                     h2("Snapshot Salt Lake: Summary")
                     ),
                     p("Data is the key to understanding how our city is growing and developing, what barriers and challenges exist when solving the affordable housing crisis, and how system design can create a more equitable place to live. This section will focus on the story the data shows about the city’s growth and development and how that affects the residents of the city. ")
              )
            ),
            br(),br(), br(),
            # create a box for SLC AMI chart ####
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h2("Salt Lake City AMI and Affodable Housing")
                     ),
                     br(),
                     p("Area Median Income (the midpoint of a region’s income distribution) was the metric used to understand the relation between income and housing affordability. Residents with household income that is less than 60% (about $45,000) are those who struggle to make housing payments. Affordability implies spending no more than 30% of the income on housing costs."), 
                     br(),
                     box(highchartOutput("graph1", height = 600), width=NULL),
                     p("Datasource from HUD 2017")
                     )
              ),
            br(),br(), br(),
            # create a box for wages for industries chart #### 
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                     h2("SLC's Average Annual Wages for the Top 10 Industries")
                     ),
                     br(),
                     p(" This tree map represents top 10 industries in Salt Lake City with respective average annual wages and the AMI percentages for each industry."),
                     p("Income is one of the most significant barriers to meeting the housing needs of Salt Lake City’s residents. The area median income for residents in Salt Lake City is nearly $20,000 less than that of the County as a whole, at $46,711. In addition, only two of the five largest employment industries in Salt Lake City pay wages high enough to afford the city’s median home price of $271,000. Thus, affording a home on a single income may be difficult for those in other industries, requiring both adults in the household to work."), 
                     br(),
                     p("Datasource from HUD 2017"),
                     box(highchartOutput("graph2", height = 600), width=NULL)
                     )
              ),
            br(),br(), br(),
            # create a box for comparison between wages and rents chart ####
            fluidRow(
              column(width=12,
                     fluidRow(class="headerText",
                              h2("The growing disparity between wages and rental rates")
                     ),
                     br(),
                     p("Although Salt Lake City is amid an unprecedented building boom, it has yet to keep pace with the rising numbers of people who want to call the city home. "),
                     p("Rising rents and vacancy rates of 2% are driving more and more city residents to either seek housing elsewhere or live burdened with housing costs that exceed 30%, and in some cases, more than 50%, of their household income."),
                     p("As a comparison, a single person household in Salt Lake County has an Area Median Income (AMI) of $51,690; the AMI for a family of four is $73,800. The graph shows a $470 average monthly gap between affordable rent for a one-person household and a one-bedroom average rent plus utilities, and $610 average monthly affordable gap between affordable rent for a four-person household and three-bedroom average rent plus utilities."),
                     br(),
                     box(highchartOutput("graph3", height = 500), width=NULL),
                     p("Datasource from CBRE 2016")
                     )
            ),
           br(),br(), br(),
           # 2 boxes for wage increases in comparison to home prices increases and rent increases ####
           fluidRow(
             h2("Wage Increase vs Price Icreases"),
             p("Homeownership is not exempt from the housing boom, nor are those who desire to purchase a home exempt from feeling excluded from the market."),
             p("This steep rise in prices has created a market in which most homes for sale are only affordable for those in high-income brackets."),
             p("Home sale prices increased 33% between 2011 and 2014, while homeowner wages increased only 8%. The rent increase of 26% is adding great pressure for renters who have only a 4% wage increase.
             "),
             box(highchartOutput("graph4", height = 400)),
             box(highchartOutput("graph5", height = 400)),
             p("Datasource from BBC Housing Market Study 2016")
   )
    ),
   # design "goals" page ####
    tabItem(tabName = "goals",
            
            fluidRow(class="getBox",
                     img(src='goals.png',class="getImage") 
            ),
            fluidRow(
              column(width=10,
             
                h3("Goal 1: INCREASE HOUSING OPTIONS: REFORM CITY PRACTICES TO PROMOTE A RESPONSIVE, AFFORDABLE, HIGH-OPPORUNITY HOUSING MARKET"),
             
              p("In order to respond to Salt Lake City’s changing demographics and the housing needs of its diverse communities, it is critical to begin to look within the City for real and responsive change that will encourage the market to develop the housing and infrastructure needed to accommodate our growing community. This goal focuses on the need to increase the diversity of housing types and opportunities in the city by seeking policy reforms that can enhance the flexibility of the land-use code and create an efficient and predictable development process for community growth. Strategic policy decisions that integrate the transportation system, development related infrastructure, financial institutions, and data, as well as innovative design and construction methods,
                can break down social and economic segregation, thus building a city for everyone."),
              p("Objective 1: Review and modify land-use and zoning regulations to reflect the affordability needs of a growing, pioneering city"),
              tableOutput("goal11"),
              p("Objective 2: Remove impediments in City processes to encourage housing development."),
              tableOutput("goal12"),
              p("Objective 3: Lead in the construction of innovative housing solutions."),
              tableOutput("goal13"),
              p("Objective 4: Provide residents, community advocates, business leaders, and elected officials with high-quality data to drive decision-making."),
              tableOutput("goal14"),
                       h3("Goal 2: AFFORDABLE HOUSING: INCREASE HOUSING OPPORTUNITIES AND STABILITY FOR COST-BURDENED HOUSEHOLDS"),
              
                       p("This goal is dedicated to serving and addressing the needs of those most vulnerable in our community. It is driven by a strong belief that housing stability is good for the entire city, adding income to small businesses, creating food stability for children, and allowing residents to enrich their neighborhoods. Salt Lake City needs to pursue a combination of strategies outlined in the objectives below to achieve this goal.
                        There is no singular initiative that will resolve this crisis, 
                         it must be addressed with a range of strategies to best fit the diverse needs of our entire community."),
                       p("Objective 1: Prioritize the development of new affordable housing with an emphasis on households earning 40% AMI and below."),
                       tableOutput("goal21"),
                       p("Objective 2: Pursue funding for affordable housing opportunities."),
                       tableOutput("goal22"),
                       p("Objective 3: Stabilize very low-income renters."),
                       tableOutput("goal23"),
                       p("Objective 4: Secure and preserve long-term affordability."),
                       tableOutput("goal24"),
                       p("Objective 5: Work with landlords to improve their housing stock and rent to very low-income households earning 40% AMI and below."),
                       tableOutput("goal25"),
                       p("Objective 6: Increase home ownership opportunities."),
                       tableOutput("goal26"),
            
                       h3("Goal 3: EQUITABLE & FAIR HOUSING: BUILD A MORE EQUITABLE CITY"),
              
                       p("Equity is not only about eliminating discrimination, 
                         it is also about increasing access to opportunity. 
                         One of the guiding principles of Plan Salt Lake is to create an equitable city by ensuring 
                         “access to all city amenities for all citizens while treating everyone equitably with fairness, 
                         justice, and respect.” The City will accomplish this by working to eliminate housing discrimination, 
                         strategically investing in neighborhoods that stand the most to gain, and building a city that meets needs of a 
                         diverse population."),
                       p("Objective 1: Eliminate incidences of housing discrimination in Salt Lake City."),
                       tableOutput("goal31"),
                       p("Objective 2: Align resources and invest in strategic expansion of opportunity throughout all neighborhoods of the city and access to existing areas of opportunity."),
                       tableOutput("goal32"),
                      p("Objective 3: Implement life cycle housing principles in neighborhoods throughout the city."),
                      tableOutput("goal33")
              ),
              
              
              
              withTags({
                div(class="header", checked=NA,
                    p("Want to check out the plan?" , a("Click Here",target="_blank",href="Growing SLC.pdf"))
                    
                )
              })
              )
            )
            
    )
    )
    )

# Put items together into a dashboardPage ####
ui <- dashboardPage(
  dashboardHeader(title = tags$a(href='http://www.slcgov.com/hand',
                                 tags$img(src='handlogo.png', class="logoImage"))),
  sidebar,
  body
)

# Server ####
server <- function(input, output) {
  project<- 200
  company<-120
  house<-3000
  # create a bar chart and a column chart in combination to show affordable units in multifamily units ####
  output$plot1<-renderHighchart({
    multifamily_plot<-highchart() %>%
      hc_title(text = "Affordable Units in Total Multi-Family Units") %>%
      hc_xAxis(categories = c("Affordable uniits", "Total Multi-family units")) %>%
      hc_yAxis(title = list(text = "Number of Units")) %>%
      hc_plotOptions(series = list(colorByPoint = TRUE),
                     column = list(dataLabels = list(enabled = TRUE)),
                     pie = list(colorByPoint = TRUE, center = c('30%', '10%'),
                                size = 150, dataLabels = list(enabled = TRUE))
      )%>%
      hc_add_series_labels_values(labels = c("Affordable units percentage", "Other units percentage"), dataLabels = list(enabled = TRUE),
                                  type="pie", name="Multifamily percentage",
                                  values =c(subset(multi, `Project Name` == "affordable percentage")$`Affordable Units`, 
                                            subset(multi,`Project Name` == "affordable percentage")$`Other Multifamily Units`))%>%
      hc_add_series(data=c(subset(multi, `Project Name` == "total")$`Affordable Units`, 
                           subset(multi,`Project Name` == "total")$`Multifamily Units Total`), 
                    type="column", name="Multifamily units" ) %>%
      
      print(multifamily_plot)
  })
  # create a column chart for new residential units ####
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
  # create a column chart for yearly constructioin trend ####
  output$plot4<-renderHighchart({
    yearly_construction_trend<-highchart() %>%
    hc_chart(type="column") %>%
      hc_title(text = "Salt Lake City's Yearly Construction Trend: 2013-2017") %>%
      hc_yAxis(title = list(text = "Number of homes")) %>%
      hc_xAxis(categories = constructionTrend$year) %>%
      hc_series(list(name="Single-family", data=constructionTrend$`single family home numbers`),
                list(name="Duplexes and twin homes", data=constructionTrend$`duplex and twin home numbers`),
                list(name="Condominiums / Townhomes", data=constructionTrend$`condominium/townhouse numbers`),
                list(name="Apartments (3 or 4 units)", data=constructionTrend$`apartment (3 or 4 units) numbers`),
                list(name="Apartments (1-3 floors)", data=constructionTrend$`apartment (1-3 floor) numbers`),
                list(name="Apartments (4+ floors)", data=constructionTrend$`apartment (4+ floor) numbers`)
      )%>%
      print(yearly_construction_trend)  
  })
  # create a bar chart for rent by neighborhood ####
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
  # create a column chart for vacancy rate ####
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
  # create column chart of median sale price with a line cross that shows median income ####
  output$plot9<-renderHighchart({
    historical_median_sale<-highchart() %>%
      hc_title(text="Salt Lake City Sale Median Price vs Median Income: 3rd Quarter 2008 - 2017") %>%
      hc_yAxis(labels=list(format="${value}")) %>%
      hc_xAxis(categories=incomeMed$year, labels=list(align="left")) %>%
      hc_plotOptions(
        line = list(dataLabels = list(enabled = TRUE)),
        column = list(dataLabels = list(enabled = TRUE))
      )%>%
      hc_add_series(name="SLC weighted average sale median prices", type="column", data=round(WAve$value, -2),
                    dataLabels=list(enabled=TRUE,format= "${point.y:,.0f}"), colorByPoint=FALSE,
                    color="#d3d3d3") %>%
      hc_add_series(name="SLC median household income", data=round(incomeMed$median, -2), type="line",
                    dataLabels=list(enabled=TRUE, format="${point.y:,.0f}"),
                    color="red", markerOptions=list(enabled=FALSE,lineWidth=2))
    print(historical_median_sale)
  }
  )
  # a pie chart for housing stock by owner vs renter ####
  output$plot10<-renderHighchart({ 
    ownerRenter4<-highchart()%>%
      hc_chart(type="pie")%>%
      hc_title(text="Salt Lake City Housing Stock by Owner vs Renter: 2014")%>%
      hc_plotOptions(series = list(showInLegend = TRUE)) %>% 
      hc_add_series_labels_values(labels = c("Owner's housing units", "Renter's housing units"), 
                                  values =c(round(100*(34697/81715),2), round(100*(41226/81715),2)), size=200, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of all housing stock'))%>%
      print(ownerRenter4)
  }
  )
  # 3 pie charts created for housing by tenure ####
  output$plot11<-renderHighchart({ 
    ownerRenter1<-highchart()%>%
      hc_chart(type="pie") %>%
      hc_title(text = "Salt Lake City: All Units") %>%
      hc_add_series_labels_values(labels = c("Attached: 10 or more units", "Attached: fewer than 10 units", "Single family detached"), 
                                  values =allUnitsRatio$ratio, size=150, dataLabels = list(enabled = FALSE))%>%
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
                                  values =ownersUnitsRatio$ratio, size=150, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of owners'))%>%
      print(ownerRenter2)
  }
  )
  
  output$plot13<-renderHighchart({ 
    ownerRenter3<-highchart()%>%
      hc_chart(type="pie") %>%
      hc_title(text = "Salt Lake City: Renters Units") %>%
      hc_add_series_labels_values(labels = c("Attached: 10 or more units", "Attached: fewer than 10 units", "Single family detached"), 
                                  values =rentersUnitsRatio$ratio, size=150, dataLabels = list(enabled = FALSE))%>%
      hc_tooltip(pointFormat = paste('{point.y}%  of renters'))%>%
      print(ownerRenter3)
  }
  )
  # create a bar chart to show housing stock age ####
  output$plot14<-renderHighchart({ 
    age<-highchart()%>%
      hc_chart(type="bar")%>%
      hc_title(text="Age of Housing Stock: Salt Lake City, 2014")%>%
      hc_xAxis(categories = c("Built 2000 or later", "Built 1980 to 1999", 
                              "Built 1960 to 1979", "Built 1940 to 1959", "Built 1939 or earlier")) %>%
      hc_yAxis(labels=list(format= "{value}%")) %>%
      hc_series(list(name ="Salt Lake City", 
                     data=houseAge$percentage, dataLabels=list(enabled=TRUE,format= "{point.y}%"))
                )
    
    print(age)
  }
  )
  # create a bar chart to show cost burden of SLC ####
  output$plot15<-renderHighchart({
    cost_burden<-highchart()%>%
      hc_chart(type="bar")%>%
      hc_title(text="Cost Burden: Salt Lake City")%>%
      hc_xAxis(categories = c("Less than 15%", "15% to 29.9%", "30% to 49.9%", "50% and more")) %>%
      hc_yAxis(labels=list(format= "{value}%")) %>%
      hc_series(list(name ="Owners with a mortgage", 
                     data=costBurden$owners_with_a_mortgage, dataLabels=list(enabled=TRUE,format= "{point.y}%")),
                list(name ="Renters", 
                     data=costBurden$renters, dataLabels=list(enabled=TRUE,format= "{point.y}%")),
                list(name ="Owners without a mortgage", 
                     data=costBurden$owners_without_a_mortgage, dataLabels=list(enabled=TRUE,format= "{point.y}%"))
      )
    print(cost_burden)
  }
  )
  # create a bar chart for city AMI in "how did we get here" page ####
  output$graph1<-renderHighchart({
    AMI_plot<-highchart() %>%
      hc_chart(type="bar") %>%
      hc_title(text = "Salt Lake City MSA Income Levels in 2017") %>%
      hc_yAxis(title = list(text = "Income in dollars")) %>%
      hc_xAxis(categories = c("1 person", "2 people", "3 people", 
                              "4 people", "5 people", "6 people",
                              "7 people", "8 people"),
               title = list(text = "Household sizes")) %>%
      hc_series(list(name="Extremely low income 30% AMI $", data=incomeLevels$extremelyLowAMI),
                list(name="Very low income 50% AMI $", data=incomeLevels$veryLow),
                list(name="Moderately low income 60% AMI $", data=incomeLevels$moderatelyLow),
                list(name="Low income 80% AMI $", data=incomeLevels$low),
                list(name="100% AMI $", data=incomeLevels$X100..AMI)
      )%>%
      print(AMI_plot)
  }
  )
  # create a treemap for income in industries in SLC ####
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
  # create a column chart for average rents and affordability ####
  output$graph3<-renderHighchart({
    affordability1<-highchart() %>%
      hc_chart(type="column") %>%
      hc_title(text= "Salt Lake City Average Rents vs Affordability (80% AMI)") %>%
      hc_xAxis(categories = c("one-person household and 1Br average rent + utilities", 
                              "four-person household and 3Br average rent + utilities")) %>%

      hc_yAxis(labels=list(format= "${value}"))%>%
      hc_series(list(name ="Affordable rent", 
                     data=averageRentsVsAffordability$affordable_rent, dataLabels=list(enabled=TRUE,format= "${point.y}")),
                list(name = "Average rent",
                     data=averageRentsVsAffordability$average_rent, dataLabels=list(enabled=TRUE,format= "${point.y}"))

      ) %>%
      print(affordability1)
  }
  )
  # create two chart for the comparison between wage increase and home sale and rent increase ####
  output$graph4<-renderHighchart({
    wageVsPrice<-highchart() %>%
    hc_chart(type="column") %>%
      hc_title(text = "Wage Increase vs Home Sale Price Increase: 2011-2014") %>%
      hc_yAxis(title = list(text = "increase in percentage"),
               labels=list(format= "{value}%")) %>%
      hc_xAxis(categories = c("Increase in homeowner wages", "Increase in home sale prices")) %>%
      hc_series(list(name="increase rate", data=wageIncreaseVsHomeSalePrice$percentage,
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
      hc_series(list(name="increase rate", data=wageIncreaseVsRent$percentage,
                     colorByPoint=TRUE)) %>%
      hc_plotOptions(series = list(boderWidth = 0,
                                   dataLabels = list(enabled = TRUE, format="{y}%") )) %>%
      print(wageVsRent)
  }
  )
  # leaflet ####
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
# Goal 1  
 output$goal11 <- renderTable(Goal11)
 output$goal12 <- renderTable(Goal12)
 output$goal13 <- renderTable(Goal13)
 output$goal14 <- renderTable(Goal14)
 #Goal 2 
 output$goal21 <- renderTable(Goal21)
 output$goal22 <- renderTable(Goal22)
 output$goal23 <- renderTable(Goal23)
 output$goal24 <- renderTable(Goal24)
 output$goal25 <- renderTable(Goal25)
 output$goal26 <- renderTable(Goal26)
 #Goal 3
 output$goal31 <- renderTable(Goal31)
 output$goal32 <- renderTable(Goal32)
 output$goal33 <- renderTable(Goal33)
 

}
# shinyApp ####
shinyApp(ui, server)
