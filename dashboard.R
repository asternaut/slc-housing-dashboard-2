library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)

setwd("~/Google Drive File Stream/My Drive/SI/DataScience/Side projects/SLC Housing Dashboard/Data/for_dashboard/")

project_con <- read_excel("Housing Database Combined Data.xlsx", sheet = "All Data")
MSA_unemployment <- read_excel("MSA-unemployment.xlsx", sheet = 1)

ui <- dashboardPage(
  dashboardHeader(title="SLC Housing Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      box(plotOutput("plot2", height=250))
    ),
    fluidRow(
      tags$iframe(src = "http://slcgov.maps.arcgis.com/apps/PublicInformation/index.html?appid=f632417a8bd94d5eb04f1f4eea728ce6", seamless=NA, height = 400, width = "100%")
    )
  )
)

server <- function(input, output) {
  output$plot1<-renderPlot({
    barchart<-barplot(table(project_con$`Type (PSH, Affordable, or Market`), 
                      main="Affordable vs Market in SLC's construction projects")
    print(barchart)
  })
  output$plot2<-renderPlot({
    linechart<-plot(x=2007:2017,y=MSA_unemployment$Aug,
                    xlab="Year", main="Unemployment rate in August 2007-2017")
    print(linechart)
  })
}

shinyApp(ui, server)
