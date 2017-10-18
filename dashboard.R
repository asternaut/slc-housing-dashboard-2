library(shiny)
library(shinydashboard)
library(ggplot2)

project_con<-read.xlsx("Housing Database Combined Data.xlsx",sheetName = "All Data")
MSA_unemployment<-read.xlsx("MSA-unemployment.xlsx", sheetIndex = 1)

ui <- dashboardPage(
  dashboardHeader(title="SLC Housing Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      box(plotOutput("plot2", height=250))
    )
  )
)

server <- function(input, output) {
  output$plot1<-renderPlot({
    barchart<-barplot(table(project_con$Type..PSH..Affordable..or.Market), 
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