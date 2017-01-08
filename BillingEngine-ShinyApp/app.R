#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(data.table)

BEData <- as.data.frame(read.csv('/Users/karansharma/Google Drive/DataScienceProjects/BillingEngine-ShinyApp/Data/BillingEngineData.csv', header = TRUE, sep = ',') )
library(data.table)
BEData <- as.data.table(BEData)
Date <- as.Date(BEData$Date,'%m/%d/%y')
BEData[,Date:=NULL]
BEData[,Date.1:=NULL]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  
   # Application title
   titlePanel("Billing Engine Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      selectInput("billingTrend","Please select the value",
                  choices = colnames(BEData)),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      ggplot(data = BEData, aes(x = Date, y = BEData[,input$billingTrend], group=1)) + 
        geom_point() +
        geom_line() +
        geom_smooth(se = F) +
        ggtitle("ShareFile Billing Trend for SMB") +
        xlab("\n\nTime")+
        ylab("Revenue (Dollars)") +
        theme(text = element_text(size=10))
  })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
