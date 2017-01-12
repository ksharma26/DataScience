#
# This is a Shiny web applicatiomn, basically a dashboard to diplay trend of the Billing for SMB customer
# over the period of time. 

library(shiny)
library(plotly)
library(data.table)

BEData <- as.data.frame(read.csv('/Users/karansharma/Google Drive/DataScienceProjects/BillingEngine-ShinyApp/Data/BillingEngineData.csv', header = TRUE, sep = ',') )
BEData <- as.data.table(BEData)

Date <- as.Date(BEData$Date,'%m/%d/%y')
BEData[,Date:=NULL]
BEData[,Date.1:=NULL]

ui <- fluidPage(
  
   # Application title
   titlePanel("Billing Engine Dashboard"),
   # Dropdown with input from the BillingEngine columns  
   sidebarLayout(
   sidebarPanel(
      selectInput("billingTrend","Please select the value:",
                  choices = colnames(BEData), selected = "Total.Revenue")
   ),
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot")
      )
   )
)

# Define server logic required to draw a plot
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
      value <- BEData[[input$billingTrend]]
      ggplot(data = BEData, aes(x = Date, y = value, group=1)) + 
        geom_point() +
        geom_line() +
        geom_smooth(se = F) +
        ggtitle(as.character(input$billingTrend)) +
        xlab("\n\nTime")+
        theme(text = element_text(size=8))

  })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
