
library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Twitter Word Cloud"),

  sidebarLayout(
    sidebarPanel(
       textInput("inputWord",
                   "Enter Word:",
                   value = "Twitter"),
       sliderInput("freq","Minimum Frequency",min = 1, max = 50, value = 20),
       sliderInput("maxWords","Maximum Number of Words", min = 1, max= 250, value = 50),
       hr(),
       actionButton("update", "Change")
    ),
    
    mainPanel(
       plotOutput("distPlot")
    )
  )
))
