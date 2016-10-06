#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#available distributions
dists <- c("uniform","normal","binomial","lognormal","chisq" )


# Define UI for application that draws a histogram
shinyUI(fluidPage(
   
  # Application title
  titlePanel("Random Numbers"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout( 
    sidebarPanel(
      h4("Please Select a distribution and two parameters")
      ,checkboxGroupInput("idtext", "Distribution Type", choices=dists, selected="uniform")
      ,sliderInput("parameter1","Parameter 1",    min = 1,max = 50,     value = 7)
      ,sliderInput("parameter2","Parameter 2",    min = 0,max = 100,    value = 30)
      ,sliderInput("breaks","Breaks in Histogram",min = 0,max = 100,    value = 25)
      ,sliderInput("N","# of simulations",        min = 0,max = 100000, value = 10000)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
