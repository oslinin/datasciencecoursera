#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    #inputs
    ipar1=input$parameter1
    ipar2=input$parameter2
    func<-input$idtext[1] #func determines random function and histogram label 
    brks <- input$breaks
    N <- input$N
    
    gen.label <- function(func, par1, par2) {
        paste( "Random ", func, " distribution.\n", 
               par1, "=", input$parameter1, "\n",
               par2, "=", as.character(ipar2), sep="")
    }
    if (is.null(func)) func <- current.selection
    if (func=="uniform")  {f<-runif;   hist.label <- gen.label(func, "min", "max")}
    if (func=="normal")   {f<-rnorm;   hist.label <- gen.label(func, "mean", "standard deviation")}
    if (func=="binomial") {f<-rbinom;  ipar2=ipar2/100; print(ipar2); 
                                       hist.label <- gen.label(func, "size", "prob")}
    if (func=="lognormal"){f<-rlnorm;  ipar1=log(ipar1); ipar2=log(ipar2)
                                       hist.label <- gen.label(func, "meanlog", "sdlog")}
    if (func=="chisq")    {f<-rchisq;  hist.label <- gen.label(func, "df", "non-centrality parameter")}
    
    print(hist.label)
    current.selection <<- func #global variable of last thing checked, in case nothing is checked
    x=f(N, ipar1, ipar2)
    print(range(x))
    
    # draw the histogram with 25 bins
    
    hist(x, breaks = brks, col = 'darkgray',  border = 'white', main=hist.label)
    print(hist(x, breaks = brks, plot=F) )
  })
  x <- reactive(input$idtext)
  output$idtext<-renderText({x()})
})
