#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # App title ----
    titlePanel("COVID19"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(width = 2,
            selectInput(inputId='country', label='Country', choices=c("United Kingdom")),
            
            # br() element to introduce extra vertical spacing ----
            br()
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Daily Cases", dygraphOutput("plot_daily")),
                        tabPanel("Cummulative", dygraphOutput("plot_cumm")),
                        tabPanel("Table", tableOutput("table"))
            )
            
        )
    )
))
