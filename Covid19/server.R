#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(magrittr)
library(readxl)
library(httr)
library(ggplot2)
# install.packages("dygraphs")
library(dygraphs)
# library('remotes')
# remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE)
library(nCov2019)
# x <- get_nCov2019(lang = 'en')
# class(x)
# str(x)
# x$lastUpdateTime
# sapply(x, class)
# dim(x$global) # 122   9
# View(x$global)

cov19 <- load_nCov2019(lang = 'en')
names(cov19) # "data"     "province" "global"   "time"     "lang" 
sapply(cov19, class)
#         data     province       global         time         lang 
# "data.frame" "data.frame" "data.frame"       "Date"  "character"
cov19$time # "2020-03-13"

df.world <- cov19['global']
class(df.world) # data.frame
names(df.world) # "time"        "country"     "cum_confirm" "cum_heal"    "cum_dead"  
nrow(subset(df.world, country == "United Kingdom")) # 37
# subset(d, country == "United Kingdom")

url.uk <- "https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data"

httr::GET(url.uk, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
df.uk <- readxl::read_xlsx(tf)
head(df.uk)

get.xts <- function(dafra, time.col, data.col) {
    v <- dafra[, data.col, drop=T]
    names(v) <- as.character(dafra[, time.col, drop=T])
    v
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Reactive expression to generate the requested distribution ----
    # This is called whenever the inputs change. The output functions
    # defined below then use the value computed from this expression
    d <- reactive({
        dist <- switch(input$dist,
                       norm = rnorm,
                       unif = runif,
                       lnorm = rlnorm,
                       exp = rexp,
                       rnorm)
        
        dist(input$n)
    })
    
    # Plot of the daily cases ----
    output$plot_daily <- renderDygraph({
        get.xts(df.uk, "DateVal", "CMODateCount") %>% 
            dygraph() %>% 
            dyOptions(fillGraph = TRUE, colors="blue", fillAlpha = 0.4) %>%
            dyRangeSelector()
    })
    
    # Plot of the cummulative cases ----
    output$plot_cumm <- renderDygraph({
        get.xts(df.uk, "DateVal", "CumCases") %>% 
            dygraph() %>%
            dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
            dyRangeSelector()
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        df.uk %>% 
            dplyr::mutate(DateVal = as.character(DateVal)) %>% 
            dplyr::rename(Date = DateVal,
                          Daily.Cases = CMODateCount,
                          Cumm.Cases = CumCases)
    })

})
