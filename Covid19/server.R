
# browser()

library(shiny)
library(magrittr)
library(readxl)
library(httr)
library(ggplot2)
# install.packages("dygraphs")
library(dygraphs)
# library('remotes')
# remotes::install_github("GuangchuangYu/nCov2019", dependencies = TRUE)
# library(nCov2019)

#==============================================================================!
# nCov19 data ####
#==============================================================================!
# x <- get_nCov2019(lang = 'en')
# class(x)
# str(x)
# x$lastUpdateTime
# sapply(x, class)
# dim(x$global) # 122   9
# View(x$global)

# load_nCov2019
# 
# cov19 <- load_nCov2019(lang = 'en')
# names(cov19) # "data"     "province" "global"   "time"     "lang" 
# sapply(cov19, class)
#         data     province       global         time         lang 
# "data.frame" "data.frame" "data.frame"       "Date"  "character"
# cov19$time # "2020-03-13"

# df.world <- cov19['global']
# class(df.world) # data.frame
# names(df.world) # "time"        "country"     "cum_confirm" "cum_heal"    "cum_dead"  
# nrow(subset(df.world, country == "United Kingdom")) # 37
# subset(d, country == "United Kingdom")

#==============================================================================!
# gov.uk data ####
#==============================================================================!
url.uk <- "https://www.arcgis.com/sharing/rest/content/items/e5fd11150d274bebaaf8fe2a7a2bda11/data"
httr::GET(url.uk, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
df.uk <- readxl::read_xlsx(tf) %>% 
    dplyr::arrange(desc(DateVal)) %>% 
    dplyr::mutate(Country = "United Kingdom") %>% 
    dplyr::select(Country, Date = DateVal, Cases = CMODateCount, CumCases)
    
head(df.uk)

get.xts <- function(dafra, time.col, data.col) {
    v <- dafra[, data.col, drop=T]
    names(v) <- as.character(dafra[, time.col, drop=T])
    v
}

#==============================================================================!
# Johns Hopkins data ####
#==============================================================================!
# Website:
# https://github.com/CSSEGISandData/COVID-19

url.jh <- paste("https://raw.githubusercontent.com/CSSEGISandData/", 
                 "COVID-19/master/csse_covid_19_data/", "csse_covid_19_time_series/", 
                 "time_series_19-covid-Confirmed.csv", sep = "")

jh.ts.wide <- readr::read_csv(url.jh)
dim(jh.ts.wide) # 421  56

jh.ts.wide[1:6, 1:6]
subset(jh.ts.wide, `Country/Region` == "United Kingdom") #%>% View
subset(jh.ts.wide, `Country/Region` == "Germany")
subset(jh.ts.wide, `Country/Region` == "US")

jh.ts.long <- jh.ts.wide %>% 
    dplyr::select(-Lat, -Long, -"Province/State",
                  Country = "Country/Region") %>% 
    data.table::melt(., id.vars=c("Country"), variable.name = "Date", value.name="Cases") %>% 
    dplyr::group_by(Country, Date) %>% 
    dplyr::summarise(Cases = sum(Cases)) %>% 
    dplyr::mutate(Date = lubridate::mdy(Date)) %>% 
    dplyr::arrange(Date) %>% 
    dplyr::group_by(Country) %>% 
    dplyr::mutate(CumCases = cumsum(Cases)) %>% 
    dplyr::arrange(Country, desc(Date))

head(jh.ts.long)
dim(jh.ts.long) # 21892     3

countries <- sort(unique(jh.ts.long$Country))
length(countries) # 125
countries
countries.top <- c("United Kingdom", "Brazil", "Germany", "US")
countries.sel <- countries[-which(countries %in% countries.top)]
countries.sel <- c(countries.top, countries.sel)
head(countries.sel)

#==============================================================================!
# Server logic ####
#==============================================================================!
shinyServer(function(input, output, session) {

    updateSelectInput(session, "country", choices=countries.sel)
    
    df <- reactive({
        land <- input$country
        if (land == "United Kingdom") {
            daten <- df.uk
        } else {
            daten <- jh.ts.long %>% 
                dplyr::filter(Country == land)
        }
        datum <- max(daten$Date)
        list(land=land, daten=daten, datum=datum)
    })
    
    # Plot of the daily cases ----
    output$plot_daily <- renderDygraph({
        get.xts(df()$daten, "Date", "Cases") %>% 
            dygraph(., main = paste0(df()$land, " (", df()$datum, ")")) %>% 
            dyOptions(fillGraph = TRUE, colors="blue", fillAlpha = 0.4) %>%
            dyRangeSelector()
    })
    
    # Plot of the cummulative cases ----
    output$plot_cumm <- renderDygraph({
        get.xts(df()$daten, "Date", "CumCases") %>% 
            dygraph(., main = paste0(df()$land, " (", df()$datum, ")")) %>%
            dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
            dyRangeSelector()
    })
    
    # Generate an HTML table view of the data ----
    output$table <- renderTable({
        df()$daten %>% dplyr::mutate(Date = as.character(Date))
    })

})
