#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyMobile)
library(lubridate)
library(ggplot2)
library(rtide)
library(dplyr)
library(plotly)
library(ggpmisc)
library(stringr)
library(httr)
library(glue)
library(jsonlite)
library(tidyr)


source("key.R")
source("functions.R")

tideData = getTideData()
dailyWeather = getWeatherData()

shinyApp(
    
ui = f7Page(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
    allowPWA = T,
    title = "My app",
    options = list(
        theme = c("ios"),
        dark = F,
        filled = FALSE,
        iosTranslucentBars = F,
        navbar = list(
            iosCenterTitle = TRUE,
            hideNavOnPageScroll = T
        ),
        pullToRefresh = T
    ),
    f7SingleLayout(
            navbar = f7Navbar(
                title = "Portland, ME Tide Chart", hairline = TRUE, shadow = TRUE
            ),
            f7Card(
                title = NULL,
                uiOutput("segControl"),
                div(align = "center", uiOutput("date"), style = "margin-top:12px; margin-bottom:6px;"),
                plotOutput("tidePlot")
            ),
            f7Card(
                title = NULL,
                uiOutput("text")
            )
        )
),
server = function(input, output) {
    data <- reactiveValues(
        selectedDate = as.Date(Sys.time(), tz = "EST5EDT"),
        todayActive = T,
        tomorrowActive = F
    )
    
    observeEvent(input$today,{
        data$selectedDate = as.Date(Sys.time(), tz = "EST5EDT")
        data$todayActive = T
        data$tomorrowActive = F
    })
    observeEvent(input$tomorrow,{
        data$selectedDate = as.Date(Sys.time(), tz = "EST5EDT")+1
        data$todayActive = F
        data$tomorrowActive = T
    })
    
    output$segControl <- renderUI({
        f7Segment(
            f7Button("today", "Today", active = data$todayActive),
            f7Button("tomorrow", "Tomorrow", active = data$tomorrowActive),
            container = "segment",
            shadow = FALSE,
            rounded = T,
            strong = T
        )
    })
    
    output$date <- renderUI({
        tags$b(format(data$selectedDate, "%A %B %d, %Y"))
    })
    
    output$text <- renderUI({
        getTextSummary(dailyWeather = dailyWeather, data = data)
    })
    
    output$tidePlot <- renderPlot({
        
        selectedData = tideData %>%
            filter(date == data$selectedDate)
        
       p =  ggplot(data = selectedData, aes(x = time, y = TideHeight)) +
            geom_line() +
            stat_peaks(colour = "black") +
            stat_valleys(colour = "black") +
            geom_line(data = tideData, alpha = 0.1) +
            # scale_x_time(
            #     name = "Time",
            #     breaks = scales::breaks_width("3 hour"),
            #     labels = scales::date_format("%I:%M %p"))+
            scale_x_time(
                name = "Time",
                breaks=hours(seq(0,24,3)),
                labels=c("midnight","3am", "6am", "9am", "noon", "3pm", "6pm", "9am", "midnight")) +
            scale_y_continuous(name = "Tide Height (m)",
                               breaks = seq(-1, 4, 0.5),
                               labels = seq(-1, 4, 0.5)) +
            theme_minimal(base_size = 14) +
            theme(axis.text.x=element_text(angle=45,hjust=1))
       
       dat = ggplot_build(p)
       peak_dat = dat$data[[2]][,c("x", "y")] %>% rowwise() %>% mutate(label = ntt2(x))
       valley_dat = dat$data[[3]][,c("x", "y")] %>% rowwise() %>% mutate(label = ntt2(x))
       
       p_text = p + 
           annotate("text",
                    x = c(peak_dat$x,valley_dat$x),
                    y = c(peak_dat$y+.15,valley_dat$y-.15),
                    label = c(peak_dat$label, valley_dat$label))
       
       p_text
        
    })
}
)






# tideDataList = unname(split(tideData[,c("time", "TideHeight")], tideData$dateFactor))

# 
# hc <- highchart() %>% 
#     hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>% 
#     hc_xAxis(categories = selectedData$time, type = "datetime") %>% 
#     hc_add_series(
#         name = unique(selectedData$Station), 
#         data = selectedData$TideHeight
#     ) %>%
#     hc_add_series_list(
#         tideDataList
#     )
# 
# hc

#hchart(selectedData, "line", hcaes(x = timeStamp, y = TideHeight, group = Station)) %>%

