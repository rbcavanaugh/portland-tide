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
library(ggpmisc)
library(stringr)
library(httr)
library(glue)
library(jsonlite)
library(tidyr)

source("functions/key.R")
source("functions/functions.R")

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
            )#,
            # f7Card(
            #     f7Row(
            #         f7Col(uiOutput("hourPicker")),
            #         f7Col(uiOutput("minutePicker")),
            #         f7Col(uiOutput("ampmPicker"))
            #     )
            # )
        )
),
server = function(input, output) {
    data <- reactiveValues(
        selectedDate = as.Date(Sys.time(), tz = "EST5EDT"),
        todayActive = T,
        tomorrowActive = F,
        hours = seq(hour(as_datetime(Sys.time(), tz = "EST5EDT")), 24, 1)
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
        
        getPlot(selectedData, tideData)
        
    })
    
    ## Card with pickers:
    
    output$hourPicker <- renderUI({
        f7Picker(
            inputId = "selectHour",
            label = "Hour",
            choices = data$hours
        )
    })
    
    output$minutePicker <- renderUI({
        f7Picker(
            inputId = "selectMinute",
            label = "Minute",
            choices = seq(0,50,10),
            value = 0
        )
    })
    
    output$ampmPicker <- renderUI({
        f7Picker(
            inputId = "selectAMPM",
            label = "am/pm",
            choices = c("AM", "PM"),
            value = ifelse(min(data$hours<12), "AM", "PM")
        )
    })
    
}
)

