
getTideData <- function(){
 rtide::tide_height(
  "Casco Bay",
  from = Sys.Date()-15, to = Sys.Date()+15,
  minutes = 10L, tz = "EST5EDT"
) %>%
  tidyr::separate(DateTime, c("date", "time"), sep = " ", remove = F) %>%
  mutate(date = ymd(date), time = hms(time), dateFactor = as.factor(date))
}

ntt2 <- function(x){
  tmp = lubridate::seconds_to_period(x)
  hour = tmp@hour
  minute = tmp@minute
  if(hour>12){
    hour = hour-12
    ampm = "pm"
  } else if(hour==12){
    ampm = "pm"
  } else {
    ampm = "am"
  }
  
  label = paste(sprintf("%02d:%02d", hour, minute), ampm)
  if(hour<10){
    label = str_remove(label, pattern = "0")
  }
  return(label)
}

getWeatherData <- function(){
  weatherData$daily %>% 
    unnest(cols = c("temp", "weather")) %>% 
    mutate(dt = as.Date(as_datetime(dt)),
           sunrise = format(as_datetime(sunrise, tz = "EST5EDT"), "%I:%M %p"),
           sunset = format(as_datetime(sunset, tz = "EST5EDT"), "%I:%M %p")) %>%
    rename(DateTime = dt, dayTemp = day, maxTemp = max) %>%
    select(-night, -eve, -morn) %>%
    unnest(cols = "feels_like") %>%
    select(DateTime, sunrise, sunset,
           dayTemp, maxTemp, feelsTemp = day,
           description, pop
    ) %>%
    mutate(dayTemp = paste0(round((dayTemp + maxTemp)/2, 0), " degrees"),
           feelsTemp = paste0(round(feelsTemp, 0), " degrees"),
           pop = ifelse(pop<0.05, "< 5%", pop)) %>%
    mutate(precip = ifelse(pop=="< 5%", pop, scales::percent(as.numeric(pop), accuracy = 1)))
  
}

getPlot <- function(selectedDat, tideDat){
  p =  ggplot(data = selectedDat, aes(x = time, y = TideHeight)) +
    geom_line() +
    stat_peaks(colour = "black") +
    stat_valleys(colour = "black") +
    geom_line(data = tideDat, alpha = 0.1) +
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
  
  return(p_text)
}

getTextSummary <- function(dailyWeather, data){
  sunrise = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$sunrise
  sunset = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$sunset
  probPrecip = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$precip
  highTemp = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$dayTemp
  feelsTemp = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$feelsTemp
  description = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$description
  
  sunText = glue("Sunrise is at {tags$b(sunrise)} and sunset ends at {tags$b(sunset)}.")
  tempText = glue("The forecast for {tags$b(format(data$selectedDate, '%B %d'))} is for {tags$b(description)} with a high temperature around {tags$b(highTemp)} (feels like {tags$b(feelsTemp)}).")
  rainText = glue("The chance of precipitation is {tags$b(probPrecip)}.")
  
  textOut = paste(sunText, tempText, rainText)
  
  return(tags$p(HTML(textOut)))
}