
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

getTextSummary <- function(dailyWeather, data){
  sunrise = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$sunrise
  sunset = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$sunset
  probPrecip = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$precip
  highTemp = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$dayTemp
  feelsTemp = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$feelsTemp
  description = dailyWeather[which(dailyWeather$DateTime==data$selectedDate),]$description
  
  sunText = glue("Sunrise is at {tags$b(sunrise)} and sunset ends at {tags$b(sunset)}.")
  tempText = glue("The forecast for {tags$b(format(data$selectedDate, '%B %d'))} is for {tags$b(description)} with a high temperature around {tags$b(highTemp)} (feels like {tags$b(feelsTemp)}).")
  rainText = glue("The chance of precipitation today is {tags$b(probPrecip)}.")
  
  textOut = paste(sunText, tempText, rainText)
  
  return(tags$p(HTML(textOut)))
}