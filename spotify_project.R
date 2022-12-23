# Loading the packages and libraries.
library("rjson")
library(jsonlite)
library(lubridate)
library(gghighlight)
library(tidyverse)
library(knitr)
library(ggplot2)
library(ggdark)
library(plotly)


# Give the input file name to the function.
streamHistory <- fromJSON("data.json", flatten = TRUE)


# Adding date and timing.
mySpotify <- streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% as_date, seconds = msPlayed / 1000, minutes = seconds / 60)


# Playback activity per week and hours.
streamingHours <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date) %>% 
  group_by(date = floor_date(date, "week")) %>%
  summarize(hours = sum(minutes) / 60) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = hours)) + 
  geom_col(aes(fill = hours)) + dark_theme_linedraw() +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "Date", y= "Hours") + 
  ggtitle("Playback activity per week")
streamingHours


# Playback activity per specific artist.
hoursArtist <- mySpotify %>% 
  group_by(artistName, date = floor_date(date, "month")) %>% 
  summarize(hours = sum(minutes) / 60) %>% 
  ggplot(aes(x = date, y = hours, group = artistName, color = artistName)) + 
  labs(x= "Date", y= "Hours") + 
  ggtitle("Playback activity per artist") +
  geom_line() + dark_theme_linedraw() +
  gghighlight(artistName == "Rilo Kiley" || artistName == "The Beatles" || artistName == "Paramore"|| artistName == "Gorillaz") 
hoursArtist


# Most listened artists.
minutesMostListened <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(artistName) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  filter(minutesListened >= 180) %>%
  ggplot(aes(x = reorder(artistName, minutesListened), y = minutesListened)) + 
  geom_col(aes(fill = minutesListened)) + dark_theme_linedraw() +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "Artist", y= "Minutes") + 
  ggtitle("Playback activity per artist") +
  theme(axis.text.x = element_text(angle = 90))
minutesMostListened + coord_flip()


# Playback activity by date and time of the day.
timeDay <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date, hour = hour(endTime)) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = date, fill = minutesListened)) + 
  geom_tile() + dark_theme_linedraw() +
  labs(x= "Time of the day", y= "Date") + 
  ggtitle("When has there been more playback activity on my Spotify?", "Activity by date and time of day") +
  scale_fill_gradient(low = "white", high = "#1db954")
timeDay


# Playback activity by time of the day.
hoursDay <- mySpotify %>% 
  filter(date >= "2019-01-01") %>% 
  group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
  summarize(minutesListened = sum(minutes))
hoursDay %>% 
  ggplot(aes(x = hour, y = minutesListened, group = date)) + 
  geom_col(fill = "#1db954") + dark_theme_linedraw() +
  labs(x= "Time of the day", y= "Minutes") + 
  ggtitle("Activity from 0 to 24 hours")


# Playback activity by time of the day and weekday - Line Chart
weekDay <- hoursDay %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = weekday)) + 
  geom_line() + dark_theme_linedraw() +
  labs(x= "Time of the day", y= "Minutes") + 
  ggtitle("Weekly activity from 0 to 24 hours") 
weekDay


# Playback activity by day time.
dayType <- hoursDay %>% 
  mutate(day_type = if_else(weekday %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
  group_by(day_type, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = day_type)) + 
  geom_line() + dark_theme_linedraw() +
  labs(x= "Time of the day", y= "Minutes") + 
  ggtitle("Weekday and weekend activity from 0 to 24 hours") 
dayType


# Number of songs I play per day : bar chart
songsByDay <- mySpotify %>% 
  filter(msPlayed >= 1000) %>%
  group_by(date) %>% 
  group_by(date = floor_date(date, "day")) %>%
  summarize(songs = n()) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = songs)) + 
  geom_col(aes(fill = songs)) + dark_theme_linedraw() +
  scale_fill_gradient(high = "#1db954", low = "#1db954") + 
  labs(x= "Date", y= "Number of Songs Played") + 
  ggtitle("Number of songs I play per day", "November 2021 to November 2022")
songsByDay


# Most listened songs.
minutesTMostListened <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(trackName) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  filter(minutesListened >= 180) %>%
  ggplot(aes(x = reorder(trackName, minutesListened), y = minutesListened)) + 
  geom_col(aes(fill = minutesListened)) + dark_theme_linedraw() +
  scale_fill_gradient(low = "#1db954", high = "#1db954") + 
  labs(x= "Song", y= "Minutes") + 
  ggtitle("Most listened songs") +
  theme(axis.text.x = element_text(angle = 90))
minutesTMostListened + coord_flip()