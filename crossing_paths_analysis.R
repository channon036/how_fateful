library(tidyverse)
library(rjson)
library(lubridate)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

#Extract: Ingesting the json files into R lists ready for further transformation

ingest_maps_data <- function(fileName) {
  records_data <- fromJSON(file = fileName)
  
  rd <- unlist(records_data, recursive = FALSE)
  
}

records_data_chan <-
  ingest_maps_data(fileName = "~/Crossing paths/TakeoutChannon/Location History/Records.json")
records_data_dan <-
  ingest_maps_data(fileName = "~/Crossing paths/TakeoutDandre/Location History/Records.json")

#Transform: converting lists into data frames

#Chan data frame construction
c_timestamp <-
  lapply(records_data_chan, pluck, "timestamp") %>% unlist() %>% data.frame()
c_latitude <-
  lapply(records_data_chan, pluck, "latitudeE7") %>% unlist() %>% data.frame()
c_longitude <-
  lapply(records_data_chan, pluck, "longitudeE7") %>% unlist() %>% data.frame()
c_accuracy <-
  lapply(records_data_chan, pluck, "accuracy") %>% unlist() %>% data.frame()
c_index <- 1:nrow(c_timestamp)

chan_rd <- cbind(c_timestamp, c_latitude, c_longitude, c_accuracy)
row.names(chan_rd) <- c_index
colnames(chan_rd) <-
  c("timestamp", "latitude", "longitude", "accuracy")

crd <- chan_rd %>%
  #Formatting data nicely
  mutate(timestamp_utc = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>%
  mutate(timestamp = with_tz(timestamp_utc, tzone = "Africa/Johannesburg")) %>%
  select(-timestamp_utc) %>%
  mutate(person = "Chan") %>%
  mutate(latitude = latitude/10000000) %>%
  mutate(longitude = longitude/10000000) %>%
  mutate(coordinates = paste(latitude, longitude, sep = ", ")) %>% 
  mutate(timestamp = round_date(timestamp, unit = "2 min")) %>% 
  unique()


#Dan data frame construction
d_timestamp <-
  lapply(records_data_dan, pluck, "timestamp") %>% unlist() %>% data.frame()
d_latitude <-
  lapply(records_data_dan, pluck, "latitudeE7") %>% unlist() %>% data.frame()
d_longitude <-
  lapply(records_data_dan, pluck, "longitudeE7") %>% unlist() %>% data.frame()
d_accuracy <-
  lapply(records_data_dan, pluck, "accuracy") %>% unlist() %>% data.frame()
d_index <- 1:nrow(d_timestamp)

dan_rd <- cbind(d_timestamp, d_latitude, d_longitude, d_accuracy)
row.names(dan_rd) <- d_index
colnames(dan_rd) <-
  c("timestamp", "latitude", "longitude", "accuracy")

drd <- dan_rd %>%
  #Formatting data nicely
  mutate(timestamp_utc = as.POSIXct(timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) %>%
  mutate(timestamp = with_tz(timestamp_utc, tzone = "Africa/Johannesburg")) %>%
  select(-timestamp_utc) %>%
  mutate(person = "Dan") %>%
  mutate(latitude = latitude/10000000) %>%
  mutate(longitude = longitude/10000000) %>%
  mutate(coordinates = paste(latitude, longitude, sep = ", ")) %>% 
  mutate(timestamp = round_date(timestamp, unit = "2 min"))  %>% 
  unique()
  

#Load- saving the data to save time in the future

save(crd, file = "Channon_Google_Maps_history.RData")
save(drd, file = "Dandre_Google_Maps_history.RData")


#Cleaning the data of low accuracy records
#and only including data from 2015 on when we were both living in Cape Town
#and only including data from before we met on 8 Aug 2020 <3

best_day <- as.POSIXct("2020/08/08 13:00:00")

crd <- crd %>%
  filter(accuracy <= 11.1) %>%
  filter(year(timestamp) >= 2015) %>%
  filter(timestamp <= best_day)

drd <- drd %>% filter(accuracy <= 11.1) %>%
  filter(year(timestamp) >= 2015) %>%
  filter(timestamp <= best_day)

#Now to combine the two data sets
combined_rd <- full_join(crd, drd, 
                         by = c("person",
                                "timestamp",
                                "coordinates",
                                "latitude",
                                "longitude",
                                "accuracy"))


#Setting some slightly broader coordinates may help group together nearby coordinates
#An accuracy of 2 decimal places is a radius of 11.1 km, 3 decimals is 111m, and 4 is 11.1m.

bunched_coords <- combined_rd %>%
  mutate(latitude = round(latitude, 4)) %>%
  mutate(longitude = round(longitude, 4)) %>%
  mutate(coordinates = paste(latitude, longitude, sep = ", ")) %>%
  unique()

most_frequent_places <- bunched_coords %>%
  group_by(coordinates, latitude, longitude) %>%
  unique() %>%
  count() %>% 
  arrange(desc(n))

#Approach #1: To group by locations, and then analyse time stamps
places <- bunched_coords %>%
  group_by(coordinates, latitude, longitude, person) %>%
  summarise(records = n()) %>%
  arrange(desc(records)) 

d_places <- places %>%
  filter(person == "Dan")

c_places <- places %>%
  filter(person == "Chan")

both_places <- inner_join(c_places, d_places, by = c("coordinates","latitude","longitude")) %>%
  mutate(both.records = sum(records.x, records.y)) %>%
  arrange(desc(both.records)) %>%
  mutate(dan_per = (records.y/both.records)*100) %>%
  mutate(chan_per = (records.x/both.records)*100) %>%
  mutate(dif_per = chan_per - dan_per)


#Visualizing all the places we had both been

my_palette <- colorRampPalette(rev(brewer.pal(3, "Spectral")))

map_both_places <- get_map(
  location = c(lon = median(both_places$longitude), lat = median(both_places$latitude)), 
  zoom = 12,
  size = c(640, 640),
  maptype = "toner-lite",
  color = "bw")

ggmap(map_both_places, darken = c(0.35, "black")) +
  geom_point(aes(x = longitude, y = latitude, size = both.records, colour = dif_per),
             data = both_places, 
             alpha = 0.6)+
  scale_radius(range = c(2,15), 
             name = "Number of records") +
  scale_colour_gradientn(colours = my_palette(3), 
                         name = "Most visited by person", 
                         breaks = c(min(both_places$dif_per), 0, max(both_places$dif_per)),
                         labels = c("Dan", "Equally visited", "Chan")) +
  theme(text=element_text(family="sans"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggtitle(label = "Common locations")

#Now to find how close in time we got in any of the matching places

grouped_by_place <- bunched_coords %>%
  filter(coordinates %in% both_places$coordinates) %>%
  group_by(coordinates, person) %>% 
  summarise(person, 
            earliest.visit = min(timestamp),
            latest.visit = max(timestamp)) %>%
  unique() %>%
  arrange(coordinates) %>%
  ungroup()

overlapping_times <- grouped_by_place %>%
  pivot_wider(names_from = c("person"), 
              values_from = c("earliest.visit","latest.visit")) %>%
  mutate(dan.first = earliest.visit_Chan > latest.visit_Dan) %>%
  mutate(chan.first = earliest.visit_Dan > latest.visit_Chan)

overlapping_times_and_places <- overlapping_times %>%
  filter(chan.first == FALSE) %>% 
  filter(dan.first == FALSE)

min_val <- function(row, output) {
  chan <- lapply(row[4], as.numeric) %>% unlist(recursive = FALSE)
  dan <- lapply(row[5], as.numeric) %>% unlist(recursive = FALSE)
  min_val <- min(abs(outer(chan, dan, "-")))
}

min_date_val <- function(row, output) {
  chan <- lapply(row[4], as.numeric) %>% unlist(recursive = FALSE)
  dan <- lapply(row[5], as.numeric) %>% unlist(recursive = FALSE)
  min_date_val <- which.min(abs(outer(chan, dan, "-")))
}
  
min.date.time.cols <- function(row, output){
  chan <- lapply(row[4], as.numeric) %>% unlist(recursive = FALSE)
  dan <- lapply(row[5], as.numeric) %>% unlist(recursive = FALSE)
 
 new_list <- list()
  for(c in chan){
    for(d in dan){
      diff <- abs(c-d)
      
      list <- data.frame(c, d, diff)
      names(list) <- c("c","d","diff")
      
      new_list <- rbind(new_list, list)
    }
  } 
 df <- as.data.frame(do.call(rbind, new_list))
 return(df)
}  


the_answer <- bunched_coords %>% 
  filter(coordinates %in% overlapping_times_and_places$coordinates) %>%
  select(-accuracy) %>%
  unique() %>%
  pivot_wider(names_from = "person", values_from = "timestamp")

the_answer$times.list <- the_answer %>% 
  apply(1, min.date.time.cols) 

the_answer$closest.time <- the_answer$times.list %>% 
  lapply(as.data.frame.numeric) 


%>%
  apply(row(3))






  
  
`the_answer %>% split(by = "coordinates", flatten = TRUE)







  lapply(mutate, time.diff = list(abs(unlist(Chan, recursive = FALSE) - unlist(Dan, recursive = FALSE)))) %>%
  lapply(mutate, min.time.diff = as.duration(min(unlist(time.diff)))) %>%
  lapply(mutate, min.date.n = which.min(unlist(time.diff))) %>%
  


  mutate(min.date = as.Date(nth(unlist(Chan),unlist(min.date.n)))) %>%
  arrange(min.time.diff)

map_times_at_places <- get_map(
  location = c(lon = median(the_answer$longitude), lat = median(the_answer$latitude)), 
  zoom = 12,
  size = c(640, 640),
  maptype = "toner-lite",
  color = "bw")

ggmap(map_times_at_places, darken = c(0.35, "black")) +
  geom_point(aes(x = longitude, y = latitude, size = 5, colour = min.time.diff),
             data = the_answer, 
             alpha = 0.6)+
  scale_colour_gradientn(colours = my_palette(20), 
                         name = "How close in time") +
  theme(text=element_text(family="sans"),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks = element_blank()) +
  ggtitle(label = "How close in time were we?")

#Approach #2: To group by 15 min intervals, and then analyse the distance between locations
