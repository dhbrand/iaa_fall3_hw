
#####################################################
#Geocode Location
#####################################################
# Need experimental version of ggmap
# also need to download and install rtools off of the internet
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
#####################################################
#TO DO THIS YOU NEED TO REGISTER TO A GOOGLE DEV ACCOUNT
#AND PROVIDE YOUR OWN API KEY!!!!!
#THEN YOU CAN GET A PRETTY MAP!
# https://www.wpgmaps.com/documentation/creating-a-google-maps-api-key/
# You will also have to get the geocode api enabled
#####################################################
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)
listings <- read_csv("listings.csv")

register_google("[YOUR API KEY HERE]")

# uncomment to run
# retrieves the lat and lon of each listing

#strtAddress <- listings$street
#lon<- matrix(0,nrow=length(strtAddress))
#lat<- matrix(0,nrow=length(strtAddress))
#for (ii in 1:length(strtAddress)){
#    latLon <- geocode(strtAddress[ii],output="latlon")
#    lon[ii] <- as.numeric(latLon[1])
#    lat[ii] <- as.numeric(latLon[2])
#}
listing_k <-data.frame(listing_id = listings$id, lat = lat, lon = lon)
#########################################
# Get a Map of Seattle
#########################################
#map <- get_map(location = "Seattle", zoom = 12)
#map2 <- get_map(location = "Seattle", zoom = 11)

#I want to find the Geo-location of these listings :-)

cluster <- words_and_clusters %>% filter(group == 9) %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)

#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)

ggmap(map2, fullpage = TRUE) +
  geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)




