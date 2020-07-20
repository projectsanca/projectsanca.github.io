#!/usr/bin/Rscript --vanilla
#install.packages("googleway",repos = "http://cran.us.r-project.org")
suppressMessages(library(tidyverse))
suppressMessages(library(googleway))


# According to a spatial query eg. "pizza campinas", "cerveja sao carlos"; 
# a spatial radius e.g. 2000 meters; and 
# a walking duration filter e.g. 2600 secs (approx 40mins), 
# demo.data.R will create a filtered.csv file of demo data. 

setwd(".")
KEY="ENTERYOURKEY"

print("Acquiring Demo Data")

#origin="dcc ufmg belo horizonte "
#origin="Universidade Federal de São Carlos"
origin="ICMC são carlos"
spatial_query="cerveja são carlos"

spatial_radius=10000 # meters of range
walk_duration_filter=10000 # secs via `date -d@2600 -u +%Hhrs:%M:%S`

fullCSV=paste(origin,
              "_",
              spatial_query, 
              "_",
              spatial_radius,
              "_",
              "_full.csv",
              sep="")
filteredCSV=paste(origin,
                  "_",
                  spatial_query, 
                  "_",
                  spatial_radius,
                  "_",
                  walk_duration_filter,
                  "_filtered.csv",
                  sep="")

ores <- google_places(search_string=origin, key=KEY)

o_name<-ores$results$name
o_lat<-ores$results$geometry$location$lat
o_lng<-ores$results$geometry$location$lng
olocation<-c(o_lat, o_lng)

dres <- google_places(location=olocation,
                      keyword=spatial_query,
                      radius=spatial_radius,
                      key=KEY)
d_name<-dres$results$name
d_lat<-dres$results$geometry$location$lat
d_lng<-dres$results$geometry$location$lng

dests<-google_distance(origins=olocation,
                       destinations=d_name,
                       mode="walking",
                       simplify=T,
                       key=KEY)
dests <-dests$rows %>% as.data.frame.table

walk_duration<-(dests$Freq.elements.duration$text)
walk_duration_value<-(dests$Freq.elements.duration$value)
walk_distance<-(dests$Freq.elements.distance$text)
walk_distance_value<-(dests$Freq.elements.distance$value)

demodata <- data.frame(o_name,
                       o_lat,
                       o_lng,
                       d_name,
                       d_lat,
                       d_lng,
                       walk_duration,
                       walk_duration_value,
                       walk_distance,
                       walk_distance_value
                       )
write.csv(demodata, file=paste("./DATALAKE/",fullCSV,sep=""),
          row.names=FALSE)

demodata<- demodata %>%
    filter(walk_duration_value<=walk_duration_filter)

write.csv(demodata, file=paste("./DATALAKE/",filteredCSV,sep=""),
          row.names=FALSE)
write.csv(demodata, file="./DATA/filtered.csv",
          row.names=FALSE)
