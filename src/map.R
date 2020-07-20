#!/usr/bin/Rscript --vanilla
suppressMessages(library(tidyverse))
suppressMessages(library(leaflet))
suppressMessages(library(htmlwidgets))
suppressMessages(library(htmltools))
suppressMessages(library(lubridate))

setwd(".")

mycsv<-"DATA/stats.csv"
walkdata<-read.csv(file.path(normalizePath(dirname(mycsv)),basename(mycsv)), header=TRUE)
na.omit(walkdata, cols=c("rank"))
walkdata

cardinals=c("N","NE","E","SE","S","SW","W","NW")

palette<-c("#dce282",
           "#ffd300",
           "#ff9f00",
           "#ff5b31",
           "#ff0065",
           "#ff009a",
           "#f400d0",
           "#f400d0")
rankpal<-colorFactor(palette, domain = walkdata$rank, ordered=T)

twalk_map <- leaflet(walkdata) %>%
    addProviderTiles("CartoDB.DarkMatter") %>%
    addMiniMap(toggleDisplay=T, position="bottomright") %>%
    addScaleBar(position="bottomleft",
                options=scaleBarOptions(maxWidth = 1000,
                                        metric = TRUE,
                                        imperial = TRUE,
                                        updateWhenIdle = TRUE)) %>%
    addMarkers(data=walkdata,
               lng=as.numeric(unlist(walkdata["o_lng"])),
               lat=as.numeric(unlist(walkdata["o_lat"])),
               label=paste("YOU ARE HERE: ",
                           as.character(unlist(walkdata["o_name"]))
                           )
               ) %>%
    addCircleMarkers(
                     lng=as.numeric(unlist(walkdata["d_lng"])),
                     lat=as.numeric(unlist(walkdata["d_lat"])),
                     color=~rankpal(walkdata$rank),
                     radius=((as.numeric(unlist(walkdata["scale"])))),
                     fillOpacity=0.05
                     ) %>%
    addCircles(data = walkdata,
               lng=as.numeric(unlist(walkdata["d_lng"])),
               lat=as.numeric(unlist(walkdata["d_lat"])),
               color=~rankpal(walkdata$rank),
               radius=((as.numeric(unlist(walkdata["scale"])))),
               stroke=T,
               fillOpacity=0.1,
               popup=paste(
                           "<b>Cardinal Direction</b> :",
                           as.character((unlist(walkdata["cardinal"]))),
                           "</br></br>",
                           "<b>Cardinal Count</b>:",
                           as.character((unlist(walkdata["cardinal_count"]))),
                           "</br></br>",
                           "<b>Cardinal Mean</b>:",
                           as.character((unlist(walkdata["cardinal_mean"]))),
                           "</br></br>",
                           "<b>Cardinal CV</b>:",
                           as.character((unlist(walkdata["cardinal_cv"]))),
                           "</br></br>",
                           "<b>Cardinal Rank</b>:",
                           as.character((unlist(walkdata["rank"]))),
                           "</br></br>",
                           as.character(unlist(walkdata["d_name"])),
                           "is",
                           "<b>",
                           as.character(seconds_to_period(unlist(walkdata["walk_duration_value"]))),
                           "</b>",
                           "walking from",
                           as.character(unlist(walkdata["o_name"]))
                           )
               ) %>%
    addLegend(position="topright",
              pal=rankpal,
              values=~(walkdata$rank),
              title="Destination Counts") %>%
    leafem::addMouseCoordinates()

f<-"MAP/twalk_map.html"
saveWidget(twalk_map,file.path(normalizePath(dirname(f)),basename(f)))
