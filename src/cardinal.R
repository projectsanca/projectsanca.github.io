#!/usr/bin/Rscript --vanilla
#install.packages("matrixStats",repos = "http://cran.us.r-project.org")
suppressMessages(library(tidyverse))
suppressMessages(library(geosphere))

setwd(".") # Set Working Dir
mycsv<-"DATA/filtered.csv"
walkdata<-read.csv(file.path(normalizePath(dirname(mycsv)),basename(mycsv)),
                   header=TRUE)
walkdata<-na.omit(walkdata)

N=0;    NE=45;  E=90;   SE=135;
S=180;  SW=225; W=270;  NW=315;
NN=360;

#NOTE    2 pairs of "lng, lat" in 'DD Coordinates' format
getCardinal <- function(p1lng, p1lat, p2lng, p2lat) {
    v <- bearingRhumb(c(p1lng, p1lat),
                      c(p2lng, p2lat)) %>%
                      trunc()

    print(paste("Cardinal:", v))
    #N=0 or 360, NE=45, E=90, SE=135, S=180, SW=225, W=270, NW=315
    morev <- function(v, x, y){
        ifelse( v< ((x+y)/2), TRUE, FALSE)
    }

    if (v == N || v == NN) {
        return("N")
    } else if (v>N && v<=NE) {
        if (morev(v,N,NE))
            return("N")
        else
            return("NE")
    } else if (v>NE && v<=E) {
        if (morev(v,NE,E))
            return("NE")
        else
            return("E")
    } else if (v > E && v<=SE) {
        if (morev(v,E,SE))
            return("E")
        else
            return("SE")
    } else if (v>SE && v<=S) {
        if (morev(v,SE,S))
            return("SE")
        else
            return("S")
    } else if (v>S && v<=SW) {
        if (morev(v,S,SW))
            return("S")
        else
            return("SW")
    } else if (v>SW && v<=W) {
        if (morev(v,SW,W))
            return("SW")
        else
            return("W")
    } else if (v>W&& v<=NW) {
        if (morev(v,W,NW))
            return("W")
        else
            return("NW")
    } else if (v>NW && v<NN) {
        if (morev(v,NW,NN))
            return("NW")
        else
            return("N")
    } else {
        return("bearing not converted to cardinal direction")
    }
}

cardinalMap <- walkdata %>%
    rowwise %>%
    mutate(cardinal = getCardinal(o_lng, o_lat, d_lng, d_lat)) 
cardinalMap
write.csv(cardinalMap,"DATA/cardinal.csv", row.names = FALSE)
