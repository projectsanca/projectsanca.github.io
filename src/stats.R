#!/usr/bin/Rscript --vanilla
#install.packages("cvequality",repos = "http://cran.us.r-project.org")
suppressMessages(library(tidyverse))

setwd(".")

cardinals = c("N","NE","E","SE","S","SW","W","NW")
mycsv<-"DATA/cardinal.csv"
walkdata<-read.csv(file.path(normalizePath(dirname(mycsv)),basename(mycsv)), header=TRUE)
walkdata<-na.omit(walkdata)

#
# Calculate Cardinals Coefficient of Variance
#
cv <- function(x) {
    ((sd(x, na.rm=TRUE)) / (mean(x, na.rm=TRUE)))
}

stats <- walkdata %>%
    group_by(cardinal) %>%
    mutate(cardinal_sd=(sd(walk_duration_value))) %>%
    mutate(cardinal_mean=(mean(walk_duration_value))) %>%
    mutate(cardinal_cv=(cv(walk_duration_value))) %>%
    arrange(cardinal, desc(cardinal_cv)) %>%
    as.data.frame.list

stats<-stats %>%
    arrange(factor(cardinal, levels=cardinals)) %>%
    mutate(min_cv=(ifelse(cardinal_cv==min(cardinal_cv,na.rm=T), T, F)))

ct=table(select(stats, cardinal)) # access it with ct["N"]
stats<-stats %>%
    mutate(cardinal_count=ct[cardinal]) %>%
    arrange(factor(cardinal_count))

stats<-stats %>%
    mutate(rank=cardinal_count 
           ) %>%
#    mutate(rank=
#           ((1/cardinal_mean)/max(cardinal_mean))+
#           ((cardinal_count/max(cardinal_count)))+ # Our variety bias
#           ((1/cardinal_cv)/max(cardinal_cv))
#       ) %>%
arrange(factor(rank))

stats<-stats %>%
    mutate(scale=(1/((stats$walk_duration_value/sum(stats$walk_duration_value, na.rm=T)))))
print(stats)

write.csv(stats,"DATA/stats.csv", row.names = FALSE)
