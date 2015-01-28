### This file constructs a database
### Unit of analysis: UA
### DV: number of NYT mentions
### IVs: country-year variables, including number of NYT

rm(list=ls())
library("plyr")
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/amnesty-uas")

# Read in data
amnesty <- read.csv("Data/total_amnesty2.csv")

# keep only relevant variables
names(amnesty)
amnesty <- subset(amnesty, select = c(index,title,ua,network.number,keywords,teaser,teaser.scrape,country,countrycode,year))

# combine teaser field
amnesty$teaser.all <- NA
amnesty$teaser.all <- as.character(amnesty$teaser)
na.index <- which(is.na(amnesty$teaser.all)) 
amnesty$teaser.all[na.index] <- amnesty$teaser.scrape[na.index]

# remove teaser columns

amnesty$teaser <- NULL
amnesty$teaser.scrape <- NULL

#### Building new variables

# read in data

rt <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/country-year-database/rt.csv")
names(rt)

# write function

get.variables <- function(variable,row){
  x <- rt[rt$ccode==amnesty$countrycode[row] & rt$year==amnesty$year[row],variable]
  return(x)
}

get.variables("polity",200)

