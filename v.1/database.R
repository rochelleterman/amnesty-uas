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
get.variables("polity",200) # testing

# polity
rows <- 1:nrow(amnesty)
amnesty$polity <- lapply(rows,get.variables,variable="polity")
amnesty$polity[amnesty$polity=="integer(0)"] <- NA
amnesty$polity[grep("NA",amnesty$polity)] <- NA

# polity2
amnesty$polity2 <- lapply(rows,get.variables,variable="polity2")
amnesty$polity2[amnesty$polity=="integer(0)"] <- NA
amnesty$polity2[grep("NA",amnesty$polity)] <- NA

# democ
amnesty$democ <- lapply(rows,get.variables,variable="democ")
amnesty$democ[amnesty$democ=="integer(0)"] <- NA
amnesty$democ[grep("NA",amnesty$democ)] <- NA

# autoc

amnesty$autoc <- lapply(rows,get.variables,variable="autoc")
amnesty$autoc[amnesty$autoc=="integer(0)"] <- NA
amnesty$autoc[grep("NA",amnesty$autoc)] <- NA

# physint

amnesty$physint <- lapply(rows,get.variables,variable="physint")
amnesty$physint[amnesty$physint=="integer(0)"] <- NA
amnesty$physint[grep("NA",amnesty$physint)] <- NA

# speech

amnesty$speech <- lapply(rows,get.variables,variable="speech")
amnesty$speech[amnesty$speech=="integer(0)"] <- NA
amnesty$speech[grep("NA",amnesty$speech)] <- NA

# gdp.pc.wdi

amnesty$gdp.pc.wdi <- lapply(rows,get.variables,variable="gdp.pc.wdi")
amnesty$gdp.pc.wdi[amnesty$gdp.pc.wdi=="integer(0)"] <- NA
amnesty$gdp.pc.wdi[grep("NA",amnesty$gdp.pc.wdi)] <- NA

# pop.wdi

amnesty$pop.wdi <- lapply(rows,get.variables,variable="pop.wdi")
amnesty$pop.wdi[amnesty$pop.wdi=="integer(0)"] <- NA
amnesty$pop.wdi[grep("NA",amnesty$pop.wdi)] <- NA

# amnesty

amnesty$amnesty <- lapply(rows,get.variables,variable="amnesty")
amnesty$amnesty[amnesty$amnesty=="integer(0)"] <- NA
amnesty$amnesty[grep("NA",amnesty$amnesty)] <- NA

# statedept

amnesty$statedept <- lapply(rows,get.variables,variable="statedept")
amnesty$statedept[amnesty$statedept=="integer(0)"] <- NA
amnesty$statedept[grep("NA",amnesty$statedept)] <- NA

# milper

amnesty$milper <- lapply(rows,get.variables,variable="milper")
amnesty$milper[amnesty$milper=="integer(0)"] <- NA
amnesty$milper[grep("NA",amnesty$milper)] <- NA

# cinc

amnesty$cinc <- lapply(rows,get.variables,variable="cinc")
amnesty$cinc[amnesty$cinc=="integer(0)"] <- NA
amnesty$cinc[grep("NA",amnesty$cinc)] <- NA

# domestic9

amnesty$domestic9 <- lapply(rows,get.variables,variable="domestic9")
amnesty$domestic9[amnesty$domestic9=="integer(0)"] <- NA
amnesty$domestic9[grep("NA",amnesty$domestic9)] <- NA

# amnesty.uas

amnesty$amnesty.uas <- lapply(rows,get.variables,variable="amnesty.uas")
amnesty$amnesty.uas[amnesty$amnesty.uas=="integer(0)"] <- NA
amnesty$amnesty.uas[grep("NA",amnesty$amnesty.uas)] <- NA

# nyt

amnesty$nyt <- lapply(rows,get.variables,variable="nyt")
amnesty$nyt[amnesty$nyt=="integer(0)"] <- NA
amnesty$nyt[grep("NA",amnesty$nyt)] <- NA

# region

amnesty$region <- lapply(rows,get.variables,variable="region")
amnesty$region[amnesty$region=="integer(0)"] <- NA
amnesty$region[grep("NA",amnesty$region)] <- NA


#TODO: Ethiopia countrycode weirdness
