## This script pre-processes my data of Amnesty mentions in NYT. 

rm(list=ls())
library("plyr")
setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/amnesty-uas")

# Read in data
am <- read.csv("Data/python-output/amnesty-mentions-in-nyt.csv")
names(am)

# Add new column for year
am$date <- as.character(am$date)
am$year <- substr(am$date, nchar(am$date)-3, nchar(am$date))
am$year <- as.integer(am$year)
summary(am$year)

###################
#### Countries ####
###################

countries <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)
countries$Value <- as.character(countries$Value)

countries$iso3c[countries$Value=="Serbia"] <- "YUG"
countries$iso3c[countries$Value=="Yugoslavia"] <- "YUG"


# Taking country from headline or locations
country.search <- function(x,y,z){
  country.index <- union(grep(x, z$headline,ignore.case=T),grep(x, z$locations,ignore.case=T))
  if (y %in% colnames(z)){
    z[,y][country.index] <- 1
  } else {
    z[[y]] <- NA
    z[,y][country.index] <- 1
  }
  return(z)
}

#testing
am.test <- am
am.test <- country.search(countries$Key[1],countries$Value[1],am) #testing

# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  am.test <- country.search(countries$Key[i],countries$Value[i],am.test)
}
sum(am.test$Afghanistan, na.rm=TRUE)

#renaming to iso3c
iso3c <- countries$iso3c
names(iso3c) <- countries$Value
am.test <- rename(am.test, replace = iso3c)

sum(am.test$AFG, na.rm=TRUE)

## Problematic countries
am.test$MAC <- NA
am.test$MKD <- NA
am.test$MNE <- NA
am.test$YUG <- NA
am.test$SRB <- NA

#Macedonia
#< 1992: "MKD"
x <- union(grep("Macedonia", am.test$headline,ignore.case=T),grep("Macedonia", am.test$locations,ignore.case=T))
y <- which(am.test$year<1992)
am.test$MKD[intersect(x,y)] <- 1

#> 1991: "MAC"
x <- union(grep("Macedonia", am.test$headline,ignore.case=T),grep("Macedonia", am.test$locations,ignore.case=T))
y <- which(am.test$year>1991)
am.test$MAC[intersect(x,y)] <- 1

# Yugoslavia
# < 2003: MKD # 1980-2002
x <- union(grep("Yugoslavia", am.test$headline,ignore.case=T),grep("Yugoslavia", am.test$locations,ignore.case=T))
y <- which(am.test$year<2003)
am.test$MKD[intersect(x,y)] <- 1

# > 2002: YUG # Serbia + Montenegro, 2003-2006
x <- union(grep("Yugoslavia", am.test$headline,ignore.case=T),grep("Yugoslavia", am.test$locations,ignore.case=T))
y <- which(am.test$year > 2002)
am.test$YUG[intersect(x,y)] <- 1


# > 2005: SRB #Serbia, after 2006
x <- union(grep("Yugoslavia", am.test$headline,ignore.case=T),grep("Yugoslavia", am.test$locations,ignore.case=T))
y <- which(am.test$year>2005)
am.test$SRB[intersect(x,y)] <- 1

# Kosovo:
# > 2007: MNE
x <- union(grep("Kosovo", am.test$headline,ignore.case=T),grep("Kosovo", am.test$locations,ignore.case=T))
y <- which(am.test$year > 2007)
am.test$MNE[intersect(x,y)] <- 1

# < 2008: MKD
x <- union(grep("Kosovo", am.test$headline,ignore.case=T),grep("Kosovo", am.test$locations,ignore.case=T))
y <- which(am.test$year < 2008)
am.test$MKD[intersect(x,y)] <- 1

# Serbia:
# < 2003: MKD # 1980-2002
x <- union(grep("Serbia", am.test$headline,ignore.case=T),grep("Serbia", am.test$locations,ignore.case=T))
y <- which(am.test$year < 2003)
am.test$MKD[intersect(x,y)] <- 1

#> 2002: YUG # Serbia + Montenegro, 2003-2006
x <- union(grep("Serbia", am.test$headline,ignore.case=T),grep("Serbia", am.test$locations,ignore.case=T))
y <- which(am.test$year > 2002)
am.test$YUG[intersect(x,y)] <- 1

#> 2005: SRB #Serbia, after 2006
x <- union(grep("Serbia", am.test$headline,ignore.case=T),grep("Serbia", am.test$locations,ignore.case=T))
y <- which(am.test$year > 2005)
am.test$SRB[intersect(x,y)] <- 1

write.csv(am.test,"amnesty_processed.csv")
