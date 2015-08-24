#### Load data

setwd("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/amnesty-uas")

#total.amnesty <- amnesty

ua <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Amnesty\ UA\ project/Data/cleaned\ data/total.ua.1990-2012.csv") # this is the data I collected
ai <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Amnesty\ UA\ project/Data/cleaned\ data/total.ai.1985-2007.csv")

names(ai)
ai$X <- NULL
ai$country <- NULL
ai$region <- NULL

names(ua)
ua$X <- NULL
ua$country <- NULL
ua$region <- NULL

########################
#### merge datasets ####
########################

total.amnesty <- merge(ai,ua,by="index",all.x=TRUE,all.y=TRUE)
names(total.amnesty) <- c("index","link","title","ua","network.number","date","keywords","teaser","year","teaser.scrape","link.scrape","title.scrape","date.scrape","year.scrape")

# merge link column
total.amnesty$link2 <- NA
total.amnesty$link2 <- total.amnesty$link.scrape
index <- which(is.na(total.amnesty$link2))
index
total.amnesty$link2 <- as.character(total.amnesty$link2)
links <- as.character(total.amnesty$link[which(is.na(total.amnesty$link2))])
total.amnesty$link2[index] <- links
is.na(links)

# writing function to turn absolute URLs to relative URLs
relative.urls <- function(x){
  link <- sub("http://www.amnesty.org","",x)
  return(link)
}
total.amnesty$link2 <- lapply(total.amnesty$link2,relative.urls)

###################
#### Countries ####
###################

# getting country via title (takes the first part of the title before the colon):
library("stringr")
extract.country <- function(title){
  country <- str_extract(title,"-?[^:]*:")
  country <- str_replace(country,".*-\\s","")
  country <- str_replace(country,":","")
  return(country)
}

total.amnesty$country <- lapply(total.amnesty$title,extract.country)

# doing the same for NA countries (i.e. UAs that I got via scraping) - this just requires running same function on $title.scrape

index <- which(is.na(total.amnesty$country))
index
index.title <- total.amnesty$title.scrape[index]
index.country <- lapply(index.title,extract.country)
total.amnesty$country[index] <- index.country

##### Adding Codes

total.amnesty <- read.csv("Data/total_amnesty2.csv") # now with country codes
names(total.amnesty)

# Read in Country Codes
countries <- read.csv("/Users/rterman/Dropbox/berkeley/Dissertation/Data\ and\ Analyais/Git\ Repos/human-rights-coverage/country_codes.csv")
countries$Key <- as.character(countries$Key)
countries$iso3c <- as.character(countries$iso3c)
countries$Value <- as.character(countries$Value)

countries$iso3c[countries$Value=="Serbia"] <- "YUG"
countries$iso3c[countries$Value=="Yugoslavia"] <- "YUG"

# write function to get code from total.amnesty#country
names(total.amnesty)
country.code <- function(x,y,z){
  country.index <- (grepl(x, z$country,ignore.case=T))
  z$countrycode[country.index] <- as.character(y)
  return(z$countrycode)
}

total.amnesty$countrycode <- NA
# Apply function to all countries in the key-value list
n <- nrow(countries)
for(i in 1:n){
  total.amnesty$countrycode <- country.code(countries$Key[i],countries$iso3c[i],total.amnesty)
}

unique(total.amnesty$country[is.na(total.amnesty$countrycode)])

# Fix problematic codes

total.amnesty$countrycode[total.amnesty$country=="DRC"] <- "COD"
total.amnesty$countrycode[total.amnesty$country=="Macedonia" & total.amnesty$year < 1992] <- "MKD"
total.amnesty$countrycode[total.amnesty$country=="Macedonia" & total.amnesty$year > 1991] <- "MAC"

total.amnesty$countrycode[total.amnesty$country=="Yugoslavia" & total.amnesty$year < 2003] <- "MKD"
total.amnesty$countrycode[total.amnesty$country=="Yugoslavia" & total.amnesty$year > 2002] <- "YUG"
total.amnesty$countrycode[total.amnesty$country=="Yugoslavia" & total.amnesty$year > 2005] <- "SRB"

total.amnesty$countrycode[total.amnesty$country=="Serbia" & total.amnesty$year < 2003] <- "MKD"
total.amnesty$countrycode[total.amnesty$country=="Serbia" & total.amnesty$year > 2002] <- "YUG"
total.amnesty$countrycode[total.amnesty$country=="Serbia" & total.amnesty$year > 2005] <- "SRB"

total.amnesty$countrycode[total.amnesty$country=="Kosovo" & total.amnesty$year > 2007] <- "MNE"

total.amnesty$countrycode[total.amnesty$country=="Montenegro" & total.amnesty$year > 2005] <- "MNE"

# remove duplicates by index
duplicate.index <- which(duplicated(total.amnesty$index))
total.amnesty <- total.amnesty[-duplicate.index,]

##############
#### year #### 
##############

names(total.amnesty)
names(total.amnesty)[9] <- "year.x"
total.amnesty$year <- NA
total.amnesty$year <- total.amnesty$year.x
index <- which(is.na(total.amnesty$year))
years <- total.amnesty$year.scrape[index]
total.amnesty$year[index] <- years
summary(total.amnesty$year)





## reading, writing
total.amnesty$link2 <- as.character(total.amnesty$link2)
total.amnesty$country <- as.character(total.amnesty$country)
write.csv(total.amnesty,file="Data/total_amnesty2.csv")

