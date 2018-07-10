#import packages;
rm(list = ls())

library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(countrycode)
library(MASS)
getwd();
data=read.csv('ufo_sighting_data.csv', header = TRUE, na.strings = c("NA","","#NA"))

#check # of rows
ncol(data);
nrow(data);


#### DATA CLEANING ######

#Fix Date
library(stringr)
dataCols<-str_split_fixed(data$Date_time, "/", 3)
yearCols<-str_split_fixed(dataCols[,3], " ", 2)
hourminCol<-str_split_fixed(yearCols[,2], ":", 2)
#cityCols[,2]<-cityCols2[,1] # this has stripped UK values, but we want the citys to be disambiguated by them still
# split variables, and combine into datcat
#                       mm dd          yy           hh mm
datcat<-cbind(dataCols[,1:2],yearCols[,1],hourminCol[,1:2])
names(datcat)[12] <- "Month"
names(datcat)[13] <- "Day"
names(datcat)[14] <- "Year"
names(datcat)[15] <- "Hour"
names(datcat)[16] <- "Min"

#now we have our new date/time variables at the end
data<-cbind(data,datcat)

#Fix State
ufo_locations=read.csv('ufo_locations.txt',sep="\t", header = FALSE )
colnames(ufo_locations)<-c('country','stateprov','names')
library(dplyr)
ufo_locations <- mutate_all(ufo_locations, funs(tolower))

#make datacopy
new <- data
#The loop below pulls stay and province names from a lookup table, and populates missing us/canada values
# col 4 country col 3 is State
for (i in 1:80332) {
  if (is.na(data[i,4])) {
    x<-match(ufo_locations[,2],new[i,3])
    x<-as.logical(x)
    
    if (1 %in% x) {
      val<-ufo_locations[which(x == TRUE),1]
      new[i,4]<-val
    }
  }
}

#Fix country
#this code makes fixes uk/### city names, as well as countries that are listed in city names
#after splitting the columns to disambiguate country, it populates missing rows
blankCon<-is.na(new$country)
cityCols<-str_split_fixed(data$city, "[()]", 3)
cityCols2<-str_split_fixed(cityCols[,2],"/",2)
countryCol<-countrycode(cityCols2[blankCon==TRUE], 'country.name', 'iso2c')

new$country<-as.character(new$country)
new$state.province<-as.character(new$state.province)
for (j in 1:80332){
  if (new$country[j]=='gb'){
    new$state.province[j]<-'XX'
  }
}
new$state.province[new$country=='gb']<-'XX'


new$country[new$country[blankCon]]<-countrycode(cityCols2[,1], 'country.name', 'iso2c')
for (j in 1:80332){
  if(blankCon[j]){
    new$country[j]<-countrycode(cityCols2[j,1], 'country.name', 'iso2c')
  }
}
new$state.province[new$country!='us' &new$country!='ca' & !is.na(new$country)  ]<-'XX'


new$country<-as.factor(new$country)
new$country <- toupper(new$country)
#final dataframe dropping the redundant/uninterpretable columns
names(new)[12] <- "Month"
names(new)[13] <- "Day"
names(new)[14] <- "Year"
names(new)[15] <- "Hour"
names(new)[16] <- "Min"
canus<-(new$country=='CA'| new$country=='US')
badindex<-is.na(new$state.province)==TRUE
levels(new$country) <- c(levels(new$country),"XX")
new$state.province[badindex==TRUE]==as.factor("XX")


new=na.omit(new)
df <- new[ -c(1,7, 8) ]

write.csv(df, file = "UFO_clean.csv")

