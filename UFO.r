#import packages;
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(countrycode)

getwd();
df=read.csv('ufo_sighting_data.csv', header = TRUE, na.strings = c("NA","","#NA"))
str(df)

# Checking missing values (missing values or empty values)
colSums(is.na(df)|df=='')

#missmap allows us to explore how much missing data we have.
missmap(df, main="UFO Sightings Missing Data",
        col=c("yellow", "black"), legend=FALSE)

#### VISUALIZATION####
ggplot(data = df, mapping = aes(x = country, y= length_of_encounter_seconds)) +
  geom_boxplot() + ggtitle("UFO Sightings in Countries for duration in Minutes") + theme(plot.title = element_text(hjust = 0.5))


ggplot(df, aes(x=country, y=length_min)) + 
  geom_boxplot() + 
  theme_bw() + xlab("Country") + ylab("Duration of appearance (days)") + ggtitle("Duration of UFO appearance per country (showing outliers)");
ggplot(data = df, mapping = aes(x = country, y= length_of_encounter_seconds)) +
  geom_boxplot() + ggtitle("UFO Sightings in Countries over time") + theme(plot.title = element_text(hjust = 0.5))

#sightings per shape
ggplot(data = df, mapping = aes(x = month_name, fill = country)) +
  geom_bar() + ggtitle("UFO sightings Per Month vs Countries") + theme(plot.title = element_text(hjust = 0.5))

#define a blank_theme
blank_theme <- theme_fivethirtyeight()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

#Overview
g1<-ggplotGrob(
  df %>% 
    group_by(UFO_shape) %>% 
    summarize(count=n()) %>% 
    ggplot(aes(x="",y=count,fill=UFO_shape)) + 
    geom_bar(width = 1, stat = "identity",color='white',size=.215) + coord_polar("y") + 
    scale_fill_manual(name="",values=viridis::inferno(length(unique(df$UFO_shape)))) + 
    blank_theme + 
    theme(axis.text.x=element_blank(), 
          legend.text=element_text(size=7),
          legend.key.size = unit(.4, "cm")))
g2<-df %>% 
  group_by(year,country) %>% 
  summarize(count=n()) %>%
  ggplot(aes(x=year,y=count,fill=country)) + 
  geom_histogram(stat='identity',width=1,color='white',size=.25) + 
  scale_fill_brewer(palette='Paired')  + theme_fivethirtyeight() + 
  ggtitle('UFO sightings overview\nby year, shape, location') + 
  theme(legend.position='right',legend.direction='vertical')

g2 + annotation_custom(grob = g1, xmin = 1900, xmax = 1990, ymin = 1500, ymax = 6500)+ coord_cartesian(ylim = c(0,7500), xlim = c(1900, 2015))

##In details : Location
countries_map <-map_data("world")
world_map<-ggplot() + 
  geom_map(data = countries_map, 
           map = countries_map,aes(long, lat, map_id = region, group = group),
           fill = "white", color = "black", size = 0.1)

world_map + geom_point(data=df,aes(x=longitude,y=latitude),color='red',alpha=.5,size=.75) + theme_fivethirtyeight() + ggtitle('Location of UFO sightings')

states_map<-map_data("state")
usMap<-ggplot() + 
  geom_map(data = states_map, map = states_map,aes(x = long, y = lat, map_id = region, group = group),fill = "white", color = "black", size = 0.1) + 
  theme_fivethirtyeight()
usMap + 
  geom_point(data=filter(df,country=='us'),aes(x=longitude,y=latitude),color='orange',alpha=.75,size=.5) + ggtitle('Location of UFO sightings in the CANADA')

##Time during the day
df %>% group_by(hour,month_name_ordered) %>% summarize(count=n()) %>% ggplot(aes(x=factor(hour),y=count,color=month_name_ordered,group=month_name_ordered)) + geom_line() + theme_fivethirtyeight() + scale_color_manual(name="",values=brewer.pal(12,'Paired')) + geom_point(color='black',size=.5,alpha=1) + ggtitle('# of UFO observations during Hour of the day')

##Time during the week
df %>% group_by(weekday,month_name_ordered) %>% summarize(count=n()) %>% ggplot(aes(x=weekday,y=count,color=month_name_ordered,group=month_name_ordered)) + geom_line() + theme_fivethirtyeight() + scale_color_brewer(name="",palette='Paired') + geom_point(color='black',size=.5,alpha=1) + ggtitle('# of UFO observations during the week')

###outliers####
df %>% filter(length_min>5e5) %>% 
  select(-c(length_of_encounter_seconds,date, latitude,longitude, date_documented,date,UFO_shape,year, month, day ,hour, min,state.province))
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(df, "length_min", which(df$length_min > 5e6), NA)
df1 = df[!is.na(df$length_min),]# deleting NA in column length_min
sapply(df1, function(x) sum(is.na(x))) #check for nullin all columns


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
df$length_min<-as.numeric(df$length_of_encounter_seconds)/60

###### CLustering######

df$length_of_encounter_seconds<-as.character(df$length_of_encounter_seconds)
df$length_of_encounter_seconds<-as.numeric(df$length_of_encounter_seconds)
df$latitude<-as.numeric(df$latitude)

df<-df[sample(nrow(df), 5000), ]
#scale the variables
#dfsubset <- subset(df, select = -c(X,city,state.province,country,UFO_shape,date_documented))
dfsubset <- subset(df, select = c(latitude,longitude,Year,Month,Day,Hour, Min))
scaled_df <- scale(dfsubset,scale=TRUE)
scaled_df<-cbind(scaled_df,df$UFO_shape,df$country,df$state.province)

#pca
pcmp <- prcomp(scaled_df, scale = TRUE)
fviz_eig(pcmp)

###
library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(pcmp)
eig.val

# Results for Variables
res.var <- get_pca_var(pcmp)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 


###########
# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(scaled_df)

# First for principal components
comp <- data.frame(pc$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))
library(rgl)
# Multi 3D plot
library(plot3D)
scatter3D(comp$PC2, comp$PC1, comp$PC3)

#netkmeans
# Determine number of clusters
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(comp,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# From scree plot elbow occurs at k = 3
# Apply k-means with k=3
k <- kmeans(comp, 3, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

# 3D plot
plot(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3D(comp$PC1, comp$PC3, comp$PC4, col=k$clust)

############kmeans
kclust <- kmeans(scaled_df,centers = 3,iter.max = 100)

pred_pc <- predict(pcmp, newdata=scaled_df)[,1:2]


ggplot(comp,aes(comp$PC1,comp$PC2))+
  geom_point(aes(color = as.factor(kclust$cluster)),size=3)
# Compare by cluster in boxplot
boxplot(df$latitude ~ k$clust,
        xlab='Cluster', ylab='Lat',
        main='Lat by Cluster')
boxplot(df$longitude ~ k$clust,
        xlab='Cluster', ylab='Long',
        main='Long by Cluster')
boxplot(df$Year ~ k$clust,
        xlab='Cluster', ylab='Year',
        main='Year by Cluster')
boxplot(df$Month ~ k$clust,
        xlab='Cluster', ylab='Month',
        main='Month by Cluster')
boxplot(df$Day ~ k$clust,
        xlab='Cluster', ylab='Day',
        main='Day by Cluster')
boxplot(df$Hour ~ k$clust,
        xlab='Cluster', ylab='Hour',
        main='Hour by Cluster')
boxplot(df$Min ~ k$clust,
        xlab='Cluster', ylab='Min',
        main='Min by Cluster')
hist(df$UFO_shape~ k$clust,
     xlab='Cluster', ylab='Shape',
     main='Shape by Cluster')
boxplot(df$Country ~ k$clust,
        xlab='Cluster', ylab='Country',
        main='Country by Cluster')
boxplot(df$state.province ~ k$clust,
        xlab='Cluster', ylab='state/prov',
        main='state/prov by Cluster')