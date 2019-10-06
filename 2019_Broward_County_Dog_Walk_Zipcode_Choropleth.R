library(tidyverse)
library(rgdal)
library(tigris)
library(DT)
library(knitr)
library(ggplot2)
library(sp)
library(dplyr)
library(maps)

require(RODBC)

#create a connection to SQL server using 64-bit DSN
myconn <- odbcConnect("istproject")

#ready the sql to send to the server

Walks<- "SELECT 
	(SELECT
COUNT(requested_service_date)
FROM order_detail OD
JOIN service_order SO
ON SO.service_order_id = OD.service_order_id
WHERE OD.requested_service_date = SO.order_date) as SameDayWalks,
(
  SELECT 
  COUNT(requested_service_date)
  FROM order_detail OD
  JOIN service_order SO
  ON SO.service_order_id = OD.service_order_id
  WHERE OD.requested_service_date != SO.order_date) as PreScheduledWalks,
  (
  SELECT
  COUNT(requested_service_date)) as TotalWalks
  FROM order_detail"
#send request to the server and store results in a variable
sqlResult<- sqlQuery(myconn, Walks)


df<- data.frame(walks = c("Same Day Walks","Pre-Scheduled Walks", "Total Amount of Walks"), amount = c(8,16,24))

g<- ggplot(data=df, aes(x=walks, y=amount)) + 
  geom_bar(stat = "identity",fill="steelblue") + 
  geom_text(aes(label=amount), vjust=1.6, color="white", size=3.5)+
  labs(title = "Same Day vs Prescheduled Walks", x="Type of Walk", y="Amount of Walks") +
  theme_minimal()
g

zipcode<-"SELECT
	home_zip
FROM ultimutt_walk_customer"
ZipResult<- sqlQuery(myconn, zipcode)
zips<-read.csv("zipcode.csv")
zips[9,4]<-1
broward<- county_subdivisions('Florida', 'Broward')
sp<- SpatialPoints(zips)

broward_fortify<- fortify(broward)
sp_fortify<- fortify(zips)
gg<- ggplot()
gg <- gg + geom_polygon(data=broward_fortify, aes(x=long, y=lat, group=group, fill="white"), color = "black", fill=NA, size=0.5) 
gg <- gg + geom_point(data=zips, aes(x=zips$long, y=zips$lat, color=zips$zip, size = zips$ct)) + scale_color_gradient(low="black", high="red")
gg <- gg +  coord_map()
gg<- gg + coord_cartesian(xlim=c(-80.3,-80),ylim = c(26.05,26.3))
gg + ggtitle("Most Serviced Areas in Broward County, FL")
gg


#create histogram for breed
Breed<- "SELECT
TOP 5 pet_breed
,COUNT(pet_breed) as Count_of_Breed
FROM pet
GROUP BY pet_breed
ORDER BY Count_of_Breed DESC"

BreedResult<-sqlQuery(myconn, Breed)
BreedResult$pet_breed<- as.factor(BreedResult$pet_breed)
b<- ggplot(BreedResult, aes(pet_breed))
b<- b + geom_histogram(aes(weight = Count_of_Breed), binwidth = 0.1, stat = count) + ylab("Count of Breed")
b



#close all connections
odbcCloseAll() 
#Fin
