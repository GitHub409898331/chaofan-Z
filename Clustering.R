#Clustering Part
###########Data preparation
airbnbtrain0 <- read.csv("airbnbtrain.csv")
airbnbtrain <- airbnbtrain0
View(airbnbtrain)
library(dplyr)
airbnbtrain_LA <- filter(airbnbtrain, city=="LA")
table(airbnbtrain_LA$city)  #still have 6 cities remaining
airbnbtrain_LA$city <- droplevels(airbnbtrain_LA$city) #removes unused levels of factor
table(airbnbtrain_LA$city) #after "droplevels" function, become 1 city (LA) left
blankcells <- filter(airbnbtrain_LA, host_response_rate=="")
airbnbtrain_LA[,1:29][airbnbtrain_LA[, 1:29] == ""] <- NA
airbnbtrain_LA <- na.omit(airbnbtrain_LA)

############Data Manipulation
library(plyr)
#make less property type
airbnbtrain_LA$property_type <- mapvalues(x = airbnbtrain_LA$property_type,
                                          from = c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular",
                                                   "Castle","Cave","Chalet","Condominium","Dorm","Earth House","Guest suite","Guesthouse",
                                                   "Hostel","Hut","In-law","Island","Lighthouse","Loft","Parking Space","Serviced apartment",
                                                   "Tent","Timeshare","Tipi","Townhouse","Train","Treehouse","Vacation home","Villa","Yurt"),
                                          to = c("Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other"))

#make 5 types of $bed_type to just 3 types categories (Airbed, Real Bed, Sofa) --> easier for dummy variables
airbnbtrain_LA$bed_type <- mapvalues(x = airbnbtrain_LA$bed_type,
                                     from = c("Couch","Futon","Pull-out Sofa"),
                                     to = c("Sofa","Sofa","Sofa"))

#make 5 types of $cancellation_policy to just 3 types categories (flexible, moderate, strict) --> easier for dummy variables
airbnbtrain_LA$cancellation_policy <- mapvalues(x = airbnbtrain_LA$cancellation_policy,
                                                from = c("super_strict_30","super_strict_60"),
                                                to = c("strict","strict"))

#host_since: change format from "year/month/date" to "year" --> and type becomes numeric
library(lubridate)
airbnbtrain_LA$host_since <- year(airbnbtrain_LA$host_since)
airbnbtrain_LA$host_has_profile_pic <- droplevels(airbnbtrain_LA$host_has_profile_pic)
airbnbtrain_LA$host_identity_verified <- droplevels(airbnbtrain_LA$host_identity_verified)
airbnb_LA <- select(airbnbtrain_LA,-c("city","amenities","description","name","thumbnail_url","zipcode","first_review","last_review")) 

#host_response_rate: remove "%" marks from character type and change type to numeric
library(stringr)
testdata <- data.frame(v1=airbnbtrain_LA$host_response_rate)
testnewdata2 <- data.frame(lapply(testdata, function(x) as.numeric(str_extract(x,'[0-9.]+'))/100) )
airbnbtrain_LA$host_response_rate <- testnewdata2$v1

#$neighbourhood: droplevels to clearout 0 or unusable cities
airbnbtrain_LA$neighbourhood <- droplevels(airbnbtrain_LA$neighbourhood)
neighbourhood_sort <- sort(table(airbnbtrain_LA$neighbourhood))

#$neighbourhood: group them into 5 groups ("North", "West", "East", "South", "Other")
airbnbtrain_LA$neighbourhood <- mapvalues(x = airbnbtrain_LA$neighbourhood,
                                          from = c("Hollywood","Hollywood Hills","North Hollywood", "Sherman Oaks", "Burbank", "Studio City", "Woodland Hills/Warner Center",
                                                   "Mid-Wilshire","Venice","West Hollywood","Santa Monica","Mar Vista","Westwood","Del Rey","Mid-City","Westchester/Playa Del Rey",
                                                   "West Los Angeles","Marina Del Rey","Malibu","Topanga","Culver City","South Robertson","Beverly Hills","Westside",
                                                   "Silver Lake","Echo Park","Pasadena","Downtown","Westlake","Glendale","Los Feliz","Highland Park","East Hollywood",
                                                   "Long Beach","South LA","Redondo Beach"),
                                          to = c("North","North","North","North","North","North","North","West","West","West","West","West","West","West","West","West","West",
                                                 "West","West","West","West","West","West","West","East","East","East","East","East","East","East","East","East","South","South","South"))
airbnbtrain_LA$neighbourhood <- as.character(airbnbtrain_LA$neighbourhood)
neighbourhood_sort1 <- sort(table(airbnbtrain_LA$neighbourhood))
library(Hmisc) #in order to use %nin% function (not in)
airbnbtrain_LA$neighbourhood[airbnbtrain_LA$neighbourhood %nin% c("North","West","East","South")] <- "Other"  #assign those not ("North","West","East","South") to "Other"

#Drop levels for all useful factor columns
str(airbnbtrain_LA)
airbnbtrain_LA$property_type <- droplevels(airbnbtrain_LA$property_type)
airbnbtrain_LA$room_type <- droplevels(airbnbtrain_LA$room_type)
airbnbtrain_LA$bed_type <- droplevels(airbnbtrain_LA$bed_type)
airbnbtrain_LA$cancellation_policy <- droplevels(airbnbtrain_LA$cancellation_policy)
airbnbtrain_LA$cleaning_fee <- droplevels(airbnbtrain_LA$cleaning_fee)
airbnbtrain_LA$host_has_profile_pic <- droplevels(airbnbtrain_LA$host_has_profile_pic)
airbnbtrain_LA$host_identity_verified <- droplevels(airbnbtrain_LA$host_identity_verified)
airbnbtrain_LA$instant_bookable <- droplevels(airbnbtrain_LA$instant_bookable)



library(dplyr)
airbnb_LA <- select(airbnbtrain_LA,-c("city","amenities","description","name","thumbnail_url","zipcode","first_review","last_review")) 

#########dummy process
library(lattice)
library(ggplot2)
library(caret)

#Propertytype
dummy_propertytype <- dummyVars("~property_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_propertytype <- data.frame(predict(dummy_propertytype, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_propertytype)
airbnb_LA <- airbnb_LA[,-3]

#Room_type
dummy_roomtype <- dummyVars("~room_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_roomtype <- data.frame(predict(dummy_roomtype, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_roomtype)
airbnb_LA <- airbnb_LA[,-3]

#Bed_type
dummy_bedtype <- dummyVars("~bed_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_bedtype <- data.frame(predict(dummy_bedtype, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_bedtype)
airbnb_LA <- airbnb_LA[,-5]

#Cancellation Policy
dummy_cancellationpolicy <- dummyVars("~cancellation_policy", data=airbnb_LA, fullRank=FALSE)
traintrsf_cancellationpolicy <- data.frame(predict(dummy_cancellationpolicy, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_cancellationpolicy)
airbnb_LA <- airbnb_LA[,-5]

#Cleaning fee

dummy_cleaningfee <- dummyVars("~cleaning_fee", data=airbnb_LA, fullRank=TRUE)
traintrsf_cleaningfee <- data.frame(predict(dummy_cleaningfee, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_cleaningfee)
airbnb_LA <- airbnb_LA[,-5]

#Host_has_profile_pic

dummy_hosthasprofilepic <- dummyVars("~host_has_profile_pic", data=airbnb_LA, fullRank=TRUE)
traintrsf_hosthasprofilepic <- data.frame(predict(dummy_hosthasprofilepic, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_hosthasprofilepic)
airbnb_LA <- airbnb_LA[,-5]

#Host_identity_verified

dummy_hostidentityverified <- dummyVars("~host_identity_verified", data=airbnb_LA, fullRank=TRUE)
traintrsf_hostidentityverified <- data.frame(predict(dummy_hostidentityverified, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_hostidentityverified)
airbnb_LA <- airbnb_LA[,-5]

#Instant_bookable

dummy_instantbookable <- dummyVars("~instant_bookable", data=airbnb_LA, fullRank=TRUE)
traintrsf_instantbookable <- data.frame(predict(dummy_instantbookable, newdata=airbnb_LA))
airbnb_LA <- cbind(airbnb_LA, traintrsf_instantbookable)
airbnb_LA <- airbnb_LA[,-7]

#Neighbourhood
dummy_neighbourhood <- dummyVars("~neighbourhood", data=airbnb_LA, fullRank=FALSE)
traintrsf_neighbourhood <- data.frame(predict(dummy_neighbourhood, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_neighbourhood)
airbnb_LA <- airbnb_LA[,-9]

###Drop id(just realized I don't need it)
airbnb_LA <- airbnb_LA[,-1]


###########Normalize Data
airbnb_LA.norm <- sapply(airbnb_LA, scale)

##########K-mean Clustering
#K.max
k.max <- 15
wss <- sapply(1:k.max,function(k){kmeans(airbnb_LA.norm,k,nstart=50,iter.max = 15)$tot.withinss})
plot(c(1:k.max),wss,type="b",pch=19,frame=FALSE,xlab="Number of Clusters K",ylab="Total within-Clusters sum of squares",main="Elbow Chart")

#k=4, kmean()
km <- kmeans(airbnb_LA.norm,3)
print(km)

#K-mean visualize
library(tidyverse)
library(cluster)
library(ggplot2)
library(factoextra)

fviz_cluster(km,data = airbnb_LA.norm,geom=c("point"))

##ggplots
library(GGally)
library(dplyr)
kmcluster <- km$cluster
airbnbtrain_LA <- cbind(airbnbtrain_LA, km$cluster)
airbnb_LA <- cbind(airbnb_LA, km$cluster)


names(airbnbtrain_LA)[30] <- "km_cluster"
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$neighbourhood))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$property_type))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$room_type))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$bed_type))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$cancellation_policy))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$cleaning_fee))+geom_histogram(binwidth = 0.5)
ggplot(airbnbtrain_LA,aes(x=airbnbtrain_LA$km_cluster,fill=airbnbtrain_LA$instant_bookable))+geom_histogram(binwidth = 0.5)

#correlation with kmcluster
library(GGally)
correlation <- select(airbnbtrain_LA,log_price,accommodates,bathrooms,
                      host_response_rate,host_since,latitude,longitude,number_of_reviews,review_scores_rating,bedrooms,beds,km_cluster)
ggpairs(correlation)