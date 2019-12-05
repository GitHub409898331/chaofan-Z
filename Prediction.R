#AD699 Group: LA
airbnbtrain0 <- read.csv("airbnbtrain.csv")
airbnbtrain <- airbnbtrain0
View(airbnbtrain)

library(dplyr)
colnames(airbnbtrain)
airbnbtrain_LA <- filter(airbnbtrain, city=="LA")
table(airbnbtrain_LA$city)  #still have 6 cities remaining
str(airbnbtrain_LA$city) #before applying "droplevels" function, there are 6 levels of factor

airbnbtrain_LA$city <- droplevels(airbnbtrain_LA$city) #removes unused levels of factor
table(airbnbtrain_LA$city) #after "droplevels" function, become 1 city (LA) left 
str(airbnbtrain_LA$city) #1 level of factor "LA"

#Step I:
#I. Missing Values
#A
anyNA(airbnbtrain_LA)
blankcells <- filter(airbnbtrain_LA, host_response_rate=="")

airbnbtrain_LA[,1:29][airbnbtrain_LA[, 1:29] == ""] <- NA
airbnbtrain_LA <- na.omit(airbnbtrain_LA)
anyNA(airbnbtrain_LA)


#Step II: Prediction (20 points)
#I.
#A
library(dplyr)
library(GGally)

str(airbnbtrain_LA)

#numeric variables: accommodates, bathrooms, latitude, longitude, number_of_reviews, review_scores_rating, bedrooms, beds
colnames(airbnbtrain_LA)

ggpairs(airbnbtrain_LA[c(6, 7, 20, 21, 24, 25, 28, 29)])

#3#make 35 types of $property_type to 3 types categories (Apartment, House, Other) --> easier for dummy variables
install.packages("plyr")
library(plyr)
airbnbtrain_LA$property_type <- mapvalues(x = airbnbtrain_LA$property_type,
                                          from = c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular",
                                                   "Castle","Cave","Chalet","Condominium","Dorm","Earth House","Guest suite","Guesthouse",
                                                   "Hostel","Hut","In-law","Island","Lighthouse","Loft","Parking Space","Serviced apartment",
                                                   "Tent","Timeshare","Tipi","Townhouse","Train","Treehouse","Vacation home","Villa","Yurt"),
                                          to = c("Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other"))

#8#make 5 types of $bed_type to just 3 types categories (Airbed, Real Bed, Sofa) --> easier for dummy variables
airbnbtrain_LA$bed_type <- mapvalues(x = airbnbtrain_LA$bed_type,
                                          from = c("Couch","Futon","Pull-out Sofa"),
                                          to = c("Sofa","Sofa","Sofa"))

#9#make 5 types of $cancellation_policy to just 3 types categories (flexible, moderate, strict) --> easier for dummy variables
airbnbtrain_LA$cancellation_policy <- mapvalues(x = airbnbtrain_LA$cancellation_policy,
                                     from = c("super_strict_30","super_strict_60"),
                                     to = c("strict","strict"))

#14#$host_has_profile_pic: need to use droplevels from 3 factors (because there's a 0 include) to 2 factors --> (t, f)
table(airbnbtrain_LA$host_has_profile_pic)
airbnbtrain_LA$host_has_profile_pic <- droplevels(airbnbtrain_LA$host_has_profile_pic)
table(airbnbtrain_LA$host_has_profile_pic)
str(airbnbtrain_LA$host_has_profile_pic)

#15#host_identity_verified: need to use droplevels from 3 factors (because there's a 0 include) to 2 factors --> (t, f)
table(airbnbtrain_LA$host_identity_verified)
airbnbtrain_LA$host_identity_verified <- droplevels(airbnbtrain_LA$host_identity_verified)
table(airbnbtrain_LA$host_identity_verified)
str(airbnbtrain_LA$host_identity_verified)

#16#$host_response_rate: exchange the type from factor to character
str(airbnbtrain_LA$host_response_rate)
airbnbtrain_LA$host_response_rate <- as.character(airbnbtrain_LA$host_response_rate)
class(airbnbtrain_LA$host_response_rate)
anyNA(airbnbtrain_LA$host_response_rate)
View(airbnbtrain_LA$host_response_rate)

#16#$host_response_rate: remove "%" marks from character type and change type to numeric
install.packages("stringr")
library(stringr)
testdata <- data.frame(v1=airbnbtrain_LA$host_response_rate)
testnewdata2 <- data.frame(lapply(testdata, function(x) as.numeric(str_extract(x,'[0-9.]+'))/100) )
airbnbtrain_LA$host_response_rate <- testnewdata2$v1
class(airbnbtrain_LA$host_response_rate)
str(airbnbtrain_LA$host_response_rate)

#17#$host_since: change format from "year/month/date" to "year" --> and type becomes numeric
install.packages("lubridate")
library(lubridate)
airbnbtrain_LA$host_since <- year(airbnbtrain_LA$host_since)

str(airbnbtrain_LA$host_since)

#23#$neighbourhood: droplevels to clearout 0 or unusable cities
airbnbtrain_LA$neighbourhood <- droplevels(airbnbtrain_LA$neighbourhood)
neighbourhood_sort <- sort(table(airbnbtrain_LA$neighbourhood))
View(neighbourhood_sort)

#23#$neighbourhood: group them into 5 groups ("North", "West", "East", "South", "Other")
airbnbtrain_LA$neighbourhood <- mapvalues(x = airbnbtrain_LA$neighbourhood,
                                     from = c("Hollywood","Hollywood Hills","North Hollywood", "Sherman Oaks", "Burbank", "Studio City", "Woodland Hills/Warner Center",
                                              "Mid-Wilshire","Venice","West Hollywood","Santa Monica","Mar Vista","Westwood","Del Rey","Mid-City","Westchester/Playa Del Rey",
                                              "West Los Angeles","Marina Del Rey","Malibu","Topanga","Culver City","South Robertson","Beverly Hills","Westside",
                                              "Silver Lake","Echo Park","Pasadena","Downtown","Westlake","Glendale","Los Feliz","Highland Park","East Hollywood",
                                              "Long Beach","South LA","Redondo Beach"),
                                     to = c("North","North","North","North","North","North","North","West","West","West","West","West","West","West","West","West","West",
                                            "West","West","West","West","West","West","West","East","East","East","East","East","East","East","East","East","South","South","South"))

View(airbnbtrain_LA$neighbourhood)

str(airbnbtrain_LA$neighbourhood)
airbnbtrain_LA$neighbourhood <- as.character(airbnbtrain_LA$neighbourhood)
class(airbnbtrain_LA$neighbourhood)
anyNA(airbnbtrain_LA$neighbourhood)
table(airbnbtrain_LA$neighbourhood)
neighbourhood_sort1 <- sort(table(airbnbtrain_LA$neighbourhood))
View(neighbourhood_sort1)

install.packages("Hmisc")  #in order to use %nin% function (not in)
library(Hmisc)
airbnbtrain_LA$neighbourhood[airbnbtrain_LA$neighbourhood %nin% c("North","West","East","South")] <- "Other"  #assign those not ("North","West","East","South") to "Other"
table(airbnbtrain_LA$neighbourhood)

#new dataframe "airbnb_LA": without unusable information columns
airbnb_LA <- airbnbtrain_LA[, -c(1,5,6,11,12,13,19,20,21,22,26,27)]
View(airbnb_LA)

##########################################Multiple Regression Model################################################

#airbnb_LA
#2#$property_type
library(caret)
dummy_propertytype <- dummyVars("~property_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_propertytype <- data.frame(predict(dummy_propertytype, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_propertytype)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-2]
View(airbnb_LA)

#3#$room_type
dummy_roomtype <- dummyVars("~room_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_roomtype <- data.frame(predict(dummy_roomtype, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_roomtype)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-2]
View(airbnb_LA)

#5#$bed_type
dummy_bedtype <- dummyVars("~bed_type", data=airbnb_LA, fullRank=FALSE)
traintrsf_bedtype <- data.frame(predict(dummy_bedtype, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_bedtype)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-3]
View(airbnb_LA)

#6#$cancellation_policy
dummy_cancellationpolicy <- dummyVars("~cancellation_policy", data=airbnb_LA, fullRank=FALSE)
traintrsf_cancellationpolicy <- data.frame(predict(dummy_cancellationpolicy, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_cancellationpolicy)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-3]
View(airbnb_LA)

#7#$cleaning_fee
dummy_cleaningfee <- dummyVars("~cleaning_fee", data=airbnb_LA, fullRank=TRUE)
traintrsf_cleaningfee <- data.frame(predict(dummy_cleaningfee, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_cleaningfee)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-3]
View(airbnb_LA)

#8#$host_has_profile_pic
dummy_hosthasprofilepic <- dummyVars("~host_has_profile_pic", data=airbnb_LA, fullRank=TRUE)
traintrsf_hosthasprofilepic <- data.frame(predict(dummy_hosthasprofilepic, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_hosthasprofilepic)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-3]
View(airbnb_LA)

#9#$host_identity_verified
dummy_hostidentityverified <- dummyVars("~host_identity_verified", data=airbnb_LA, fullRank=TRUE)
traintrsf_hostidentityverified <- data.frame(predict(dummy_hostidentityverified, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_hostidentityverified)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-3]
View(airbnb_LA)

#12#$instant_bookable
dummy_instantbookable <- dummyVars("~instant_bookable", data=airbnb_LA, fullRank=TRUE)
traintrsf_instantbookable <- data.frame(predict(dummy_instantbookable, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_instantbookable)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-5]
View(airbnb_LA)

#13#$neighbourhood
dummy_neighbourhood <- dummyVars("~neighbourhood", data=airbnb_LA, fullRank=FALSE)
traintrsf_neighbourhood <- data.frame(predict(dummy_neighbourhood, newdata=airbnb_LA))

airbnb_LA <- cbind(airbnb_LA, traintrsf_neighbourhood)
View(airbnb_LA)
airbnb_LA <- airbnb_LA[,-5]
View(airbnb_LA)

anyNA(airbnb_LA)

#partition: training set - 60% | validation set - 40%
set.seed(168)
dim(airbnb_LA)
airbnb_LA_1 <- sample_n(airbnb_LA, 10068)
View(airbnb_LA_1)
airbnb_LA_1train <- slice(airbnb_LA_1, 1:6041)
airbnb_LA_1valid <- slice(airbnb_LA_1, 6042:10068)

#create Multiple Linear Regression Model
colnames(airbnb_LA_1)

airbnb_LA_1trainfitmlr <- lm(log_price~bathrooms+host_response_rate+host_since+number_of_reviews+review_scores_rating+
                               bedrooms+beds+property_type.Apartment+property_type.Other+property_type.House+room_type.Entire.home.apt+
                               room_type.Private.room+room_type.Shared.room+bed_type.Airbed+bed_type.Sofa+bed_type.Real.Bed+
                               cancellation_policy.flexible+cancellation_policy.moderate+cancellation_policy.strict+cleaning_fee.True+
                               host_has_profile_pic.t+host_identity_verified.t+instant_bookable.t+neighbourhoodEast+neighbourhoodNorth+
                               neighbourhoodOther+neighbourhoodSouth+neighbourhoodWest, data=airbnb_LA_1train)
airbnb_LA_1trainfitmlr
summary(airbnb_LA_1trainfitmlr)

#Backward Elimination
airbnb_LA_1trainfitmlrstep <- step(airbnb_LA_1trainfitmlr, direction = "backward")
summary(airbnb_LA_1trainfitmlrstep)

#Final Multiple Linear Regression Model
myjudgementmlr <- lm(log_price~bathrooms+number_of_reviews+review_scores_rating+
                       bedrooms+beds+property_type.House+room_type.Entire.home.apt+room_type.Private.room+
                       bed_type.Sofa+cancellation_policy.strict+cleaning_fee.True+host_identity_verified.t+
                       instant_bookable.t+neighbourhoodWest, data=airbnb_LA_1train)
myjudgementmlr
summary(myjudgementmlr)

#accuracy of model
library(forecast)
predict2_train <- predict(myjudgementmlr, airbnb_LA_1train)
accuracy(predict2_train, airbnb_LA_1train$log_price)
mean(airbnb_LA_1train$log_price)
0.3873512/4.692251  #RMSE/mean (training set)

predict2_valid <- predict(myjudgementmlr, airbnb_LA_1valid)
accuracy(predict2_valid, airbnb_LA_1valid$log_price)
mean(airbnb_LA_1valid$log_price)
0.3829451/4.676853  #RMSE/mean (validation set)