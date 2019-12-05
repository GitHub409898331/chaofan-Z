
airbnb <- read.csv("airbnbtrain.csv")
View(airbnb)
cityLA <- airbnb[airbnb$city == "LA",]
View(cityLA)


########################################## Part 3: K-nearest neighbors #########################################

# A:
# Possible predictors: log_price (2); property_type (3); room_type (4); accomodates (6); host_identity_verified (15); neighbourhood (23)
# Final outcome: cleaning_fee (10)
airbnbKnn <- cityLA[, c(2:4, 6, 15, 23, 10)]
anyNA(airbnbKnn)
View(airbnbKnn)

# Make 35 types of $property_type to 3 types categories (Apartment, House, Other) --> easier for dummy variables
library(plyr)
airbnbKnn$property_type <- mapvalues(x = airbnbKnn$property_type,
                                          from = c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular",
                                                   "Castle","Cave","Chalet","Condominium","Dorm","Earth House","Guest suite","Guesthouse",
                                                   "Hostel","Hut","In-law","Island","Lighthouse","Loft","Parking Space","Serviced apartment",
                                                   "Tent","Timeshare","Tipi","Townhouse","Train","Treehouse","Vacation home","Villa","Yurt"),
                                          to = c("Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other"))

# Host_identity_verified: need to use droplevels from 3 factors (because there's a 0 include) to 2 factors --> (t, f)
table(airbnbKnn$host_identity_verified)
airbnbKnn <- filter(airbnbKnn, host_identity_verified %in% c("f", "t"))          
airbnbKnn$host_identity_verified <- droplevels(airbnbKnn$host_identity_verified)
table(airbnbKnn$host_identity_verified)

# Neighbourhood: droplevels to clearout 0 or unusable cities
table(airbnbKnn$neighbourhood)
airbnbKnn$neighbourhood <- droplevels(airbnbKnn$neighbourhood)
airbnbKnn$neighbourhood[airbnbKnn$neighbourhood == ""] <- NA
anyNA(airbnbKnn$neighbourhood)
airbnbKnn$neighbourhood <- droplevels(airbnbKnn$neighbourhood)
neighbourhood_sort <- sort(table(airbnbKnn$neighbourhood))
View(neighbourhood_sort)

# Neighbourhood: group them into 5 groups ("North", "West", "East", "South", "Other")
library(tidyr)
airbnbKnn$neighbourhood <- mapvalues(x = airbnbKnn$neighbourhood,
                                          from = c("Hollywood","Hollywood Hills","North Hollywood", "Sherman Oaks", "Burbank", "Studio City", "Woodland Hills/Warner Center",
                                                   "Mid-Wilshire","Venice","West Hollywood","Santa Monica","Mar Vista","Westwood","Del Rey","Mid-City","Westchester/Playa Del Rey",
                                                   "West Los Angeles","Marina Del Rey","Malibu","Topanga","Culver City","South Robertson","Beverly Hills","Westside",
                                                   "Silver Lake","Echo Park","Pasadena","Downtown","Westlake","Glendale","Los Feliz","Highland Park","East Hollywood",
                                                   "Long Beach","South LA","Redondo Beach"),
                                          to = c("North","North","North","North","North","North","North","West","West","West","West","West","West","West","West","West","West",
                                                 "West","West","West","West","West","West","West","East","East","East","East","East","East","East","East","East","South","South","South"))

View(airbnbKnn$neighbourhood)
anyNA(airbnbKnn$neighbourhood)
airbnbKnn <- airbnbKnn %>% drop_na(neighbourhood)
anyNA(airbnbKnn$neighbourhood)
View(airbnbKnn$neighbourhood)

# Assign those not ("North","West","East","South") to "Other"
airbnbKnn$neighbourhood <- as.character(airbnbKnn$neighbourhood)
library(Hmisc)
airbnbKnn$neighbourhood[airbnbKnn$neighbourhood %nin% c("North","West","East","South")] <- "Other"
table(airbnbKnn$neighbourhood)
airbnbKnn$neighbourhood <- as.factor(airbnbKnn$neighbourhood)

# Convert predictors that are categorical to binary dummies
str(airbnbKnn)
table(airbnbKnn$property_type)
table(airbnbKnn$room_type)
table(airbnbKnn$host_identity_verified)
table(airbnbKnn$neighbourhood)

library(caret)
dmy1 <- dummyVars("~property_type", data = airbnbKnn)
trsf1 <- data.frame(predict(dmy1, newdata = airbnbKnn))
airbnbKnn1 <- cbind(airbnbKnn, trsf1)

dmy2 <- dummyVars("~room_type", data = airbnbKnn1)
trsf2 <- data.frame(predict(dmy2, newdata = airbnbKnn1))
airbnbKnn2 <- cbind(airbnbKnn1, trsf2)

dmy3 <- dummyVars("~host_identity_verified", data = airbnbKnn2, fullRank = TRUE)
trsf3 <- data.frame(predict(dmy3, newdata = airbnbKnn2))
airbnbKnn3 <- cbind(airbnbKnn2, trsf3)

dmy4 <- dummyVars("~neighbourhood", data = airbnbKnn3)
trsf4 <- data.frame(predict(dmy4, newdata = airbnbKnn3))
airbnbKnn4 <- cbind(airbnbKnn3, trsf4)

View(airbnbKnn4)
airbnbKnn4 <- airbnbKnn4[, -c(2:3, 5:6)]
airbnbKnn4 <- airbnbKnn4[, c(1:2, 4:15, 3)]
View(airbnbKnn4)

# Create a dataframe that contains information for the predictors
mm <- filter(airbnbKnn, property_type == "House" & room_type == "Entire home/apt")
Mean1 <- mean(mm$log_price)
SD1 <- sd(mm$log_price)
mylogprice <- rnorm(1, mean = Mean1, sd = SD1)   # 5.59

Mean2 <- mean(mm$accommodates)
SD2 <- sd(mm$accommodates)
myAccomodates <- rnorm(1, mean = Mean2, sd = SD2)  # 7

my_house <- data.frame(log_price = mylogprice, accommodates = 7, property_type.Apartment = 0, property_type.Other = 0, 
                       property_type.House = 1, room_type.Entire.home.apt = 1, room_type.Private.room = 0, 
                       room_type.Shared.room = 0, host_identity_verified.t = 1, neighbourhood.East = 0, 
                       neighbourhood.North = 0, neighbourhood.Other = 0, neighbourhood.South = 0, neighbourhood.West = 1)
View(my_house)

# Create a data partition using assigned seed value
set.seed(168)
airbnbKnn5 <- sample_n(airbnbKnn4, 17040)
train <- slice(airbnbKnn5, 1:10224)
valid <- slice(airbnbKnn5, 10225:17040)
View(train)
View(valid)

# Normalization
train.norm <- train
valid.norm <- valid
my_house.norm <- my_house
View(my_house.norm)
norm.values <- preProcess(train[, 1:14], method = c("center", "scale"))
train.norm[, 1:14] <- predict(norm.values, train[, 1:14])
valid.norm[, 1:14] <- predict(norm.values, valid[, 1:14])
my_house.norm[, 1:14] <- predict(norm.values, my_house[, 1:14])

# Knn model
library(FNN)
library(e1071)
nn <- knn(train = train.norm[, c(1:14)], test = my_house.norm[1:14], cl = train.norm[, 15], k = 7)

row.names(train.norm)[attr(nn, "nn.index")]
nn
# Find better k value
library(caret)
accuracy <- data.frame(k = seq(1, 60, 1), accuracy = rep(0, 60))
for (i in 1:60){
  knn.pred <- knn(train.norm[, 1:14], valid.norm[, 1:14], 
                  cl = train.norm[, 15], k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, valid.norm[, 15])$overall[1]
}
accuracy
View(accuracy)

# Re-run knn() function with new optimal k-value
nn <- knn(train = train.norm[, c(1:14)], test = my_house.norm[1:14], cl = train.norm[, 15], k = 13)
nn
row.names(train.norm)[attr(nn, "nn.index")]



