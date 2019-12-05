decisiontree <- read.csv("airbnbtrain.csv")
classification <- decisiontree[decisiontree$city == "LA",]
classification <- classification[, c(2:4, 6:10, 13:19, 23:25, 28:29)]

### host_since data manipulation
classification$host_since <- as.character(classification$host_since)
anyNA(classification$host_since)
library(dplyr)
library(lubridate)
classification <- filter(classification, host_since != "")
classification$host_since <- year(classification$host_since)
table(classification$host_since)
str(classification$host_since)
classification$host_since <- as.factor(classification$host_since)

## last_review data manipulation
classification$last_review <- as.character(classification$last_review)
anyNA(classification$last_review)
classification <- filter(classification,last_review != "")
classification$last_review <- year(classification$last_review)
table(classification$last_review)
classification$last_review <- as.factor(classification$last_review)

## first_review data manipulation
anyNA(classification$first_review)
classification$first_review <- as.character(classification$first_review)
classification <- filter(classification,first_review != "")
classification$first_review <- year(classification$first_review)
table(classification$first_review)
classification$first_review <- as.factor(classification$first_review)

#process neighbourhood
##clear blank cells
anyNA(classification$neighbourhood)
classification$neighbourhood[classification$neighbourhood == ""] <- NA
library(tidyr)
classification <- classification %>% drop_na(neighbourhood)
classification$neighbourhood <- droplevels(classification$neighbourhood)
## Grouping neighbourhood into 5 categories
library(plyr)
library(dplyr)
classification$neighbourhood <- mapvalues(x = classification$neighbourhood,
                                          from = c("Hollywood","Hollywood Hills","North Hollywood", "Sherman Oaks", "Burbank", "Studio City", "Woodland Hills/Warner Center",
                                                   "Mid-Wilshire","Venice","West Hollywood","Santa Monica","Mar Vista","Westwood","Del Rey","Mid-City","Westchester/Playa Del Rey",
                                                   "West Los Angeles","Marina Del Rey","Malibu","Topanga","Culver City","South Robertson","Beverly Hills","Westside",
                                                   "Silver Lake","Echo Park","Pasadena","Downtown","Westlake","Glendale","Los Feliz","Highland Park","East Hollywood",
                                                   "Long Beach","South LA","Redondo Beach"),
                                          to = c("North","North","North","North","North","North","North","West","West","West","West","West","West","West","West","West","West",
                                                 "West","West","West","West","West","West","West","East","East","East","East","East","East","East","East","East","South","South","South"))

classification$neighbourhood <- as.character(classification$neighbourhood)
library(Hmisc)
classification$neighbourhood[classification$neighbourhood %nin% c("North","West","East","South")] <- "Other"
table(classification$neighbourhood)
classification$neighbourhood <- as.factor(classification$neighbourhood)
str(classification$neighbourhood)

# delete blank cells
anyNA(classification$log_price)
summary(classification$log_price)
classification$log_price[classification$log_price == ""] <- NA
anyNA(classification$log_price)

# delete blank cell and NA
anyNA(classification$review_scores_rating)
summary(classification$review_scores_rating)
classification$review_scores_rating[classification$review_scores_rating == ""] <- NA
summary(classification$review_scores_rating)
classification <- classification %>% drop_na(review_scores_rating)
summary(classification$review_scores_rating)

# Grouping Property_type
classification$property_type <- mapvalues(x = classification$property_type,
                                          from = c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular",
                                                   "Castle","Cave","Chalet","Condominium","Dorm","Earth House","Guest suite","Guesthouse",
                                                   "Hostel","Hut","In-law","Island","Lighthouse","Loft","Parking Space","Serviced apartment",
                                                   "Tent","Timeshare","Tipi","Townhouse","Train","Treehouse","Vacation home","Villa","Yurt"),
                                          to = c("Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                                 "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other"))
table(classification$property_type)
summary(classification$property_type)

#check existence of blankcell or NA in Accommodates
summary(classification$accommodates)
classification$bathrooms[classification$bathrooms == ""] <- NA
summary(classification$accommodates)

#check existence of blankcell or NA in bathrooms
summary(classification$bathrooms)
classification$bathrooms[classification$bathrooms == ""] <- NA
summary(classification$bathrooms)
classification <- classification %>% drop_na(bathrooms)
anyNA(classification$bathrooms)

#delete blank cells in host_has_profile_pic
table(classification$host_has_profile_pic)
classification <- filter(classification, host_has_profile_pic %in% c("f","t"))
classification$host_has_profile_pic <- droplevels(classification$host_has_profile_pic)
table(classification$host_has_profile_pic)
summary(classification$host_has_profile_pic)

#delete blank cells in host_identity_verified
table(classification$host_identity_verified)
classification <- filter(classification, host_identity_verified %in% c("f","t"))
classification$host_identity_verified <- droplevels(classification$host_identity_verified)
table(classification$host_identity_verified)
summary(classification$host_identity_verified)

#delete blank cells in host response rate and transfer it to be numberic
anyNA(classification$host_response_rate)
summary(classification$host_response_rate)
classification$host_response_rate[classification$host_response_rate == ""] <- NA
anyNA(classification$host_response_rate)
classification <- classification %>% drop_na(host_response_rate)
anyNA(classification$host_response_rate)
classification$host_response_rate <- as.character(classification$host_response_rate)
library(stringr)
testdata <- data.frame(v1=classification$host_response_rate)
testnewdata2 <- data.frame(lapply(testdata, function(x) as.numeric(str_extract(x,'[0-9.]+'))/100) )
classification$host_response_rate <- testnewdata2$v1
summary(classification$host_response_rate)

#bed types
table(classification$bed_type)

#cleaning fee
table(classification$cleaning_fee)

#instand_bookable
table(classification$instant_bookable)

#number of reviews
summary(classification$number_of_reviews)
classification$number_of_reviews[classification$number_of_reviews == ""] <- NA
anyNA(classification$number_of_reviews)

#bedsroom
summary(classification$bedrooms)
classification$number_of_reviews[classification$bedrooms == ""] <- NA
summary(classification$bedrooms)
classification <- classification %>% drop_na(bedrooms)
anyNA(classification$bedrooms)

#beds
summary(classification$beds)
classification$beds[classification$beds == ""] <- NA
summary(classification$beds)
classification <- classification %>% drop_na(beds)
anyNA(classification$beds)

#room type
table(classification$room_type)

# size down for cancellation_policy
table(classification$cancellation_policy)
classification$cancellation_policy <- mapvalues(x = classification$cancellation_policy,
                                                from = c("super_strict_30","super_strict_60"),
                                                to = c("strict","strict"))
table(classification$cancellation_policy)
## data partition
set.seed(168)
classification_tree <- sample_n(classification, 11706)
train_tree <- slice(classification_tree, 1:7024)
valid_tree <- slice(classification_tree, 7025:11706)
View(train_tree)
View(valid_tree)

# Decision tree plot
library(rpart)
library(rpart.plot)
tree1 <- rpart(cancellation_policy~., data = train_tree, method = "class",
               minsplit = 200, minbucket = 200, cp = 0)
rpart.plot(tree1)
prp(tree1)
tree2 <- rpart(cancellation_policy~., data = train_tree, method = "class",
               minsplit = 300, minbucket = 300, cp = 0)
rpart.plot(tree2)
prp(tree2)
tree3 <- rpart(cancellation_policy~., data = train_tree, method = "class", 
               minsplit = 400, minbucket = 400, cp = 0)
rpart.plot(tree3)
prp(tree3)

#Another partition
set.seed(169)
classification_tree1 <- sample_n(classification, 11706)
train_tree1 <- slice(classification_tree1, 1:7024)
valid_tree1 <- slice(classification_tree1, 7025:11706)
View(train_tree1)
View(valid_tree1)

tree4 <- rpart(cancellation_policy~., data = train_tree1, method = "class", minsplit = 200, minbucket = 200, cp = 0)
rpart.plot(tree4)
tree5 <- rpart(cancellation_policy~., data = train_tree1, method = "class", minsplit = 300, minbucket = 300, cp = 0)
rpart.plot(tree5)
tree6 <- rpart(cancellation_policy~., data = train_tree1, method = "class", minsplit = 400, minbucket = 400, cp = 0)
rpart.plot(tree6)

#Another partition
set.seed(180)
classification_tree2 <- sample_n(classification, 11706)
train_tree2 <- slice(classification_tree2, 1:7024)
valid_tree2 <- slice(classification_tree2, 7025:11706)
View(train_tree2)
View(valid_tree2)

tree7 <- rpart(cancellation_policy~., data = train_tree2, method = "class", minsplit = 200, minbucket = 200, cp = 0)
rpart.plot(tree7)
tree8 <- rpart(cancellation_policy~., data = train_tree2, method = "class", minsplit = 300, minbucket = 300, cp = 0)
rpart.plot(tree8)
tree9 <- rpart(cancellation_policy~., data = train_tree2, method = "class", minsplit = 400, minbucket = 400, cp = 0)
rpart.plot(tree9)

# Generate confusion matrices to show model’s performance against training and validation sets
library(caret)
tree.pred1 <- predict(tree2, newdata = train_tree, type = "class")
confusionMatrix(tree.pred1, train_tree$cancellation_policy)
tree.pred2 <- predict(tree2, newdata = valid_tree, type = "class")
confusionMatrix(tree.pred2, valid_tree$cancellation_policy)

tree.pred1 <- predict(tree3, newdata = train_tree, type = "class")
confusionMatrix(tree.pred1, train_tree$cancellation_policy)
tree.pred2 <- predict(tree3, newdata = valid_tree, type = "class")
confusionMatrix(tree.pred2, valid_tree$cancellation_policy)

tree.pred1 <- predict(tree6, newdata = train_tree1, type = "class")
confusionMatrix(tree.pred1, train_tree1$cancellation_policy)
tree.pred2 <- predict(tree6, newdata = valid_tree1, type = "class")
confusionMatrix(tree.pred2, valid_tree1$cancellation_policy)

tree.pred1 <- predict(tree8, newdata = train_tree2, type = "class")
confusionMatrix(tree.pred1, train_tree2$cancellation_policy)
tree.pred2 <- predict(tree8, newdata = valid_tree2, type = "class")
confusionMatrix(tree.pred2, valid_tree2$cancellation_policy)

tree.pred1 <- predict(tree9, newdata = train_tree2, type = "class")
confusionMatrix(tree.pred1, train_tree2$cancellation_policy)
tree.pred2 <- predict(tree9, newdata = valid_tree2, type = "class")
confusionMatrix(tree.pred2, valid_tree2$cancellation_policy)
