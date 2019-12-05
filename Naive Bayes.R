######Naive Bayes
airbnb_Naive<-read.csv("airbnbtrain.csv")
Naive<-airbnb_Naive
Naive<- Naive[Naive$city == "LA",]

##binning log_price
anyNA(Naive$log_price)
fivenum(Naive$log_price)
str(Naive$log_price)
summary(Naive$log_price)
nn<-filter(Naive,log_price>7.600402)
Naive$log_price<-cut(Naive$log_price,breaks = c(2.302584, 4.248495, 4.605170, 5.135798, 7.600403), 
                     labels = c("Student Budget","Below Average","Above Average","Pricey Digs"))
table(Naive$log_price)
summary(Naive$log_price)
anyNA(Naive$log_price)

#### Choose any five variables
# We keep property_type(3), room type(4),host_identity_verified(15),host_since(17),neibourhood(23)
colnames(Naive)
Naive1<-Naive[,c(2:4,15,17,23)]

# Make 35 types of $property_type to 3 types categories (Apartment, House, Other) --> easier for dummy variables
# Grouping Property_type
library(plyr)
Naive1$property_type <- mapvalues(x = Naive1$property_type,
                                  from = c("Bed & Breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular",
                                           "Castle","Cave","Chalet","Condominium","Dorm","Earth House","Guest suite","Guesthouse",
                                           "Hostel","Hut","In-law","Island","Lighthouse","Loft","Parking Space","Serviced apartment",
                                           "Tent","Timeshare","Tipi","Townhouse","Train","Treehouse","Vacation home","Villa","Yurt"),
                                  to = c("Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                         "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other",
                                         "Other","Other","Other","Other","Other","Other","Other","Other","Other","Other","Other"))
table(Naive1$property_type)
summary(Naive1$property_type)

#Check room_type
table(Naive1$room_type)
summary(Naive1$room_type)

#delete blank cells in host_identity_verified
table(Naive1$host_identity_verified)
library(dplyr)
Naive1<-filter(Naive1,host_identity_verified %in% c("f","t"))
table(Naive1$host_identity_verified)
Naive1$host_identity_verified<-droplevels(Naive1$host_identity_verified)
table(Naive1$host_identity_verified)
summary(Naive1$host_identity_verified)

#change class of host_since to numeric
Naive1$host_since<-as.character(Naive1$host_since)
library(lubridate)
Naive1$host_since <- year(Naive1$host_since)
table(Naive1$host_since)
str(Naive1$host_since)
Naive1$host_since <- as.factor(Naive1$host_since)

# Process neighbourhood
  # Clear blank cells
anyNA(Naive1$neighbourhood)
Naive1$neighbourhood[Naive1$neighbourhood==""]<-NA
anyNA(Naive1$neighbourhood)
library(tidyr)
Naive1<-Naive1 %>%drop_na(neighbourhood)
anyNA(Naive1$neighbourhood)
Naive1$neighbourhood<-droplevels(Naive1$neighbourhood)

  # Grouping neighbourhood into 5 categories
Naive1$neighbourhood <- mapvalues(x = Naive1$neighbourhood,
                                  from = c("Hollywood","Hollywood Hills","North Hollywood", "Sherman Oaks", "Burbank", "Studio City", "Woodland Hills/Warner Center",
                                           "Mid-Wilshire","Venice","West Hollywood","Santa Monica","Mar Vista","Westwood","Del Rey","Mid-City","Westchester/Playa Del Rey",
                                           "West Los Angeles","Marina Del Rey","Malibu","Topanga","Culver City","South Robertson","Beverly Hills","Westside",
                                           "Silver Lake","Echo Park","Pasadena","Downtown","Westlake","Glendale","Los Feliz","Highland Park","East Hollywood",
                                           "Long Beach","South LA","Redondo Beach"),
                                  to = c("North","North","North","North","North","North","North","West","West","West","West","West","West","West","West","West","West",
                                         "West","West","West","West","West","West","West","East","East","East","East","East","East","East","East","East","South","South","South"))

Naive1$neighbourhood <- as.character(Naive1$neighbourhood)
library(Hmisc)
Naive1$neighbourhood[Naive1$neighbourhood %nin% c("North","West","East","South")] <- "Other"
table(Naive1$neighbourhood)
Naive1$neighbourhood <- as.factor(Naive1$neighbourhood)
str(Naive1$neighbourhood)


# Partition the data into training and validation sets using the seed value
set.seed(470)
naiveBay <- sample_n(Naive1, 17040)
trainNaive <- slice(naiveBay, 1:10224)
validNaive <- slice(naiveBay, 10225:17040)
View(trainNaive)
View(validNaive)

# Build a naive bayes model
library(e1071)
cityLA.nb <- naiveBayes(log_price~., data = trainNaive)
cityLA.nb

# Show a confusion matrix that compares the performance of your model against the training & validation sets
library(caret)
nbT.pred <- predict(cityLA.nb, newdata = trainNaive)
confusionMatrix(nbT.pred, trainNaive$log_price)
nbV.pred <- predict(cityLA.nb, newdata = validNaive)
confusionMatrix(nbV.pred, validNaive$log_price)

# Fictional Apartment Prediction
  # Predict probabilities
pred.prob <- predict(cityLA.nb, newdata = validNaive, type = "raw")
  # Predict class membership
pred.class <- predict(cityLA.nb, newdata = validNaive)

apt <- data.frame(actual = validNaive$log_price, predicted = pred.class, pred.prob) 
apt[validNaive$property_type == "House" & validNaive$room_type == "Entire home/apt" & 
      validNaive$host_identity_verified == "t" & validNaive$host_since == "2016" & 
      validNaive$neighbourhood == "West",]

  # Alternative 2:
student_budget <- 0.22*0.43*0.11*0.66*0.24*0.26
below_average <- 0.24*0.31*0.5*0.72*0.17*0.35
above_average <- 0.28*0.28*0.79*0.73*0.19*0.44
pricy_digs <- 0.26*0.54*0.94*0.74*0.18*0.48

  # Possibility of Student Budget
p1 <- student_budget / sum(student_budget, below_average, above_average, pricy_digs)
p1

  # Possibility of Below Average
p2 <- below_average / sum(student_budget, below_average, above_average, pricy_digs)
p2

  # Possibility of Above Average
p3 <- above_average / sum(student_budget, below_average, above_average, pricy_digs)
p3

  # Possibility of Pricy Digs
p4 <- pricy_digs / sum(student_budget, below_average, above_average, pricy_digs)
p4


# Create a data visualizations that show the relationship between log_price and predictors
library(ggplot2)
ggplot(data = Naive1, aes(x = log_price, fill = property_type)) + geom_bar() + ggtitle("Relationship Between Log Price & Property Type") + 
  xlab("Log Prices") + ylab("Property Type") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Naive1, aes(x = log_price, fill = room_type)) + geom_bar() + ggtitle("Relationship Between Log Price & Room Type") + 
  xlab("Log Prices") + ylab("Room Type") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Naive1, aes(x = log_price, fill = host_identity_verified)) + geom_bar() + ggtitle("Relationship Between Log Price & Host Identity") + 
  xlab("Log Prices") + ylab("Host Identity") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Naive1, aes(x = host_since, fill = log_price)) + geom_bar() + ggtitle("Relationship Between Log Price & Host Since") + 
  xlab("Log Prices") + ylab("Host Since") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = Naive1, aes(x = log_price, fill = neighbourhood)) + geom_bar() + ggtitle("Relationship Between Log Price & Neighbourhood") + 
  xlab("Log Prices") + ylab("Neighbourhood") + theme(plot.title = element_text(hjust = 0.5))
