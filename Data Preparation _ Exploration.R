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

#Step I:Data preparation & Exploration
#I. Missing Values
anyNA(airbnbtrain_LA) 

airbnbtrain_LA[,1:29][airbnbtrain_LA[, 1:29] == ""] <- NA  #turn blank cell into NA
airbnbtrain_LA <- na.omit(airbnbtrain_LA)  #remove rows with NA
anyNA(airbnbtrain_LA)

#II. Summary Statistics
#(1) nrow()/ncol()
nrow(airbnbtrain_LA) #number of airbnb available in LA (row)
ncol(airbnbtrain_LA) #number of varables we're looking into (column)

#(2) summary()
summary(airbnbtrain_LA$log_price) #get five numbers plus mean of the log price

#(3) table()
table(airbnbtrain_LA$room_type) #distribution of 3 types of room

#(4) sd()
sd(airbnbtrain_LA$review_scores_rating) #standard deviation between each rating

#(5) levels()
levels(airbnbtrain_LA$property_type) #35 levels of property type



#III. Visualization

#(1) Barplot
install.packages("lubridate")
library(lubridate)
airbnbtrain_LA$host_since <- year(airbnbtrain_LA$host_since)
str(airbnbtrain_LA$host_since) 
airbnbtrain_LA$host_since <- as.factor(airbnbtrain_LA$host_since)

library(ggplot2)
ggplot(airbnbtrain_LA, aes(x=host_since)) + geom_bar(col = "snow",fill = "skyblue2")+
  ggtitle("Host Since Count (year)") + xlab("Host Since (year)") + ylab("Count")+
  theme(plot.title = element_text(hjust = 0.5))



#(2) Scatterplot
airbnbtrain_LA$log_price <- as.numeric(airbnbtrain_LA$log_price)
airbnbtrain_LA$number_of_reviews <- as.numeric(airbnbtrain_LA$number_of_reviews)
ggplot(airbnbtrain_LA, aes(x=number_of_reviews,y=log_price,color=review_scores_rating)) + geom_point(cex = 0.5) +
  ggtitle("Scatterplot of Reviews and Prices") + xlab("Number of Reviews") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(limits=c(0, 550), breaks=c(0,50,100,150,200,250,300,350,400,450,500,550)) + 
  scale_y_continuous(limits=c(2, 8), breaks=c(2,3,4,5,6,7,8))



#(3) Histogram
airbnbtrain_LA$log_price <- as.numeric(airbnbtrain_LA$log_price)
ggplot(airbnbtrain_LA, aes(x=log_price)) + geom_histogram(aes(y=..density..),binwidth=0.1, fill="lightblue", color="lightblue") +
  ggtitle("Histogram of Prices") + xlab("Price") + ylab("Count") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_density(alpha=0.3, fill="#FF6666") + geom_vline(aes(xintercept=mean(log_price, na.rm=T)),color="rosybrown4", linetype="dashed", size=0.8)



#(4) Frequency Polygon
ggplot(airbnbtrain_LA, aes(accommodates, colour = room_type)) + geom_freqpoly(binwidth = 1) + theme_minimal() +
  ggtitle("Counts between roomtype and accommodates") + theme(plot.title = element_text(hjust = 0.5))



#(5) Density plot
ggplot(airbnbtrain_LA, aes(x=latitude, y=longitude)) + geom_point(cex = 0.7) + stat_density2d() +
  ggtitle("Density of airbnb in LA") + theme(plot.title = element_text(hjust = 0.5))

