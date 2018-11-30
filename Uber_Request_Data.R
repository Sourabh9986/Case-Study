

library(ggplot2)

#Import as CSV file and name it as uber
uber<- read.csv("Uber_Request_Data.csv")
View(uber)

#View stucture of dataset
str(uber)


#Change the data and time in standard format for Request.timestamp

uber$New = as.POSIXlt(uber$Request.timestamp, format = "%d/%m/%Y %H:%M")
uber$New1= as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")

#Change the data and time in standard format for Drop.timestamp

uber$New2= as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$New3= as.POSIXlt(uber$Drop.timestamp, format = "%d/%m/%Y %H:%M")



#As New,New1,New2 and New3 will have NA values we would merge New, New1
#and New2, New3 in one column Request.timestamp1 and Drop.timestamp1 respectively
#by excluding NA values using ifesle condition

uber$Request.timestamp1<-ifelse(is.na(uber$New),paste(uber$New1),paste(uber$New))
uber$Drop.timestamp1<-ifelse(is.na(uber$New2),paste(uber$New3),paste(uber$New2))


#Change the newly created columns to POSIXlt format

uber$Request.timestamp1 = as.POSIXlt(uber$Request.timestamp1, format = "%Y-%m-%d %H:%M:%S")
uber$Drop.timestamp1 = as.POSIXlt(uber$Drop.timestamp1, format = "%Y-%m-%d %H:%M:%S")


#Create a new data frame with required columns
uber1<- data.frame(uber$Request.id,uber$Pickup.point,uber$Driver.id,uber$Status,uber$Request.timestamp1,uber$Drop.timestamp1, check.names = TRUE, stringsAsFactors = TRUE)

#Specify required names to columns
colnames(uber1)<- c("Request.id", "Pickup.point","Driver.id","Status","Request.timestamp", "Drop.timestamp")


#Check the NA value for each column.

sum(is.na(uber1$Request.id) == 1)
sum(is.na(uber1$Pickup.point) == 1)
sum(is.na(uber1$Driver.id) == TRUE)
sum(is.na(uber1$Status) == TRUE)
sum(is.na(uber1$Request.timestamp) == TRUE)
sum(is.na(uber1$Drop.timestamp) == TRUE)


#Extracting hours and Day in new column for Request.timestamp

uber1$rthour <- format(uber1$Request.timestamp, "%H")
uber1$rtday <- format(uber1$Request.timestamp, "%A")

#Extracting hours and Day in new column for Drop.timestamp

uber1$dthour <- format(uber1$Drop.timestamp, "%H")
uber1$dtday <- format(uber1$Drop.timestamp, "%A")


#plotting histogram for the booking requests at each hours and day. 

ggplot(uber1, aes(rthour)) + geom_histogram(stat = "count")
ggplot(uber1, aes(rtday)) + geom_histogram(stat = "count")


summary(uber1$Status)


#Extracting values for only bookings which are cancelled, no cars available
#and trip completed in uber1 

cancelled <- subset(uber1, Status=="Cancelled")
nca<-subset(uber1, Status=="No Cars Available")
tc<-subset(uber1, Status=="Trip Completed")

#plotting histogram for the cancelled booking requests at each hour and day 

ggplot(cancelled, aes(rthour)) + geom_histogram(stat = "count")
ggplot(cancelled, aes(rtday)) + geom_histogram(stat = "count")

#plotting histogram for the no car available requests at each hour and day 

ggplot(nca, aes(rthour)) + geom_histogram(stat = "count")
ggplot(nca, aes(rtday)) + geom_histogram(stat = "count")

#plotting histogram for the trip completed requests at each hour and day 

ggplot(tc, aes(rthour)) + geom_histogram(stat = "count")
ggplot(tc, aes(rtday)) + geom_histogram(stat = "count")


#plotting histogram for the booking requests from airport to city.

air_c<- subset(uber1, Pickup.point=="Airport")


#plotting histogram for the booking requests from city to airport.

c_air<- subset(uber1, Pickup.point=="City")

View(air_c)


#plotting histogram for the booking requests from airport to city at each hour.

ggplot(air_c, aes(rthour)) + geom_histogram(stat = "count")


#plotting histogram for the booking requests from city to airport at each hour.

ggplot(c_air, aes(rthour)) + geom_histogram(stat = "count")


#Extracting values for only bookings which are cancelled between airport to 
#city each hour and plotting histogram
air_c_cancel<- subset(air_c, Status== "Cancelled")
ggplot(air_c_cancel, aes(rthour)) + geom_histogram(stat = "count")
View(air_c_cancel)


#Extracting values for only bookings which are cancelled between city to airport
#each hour and plotting histogram

c_air_cancel<- subset(c_air, Status== "Cancelled")
ggplot(c_air_cancel, aes(rthour)) + geom_histogram(stat = "count")



#Extracting values for only bookings which shows no car available between airport to city
#each hour and plotting histogram

air_c_nca<- subset(air_c, Status== "No Cars Available")
ggplot(air_c_nca, aes(rthour)) + geom_histogram(stat = "count")



#Extracting values for only bookings which shows no car available between city to airport
#each hour and plotting histogram

c_air_nca<- subset(c_air, Status== "No Cars Available")
ggplot(c_air_nca, aes(rthour)) + geom_histogram(stat = "count")




# Write data frames to analyze through tabeleau.

write.csv(uber1,"Uber_Request_Data.csv")
write.csv(cancelled,"cancelled.csv")
write.csv(nca,"nca.csv")
write.csv(air_c,"air_c.csv")
write.csv(c_air,"c_air.csv")
write.csv(air_c_cancel,"air_c_cancel.csv")
write.csv(air_c_nca, "air_c_nca.csv")
write.csv(c_air_cancel, "c_air_cancel.csv")
write.csv(c_air_nca, "c_air_nca.csv")
