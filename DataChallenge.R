###########################################
##Data Science Challenge From Captial One##
##Date:12/15/2016-12/17/2016                        ##
##Written by Weiwei Ouyang               ##
###########################################

################
##Question 1####
################

##Install packages to extract the data link from website###
install.packages("RCurl")
require(RCurl)  ##Load the needed package##

require(ggplot2)
DataFile <- getURL('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
mydat <- read.csv(textConnection(DataFile), header=T)
head(mydat)

################
##Question 2####
################

##Load ggplots for figures##
require(ggplots)

length(which(is.na(mydat$Trip_distance))) #Check the missing values##

length(which(mydat$Trip_distance==0))  #Check how many zeros for this variable

summary(mydat$Trip_distance) ##Summary of the variable###

##Plot the histgram of Trip distance#######

p<-ggplot(mydat, aes(x=Trip_distance)) + 
  geom_histogram(color="black", fill="white")
p

##plot the hisgram of log(Trip distance)####
mydat$Log_TripDistance<-log(mydat$Trip_distance)

p<-ggplot(mydat, aes(x=Log_TripDistance)) + 
  geom_histogram(color="black", fill="white")
p

##plot the density estimation of log(Trip distance)####
p1<-ggplot(mydat, aes(x=Log_TripDistance)) + 
 geom_histogram(aes(y=..density..), colour="black", fill="white")+
 geom_density(alpha=.2, fill="blue") 
p1

##QQ plot for Log_TripDistance####################

ggplot(mydat, aes(sample=Log_TripDistance))+stat_qq()

##############
##Question 3##
##############

install.packages("scales")
library(scales)

##Extract the hour of day from date data##########
t<-strftime(mydat$Lpep_dropoff_datetime, format="%H:%M:%S")
TimeHour<-array()
for (i in 1:length(t))
{
TimeHour[i]<-as.numeric(substr(t[i],1,2))+1
print(i)
}

##Calculate the mean and median trip distance grouped by hour of day###

Mean_Dis<-aggregate(mydat$Trip_distance~TimeHour,mydat, mean)
Median_Dis<-aggregate(mydat$Trip_distance~TimeHour,mydat, median)

plot(x = seq(1:24),y = Mean_Dis[,2],type="o", xlab="Hour of the day", 
ylab="Mean of Trip Distance")

plot(x = seq(1:24),y = Median_Dis[,2],type="o", xlab="Hour of the day", 
ylab="Median of Trip Distance")

##Calculate the transactions in local airport###
length(which(mydat$RateCodeID==2))  ##numbers of transactions of JFK##

length(which(mydat$RateCodeID==3))  ##numebers of transactions of Newark##

Dat_JFK<-mydat[which(mydat$RateCodeID==2),]  ##Subset Dataset of JFK##

AveFare_JFK=sum(Dat_JFK$Fare_amount)/length(which(mydat$RateCodeID==2)) #Average Fare#

Dat_Newark<-mydat[which(mydat$RateCodeID==3),]  ##Subset Dataset of Newark##

AveFare_JFK=sum(Dat_Newark$Fare_amount)/length(which(mydat$RateCodeID==3)) #Average Fare#

t.test(Dat_JFK$Fare_amount,Dat_Newark$Fare_amount)  ##Two sample welch t test###



##Plot the boxplot figure 3.3

Airport<-c(rep("JFK",length(which(mydat$RateCodeID==2))),rep("Newark",length(which(mydat$RateCodeID==3))))

Airport_TripDis<-data.frame(c(Dat_JFK$Trip_distance,Dat_Newark$Trip_distance),Airport)
names(Airport_TripDis)<-c("Trip_Distance","Airport")


p1<-ggplot(Airport_TripDis, aes(x=Airport, y=Trip_Distance,fill=Airport)) + geom_boxplot()
p2<-ggplot(Airport_Fare_amount, aes(x=Airport, y=Fare_amount,fill=Airport)) + geom_boxplot()

rm(Dat_JFK,Dat_Newark)

###############
##Question 4###
###############

##Extract the hour of day from date data##########
t<-strftime(mydat$Lpep_dropoff_datetime, format="%H:%M:%S")
Date<-strftime(mydat$Lpep_dropoff_datetime, format="%Y-%m-%d")
TimeHour<-Day<-array()
for (i in 1:length(t))
{
TimeHour[i]<-as.numeric(substr(t[i],1,2))+1
Day[i]<-as.numeric(substr(Date[i],9,10))
print(i)
}


WeekDay<-array()
WeekDay[which((Day==7)|(Day==14)|(Day==21)|(Day==28))]<-1
WeekDay[which((Day==1)|(Day==8)|(Day==15)|(Day==22)|(Day==29))]<-2
WeekDay[which((Day==2)|(Day==9)|(Day==16)|(Day==23)|(Day==30))]<-3
WeekDay[which((Day==3)|(Day==10)|(Day==17)|(Day==24))]<-4
WeekDay[which((Day==4)|(Day==11)|(Day==18)|(Day==25))]<-5
WeekDay[which((Day==5)|(Day==12)|(Day==19)|(Day==26))]<-6
WeekDay[which((Day==6)|(Day==13)|(Day==20)|(Day==27))]<-7

Week<-array()
Week[which((Day>=1)&(Day<=6))]<-1
Week[which((Day>=7)&(Day<=13))]<-2
Week[which((Day>=14)&(Day<=20))]<-3
Week[which((Day>=21)&(Day<=27))]<-4
Week[which((Day>=28)&(Day<=30))]<-5
mydat$TimeHour<-TimeHour
mydat$Day<-Day
mydat$WeekDay<-WeekDay
mydat$Week<-Week


##Create the Per_Tip as the variable for tip as percentage##
mydat$Per_Tip<-Tip_amount/mydat$Total_amount

##Histogram of Percentage Tip for all trips (Figure 4.1)#################
p<-ggplot(mydat, aes(x=Per_Tip)) + 
  geom_histogram(color="black", fill="white")
p


#Calculate the percentage of passangers paid in cash among %0 tips trips## 
length(which((mydat$Per_Tip==0)&(mydat$Payment_type==2)))/length(which(mydat$Per_Tip==0))

##Calculate the percentage Tip among trips paid in cash###
mean(mydat$Per_Tip[which(mydat$Payment_type==2)],na.rm=TRUE)

##Calculate the percentage Tip among trips paid with credit card##
mean(mydat$Per_Tip[which(mydat$Payment_type==1)],na.rm=TRUE)

##Histgram of Percentage Tips fpr trips paid with credit card (Figure 4.2)##
mydat_Card<-mydat[which(mydat$Payment_type==1),]

p<-ggplot(mydat_Card, aes(x=Per_Tip)) + 
  geom_histogram(color="black", fill="white")
p

#####Remove the trips with total_amount=0######

mydat_Card_update1<-mydat_Card[which(mydat_Card$Total_amount!=0),]
rm(mydat_Card)

#mydat_Card_update1 is the final dattasets we conducted analysis###

##Desciption of variables#########################
require(ggplot2)
p1<-ggplot(mydat_Card_update1, aes(x=factor(VendorID), y=Per_Tip,fill=factor(VendorID))) + geom_boxplot()
p2<-ggplot(mydat_Card_update1, aes(x=factor(Passenger_count), y=Per_Tip,fill=factor(Passenger_count))) + geom_boxplot()
p3<-ggplot(mydat_Card_update1, aes(x=factor(RateCodeID), y=Per_Tip,fill=factor(RateCodeID))) + geom_boxplot()
p4<-ggplot(mydat_Card_update1, aes(x=Store_and_fwd_flag, y=Per_Tip,fill=Store_and_fwd_flag)) + geom_boxplot()

p5<-ggplot(mydat_Card_update1, aes(x=factor(TimeHour), y=Per_Tip,fill=factor(TimeHour))) + geom_boxplot()
p6<-ggplot(mydat_Card_update1, aes(x=factor(WeekDay), y=Per_Tip,fill=factor(WeekDay))) + geom_boxplot()
p7<-ggplot(mydat_Card_update1, aes(x=factor(Week), y=Per_Tip,fill=factor(Week))) + geom_boxplot()
p7<-ggplot(mydat_Card_update1, aes(x=factor(Trip_type), y=Per_Tip,fill=factor(Trip_type))) + geom_boxplot()


mydat_Card_update11<-mydat_Card_update1[which(mydat_Card_update1$RateCodeID!=99),] #Remove the missing value##


##split the data with 80/20 rule##
set.seed(123)
samp <- sample(nrow(mydat_Card_update11), 0.8 * nrow(mydat_Card_update11))
train <- mydat_Card_update11[samp, ]
test <- mydat_Card_update11[-samp, ]


###Model selection on all variables###
Full<-lm(Per_Tip~VendorID+Store_and_fwd_flag+RateCodeID+Pickup_longitude+Pickup_latitude
+Dropoff_longitude+Dropoff_latitude+Passenger_count+Trip_distance+Total_amount+Fare_amount+Trip_type
+TimeHour+WeekDay+Week+Tip_amount,data=train)

Null<-lm(Per_Tip~1,data=train)


step(Null, scope = list(upper=Full), data=train, direction="both")
step(Null, scope=list(lower=Null, upper=Full), direction="forward")
step(Full, data=train, direction="backward")


Step_Out<-lm(Per_Tip ~ Tip_amount + Total_amount + Trip_distance + RateCodeID + 
    VendorID + WeekDay + Trip_type + Passenger_count + Week +Fare_amount + TimeHour + Pickup_longitude + Pickup_latitude,data=train)

Step_Out_update<-lm(Per_Tip ~ Tip_amount + Total_amount + Trip_distance + RateCodeID + 
    VendorID + WeekDay + Trip_type + Passenger_count + Week+ TimeHour + Pickup_longitude + Pickup_latitude,data=train)

summary(Step_Out_update)

prediction2 <- predict(Step_Out_update, newdata = test)

MSE<-sum((test$Per_Tip-prediction2)^2)/dim(test)[1]

var(test$Per_Tip)

cor(test$Per_Tip,prediction2)

###############
##Question 5###
###############
##Extract the hour of day from date data##########


t<-strftime(mydat$Lpep_dropoff_datetime, format="%Y-%m-%d %H:%M:%S")
t1<-strftime(mydat$lpep_pickup_datetime, format="%Y-%m-%d %H:%M:%S")
Time_Dur<-as.numeric(difftime(t,t1,units="min")) 
Ave_Speed<-mydat$Trip_distance/Time_Dur   #Derive the average speed datasets###


##Remove the missing and extreme datasets##
Dat_AveSpeed<-data.frame(Ave_Speed,Time_Dur,Week)
rm_ID<-which((Dat_AveSpeed$Ave_Speed==Inf)|(is.na(Dat_AveSpeed$Ave_Speed))) 
Dat_AveSpeed_Update<-Dat_AveSpeed[-rm_ID,]

##Anova test###
out<-lm(Ave_Speed~factor(Week),Dat_AveSpeed_Update)
anova(out)


plot(x = seq(1:24),y = Mean_Dis[,2], xlab="Hour of the day", 
ylab="Average Speed (min)")

##Fit smooth line to the data###


Dat_AveSpeed_Hour<-data.frame(Ave_Speed,TimeHour)
rm_ID1<-which((Dat_AveSpeed_Hour$Ave_Speed==Inf)|(is.na(Dat_AveSpeed_Hour$Ave_Speed)))
Dat_AveSpeed_Hour_Update<-Dat_AveSpeed_Hour[-rm_ID1,]

Mean_Dis<-aggregate(Ave_Speed~TimeHour,Dat_AveSpeed_Hour_Update, mean)

lo <- loess(Mean_Dis[,2]~seq(1:24))
plot(Mean_Dis[,2]~seq(1:24), xlab="Hour of the day", ylab="Average Speed (min)")
lines(predict(lo), col='red', lwd=2)   #plot the fitted smooth function line##


