library(Quandl)
library(tidyverse)
library(ggplot2)
library(tidyquant)
library(gganimate)
library(plyr)
library(gridExtra)
library(grid)

library(forecast)
# API KEY FOR QUANDL 2MrJyxVvPtkwJ2JJ6TeA
rm(list = ls())
Quandl.api_key("2MrJyxVvPtkwJ2JJ6TeA")
setwd("F:/SEM5/CSE3501/project")
bob = Quandl("NSE/BANKBARODA", collapse = "daily", start_date = "2017-01-01", type = "raw")
sbi= Quandl("NSE/SBIN",collapse="daily",start_date="2017-01-01",type="raw")
canara=Quandl("NSE/CANBK",collapse="daily",start_date="2017-01-01",type="raw")

#add a stock column
sbi<-cbind(sbi,Stock="")
canara<-cbind(canara,Stock="")
bob<-cbind(bob,Stock="")


# Paste the stock name in stock column
# for plotting purpose
sbi$Stock<-paste(sbi$Stock,"SBI",sep="")
canara$Stock<-paste(canara$Stock,"CANARA",sep="")
bob$Stock<-paste(bob$Stock,"BOB",sep="")


## Consolidate under one dataset
Master_Data = rbind(bob, sbi, canara)

#Convert the dates into character in order to split the column into "Y" "m" "dd"" columns
Master_Data$Date = as.character(Master_Data$Date)
#split the date column by - and create a list for the same
list = strsplit(Master_Data$Date, "-")


# to return results of the above list in a data frame
Master_date = ldply(list)
#changing the column name of the master date dataframe
colnames(Master_date) = c("Year", "Month", "Day")

#now we'll column bind the above master_date data to the Master_data
Master_Data<-cbind(Master_Data,Master_date)
names(Master_Data)

# Change the scale for Traded Quantity
# Master_Data$`Total.Trade.Quantity` = Master_Data$`Total.Trade.Quantity`/100000

# Convert the Date to as.Date()
Master_Data$Date<-as.Date(Master_Data$Date)

Master_Data$Month<-as.integer(Master_Data$Month)
Master_Data$Year<-as.integer(Master_Data$Year)
Master_Data$Day<-as.integer(Master_Data$Day)



P<- Master_Data %>% ggplot(aes(factor(Stock), Close, color=Stock)) +
  geom_jitter(aes(size = Close, colour=Stock, alpha = 0.03)) +
  ylim(0,3000)+
  labs(title = "Bank stock Yearly Prices", x = "Bank", y= "Close Price") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
        legend.position = "right")+
  transition_reveal(Year)

P

## Group By Stock

Master_Data<-Master_Data%>%
  tibble::as_tibble()%>%
  group_by(Stock)

## Visualization for month-wise daily stock prices
##facet_wrap is used to group the plots by respective stocks
##and also display in a row-col format

ggplot(Master_Data, aes(x = Date, y = Close, color = Stock)) +
  geom_point() +
  labs(title = "Daily Close Price", x = "Month",y="Close Price") +
  facet_wrap(~ Stock, ncol = 3, scale = "free_y") +
  theme_tq() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(colour="black"),
    plot.title = element_text(hjust = 0.5,size=18,colour="brown"),
    panel.background = element_rect(fill = 'lemonchiffon'),
    legend.position = 'none'
  )


#=====Finding the Density Distribution of Deviation of High Price from Open Price
#Deviation from High & Low Price
#mutate() adds new variables and preserves existing ones
#here Dev_high and Dev_low are the new variables and are
#appended to the master_data and stored in new variables --> Master_Data_High & Master_Data_Low
#-------------------------------------------------------------------------------
#the motive is to get the price range which will be useful for intraday trading 
Master_Data_High <- mutate(Master_Data , Dev_High = High-Open)
Master_Data_Low <-mutate(Master_Data , Dev_Low = Open-Low)
#computing the weekly high prices
#for this we use tq_transmute from the tidyquant package
#this method adds new variables to and existing dataset and
#returns only newly created columns, typically used when periodicity changes
Master_Data_High_Week <- Master_Data_High %>%
  tq_transmute(
    select     = Dev_High,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Dev_High_Mean"
  )



#similarly doing to compute weekly low prices

Master_Data_Low_Week<-Master_Data_Low%>%
  tq_transmute(
    select  = Dev_Low,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "Dev_Low_Mean"
  )


##Visualization of density distribution of high Price
High<-Master_Data_High_Week%>%ggplot(aes(x=Dev_High_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  labs(title="Distribution of High Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  theme_tq()+
  theme(
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line=element_line(colour="black"),
    plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"),
    legend.position =  'none',
    panel.background = element_rect(fill = 'lemonchiffon')
  )
High

Low<-Master_Data_Low_Week%>%ggplot(aes(x=Dev_Low_Mean,color=Stock))+
  geom_dotplot(binwidth=0.50,aes(fill=Stock))+
  xlim(0,10)+
  labs(title="Distribution of Weekly Low Price Deviation from Open Price",x="Weekly Mean Deviation")+
  facet_wrap(~Stock,ncol=3,scale="free_y")+
  theme_tq()+
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    axis.line=element_line(colour="black"),
    plot.title = element_text(hjust = 0.5,size=16,colour="indianred4"),
    legend.position =  'none',
    panel.background = element_rect(fill = 'lemonchiffon')
  )
Low

##arrange both the graphs in one plot
grid.arrange(High, Low)

#visualizing the volatility of the stock using bollinger bands
end<-ymd("2019-01-04")
start<-ymd("2017-01-04")

bollinger_bob = Master_Data %>%filter(Stock == "BOB")%>% ggplot(aes(x = Date, y = Close))+
  geom_line(size =1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA,n = 30,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  labs(title = "Bollinger Band BOB", x = "Date", y = "Price")+
  theme(axis.title.x = element_text(hjust = 0.5, size = 16)
        ,axis.title.y = element_text(hjust = 0.5, size = 16)
        ,plot.title = element_text(hjust = 0.5, color = "navyblue", size = 20)
        ,axis.title = element_text(color = 'navyblue', size = 20))

bollinger_bob


#visualizing the volatility of all the stocks
bollinger_all = Master_Data %>%filter(Stock == "BOB"|Stock == "CANARA"|Stock =="SBI")%>% ggplot(aes(x = Date, y = Close))+
  geom_line(size =1)+
  facet_wrap(~Stock, scales = "free_y", ncol = 3)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA,n = 30,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  labs(title = "Bollinger Band BOB", x = "Date", y = "Price")+
  theme(axis.title.x = element_text(hjust = 0.5, size = 16)
        ,axis.title.y = element_text(hjust = 0.5, size = 16)
        ,plot.title = element_text(hjust = 0.5, color = "navyblue", size = 20)
        ,axis.title = element_text(color = 'navyblue', size = 20))

bollinger_all


################# BOB ARIMA #########################################
#data
bob_new = Quandl("NSE/BANKBARODA", collapse = "daily", start_date = "2017-01-01", type = "raw")
bob_dff = map_df(select(bob_new, "Date", "Close"), rev)


#plot
plot(bob_dff, type = "l")
plot(y = bob_dff$Close,x = bob_dff$Date, type = 'l')
plot(diff(bob_dff$Close), type = 'l', main = "bob differenced")

time_series_bob = ts(bob_dff$Close, frequency = 30)
ddbob = decompose(time_series_bob, "multiplicative")
plot(ddbob)

#ARIMA
acf(diff(bob_dff$Close), main = "bob acf plot")#p = 2
pacf(diff(bob_dff$Close), main = "bob pacf plot")#q = 1

#MODEL FITTING
bob_model = arima(log(bob_dff$Close), c(2,1,1), seasonal=list(order=c(2,1,1), period=7))
bob_model
bob_model$residual 

forecastAP1 <- forecast(time_series_bob, level = c(95), h = 36)
autoplot(forecastAP1)



################# SBI ARIMA #########################################
sbi_new = Quandl("NSE/SBIN", collapse = "daily", start_date = "2017-01-01", type = "raw")

sbi_dff = map_df(select(sbi_new, "Date", "Close"), rev)

#plot
plot(sbi_dff, type = "l", main = "state bank of india Stock Price")
plot(diff(sbi_dff$Close), type = 'l', main = "sbi differenced")

time_series_sbi = ts(sbi_dff$Close, frequency = 30)
ddsbi = decompose(time_series_sbi, "multiplicative")
plot(ddsbi)

#ARIMA
acf(diff(sbi_dff$Close), main = "SBI acf plot")#p = 1
pacf(diff(sbi_dff$Close), main = "SBI pacf plot")#q = 1

#MODEL FITTING
sbi_model = arima(log(sbi_dff$Close), c(1,1,1), seasonal=list(order=c(1,1,1), period=7))
sbi_model
sbi_model$residual 

forecastAP <- forecast(time_series_sbi, level = c(95), h = 36)
autoplot(forecastAP)


# Moving Average For BOB
bob_new = Quandl("NSE/BANKBARODA", collapse = "daily", start_date = "2017-01-01", type = "raw")
bob_df = map_df(select(bob_new, "Date", "Close"), rev)
bob_df$prediction=NA
for(x in 1:4)
{
  bob_df[nrow(bob_df) + 1,] = c(bob_df[nrow(bob_df),1]+1,NA,NA)
}
k=3
b=nrow(bob_df)-k
for(i in 1:b){
  B=i+(k-1)
  bob_df$prediction[i+k]=mean(bob_df$Close[i:B])
  if(is.na(bob_df$Close[i+k])){
    bob_df$Close[i+k]=bob_df$prediction[i+k]
  }
}

# Moving Average For SBI
sbi_new = Quandl("NSE/SBIN", collapse = "daily", start_date = "2017-01-01", type = "raw")
sbi_df = map_df(select(sbi_new, "Date", "Close"), rev)
sbi_df$prediction=NA
for(x in 1:4)
{
  sbi_df[nrow(sbi_df) + 1,] = c(sbi_df[nrow(sbi_df),1]+1,NA,NA)
}
k=3
b=nrow(sbi_df)-k
for(i in 1:b){
  B=i+(k-1)
  sbi_df$prediction[i+k]=mean(sbi_df$Close[i:B])
  if(is.na(sbi_df$Close[i+k])){
    sbi_df$Close[i+k]=sbi_df$prediction[i+k]
  }
}

