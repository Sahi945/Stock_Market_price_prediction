library(Quandl)
library(tidyverse)
library(tidyquant)
library(gganimate)
library(plyr)
library(forecast)
library(tseries)



# API KEY FOR QUANDL C677MQeRPrHgv8JvuF82
rm(list = ls())
Quandl.api_key("C677MQeRPrHgv8JvuF82")

#get the dataset
TCS = Quandl("NSE/TCS", collapse = "daily", start_date = "2017-01-01", type = "raw")
WIPRO= Quandl("NSE/WIPRO",collapse="daily",start_date="2017-01-01",type="raw")
HCL=Quandl("NSE/HCLTECH",collapse="daily",start_date="2017-01-01",type="raw")
INFOSYS=Quandl("NSE/INFY",collapse="daily",start_date="2017-01-01",type="raw")
TECHM=Quandl("NSE/TECHM",collapse="daily",start_date="2017-01-01",type="raw")
LTI=Quandl("NSE/LTI",collapse="daily",start_date="2017-01-01",type="raw")

#adding the stock column for future use while plotting the graph
TCS<-cbind(TCS,Stock="")
WIPRO<-cbind(WIPRO,Stock="")
HCL<-cbind(HCL,Stock="")
INFOSYS<-cbind(INFOSYS,Stock="")
TECHM<-cbind(TECHM,Stock="")
LTI<-cbind(LTI,Stock="")

# Paste the stock name in stock column
# again usefull for plotting purpose
TCS$Stock<-paste(TCS$Stock,"TCS",sep="")
WIPRO$Stock<-paste(WIPRO$Stock,"WIPRO",sep="")
HCL$Stock<-paste(HCL$Stock,"HCL",sep="")
INFOSYS$Stock<-paste(INFOSYS$Stock,"INFOSYS",sep="")
TECHM$Stock<-paste(TECHM$Stock,"TECHM",sep="")
LTI$Stock<-paste(LTI$Stock,"LTI",sep="")


# Consolidate under one master dataset
all_stocks = rbind(TCS, WIPRO, HCL, INFOSYS, TECHM, LTI)

#Convert the dates into character in order to split the column into "Y" "m" "dd"" columns
all_stocks$Date = as.character(all_stocks$Date)
#split the date column by - and create a list for the same
date_split = strsplit(all_stocks$Date, "-")


# to return results of the above list in a data frame
Master_date = ldply(date_split)
#changing the column name of the master date dataframe
colnames(Master_date) = c("Year", "Month", "Day")

#now we'll column bind the above master_date data to the Master_data
all_stocks<-cbind(all_stocks,Master_date)
names(all_stocks)

# Change the scale for Traded Quantity
all_stocks$`Total Trade Quantity` = all_stocks$`Total Trade Quantity`/100000

# Convert the Date to as.Date()
all_stocks$Date<-as.Date(all_stocks$Date)

all_stocks$Month<-as.integer(all_stocks$Month)
all_stocks$Year<-as.integer(all_stocks$Year)
all_stocks$Day<-as.integer(all_stocks$Day)


P<- all_stocks %>% ggplot(aes(factor(Stock), Close, color=Stock)) +
  geom_jitter(aes(size = Close, colour=Stock, alpha = 0.03)) +
  ylim(0,3000)+
  labs(title = "IT Stock Yearly Prices", x = "IT Company", y= "Close Price") +
  theme(panel.border = element_blank(),
        panel.grid.major = element_line(colour = "grey61", size = 0.5, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        axis.line=element_line(colour="black"),
        plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
        legend.position = "right")+
        transition_reveal(Year)

P



## Visualization for month-wise daily stock prices
##facet_wrap is used to group the plots by respective stocks
##and also display in a row-col format

ggplot(all_stocks, aes(x = Date, y = Close, color = Stock)) +
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

#used for plotting the 
all_stocks<-all_stocks%>%
  tibble::as_tibble()%>%
  group_by(Stock)
all_stocks_high <- mutate(all_stocks , Dev_High = High-Open)
all_stocks_low <-mutate(all_stocks , Dev_Low = Open-Low)

#computing the weekly high prices
#for this we use tq_transmute from the tidyquant package
#this method adds new variables to and existing dataset and
#returns only newly created columns, typically used when periodicity changes
all_stocks_high_week <- all_stocks_high %>%
  tq_transmute(
    select     = Dev_High,
    mutate_fun = apply.weekly, 
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "Dev_High_Mean"
  )



#similarly doing to compute weekly low prices

all_stocks_low_week<-all_stocks_low%>%
  tq_transmute(
    select  = Dev_Low,
    mutate_fun = apply.weekly,
    FUN = mean,
    na.rm = TRUE,
    col_rename = "Dev_Low_Mean"
  )



##Visualization of density distribution of high Price
High<-all_stocks_high_week%>%ggplot(aes(x=Dev_High_Mean,color=Stock))+
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

Low<-all_stocks_low_week%>%ggplot(aes(x=Dev_Low_Mean,color=Stock))+
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
#grid.arrange(High, Low)

#visualizing the volatility of the stock using bollinger bands
end<-ymd("2019-01-04")
start<-ymd("2017-01-04")

bollinger_tcs = all_stocks %>%filter(Stock == "TCS")%>% ggplot(aes(x = Date, y = Close))+
  geom_line(size =1)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA,n = 30,size=0.75,
             color_ma = "royalblue4", color_bands = "red1")+
  labs(title = "Bollinger Band TCS", x = "Date", y = "Price")+
  theme(axis.title.x = element_text(hjust = 0.5, size = 16)
        ,axis.title.y = element_text(hjust = 0.5, size = 16)
        ,plot.title = element_text(hjust = 0.5, color = "navyblue", size = 20)
        ,axis.title = element_text(color = 'navyblue', size = 20))

bollinger_tcs


#visualizing the volatility of all the stocks
bollinger_all = all_stocks %>%filter(Stock == "TCS"|Stock == "WIPRO"|Stock =="LTI"|Stock == "HCL"|Stock == "TECHM"|Stock == "INFOSYS")%>% ggplot(aes(x = Date, y = Close))+
  geom_line(size =1)+
  facet_wrap(~Stock, scales = "free_y", ncol = 3)+
  geom_bbands(aes(high = High, low = Low, close = Close), ma_fun = SMA, sd=2,n = 20,size=0.75,
              color_ma = "royalblue4", color_bands = "red1")+
  coord_x_date(xlim = c(start, end), expand = TRUE)+
  labs(title = "Bollinger Bands", x = "Date", y = "Price")+
  theme(axis.title.x = element_text(hjust = 0.5, size = 16)
        ,axis.title.y = element_text(hjust = 0.5, size = 16)
        ,plot.title = element_text(hjust = 0.5, color = "navyblue", size = 20)
        ,axis.title = element_text(color = 'navyblue', size = 20))

bollinger_all


################# TCS ARIMA #########################################
#data
TCS_NEW = Quandl("NSE/TCS", collapse = "daily", start_date = "2017-01-01", type = "raw")
tcs_df = map_df(select(TCS_NEW, "Date", "Close"), rev)


#plot
plot(tcs_df, type = "l")
plot(y = tcs_df$Close,x = tcs_df$Date, type = 'l')
plot(diff(tcs_df$Close), type = 'l', main = "tcs differenced")

time_series_tcs = ts(tcs_df$Close, frequency = 30)
ddtcs = decompose(time_series_tcs, "multiplicative")
plot(ddtcs)

#ARIMA
acf(diff(tcs_df$Close), main = "tcs acf plot")#p = 1
pacf(diff(tcs_df$Close), main = "tcs pacf plot")#q = 1

#MODEL FITTING
tcs_model = arima(log(tcs_df$Close), c(1,1,1), seasonal=list(order=c(1,1,1), period=7))
tcs_model
tcs_model$residual 
model1 = 2.178^(tcs_model$residual)

pred_tcs=predict(tcs_model, n.ahead = 7)
pred1=2.718^pred_tcs$pred
pred1

plot(tcs_df$Close, type = 'l', xlim = c(450, 520), main = "7- Day prediction graph for TCS", ylab = 'Close Price', xlab = "time frame")
lines(pred1, type = 'l', col = 'red')


################# TECHM ARIMA #########################################

TECHM_NEW = Quandl("NSE/TECHM", collapse = "daily", start_date = "2017-01-01", type = "raw")
techm_df = map_df(select(TECHM_NEW, "Date", "Close"), rev)

#plot
plot(techm_df, type = "l", main = "Tech Mahindra Stock Price")
plot(diff(techm_df$Close), type = 'l', main = "TECHM differenced")

time_series_techm = ts(techm_df$Close, frequency = 30)
ddtcs = decompose(time_series_techm, "multiplicative")
plot(ddtcs)

#ARIMA
acf(diff(techm_df$Close), main = "TECHM acf plot")#p = 0
pacf(diff(techm_df$Close), main = "TECHM pacf plot")#q = 1

#MODEL FITTING
techm_model = arima(log(techm_df$Close), c(0,1,0), seasonal=list(order=c(0,1,0), period=7))
techm_model
techm_model$residual 
model2 = 2.178^(techm_model$residual)

pred_techm = predict(techm_model, n.ahead = 4)
pred2=2.718^pred_techm$pred
pred2

plot(techm_df$Close, type = 'l', xlim = c(450, 520), main = "7- Day prediction graph for TECHM", ylab = 'Close Price', xlab = "time frame")
lines(pred2, type = 'l', col = 'red')
