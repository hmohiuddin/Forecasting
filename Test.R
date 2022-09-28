library(ggplot2)
library(forecast)
library(readxl)

gerretail <- read_excel("GER_retail_sales.xlsx")
head(gerretail)

gerretaildata <- ts(data = gerretail[,2],start = c(2000,1), end = c(2020,3),frequency = 12)
gerretaildata

### Ploting the data ###
autoplot(gerretaildata) + 
  ylab("Sales (??? Million)") + xlab("Year")+ 
  ggtitle("Time Plot",subtitle = "German Retail Sales")+theme_light()
ggseasonplot(gerretaildata,year.labels = TRUE,year.labels.left = TRUE)+ 
  ylab("Sales (??? Million)")+ ggtitle("Seasonal Plot",subtitle = "German Retail Sales")+
  theme_light()


retailtrain <- window(x = gerretaildata, start = c(2000,1), end = c(2015,12))


################### Naive #################################
#gerretailnaive <- naive(retailtrain,h=9)
#summary(gerretailnaive)
#autoplot(gerretailnaive) + xlab("Year") + ylab("Sales ($ billion)")+theme_light()


forecastnaive <- function(x, h){forecast(naive(x), h=h)}
testfcenaive <- gerretaildata %>% tsCV(forecastfunction = forecastnaive,h=1) %>%  window(start = c(2016,1),end = c(2020,3))
testmsenaive <- mean(testfcenaive^2,na.rm = TRUE)  
#MSE = 16065549
testrmsenaive <- sqrt(testmsenaive)
#RMSE = 4008.185
testmaenaive <- mean(abs(testfcenaive),na.rm = TRUE)
#MAE = 2722.47
testmapenaive <- 100*mean((abs(testfcenaive)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 5.821591

######################## Seasonal Naive ######################
#gerretailsnaive <- snaive(retailtrain,h=9)
#summary(gerretailsnaive)
#autoplot(gerretailsnaive) + xlab("Year") + ylab("Sales ($ billion)")+theme_light()

forecastsnaive <- function(x, h){forecast(snaive(x), h=h)}
testfcesnaive <- gerretaildata %>% tsCV(forecastfunction = forecastsnaive,h=1) %>%  window(start = c(2016,1),end = c(2020,3))
testmsesnaive <- mean(testfcesnaive^2,na.rm = TRUE)
#MSE = 2575757
testrmsesnaive <- sqrt(testmsesnaive)
#RMSE = 1604.916
testmaesnaive <- mean(abs(testfcesnaive),na.rm = TRUE)
#MAE = 1400.178
testmapesnaive <- 100*mean((abs(testfcesnaive)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#3.007921


##################### LES #################################
retailsholt <- holt(y = retailtrain, initial = "simple",h = 9)
summary(retailsholt)
#  alpha = 0.089 , beta  = 0.4385
autoplot(retailsholt) + autolayer(fitted(retailsholt))+xlab("Year")+ylab("Sales (??? Million)")+theme_light()

forecastfcholt <- function(x,h){forecast(holt(x,initial = "simple",alpha = 0.089,beta = 0.4385),h=h)}
testfceholt <- gerretaildata %>% tsCV(forecastfunction = forecastfcholt,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmseholt <- mean(testfceholt^2,na.rm = TRUE)
#MSE = 10923324
testrmseholt <- sqrt(testmseholt)
#RMSE = 3305.045
testmaeholt <- mean(abs(testfceholt),na.rm = TRUE)
#MAE =  2410.42
testmapeholt <- 100*mean((abs(testfceholt)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 5.121491

######################### LES Damped ####################
retailsholtd <- holt(y = retailtrain, damped = TRUE,h = 9)
summary(retailsholtd)
# alpha = 0.0001 beta  = 0.0001 phi   = 0.8529
autoplot(retailsholtd) + autolayer(fitted(retailsholtd)) +xlab("Year")+ylab("Sales (??? Million)")+theme_light()

forecastfcholtd <- function(x,h){forecast(holt(x,damped = TRUE,alpha = 0.0001,beta = 0.0001,phi = 0.8529),h=h)}
testfceholtd <- gerretaildata %>% tsCV(forecastfunction = forecastfcholtd,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmseholtd <- mean(testfceholtd^2,na.rm = TRUE)
#MSE = 29898937
testrmseholtd <- sqrt(testmseholtd)
#RMSE = 5467.992
testmaeholtd <- mean(abs(testfceholtd),na.rm = TRUE)
#MAE = 4573.815
testmapeholtd <- 100*mean((abs(testfceholtd)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 9.411423

##################### Holt-Winters' method ###########################

## HW Additive
retailshw <- hw(y = retailtrain,seasonal = "additive",initial = "simple",h = 9)
summary(retailshw)
autoplot(retailshw) + autolayer(fitted(retailshw)) +xlab("Year")+ylab("Sales (??? Million)")+theme_light()
#alpha = 0.1424 beta  = 0.147 gamma = 0.3471

forecastfchw <- function(x,h){forecast(hw(x,seasonal = "additive",initial = "simple",
                                          alpha = 0.1424,beta = 0.147,gamma = 0.3471),h=h)}
testfcehw <- gerretaildata %>% tsCV(forecastfunction = forecastfchw,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmsehw <- mean(testfcehw^2,na.rm = TRUE)
#MSE = 1188958
testrmsehw <- sqrt(testmsehw)
#RMSE = 1090.393
testmaehw <- mean(abs(testfcehw),na.rm = TRUE)
#MAE = 904.2609
testmapehw <- 100*mean((abs(testfcehw)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 1.926935


#HW Multiplicative
retailshwm <- hw(y = retailtrain,seasonal = "multiplicative",initial = "simple",h = 9)
summary(retailshwm)
autoplot(retailshwm) + autolayer(fitted(retailshwm)) +xlab("Year")+ylab("Sales (??? Million)")+theme_light()
#alpha = 0.1452 beta  = 0.1375 gamma = 0.344

forecastfchwm <- function(x,h){forecast(hw(x,seasonal = "multiplicative",initial = "simple",
                                          alpha = 0.1452,beta = 0.1375,gamma = 0.344),h=h)}
testfcehwm <- gerretaildata %>% tsCV(forecastfunction = forecastfchwm,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmsehwm <- mean(testfcehwm^2,na.rm = TRUE)
#MSE = 1300338
testrmsehwm <- sqrt(testmsehwm)
#RMSE 1140.324
testmaehwm <- mean(abs(testfcehwm),na.rm = TRUE)
#MAE = 938.7477
testmapehwm <- 100*mean((abs(testfcehwm)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 1.993322


## HW Damped
retailshwd <- hw(y = retailtrain,damped = TRUE,h = 9)
summary(retailshwd)
autoplot(retailshwd)+autolayer(fitted(retailshwd)) +xlab("Year")+ylab("Sales (??? Million)")+theme_light()
#alpha = 0.0625 beta  = 0.0468 gamma = 0.0001 phi   = 0.8464 

forecastfchwd <- function(x,h){forecast(hw(x,damped = TRUE,
                                           alpha = 0.0625,beta = 0.0468,gamma = 0.0001,phi = 0.8464),h=h)}
testfcehwd <- gerretaildata %>% tsCV(forecastfunction = forecastfchwd,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmsehwd <- mean(testfcehwd^2,na.rm = TRUE)
#MSE = 1284925
testrmsehwd <- sqrt(testmsehwd)
#RMSE = 1133.545 
testmaehwd <- mean(abs(testfcehwd),na.rm = TRUE)
#MAE = 891.1154
testmapehwd <- 100*mean((abs(testfcehwd)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 1.885251



###################### ARIMA ###############################

retailarima <- auto.arima(retailtrain)
summary(retailarima)
# ARIMA(2,1,2)(0,1,1)[12]  

forecastARIMA <- function(x,h){forecast(x,model = retailarima,h=h)}
testfcearima <- gerretaildata %>% tsCV(forecastfunction = forecastARIMA,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmsearima <- mean(testfcearima^2,na.rm = TRUE)
#MSE = 1147330
testrmsearima <- sqrt(testmsearima)
#RMSE = 1071.135
testmaearima <- mean(abs(testfcearima),na.rm = TRUE)
#MAE = 880.5154
testmapearima <- 100*mean((abs(testfcearima)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 1.899161


################ ETS Method ######################

retailsets <- ets(y = retailtrain)
summary(retailsets)
#Model: ETS(A,N,A) 

forecastets <- function(x, h){forecast(ets(x, model=retailsets, use.initial.values=TRUE), h=h)}
testfceets <- gerretaildata %>% tsCV(forecastfunction = forecastets,h = 1) %>% window(start = c(2016,1),end = c(2020,3))
testmseets <- mean(testfceets^2,na.rm = TRUE)
#MSE = 1538659
testrmseets <- sqrt(testmseets)
#RMSE = 1240.427
testmaeets <- mean(abs(testfceets),na.rm = TRUE)
#MAE = 983.2683
testmapeets <- 100*mean((abs(testfceets)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 2.082782



#################### Forecast ###########################

## Model: ARIMA(2,1,2)(0,1,1)[12] 

retailforecastets <- forecast(gerretaildata,h = 9,model = retailarima)
summary(retailforecastets)
autoplot(retailforecastets) + xlab("Year") + ylab("Sales (??? Million)") + 
  ggtitle("Forecast",subtitle = "Model: ARIMA(2,1,2)(0,1,1)[12] ") + theme_light()

#Forecasts:
#  Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Apr 2020       49781.46 48636.40 50926.52 48030.24 51532.68
#May 2020       50183.40 49035.10 51331.71 48427.23 51939.58
#Jun 2020       47705.11 46556.59 48853.63 45948.60 49461.62
#Jul 2020       49971.89 48654.43 51289.35 47957.01 51986.77
#Aug 2020       48482.50 47161.24 49803.76 46461.81 50503.19
#Sep 2020       47318.72 45969.03 48668.40 45254.55 49382.88
#Oct 2020       50599.86 49191.37 52008.36 48445.76 52753.97
#Nov 2020       52480.91 51059.52 53902.31 50307.07 54654.75
#Dec 2020       56853.02 55396.01 58310.04 54624.71 59081.33




################## Change of Estimation and Hold out sample #############

## Estimation sample From 2000 to 2017 and Hold out 2018/1 - 2020/3
retailtrain1 <- window(x = gerretaildata, start = c(2000,1), end = c(2017,12))
retailarima1 <- auto.arima(retailtrain1)
summary(retailarima1)

forecastARIMA1 <- function(x,h){forecast(x,model = retailarima1,h=h)}
testfcearima1 <- gerretaildata %>% tsCV(forecastfunction = forecastARIMA1,h = 1) %>% window(start = c(2018,1),end = c(2020,3))
testmsearima1 <- mean(testfcearima1^2,na.rm = TRUE)
#MSE = 920314.3
testrmsearima1 <- sqrt(testmsearima1)
#RMSE = 959.3302
testmaearima1 <- mean(abs(testfcearima1),na.rm = TRUE)
#MAE = 767.1016
testmapearima1 <- 100*mean((abs(testfcearima1)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 1.61738

retailforecastets1 <- forecast(gerretaildata,h = 9,model = retailarima1)
summary(retailforecastets1)

#Forecasts:
#  Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Apr 2020       49929.36 48761.10 51097.61 48142.66 51716.05
#May 2020       50005.21 48836.57 51173.85 48217.92 51792.49
#Jun 2020       47851.90 46678.43 49025.36 46057.23 49646.56
#Jul 2020       49838.06 48523.89 51152.22 47828.21 51847.90
#Aug 2020       48503.99 47184.27 49823.71 46485.65 50522.33
#Sep 2020       47413.49 46066.62 48760.35 45353.63 49473.34
#Oct 2020       50517.30 49117.52 51917.09 48376.52 52658.09
#Nov 2020       52478.41 51062.33 53894.49 50312.70 54644.12
#Dec 2020       56883.15 55436.32 58329.99 54670.41 59095.90

## Estimation sample From 2000 to 2013 and Hold out 2014/1 - 2020/3
retailtrain2 <- window(x = gerretaildata, start = c(2000,1), end = c(2013,12))
retailarima2 <- auto.arima(retailtrain2)
summary(retailarima2)

forecastARIMA2 <- function(x,h){forecast(x,model = retailarima2,h=h)}
testfcearima2 <- gerretaildata %>% tsCV(forecastfunction = forecastARIMA2,h = 1) %>% window(start = c(2014,1),end = c(2020,3))
testmsearima2 <- mean(testfcearima2^2,na.rm = TRUE)
#MSE = 2154913
testrmsearima2 <- sqrt(testmsearima2)
#RMSE = 1467.962
testmaearima2 <- mean(abs(testfcearima2),na.rm = TRUE)
#MAE = 1206.974
testmapearima2 <- 100*mean((abs(testfcearima2)/lag(gerretaildata,k = 1)),na.rm = TRUE)
#MAPE = 2.609907

retailforecast2 <- forecast(object = gerretaildata,h=9,model = retailarima2)
summary(retailforecast2)

#Forecasts:
#  Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#Apr 2020       48110.65 46881.23 49340.07 46230.42 49990.88
#May 2020       47706.90 46468.59 48945.21 45813.07 49600.73
#Jun 2020       46462.35 45216.13 47708.56 44556.43 48368.27
#Jul 2020       47700.96 46447.71 48954.20 45784.28 49617.63
#Aug 2020       46275.95 45016.45 47535.46 44349.71 48202.20
#Sep 2020       45855.90 44590.82 47120.99 43921.13 47790.68
#Oct 2020       48153.12 46883.07 49423.17 46210.74 50095.50
#Nov 2020       49690.93 48416.45 50965.41 47741.78 51640.08
#Dec 2020       54666.14 53387.70 55944.57 52710.94 56621.33


