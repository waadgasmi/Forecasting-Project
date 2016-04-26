require(forecast)
require(tseries)
require(ets)
require(xts)

#load the dataset: 

TR <- read.csv("TrainingSet.csv") ##load dataset 
names (TR)

##  [1] "date" "v1"   "v2"   "v3"   "v4"   "v5"   "v6"   "v7"   "v8"   "v9"  
##[11] "v10"  "v11"  "v12"  "v13"  "v14"  "v15" 

DateTime <-as.POSIXct(as.character(TR$date), format = "%m/%d/%Y %H:%M") ## convert factor column to time series object
fix(DateTime)
class(DateTime)

##[1] "POSIXct" "POSIXt"  

plot.ts(TR$v1)
x.TR <- xts(x=TR$v1, order.by =DateTime)
class(x.TR)

##[1] "xts" "zoo"
plot(x.TR)

# To get the start date (355)
as.POSIXlt(x = "12/12/2016 0:00:00", origin="12/12/2016 0:00:00")$yday
##    [1] 354
# Add one since that starts at "0"

x.ts = ts(TR$v1,freq=696, start= c(2016, 355))
#will remove the last 35 row (which has NA value) in V1
x.ts <- x.ts[1:696]
length(x.ts)
##Performe forecasting for 35 periods (35 hours) for V1 using ETS model (Exponential smoothing, it chooses a model by default)
ETS.forecast <- forecast(ets(x.ts),35)
plot(forecast(ets(x.ts),35))


ETS.forecast

#some of ETS forecast Values:

  ####Point Forecast      Lo 80     Hi 80       Lo 95    Hi 95
#697      97.795358   82.30320 113.28752   74.102145 121.4886
#698      67.331196   28.91191 105.75048    8.573963 126.0884
#699      40.842284  -17.54030  99.22487  -48.446173 130.1307
#700      17.809896  -60.11662  95.73642 -101.368432 136.9882
#701      -2.217009  -99.51997  95.08596 -151.029053 146.5950
#702     -19.630614 -136.12556  96.86433 -197.794262 158.5330
#703     -34.771929 -170.21278 100.66893 -241.910840 172.3670
#704     -47.937461 -202.01880 106.14388 -283.584534 187.7096
#705     -59.385031 -231.75471 112.98465 -323.001711 204.2316
#706     -69.338814 -259.61144 120.93381 -360.335678 221.6580

#based on the forecasting ETS values above, i can say that the results are not good. i will use this time the ARIMA model. 

##Performe forecasting for 35 periods (35 hours) for V1 using ARIMA model

ARIMA.forecast <- forecast(auto.arima(x.ts),35)
ARIMA.forecast 
plot(ARIMA.forecast, xlab="Number of Hours",ylab="number of Clicks for Borwser V1")

#some of ARIMA forecast Values: 
 #   Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#697      110.31720  98.14688 122.4875  91.70430 128.9301
#698       96.75095  77.17638 116.3255  66.81423 126.6877
#699       93.26273  66.12206 120.4034  51.75466 134.7708
#700       98.36131  64.08549 132.6371  45.94096 150.7817
#701      114.22605  73.12366 155.3284  51.36536 177.0867
#702      137.71938  91.02564 184.4131  66.30746 209.1313
#703      167.53723 116.53761 218.5368  89.54004 245.5344
#704      201.29085 147.33843 255.2433 118.77773 283.8040
#705      236.80575 181.11909 292.4924 151.64036 321.9711


## QUESTION 2: Multiple Regression Model, will use v2 as a second variable for prediction: 

coplot(DateTime~v1|v2, TR)
mean(TR$v1)
RegModel = lm(v1~DateTime*v2, data =TR)
plot(RegModel)
summary(TR$v1)
summary(TR$v2)

v1.PredValues <- TR$v1
v2.PredValues <- TR$v2
newData <- data.frame(v1=v1.PredValues, v2= v2.PredValues)
newData
Pred<- predict(RegModel,newData,35)
Pred

# Predicion results using the multiple regression Model :

#697       698       699       700 
#1.6911808 1.6088246 1.6103393 1.6618307 2.0419324 2.5222930 2.5072223 2.8661126 2.7238144 2.5068425 
 #     701       702       703       704       705       706       707       708       709       710 
#2.0259378 1.7854194 1.6809017 1.6707478 1.8085085 2.2425415 2.6304353 3.2160468 3.5095139 3.4257283 
 #     711       712       713       714       715       716       717       718       719       720 
#3.4671973 3.3259161 3.1393050 2.2534715 1.9561189 1.6972005 1.7503573 1.8032723 2.0899912 2.4393431 
 #     721       722       723       724       725       726       727       728       729       730 
#2.8744288 2.9950478 2.8364199 2.4431704 1.9756027 1.8080534 1.7465546 1.7875736 1.8157429 1.8702684 
 #     731 
#2.6299516 

 

