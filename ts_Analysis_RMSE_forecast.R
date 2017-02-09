library('forecast')
#library(splines)

#train data

data=read.csv('C:/Users/swaya/machine_data_161221_daily.csv')
data_train = data[,-which(names(data) %in% c("delivery_date","cs_amount_all","cs_amount_payment_volume",
                                             "cs_amount_store_sales","cs_transaction_all",
                                             "cs_transaction_payment_volume","cs_transaction_store_sales","subs_new",
                                             "subs_churned","subs_eop","panel_sales_year","week_of_year",
                                             "week_begin_date","week_end_date"))]

data_train$salesIndex <- with(data_train,index_amount/index_normalizer)
data_train$TransactionIndex <- with(data_train,index_transaction/index_normalizer)

data_train <- na.omit(data_train)


data_train$date <- as.Date(data_train$date,"%m/%d/%Y")

data_train = subset(data_train,date <= "2015-12-31")

data_train = subset(data_train,index_amount != 0)


#test data
data_test = read.csv('C:/Users/swaya/machine_data_170104_daily.csv')
data_test$date <- as.Date(data_test$date,"%m/%d/%Y")

data_test = subset(data_test,date >= "2016-01-01")

data_test = data_test[,-which(names(data_test) %in% c("delivery_date","cs_amount_all",
                                                      "cs_amount_payment_volume","cs_amount_store_sales","cs_transaction_all","cs_transaction_payment_volume",
                                                      "cs_transaction_store_sales","subs_new","subs_churned","subs_eop","panel_sales_year","week_of_year",
                                                      "week_begin_date","week_end_date"))]

data_test$salesIndex <- with(data_test,index_amount/index_normalizer)
data_test$TransactionIndex <- with(data_test,index_transaction/index_normalizer)

data_test <- na.omit(data_test)
data_test = subset(data_test,index_amount != 0)

#unique list of tickers
uniqueTabs = unique(data_test$tab)

#added 
uniqueTabs = uniqueTabs[88]
#end of added

TickersAndRMSE <- data.frame(tab=uniqueTabs)
RMSEOfTicker <- c()

TickersAndForecast <- data.frame(tab=uniqueTabs)
ForecastOfTicker <- list()
#Begin for loop
for(tb in uniqueTabs) 
{
  print(tb)
  data_trial = subset(data_train,tab==tb)
  data_trial = data.frame("date" = data_trial$date,
                          "index_amount" = data_trial$index_amount)
  
  #data_test_sub = subset(data_test,date > "2016-12-08")
  
  data_test_sub = subset(data_test,tab == tb)
  
  data_test_sub = data.frame("date" = data_test_sub$date,"index_amount" = 
                               data_test_sub$index_amount)
  
  
  #Daily forecast with regression with arima errors
  #library(forecast)
  
  #below fails with error because of the "end"
  #gas <- ts(data_trial$index_amount,frequency = 365.25,start=c(2012,5,2),
  #          end=c(2015,12,31))
  
  #gas <- ts(data_trial$index_amount,frequency = 365.25,start=c(2012,5,2))
  
  gas <- ts(data_trial$index_amount,frequency = 365.25,
            start=start(data_trial$date))
  
  #gas <- ts(data_trial$index_amount,frequency = 365.25,
  #          start=min(data_trial$date))
  
  bestfit <- list(aicc=Inf)
  temp = 0
  for(i in 1:10)
  {
    fit <- try(auto.arima(gas, xreg=fourier(gas, K=i), seasonal=TRUE))
    if(inherits(fit, "try-error"))
    {
      next
    }
    if(fit$aicc < bestfit$aicc)
    {
      temp = i
      bestfit <- fit
    }
    else break;
  }
  fc <- try(forecast(bestfit, xreg=fourier(gas, K=temp, h=731)))
  if(inherits(fc, "try-error"))
  {
    RMSEOfTicker <- c(RMSEOfTicker,0)
    listofZeros = as.list(rep(0,731))
    ForecastOfTicker <- list(ForecastOfTicker,listofZeros)
    next
  }  
  plot(fc)
  xr_1 = c(start(gas),end(gas)+2)
  #wasnt there ts_xr = ts(xr,start = start(gas),end=end(gas),frequency = frequency(gas))
  plot(gas, xlim=xr_1, ylab = "Sales", xlab = "Days")
  lines(fc$mean,x=seq(end(gas),end(gas)+2,length=731), col="blue")
  lines(fc$lower[,2],x=seq(end(gas),end(gas)+2,length=731), col="green")
  lines(fc$upper[,2],x=seq(end(gas),end(gas)+2,length=731), col="red")
  accr <- data.frame(accuracy(fc, data_test_sub$index_amount))
  RMSE <- accr[2,2] #Gets RMSE of the testing set
  RMSEOfTicker <- c(RMSEOfTicker,RMSE)
  ForecastOfTicker <- list(ForecastOfTicker,fc$mean)
  
}
#write.csv(fc,file="foo.csv")

TickersAndRMSE$RMSE <- RMSEOfTicker
TickersAndForecast$forecast <- list(ForecastOfTicker)

print(TickersAndRMSE)

SortedTickersAndRMSE <- TickersAndRMSE[order(TickersAndRMSE$RMSE),]

print(SortedTickersAndRMSE)