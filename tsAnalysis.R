library('forecast')
#library(splines)

#train data

data=read.csv('H:/machine_data_161221_daily.csv')
data_train = data[,-which(names(data) %in% c("delivery_date","cs_amount_all","cs_amount_payment_volume",
                                             "cs_amount_store_sales","cs_transaction_all",
                                             "cs_transaction_payment_volume","cs_transaction_store_sales","subs_new",
                                             "subs_churned","subs_eop"))]

data_train$salesIndex <- with(data_train,index_amount/index_normalizer)
data_train$TransactionIndex <- with(data_train,index_transaction/index_normalizer)

data_train <- na.omit(data_train)

data_train$date <- as.Date(data_train$date,"%m/%d/%Y")

data_trial = subset(data_train,tab=="ANF"& date <= "2015-12-31")
data_trial = data.frame("date" = data_trial$date,
                        "index_amount" = data_trial$index_amount)






#test data
data_test = read.csv('H:/machine_data_170104_daily.csv')
data_test$date <- as.Date(data_test$date,"%m/%d/%Y")

data_test_sub = data_test[,-which(names(data_test) %in% c("delivery_date","cs_amount_all",
                                                          "cs_amount_payment_volume","cs_amount_store_sales","cs_transaction_all","cs_transaction_payment_volume",
                                                          "cs_transaction_store_sales","subs_new","subs_churned","subs_eop"))]

data_test_sub$salesIndex <- with(data_test_sub,index_amount/index_normalizer)
data_test_sub$TransactionIndex <- with(data_test_sub,index_transaction/index_normalizer)

data_test_sub <- na.omit(data_test_sub)


#data_test_sub = subset(data_test,date > "2016-12-08")

data_test_sub = subset(data_test,tab == "ANF" & date >= "2016-01-01")

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
for(i in 1:25)
{
  fit <- auto.arima(gas, xreg=fourier(gas, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
  {
    temp = i
    bestfit <- fit
  }
  else break;
}
fc <- forecast(bestfit, xreg=fourier(gas, K=temp, h=363))
plot(fc)




#Scott's method
plot(data_trial,ylab="sales",type="o",pch=20)
gas <- ts(data_trial$index_amount,frequency = 365.25,
          start=start(data_trial$date)) #ask about this, not sure
                                      #if the ts is created correctly
decompM <- decompose(gas,type="multiplicative")
plot(decompM)

plot(gas - (decompM$trend*decompM$seasonal))


#Continue with improving the regression with arima errors model, 
#keep solving scott's model (you are close), and try the tbats
#model (get it to work)

#Tbats -> not working, try at home
#gas <- ts(data_trial$index_amount,frequency = 365.25,start=c(2012,5,2))
#y <- msts(gas,seasonal.periods = c(7,365.25))
#model12 <- tbats(y)
#fc <- forecast(model12,h=3)
#plot(fc)
daily <- msts(data_trial$index_amount,seasonal.periods = c(7,365.25))
fit <- tbats(daily)
fc <- forecast(tbats)


#Try moving average.
#Try naive forecast
#Try average all past forecast
#Try exponential smoothing
#Try auto.arima -> handles autocorrelation, autoregressive model
#Try tbats model

#Moving avg model
moving_average = forecast(ma(data_trial$index_amount, order=1), h=5)
#moving_average_accuracy = accuracy(moving_average, data_test_sub[1:5,]$index_amount)
#moving_average; moving_average_accuracy
#plot(data_test_sub[1:5,]$date,moving_average)
plot(m,ylim=c(0,250000))
lines(data_test_sub$index_amount,col="red")


#Simple exponential Smoothing
exp <- ses(data_trial$index_amount, 363, initial="simple")
exp_accuracy = accuracy(exp, data_test_sub$index_amount)
exp; exp_accuracy
plot(exp, ylim=c(0,300000))
lines(data_test_sub$index_amount,col="red")



#automatic forecasting function that will run through possible models and select the most appropriate model give 
#the data. This could be an auto regressive model of the first oder (AR(1)), an ARIMA model with the right values 
#for p, d, and q, or something else that is more appropriate.


train = data_trial$index_amount
test = data_test_sub$index_amount
arma_fit <- auto.arima(train)
arma_forecast <- forecast(arma_fit, h = 363)
arma_fit_accuracy <- accuracy(arma_forecast, test)
arma_fit; arma_forecast; arma_fit_accuracy
plot(arma_forecast, ylim=c(0,250000))
lines(data_test_sub$index_amount,col="red")




