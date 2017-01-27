library('forecast')

#train data

data=read.csv('H:/machine_data_161221_daily.csv')
data_train = data[,-which(names(data) %in% c("delivery_date","cs_amount_all","cs_amount_payment_volume",
"cs_amount_store_sales","cs_transaction_all","cs_transaction_payment_volume","cs_transaction_store_sales","subs_new",
"subs_churned","subs_eop"))]

data_train$salesIndex <- with(data_train,index_amount/index_normalizer)
data_train$TransactionIndex <- with(data_train,index_transaction/index_normalizer)

data_train <- na.omit(data_train)

#test data
data_test = read.csv('H:/machine_data_170104_daily.csv')

data_test$date <- as.Date(data_test$date,"%m/%d/%Y")

data_test_sub = subset(data_test,date > "2016-12-08")

data_test_sub = data_test_sub[,-which(names(data_test_sub) %in% c("delivery_date","cs_amount_all",
"cs_amount_payment_volume","cs_amount_store_sales","cs_transaction_all","cs_transaction_payment_volume",
"cs_transaction_store_sales","subs_new","subs_churned","subs_eop"))]

data_test_sub$salesIndex <- with(data_test_sub,index_amount/index_normalizer)
data_test_sub$TransactionIndex <- with(data_test_sub,index_transaction/index_normalizer)

data_test_sub <- na.omit(data_test_sub)


#Below, plotting the index amount and index transaction amt against each other across all the tickers, revealed
#more or less a linear pattern.
# data_trial = data.frame("index_amount" = data_train$index_amount,"index_transaction" = data_train$index_transaction)
# pairs(data_trial)

#Below, plotting the sales index and transaction index  against each other across all the tickers, revealed
#more or less a linear pattern.
# data_trial_normalized = data.frame("salesIndex" = data_train$salesIndex,"TransactionIndex" = data_train$TransactionIndex)
# pairs(data_trial_normalized)



#Lets do the modelling


#Lets do the normalized data first (aka sales Index and Transaction Index)

data_trial_normalized = data.frame("salesIndex" = data_train$salesIndex,"TransactionIndex" = data_train$TransactionIndex)

data_trial_normalized_sub = apply(data_trial_normalized,1,function(row) all(row !=0))
data_trial_normalized = data_trial_normalized[data_trial_normalized_sub,]

salesIndex = data_trial_normalized$salesIndex
TransactionIndex = data_trial_normalized$TransactionIndex

lm.fit_norm=lm(salesIndex~TransactionIndex,data=data_trial_normalized)
summary(lm.fit_norm)

#Trying polynomial regression
fit.2 = lm(salesIndex~poly(TransactionIndex,2),data=data_trial_normalized)
fit.3 = lm(salesIndex~poly(TransactionIndex,3),data=data_trial_normalized)
fit.4 = lm(salesIndex~poly(TransactionIndex,4),data=data_trial_normalized)
fit.5 = lm(salesIndex~poly(TransactionIndex,5),data=data_trial_normalized)
anova(lm.fit_norm,fit.2,fit.3,fit.4,fit.5)


#lm.fit_norm_poly_T_3=lm(salesIndex~poly(TransactionIndex,5),data=data_trial_normalized)
# lm.fit_norm_poly_3=lm(salesIndex~poly(45.874*TransactionIndex+1.469,2,raw=TRUE),data=data_trial_normalized)


for(i in 1:2) {
Trans <- BoxCox.lambda(data_trial_normalized[,i])
data_trial_normalized[,i] <- BoxCox(data_trial_normalized[,i],Trans)
}

lm.fit_norm_new=lm(salesIndex~TransactionIndex,data=data_trial_normalized)
summary(lm.fit_norm_new)


#prediction now on the test data

#Without boxcox

#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)
final.df <- data.frame(predict(lm.fit_norm,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab

print(subset(final.df,tab=="MCD")) #value prediction with a sample ticker

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time. 
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))


#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)
final.df <- data.frame(predict(fit.2,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab

print(subset(final.df,tab=="MCD")) #value prediction with a sample ticker

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time. 
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)
final.df <- data.frame(predict(fit.3,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab

print(subset(final.df,tab=="MCD")) #value prediction with a sample ticker

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time. 
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)
final.df <- data.frame(predict(fit.4,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab

print(subset(final.df,tab=="MCD")) #value prediction with a sample ticker

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time. 
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)
final.df <- data.frame(predict(fit.5,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab

print(subset(final.df,tab=="MCD")) #value prediction with a sample ticker

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time. 
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#With Boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,TransactionIndex=tabData$TransactionIndex)

Trans <- BoxCox.lambda(data_trial_normalized[,2])
tabData.df[,2] <- BoxCox(tabData.df[,2],Trans)
final.df <- data.frame(predict(lm.fit_norm_new,tabData.df,interval="predict",level=0.95))
final.df$salesIndex = tabData$salesIndex
Trans <- BoxCox.lambda(data_trial_normalized[,1])
final.df$salesIndex <- BoxCox(final.df$salesIndex,Trans)
final.df$TestValueInRange <- ifelse((final.df$salesIndex >= final.df$lwr) & (final.df$salesIndex <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))




# non-normalized

data_trial = data.frame("index_amount" = data_train$index_amount,"index_transaction" = 
                                     data_train$index_transaction)

data_trial_sub = apply(data_trial,1,function(row) all(row !=0))
data_trial = data_trial[data_trial_sub,]

index_amount = data_trial$index_amount
index_transaction = data_trial$index_transaction

lm.fit_non_norm=lm(index_amount~index_transaction,data=data_trial)
summary(lm.fit_non_norm)

#Trying polynomial regression
fit.2 = lm(index_amount~poly(index_transaction,2),data=data_trial)
fit.3 = lm(index_amount~poly(index_transaction,3),data=data_trial)
fit.4 = lm(index_amount~poly(index_transaction,4),data=data_trial)
fit.5 = lm(index_amount~poly(index_transaction,5),data=data_trial)
anova(lm.fit_non_norm,fit.2,fit.3,fit.4,fit.5)


#Without boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,index_transaction=tabData$index_transaction)
final.df <- data.frame(predict(lm.fit_non_norm,tabData.df,interval="predict",level=0.95))
final.df$index_amount = tabData$index_amount
final.df$TestValueInRange <- ifelse((final.df$index_amount >= final.df$lwr) & (final.df$index_amount <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time.
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))




#Without boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,index_transaction=tabData$index_transaction)
final.df <- data.frame(predict(fit.2,tabData.df,interval="predict",level=0.95))
final.df$index_amount = tabData$index_amount
final.df$TestValueInRange <- ifelse((final.df$index_amount >= final.df$lwr) & (final.df$index_amount <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time.
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#Without boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,index_transaction=tabData$index_transaction)
final.df <- data.frame(predict(fit.3,tabData.df,interval="predict",level=0.95))
final.df$index_amount = tabData$index_amount
final.df$TestValueInRange <- ifelse((final.df$index_amount >= final.df$lwr) & (final.df$index_amount <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time.
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#Without boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,index_transaction=tabData$index_transaction)
final.df <- data.frame(predict(fit.4,tabData.df,interval="predict",level=0.95))
final.df$index_amount = tabData$index_amount
final.df$TestValueInRange <- ifelse((final.df$index_amount >= final.df$lwr) & (final.df$index_amount <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time.
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))

#Without boxcox
#tabData = subset(data_test_sub,tab == "ANF" | tab == "AMZN" | tab == "DNKN" | tab == "CMG" | tab == "MCD" | tab == "SBUX" | tab == "YUM-TB" )
tabData = data_test_sub
tabData.df <-  data.frame(tab=tabData$tab,index_transaction=tabData$index_transaction)
final.df <- data.frame(predict(fit.5,tabData.df,interval="predict",level=0.95))
final.df$index_amount = tabData$index_amount
final.df$TestValueInRange <- ifelse((final.df$index_amount >= final.df$lwr) & (final.df$index_amount <= final.df$upr),"YES","NO")
final.df$tab = tabData$tab
#print(final.df)

print(subset(final.df,tab=="MCD"))

#The line Below shows the accuracy of the model on the test data set. The result was that the 
# 95% prediction interval given by the model contained the true value 93% of the time.
print(sum(final.df$TestValueInRange=="YES")/(sum(final.df$TestValueInRange=="YES")+
                                               sum(final.df$TestValueInRange=="NO")))









#lm.fit=lm(data_trial$index_amount~data_trial$index_transaction,data=data_trial)


#> plot(data_trial$index_transaction,data_trial$index_amount)
#> abline(lm.fit)
#> abline(lm.fit,lwd=3,col="red")

#trying to handle outliers using studentized residuals
#> data_trial_sub=subset(data_trial,rstudent(lm.fit)<=10)
#> lm.fit_bk=lm(data_trial_sub$index_amount~data_trial_sub$index_transaction,data=data_trial_sub)
#> summary(lm.fit_bk)
#removing some possible outliers, didnt really have too much impact on the adjusted R2




#next predict the percent change in sales vs percent change in transaction (both normalized and non-normalized)

#also predict based on data for individual companies, and see how the results from that compare with the
#results from the generic model


#Writing

#first talk about how you have 2 R files; one containing how you went about the research and
#the other contianing the final finished program

#Talk about how you saved the training and testing data set files

#for normalized and non-normalized data, do all of the below

#Talk about how you cleaned the data initially (both training and testing)

#Talk about how you explored for outliers and leverage stat points, but removing some, didnt have
#too much of a material impact on the model

#Talk about which model you fit, what was the results from it, what were the coefficients,confint of the
#coefficients, adjusted R2 and performance on the testing data set

#Compare the linear model to results from polynomial regression. Argue that it makes sense to keep the model linear
#(give evidence of the residual plot as well as the only slight improvement through poly regression)

#for tickers such as MCD,AMZN and find some others, try to fit individual models. Talk about how the prediction
#for these tickers has improved by fitting them their individual models (explain how if we fit an individual model
#for each ticker, we would get better results, and probably increase the correctness for predicting the interval
#containing the testing data).