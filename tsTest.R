#t	Sales
#1	1000
#2	3000
#3	2000
#4	2000
#5	4000
#6	3000
#7	3000
#8	5000
#9	4000

#data
library('forecast')
t = c(1,2,3,4,5,6,7,8,9)
Sales = c(1000,3000,2000,2000,4000,3000,3000,5000,4000)
data = data.frame(t,Sales)

data_ts <- ts(data$Sales,start=1,frequency = 1)
#data = ts(data,start=1,frequency=1)
#print(data)
#plot(data)

temp <- ma(data$Sales,order=2,centre = TRUE)
frcast <- forecast(temp,h=3)
plot(frcast)
#moving_average = forecast(ma(data_trial$index_amount, order=1), h=5)

#plot.ts(data)
#decompose(data)

m_aa = auto.arima(data_ts)
f_aa = forecast(m_aa, h=3)
plot(f_aa)


m_tbats = tbats(data_ts)
f_tbats = forecast(m_tbats, h=3)
plot(f_tbats)
