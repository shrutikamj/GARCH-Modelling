#GARCH MODELING FOR VOLATILITY 
install.packages("PerformanceAnalytics")
install.packages("TSstudio")
library(PerformanceAnalytics) #Analysing the performance and risk of stock data
library(quantmod)#financial modelling
library(rugarch)#GARCH analysis
library(tseries)#timeseires analysis
library(TSstudio) #plotting t vs yt

#Retrieving the data using quantmod package -
getSymbols("AAPL" ,SRC="Yahoo", from = "2008-01-01",
           to = "2021-03-12")#retrieving data of Apple
#viewing the first and last elements of the data -
head(AAPL)
tail(AAPL)
#Plotting -
#candle stick format-
chartSeries(AAPL["2019-12"])

#series - 
chartSeries(AAPL)
#many fluctuations
#Stock prices gone up and then pulled back.
#difficult to estimate using std time series. 

##Daily returns - of closed prices - 
#returns are considered to estimate GARCH model
return <- CalculateReturns(AAPL$AAPL.Close)
View(return)
#removing NA
return <- return[-1]

#plottig histogram of returns -
hist(return) #symmetric distribution returns.
#daily returns on avg are 0 
#some days of high and low returns-

#plotting Normal Dist- 
chart.Histogram(return,
                methods = c('add.density', 'add.normal'))

##plotting series - 
chartSeries(return, theme = 'white')
#Stationary ts. no trend/seasonal components. But has volatility. 
#2008 GFC - financial volatility. 


#plotting the monthly and annual returns on the apple stock - 
#22 trading days in a month thus the width is 22. scale = total days 

chart.RollingPerformance(R= return["2008::2021"],
                         width = 22, 
                         scale = 252,
                         main = "Apple's monthly returns")

#we see volatility here. And hence we can say the there is no constant variance.
#observing annual graph -
chart.RollingPerformance(R= return["2008::2021"],
                         width = 252, 
                         scale = 252,
                         main = "Apple's yearly returns")
#returns are not constant and are volatility. 
#we see that the variance is not constant and a normal tseries can't be performed.

#converting to TS object and plot
values <- AAPL
AAPL_timeseries <- ts(values,start=c(2008,1),end=c(2021,3),frequency=365)

ts_info(AAPL_timeseries)
ts_plot(AAPL_timeseries)
ts_plot(AAPL_timeseries,
        title = "Apple Stock Prices",
        Xtitle = "Time(T)", Ytitle = "volume(Yt)",
        slider = T)

##GARCH ESTIMATES - 
#1. Standard GARCH model - SGarch with a constant mean
ugarchspec()#to check default garch spec.
#since it is a constant mean thus ARMA=0,0
#we specify the model further.
specification <- ugarchspec(mean.model = list(armaOrder = c(0,0)))
specification   #checking the specification       

#Modeling - 
m1 <- ugarchfit(spec = specification, data = return, out.sample = 0)
m1
#interpretation -
#4 parameters - mu,omega,beta,alfa.
#p is very low thus stat significant. 


#plotting the GARCH model -
plot(m1)
5
6
4
8
10
11
12
0
#Interpreation on ACF, Autocorrelation, Impact curve  -
#1.ACF- all values are above the red line (horizontal) which means there is significant presence of autocorrelation
#Same for both squared and absolute values.
#2.Histogram of residuals or errors -  it is seen that the histogram is little different as compared to earlier.
#3.Autocorrelation for standardized and squared residuals -
#no autocorrelation observed since the values are above red line.
#4.	Impact curve - This indicate that whether any news have positive or negative impact on returns.
#From the plot, same weightage is observed. 


###model forecast- 
forecast <- ugarchforecast(fitORspec = m1, 
                          n.ahead = 20)
                          
plot(forecast)

1
0
#we see constant forecast predictions for next 20 days. 
#plotting fitted values of the forecast just to confirm
plot(fitted(forecast))
0
#constant predictions 

#plotting variability -
#variability measured using sigma. 
plot(sigma(forecast))
#the volatility is expected to decrease in the next 20 days. 


#----------------------------------------------------------------------
#VARIANT OF GARCH MODEL. 

#Portfolio allocation -
#annualised volatility -
#252 trading days in a yr
v <- sqrt(252) * sigma(m1)
#weights assigned to risky assets-
#assuming 5 % target -
w <- 0.05/v

#plot -
plot(merge(v,w),
     multi.panel = T)

#top for volatility and bottom for w
#when volatility is lowed, weightes assigned to risky assets is higher
#v high in 2008 and 2020, weight assigned to risky asset is low.
#we see weights assigned to risky assets 

##variants of Garch model and Comparing which is better for simulation -

#2. GARCH with skewed Student T distribution - SSTD -
specification2 <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
specification2
m2 <- ugarchfit(spec = specification2,
                data = return)
m2
#values of goodness of fit over 0.05. 
#can reject H0 and values are better.
plot(m2)
4
5
6
8
12

#we see the tails are much alligned for sstd.
#the IC has improved. 

#skew - 0.05. +vely skewed.
#when skew = 1 symetric skewd. 

#further using more variants like GJR Garch and etc the model can be made much better
#and can be used for simulation purpose. 