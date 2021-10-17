https://archive.ics.uci.edu/ml/datasets/Online+Retail
## Author Shafqaat Ahmad##########
rm(list = ls())


library(readxl)
library(caret)
library(ggplot2)
library(caret)
library(tseries)
library(lubridate)
library(forecast)


my_retail <- read_xlsx("Online_Retail.xlsx")

str (my_retail)
summary(my_retail)
colSums(is.na(my_retail))
my_retail<-na.omit(my_retail) # removing null
#Summary show we have quantities less than 1 even in negative and we have significant 
#oultliers in unit price and quantity
nrow(my_retail)
my_retail<-my_retail[my_retail$Quantity>0,]
#multiplying quantity*unit price
my_retail$total_price<-my_retail$Quantity*my_retail$UnitPrice
#removing outliers
outvals<-boxplot(my_retail$total_price)$out
my_retail<-my_retail[!(my_retail$total_price%in%outvals),]
str(outvals)

# Removing extra columns
my_retail_date<-my_retail[,c(5,9)]#5,9
# Aggregating on date/Day
my_retail_date$InvoiceDate <- as.POSIXct(my_retail$InvoiceDate, format = "%Y-%m-%d %H:%M:%S" )
#my_retail_date$InvoiceDate<-format(as.Date(my_retail_date$InvoiceDate),format="%Y-%m")
my_retail_date$InvoiceDate<-as.Date(my_retail_date$InvoiceDate)
my_retail_date<- aggregate(my_retail_date[,2], by = list(my_retail_date$InvoiceDate), "sum")

# Building time series

# Cleaning
clean_ts <- ts(my_retail_date$total_price)
my_retail_date$clean<-tsclean(clean_ts)

# Converting to time series
my_retail_timeseries <- ts(my_retail_date$clean,frequency = 30)
plot.ts(my_retail_timeseries)

# Decompose series default is additive
my_retail_timeseries_decomp = stl(my_retail_timeseries, s.window="periodic")
plot(my_retail_timeseries_decomp)


#Seasononal adjustment
my_retail_deseasonal <- seasadj(my_retail_timeseries_decomp)
# differencing the series
my_retail_diff = diff(my_retail_deseasonal, differences = 1)

adf.test(my_retail_diff, alternative ="stationary")
plot(my_retail_diff)



#par(mar=c(1))

acf(my_retail_diff, main="ACF of Retail Series")

pacf(my_retail_diff,main="PACF of Retail Series")


## Building First Model based on ACF and PACF
my_retail_medel1 <- arima(my_retail_deseasonal, order=c(2,1,6))   
#dev.off()
tsdisplay(residuals(my_retail_medel1), lag.max =30, main ='Fitting the Model')

# Building model 2 using auto arima

auto.arima(my_retail_timeseries, seasonal = TRUE)

my_retail_medel2 <- arima(my_retail_timeseries, order=c(1,1,2),seasonal = list(order = c(1, 0, 0), period=30))

tsdisplay(residuals(my_retail_medel2), lag.max =30, main ='Fitting the Model')



