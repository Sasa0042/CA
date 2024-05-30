
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  ##工作路径改为当前运行文件所在路径
getwd()

data <- read.csv('olist_orders_dataset.csv')
data <- data[, c(1,3,4)]
#data <- distinct(data)


# 订单量 ---------------------------------------------------------------------

data$order_purchase_timestamp <- as.Date(data$order_purchase_timestamp) 

data$year <- format( data$order_purchase_timestamp," %Y ")
data$month <- format( data$order_purchase_timestamp," %m ")

table(data$order_status)


### 1: "delivered"

data_1 <- data[data$order_status == 'delivered', ]

freq <- table(data_1$year,data_1$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_1.csv",sep=",")


### 2: "shipped"

data_2 <- data[data$order_status == "shipped", ]

freq <- table(data_2$year,data_2$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_2.csv",sep=",")

### 3: "canceled"

data_3 <- data[data$order_status == "canceled", ]

freq <- table(data_3$year,data_3$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_3.csv",sep=",")

### 4: "unavailable"

data_4 <- data[data$order_status == "unavailable", ]

freq <- table(data_4$year,data_4$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_4.csv",sep=",")

### 5: "invoiced"

data_5 <- data[data$order_status == "invoiced", ]

freq <- table(data_5$year,data_5$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_5.csv",sep=",")

### 6: "processing"

data_6 <- data[data$order_status == "processing", ]

freq <- table(data_6$year,data_6$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"orders_6.csv",sep=",")



# 销售额 ---------------------------------------------------------------------

data_payment <- read.csv('olist_order_payments_dataset.csv')
data_payment <- data_payment[,c(1,3,5)]

library(dplyr)

data1 <- right_join(x = data, y = data_payment, "order_id")
## only considered delivered order
data1 <- data1[ data1$order_status == 'delivered', ]

table(data1$payment_type)


### 1: "boleto"

data_1 <- data1[data1$payment_type == "boleto", ]

freq <- aggregate(data_1$payment_value, list(data_1$year, data_1$month), sum)
freq <- as.data.frame(freq)
freq$date <- paste(freq$Group.1, freq$Group.2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"sales_1.csv",sep=",")

### 2: "credit_card"

data_2 <- data1[data1$payment_type == "credit_card", ]

freq <- aggregate(data_2$payment_value, list(data_2$year, data_2$month), sum)
freq <- as.data.frame(freq)
freq$date <- paste(freq$Group.1, freq$Group.2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"sales_2.csv",sep=",")

### 3: "debit_card"

data_3 <- data1[data1$payment_type == "debit_card", ]

freq <- aggregate(data_3$payment_value, list(data_3$year, data_3$month), sum)
freq <- as.data.frame(freq)
freq$date <- paste(freq$Group.1, freq$Group.2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"sales_3.csv",sep=",")

### 4: "voucher"

data_4 <- data1[data1$payment_type == "voucher", ]

freq <- aggregate(data_4$payment_value, list(data_4$year, data_4$month), sum)
freq <- as.data.frame(freq)
freq$date <- paste(freq$Group.1, freq$Group.2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"sales_4.csv",sep=",")



# 分期付款 --------------------------------------------------------------------

data_payment <- read.csv('olist_order_payments_dataset.csv')
data_payment <- data_payment[,c(1,4)]

data_payment <- data_payment[ data_payment$payment_installments != 0, ]
data_payment[ data_payment$payment_installments > 10, ] <- 11


freq <- table(data_payment$payment_installments)
freq <- as.data.frame(freq)
freq <- freq[order(freq$Var1), ]


library(ggplot2)
library(ggforce)
ggplot()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title=element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank())+#去除没用的ggplot背景，坐标轴
  xlab("")+ylab('')+#添加颜色
  scale_fill_manual(values = c('#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', 
                               '#D6E7A3', '#57C3F3', '#476D87',
                               '#E59CC4', '#AB3282', '#23452F', '#BD956A'))+
  geom_arc_bar(data=freq,
               stat = "pie",
               aes(x0=0,y0=0,r0=1,r=2,
                   amount=Freq,fill=Var1)
  )


# 活跃用户数 -------------------------------------------------------------------

data <- read.csv('olist_orders_dataset.csv')
data <- data[, c(1,2,3,4)]

data$order_purchase_timestamp <- as.Date(data$order_purchase_timestamp) 
data$year <- format( data$order_purchase_timestamp," %Y ")
data$month <- format( data$order_purchase_timestamp," %m ")

## only considered delivered order
data1 <- data[data$order_status == 'delivered', ]

## 消费者：

data_customers <- read.csv('olist_customers_dataset.csv')
data_customers <- data_customers[, c(1,2)]


data2 <- left_join(x = data1, y = data_customers, "customer_id")
data2 <- data2[, c(5,6,7)]

data2 <- distinct(data2, .keep_all= TRUE)


freq <- table(data2$year,data2$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"customers.csv",sep=",")

## 卖家：

data_sellers <- read.csv('olist_order_items_dataset.csv')
data_sellers <- data_sellers[, c(1,4)]
data_sellers <- distinct(data_sellers, .keep_all= TRUE)

#### one order, two seller

data2 <- left_join(x = data1, y = data_sellers, "order_id")
data2 <- data2[, c(5,6,7)]


data2 <- distinct(data2, .keep_all= TRUE)


freq <- table(data2$year,data2$month)
freq <- as.data.frame(freq)

freq$date <- paste(freq$Var1, freq$Var2, sep = '/')
freq <- freq[, c(4,3)]

write.table(freq,"sellers.csv",sep=",")


# time series1 ------------------------------------------------------------

### 每天订单量

data1 <- data[data$order_status == 'delivered', ]
data1$order_purchase_timestamp <- as.POSIXct(data1$order_purchase_timestamp, 
                                             format = "%Y/%m/%d")

### 2017年 1月9日到10月底

data1$Y <- format( data1$order_purchase_timestamp,"%Y")
data1 <- data1[data1$Y != '2016', ]
data1 <- data1[data1$Y != '2018', ]

data1$YM <- format( data1$order_purchase_timestamp,"%Y/%m")
data1 <- data1[data1$YM != '2017/11', ]
data1 <- data1[data1$YM != '2017/12', ]

data1 <- data1[data1$order_purchase_timestamp != '2017/01/05', ]
data1 <- data1[data1$order_purchase_timestamp != '2017/01/06', ]
data1 <- data1[data1$order_purchase_timestamp != '2017/01/07', ]
data1 <- data1[data1$order_purchase_timestamp != '2017/01/08', ]

## 2017年1月9号是周一


freq <- table(data1$order_purchase_timestamp)
freq <- as.data.frame(freq)
freq <- freq[order(freq$Var1), ]
freq$Var1 <- as.Date(freq$Var1, format = "%Y-%m-%d")


#### ts1

library(TSstudio)
library(feasts)
library(tsibble)
library(tidyverse)


ts_tsbl<-as_tsibble(freq, index = Var1)
ts_tsbl %>% ts_plot()  ## trend


ts <- ts(freq$Freq,frequency =7)
acf(ts) # 有周期性

ts <- ts(freq$Freq,frequency =28)
# Box plot by day
my_colors <- rainbow(28)
boxplot(split(ts, cycle(ts)),
        xlab = "Day", ylab = "Number of Orders",
        col = my_colors, 
        border = "black", 
        main = "Dayly Orders Counts by Day",
        #names = month.abb,  # Use abbreviated month names as labels
        outline = FALSE)  # Remove outliers

ts <- ts(freq$Freq,frequency =7)
ts_heatmap(ts,color="Reds") 


# time series2--------------------------------------------------------------------

## Black Friday: '2017-11-24' - '2017-11-30'
## 每小时订单量

data1 <- data[data$order_status == 'delivered', ]
data1$order_purchase_timestamp <- as.POSIXct(data1$order_purchase_timestamp, 
                                             format = "%Y/%m/%d %H")

### 2017年 11月24日-12月

data1$YM <- format( data1$order_purchase_timestamp,"%Y/%m")
data1 <- data1[ (data1$YM == '2017/11') | (data1$YM == '2017/12') , ]

freq <- table(data1$order_purchase_timestamp)
freq <- as.data.frame(freq)
freq <- freq[order(freq$Var1), ]

data1 <- data1[ order(data1$order_purchase_timestamp),]
temp <- distinct(data1, order_purchase_timestamp)
freq$date <- temp$order_purchase_timestamp


library (lubridate)
date <- seq(ymd(' 2017-11-24'), ymd(' 2017-12-31'), by='day ')
time <- c('00:00:00','01:00:00', '02:00:00','03:00:00', '04:00:00','05:00:00', 
          '06:00:00','07:00:00', '08:00:00','09:00:00', '10:00:00','11:00:00', 
          '12:00:00','13:00:00', '14:00:00','15:00:00', '16:00:00','17:00:00', 
          '18:00:00','19:00:00', '20:00:00','21:00:00', '22:00:00','23:00:00')
date_index <- 0

for (i in 1:38){
  c <- rep(0,24)
  for (j in 1:24) {
    c[j] <- paste(date[i], time[j], sep = ' ')
  }
  date_index <- c(date_index, c)
}
date_index <- as.data.frame(date_index[-1])
colnames(date_index) <- 'date'
date_index$date <- as.POSIXct(date_index$date, 
                              format = "%Y-%m-%d %H")


library(dplyr)
data2 <- left_join(x = date_index, y = freq, "date")
data2 <- data2[, c(1,3)]
data2[is.na(data2)] <- 0 


##### ts2

ts_tsbl<-as_tsibble(data2, index = date)
ts_tsbl %>% ts_plot()


ts <- ts(data2$Freq, frequency = 24)
acf(ts) # 有周期性


ts <- ts(data2$Freq, frequency = 168)
# Box plot by hour
my_colors <- rainbow(168)
boxplot(split(ts, cycle(ts)),
        xlab = "Day", ylab = "Number of Orders",
        col = my_colors, 
        border = "black", 
        main = "Orders Counts per hour in a week",
        #names = month.abb,  # Use abbreviated month names as labels
        outline = FALSE)  # Remove outliers


ts <- ts(data2$Freq, frequency = 24)
# Box plot by hour
my_colors <- rainbow(24)
boxplot(split(ts, cycle(ts)),
        xlab = "Day", ylab = "Number of Orders",
        col = my_colors, 
        border = "black", 
        main = "Orders Counts per hour in a day",
        #names = month.abb,  # Use abbreviated month names as labels
        outline = FALSE)  # Remove outliers


# time series3 ------------------------------------------------------------

## 2018/01 - 2018/08 每天订单量

data1 <- data[data$order_status == 'delivered', ]
data1$order_purchase_timestamp <- as.POSIXct(data1$order_purchase_timestamp,
                                                          format = "%Y/%m/%d")

data1$Y <- format( data1$order_purchase_timestamp,"%Y")
data1 <- data1[data1$Y != '2016', ]
data1 <- data1[data1$Y != '2017', ]

## 2018年1月1号是周一
data1 <- data1[data1$order_purchase_timestamp != '2018/08/27', ]
data1 <- data1[data1$order_purchase_timestamp != '2018/08/28', ]
data1 <- data1[data1$order_purchase_timestamp != '2018/08/29', ]

freq <- table(data1$order_purchase_timestamp)
freq <- as.data.frame(freq)
freq <- freq[order(freq$Var1), ]
freq$Var1 <- as.Date(freq$Var1, format = "%Y-%m-%d")

##### ts3

ts_tsbl<-as_tsibble(freq, index = Var1)
ts_tsbl %>% ts_plot()

ts <- ts(freq$Freq,frequency =7)
acf(ts) # 有周期性


dts <- diff(ts, 7)
plot(dts)

library(tseries)
adf.test(dts)


library(forecast)
fit = auto.arima(ts, d = 0, D = 1)
summary(fit)

plot(fit$residuals)
qqnorm(fit$residuals)
qqline(fit$residuals)

f<-forecast(fit, level=c(95), h=1*7)
plot(f)


# bsts --------------------------------------------------------------------

## Bayesian Structural Time Series 可以handle节假日

data1 <- data[data$order_status == 'delivered', ]
data1$order_purchase_timestamp <- as.POSIXct(data1$order_purchase_timestamp, 
                                             format = "%Y/%m/%d")

data1$Y <- format( data1$order_purchase_timestamp,"%Y")
data1 <- data1[data1$Y != '2016', ]
data1 <- data1[data1$Y != '2017', ]

data1 <- data1[data1$YM != '2018/08', ]

freq <- table(data1$order_purchase_timestamp)
freq <- as.data.frame(freq)
freq <- freq[order(freq$Var1), ]
freq$Var1 <- as.Date(freq$Var1, format = "%Y-%m-%d")

### forecast

library(tidyverse, quietly = TRUE)
library(bsts, quietly = TRUE)  

ts <- zoo(freq$Freq, freq$Var1)

model_components <- list()
# Add a Trend Component
model_components <- AddLocalLinearTrend(model_components, y = ts)
# Add a Seasonal Component
model_components <- AddSeasonal(model_components, y = ts, 
                                nseasons  = 52)

Black_Friday <- FixedDateHoliday('Black Friday',
                 month = "November",
                 day=24,
                 days.before = 0,
                 days.after = 6)

model_components <- AddRandomWalkHoliday(model_components, y = ts, Black_Friday)


fit <- bsts(ts, model_components, niter = 1000, seed = 529)
plot(fit, "comp")
plot(model_components[[1]], fit)
plot(model_components[[2]], fit)
plot(model_components[[3]], fit)


burnin <- 500 # Throw away first 500 
pred <- predict(fit, horizon = 10, burn = burnin, quantiles = c(.05, .95))
plot(pred)

plot(residuals(fit, burn=500, mean.only = T))


# 卖家到顾客流动 -----------------------------------------------------------------

data <- read.csv('data_model.csv')
data <- data[, c(2,21:24)]

data1 <- data[, c(5,3)]

freq <- table(data1)
freq <- as.data.frame(freq)

write.table(freq,"state.csv",sep=",")


# boxplot -----------------------------------------------------------------

data1 <- data[data$order_status == 'delivered', ]
data1$order_purchase_timestamp <- as.POSIXct(data1$order_purchase_timestamp, 
                                             format = "%Y/%m/%d")

freq <- table(data1$order_purchase_timestamp)
freq <- as.data.frame(freq)

library(tidyverse)
library(hrbrthemes)
library(viridis)


freq %>%
  ggplot( aes(x=temp, y=Freq, fill=temp)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")





