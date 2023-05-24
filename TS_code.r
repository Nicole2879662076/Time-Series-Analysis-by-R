# 导入CSV格式数据集（绝对路径）
train <- read.csv("C:/Users/Administrator/Desktop/SSE_index.csv")

# 将日期列转换为时间序列对象
dates <- as.Date(train$Date)
close <- train$close
time_series <- ts(close, start = min(dates), frequency = 1)

# 绘制时间序列图
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(dates, time_series, ylab = "Price", xlab = "Date", type = "o")


#--------------------------------------------------------------------
# 对数据进行 ln 转换
ln_transformed <- log(time_series)

# 绘制 ln 转换后的时间序列图
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(dates, ln_transformed, ylab = "ln(Price)", xlab = "Date", type = "l")

#--------------------------------------------------------------------
# Box-Cox
library(forecast)

# 将时间序列数据转换为向量
time_series_values <- as.vector(time_series)

# 计算Box-Cox变换的lambda值
lambda <- BoxCox.lambda(time_series_values, method = c("guerrero", "loglik"), lower = -2, upper = 2)
cat("best_lambda = ", lambda)

# 进行Box-Cox转换
transformed_data <- BoxCox(time_series_values, lambda)

# 创建Box-Cox转换后的时间序列对象
boxcox_time_series <- ts(transformed_data, start = start(time_series), frequency = frequency(time_series))

# 绘制Box-Cox转换后的时间序列图
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(dates, boxcox_time_series, ylab = "Box-Cox(Price)", xlab = "Date", type = "l")

#--------------------------------------------------------------------
# 对 ln_transformed 进行一阶差分
diff_ln_transformed <- diff(ln_transformed)

# 绘制一阶差分后的时间序列图
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(dates[-1], diff_ln_transformed, ylab = "First Difference of ln(Price)", xlab = "Date", type = "l")

# 对 box 进行一阶差分
diff_boxcox <- diff(boxcox_time_series)

# 绘制一阶差分后的时间序列图
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(dates[-1], diff_boxcox, ylab = "First Difference of Box-Cox(Price)", xlab = "Date", type = "l")

#--------------------------------------------------------------------
# ADF
library(tseries)
adf.test(diff_ln_transformed)
adf.test(diff_boxcox)

#--------------------------------------------------------------------
# ACF
par(mar = c(2, 2, 2, 2))
acf_result <- acf(diff_ln_transformed)
#acf_result$acf <- acf_result$acf[-1]
#acf_result$lag <- acf_result$lag[-1]

# 绘制 ACF 图像
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(acf_result, main = "ACF Plot for diff_ln_transformed")

# PACF 
par(mar = c(2, 2, 2, 2))
pacf_result <- pacf(diff_ln_transformed)

# 绘制 PACF 图像
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(pacf_result, main = "PACF Plot for diff_ln_transformed")

# EACF
library(TSA)
eacf_result <- eacf(diff_ln_transformed, ar.max = 7, ma.max = 13)

#--------------------------------------------------------------------
# BIC
set.seed(5000)
test <- arima.sim(model = list(ar = c(rep(0, 11), 0.8), ma = c(rep(0, 11), 0.7)), n = length(diff_ln_transformed))
res <- armasubsets(y = diff_ln_transformed, nar = 14, nma = 14, y.name = 'diff', ar.method = 'ols')
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(res)


#--------------------------------------------------------------------
model_1 <-stats::arima(diff_ln_transformed,order=c(0,0,9),fixed=c(0,0,0,0,0,0,0,0,NA,NA),include.mean = TRUE,method='ML')
model_1

model_2 <-stats::arima(diff_ln_transformed,order=c(0,0,9),fixed=c(0,0,0,0,0,0,0,0,NA),include.mean = FALSE,method='ML')
model_2 #MA(9)

model_3 <-stats::arima(diff_ln_transformed,order=c(9,0,8),fixed=c(0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,NA,NA),include.mean = TRUE,method='ML')
model_3

model_4 <-stats::arima(diff_ln_transformed,order=c(9,0,8),fixed=c(0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,NA),include.mean = FALSE,method='ML')
model_4 #AR(9)MA(8)

model_5 <-stats::arima(diff_ln_transformed,order=c(9,0,0),fixed=c(0,0,0,0,0,0,0,0,NA,NA),include.mean = TRUE,method='ML')
model_5

model_6 <-stats::arima(diff_ln_transformed,order=c(9,0,0),fixed=c(0,0,0,0,0,0,0,0,NA),include.mean = FALSE,method='ML')
model_6 #AR(9)

model_7 <-stats::arima(diff_ln_transformed,order=c(9,0,8),fixed=c(0,0,0,0,0,0,0,0,NA,NA,0,0,0,0,0,0,NA,NA),include.mean = TRUE,method='ML')
model_7

model_8 <-stats::arima(diff_ln_transformed,order=c(9,0,8),fixed=c(0,0,0,0,0,0,0,0,NA,NA,0,0,0,0,0,0,NA),include.mean = FALSE,method='ML')
model_8 # Drop MA(1) -> AR(9)MA(8)

#--------------------------------------------------------------------
# Standardized Residuals
win.graph(width = 7.8, height = 4, pointsize = 8) #MA9
plot(dates[-1], rstandard(model_2),xlab='Time',ylab='Standardized Residuals',main="Residuals versus Time",type='o')

win.graph(width = 7.8, height = 4, pointsize = 8) #AR9
plot(dates[-1], rstandard(model_6),xlab='Time',ylab='Standardized Residuals',main="Residuals versus Time",type='o')

win.graph(width = 7.8, height = 4, pointsize = 8) #AR9MA8
plot(dates[-1], rstandard(model_4),xlab='Time',ylab='Standardized Residuals',main="Residuals versus Time",type='o')

#--------------------------------------------------------------------
# Residuals Hist
win.graph(width = 5, height = 4, pointsize = 8)
hist(residuals(model_2),xlab='Standardized Residuals',ylab='Frequency',main='Histogram of Standardized Residuals')

win.graph(width = 5, height = 4, pointsize = 8)
hist(residuals(model_6),xlab='Standardized Residuals',ylab='Frequency',main='Histogram of Standardized Residuals')

win.graph(width = 5, height = 4, pointsize = 8)
hist(residuals(model_4),xlab='Standardized Residuals',ylab='Frequency',main='Histogram of Standardized Residuals')

#--------------------------------------------------------------------
# Residuals QQplot
win.graph(width = 4.5, height = 4, pointsize = 8)
qqnorm(residuals(model_2),xlab='Theortical Quantiles',ylab='Sample Quantiles',main='Q-Q Plot:Standardized Residuals')
qqline(residuals(model_2))

win.graph(width = 4.5, height = 4, pointsize = 8)
qqnorm(residuals(model_6),xlab='Theortical Quantiles',ylab='Sample Quantiles',main='Q-Q Plot:Standardized Residuals')
qqline(residuals(model_6))

win.graph(width = 4.5, height = 4, pointsize = 8)
qqnorm(residuals(model_4),xlab='Theortical Quantiles',ylab='Sample Quantiles',main='Q-Q Plot:Standardized Residuals')
qqline(residuals(model_4))

#--------------------------------------------------------------------
# SW test
shapiro.test(residuals(model_2))
shapiro.test(residuals(model_6))
shapiro.test(residuals(model_4))

# JB test
#library(rugarch)
#jbTest(residuals(model_2), title = NULL, description = NULL)
#jbTest(residuals(model_6), title = NULL, description = NULL)
#jbTest(residuals(model_4), title = NULL, description = NULL)

#--------------------------------------------------------------------
win.graph(width = 7.8, height = 6.5, pointsize = 8)
tsdiag(model_2,gof=15,omit.initial=F)

win.graph(width = 7.8, height = 6.5, pointsize = 8)
tsdiag(model_6,gof=15,omit.initial=F)

win.graph(width = 7.8, height = 6.5, pointsize = 8)
tsdiag(model_4,gof=15,omit.initial=F)

#--------------------------------------------------------------------
# Model     AIC
# MA9       -926.59
# AR9       -924.56
# AR9MA8    -926.15 --> best

#--------------------------------------------------------------------
library(forecast)
# 导入测试集数据
test <- read.csv("C:/Users/Administrator/Desktop/SSE_test.csv")
dates_l <- as.Date(test$Date)
close_l <- test$close
test_data <- ts(close_l, start = 19315, frequency = 1)



diff_ln_transformed1<-diff(ts(c(ln_transformed,log(test_data)), start =  min(dates), frequency = 1))

# 进行预测
forecast_result <- forecast::forecast(model_4, h = 20)
forecast_result

# 绘制预测结果和测试集数据的图形
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(forecast_result, main = "Forecast vs. Test Data", xaxt = "n")
lines(diff(log(test_data)), col = "red", type = "b")
axis(1, at = dates_l)
legend("topleft", legend = c("Forecast", "Test Data"), col = c("blue", "red"), lty = 1, pch = 1)


accuracy(forecast_result$mean,diff_ln_transformed1[(length(diff_ln_transformed1-19)):length(diff_ln_transformed1)])


# 设置滑动窗口大小为20天
window_size <- 20
# 创建一个空的预测结果向量
forecast_vec <- c()

# 使用滑动窗口进行预测
for (i in 1:(length(diff_ln_transformed1)-window_size+1)) {
  # 从训练数据中取出当前滑动窗口的数据
  current_window <- diff_ln_transformed1[i:(i+window_size-1)]
  
  # 训练ARIMA模型
  model <- stats::arima(current_window, order=c(9,0,8), fixed=c(0,0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,NA), include.mean = FALSE, method='ML')
  
  # 预测未来1天的数据
  forecast_result <- forecast::forecast(model, h = 1)
  
  # 将预测结果添加到预测结果向量中
  forecast_vec <- c(forecast_vec,  forecast_result$mean)
}
# 绘制预测结果和测试集数据的图形
win.graph(width = 7.8, height = 4, pointsize = 8)
plot(diff_ln_transformed1, main = "Forecast vs. Test Data", xaxt = "n",col="blue")
lines(ts(forecast_vec[(length(forecast_vec)-19):length(forecast_vec)],end = 19334,frequency = 1), col = "red", type = "b")
axis(1, at = dates_l)
legend("topleft", legend = c("Forecast", "Test Data"), col = c("red", "blue"), lty = 1, pch = 1)


accuracy(forecast_vec[(length(forecast_vec)-19):length(forecast_vec)],diff_ln_transformed1[(length(diff_ln_transformed1-19)):length(diff_ln_transformed1)])
