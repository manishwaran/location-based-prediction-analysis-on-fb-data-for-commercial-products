
predict_arima<-function(data,ahead)
{
  data<-ts(data)
  (fit <- arima(log(data), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
  pred <- predict(fit, n.ahead = ahead)
  ts.plot(data,2.718^pred$pred, log = "y", lty = c(1,3))
}

predict_arima<-function(data,ahead)
{
  data<-ts(data)
  (fit <- arima(log(data), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))
  pred <- predict(fit, n.ahead = ahead)
  ts.plot(data,2.718^pred$pred, log = "y", lty = c(1,3))
}

