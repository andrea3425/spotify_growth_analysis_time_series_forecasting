####SPOTIFY DATA ANALYSIS

###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(xts)
##############################################

#########################################
### ENVIRONMENT SET UP AND DATA LOADING
#########################################

SPOT_GREEN = "#1DB954"
SPOT_BLUE = "#040504"

spotify <- read.csv("data/ch3f.csv")
str(spotify)

##Set the date language to English
Sys.setlocale("LC_TIME", "en_US")

##create a variable 'date'
date <- as.Date(spotify$date, format = "%Y-%m-%d")
streams <- spotify$streams
listeners <- spotify$listeners

#########################################
### DEFINING CUSTOM PLOT
#########################################

myplot <- function(data, type="l", lwd = 3, color=SPOT_GREEN, alpha = 0.5, xlab = "Weeks", ylab = "Streams (in k)", title = "Streams", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(min(data),max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim)
  # Add colored area below the curve
  polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.2), border = NA)
  # Add grid
  grid(nx = 5, ny = 6, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.8)
  axis(2, las=1, cex.axis=0.8)
}

mylines <- function(data, lwd=3, col=SPOT_BLUE){
  lines(data/1000, lwd=lwd, col=col)
}

#########################################
### TURNING DAILY DATA INTO WEEKLY DATA
#########################################

starting_date <- as.Date("2021-01-01")
streams_xts <- xts(streams, order.by = starting_date + 0:(length(streams) - 1))
streams_per_week <- apply.weekly(streams_xts, FUN = sum)
streams_per_week <- streams_per_week[1:length(streams_per_week)-1]

#########################################
### EXPLORATORY DATA ANALYSIS AND LINEAR MODEL
#########################################

## Creating ts object
streams_per_week.ts <- ts(streams_per_week)
weeks <- as.Date(time(streams_per_week), format = "%Y-%m-%d")

## Plotting weekly data
myplot(streams_per_week.ts)

## Fitting Linear Model
fit_2 <- tslm(streams_per_week.ts~trend)
summary(fit_2)

## Plotting Linear Model
myplot(streams_per_week.ts)
mylines(fitted(fit_2))

## Residuals
res <- residuals(fit_2)
plot(res/1000, ylab="Residuals (in k)", xlab="weeks")
abline(h=0, lty="dashed", col=SPOT_GREEN)

## Durbin Watson test for Autocorrelation
dwtest(fit_2)

## Plotting Correlogram
acf(streams_per_week.ts)

#########################################
### BASS MODEL
#########################################

## Fitting Bass Model
bm_streams <- BM(streams_per_week, display = F)
summary(bm_streams)

## Making Prediction With Bass Model
pred_bm_streams<- predict(bm_streams, newx=c(1:200))
pred.inst_streams<- make.instantaneous(pred_bm_streams)

myplot(cumsum(streams_per_week.ts), lwd = 8, ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0,10000))
mylines(pred_bm_streams)

myplot(streams_per_week.ts, xlim=c(1,200))
mylines(pred.inst_streams)

## Plotting Residuals
bm_streams_res <- residuals(bm_streams)
plot(bm_streams_res/1000, type = "o", cex=0.5, pch=16, lty=1, xlab="weeks", ylab="residuals (in k)")
abline(h = 0, col=SPOT_GREEN, lty="dashed", lwd=2)

#########################################
### GENERALIZED BASS MODEL
#########################################

###GBM With Rectangular Shock
GBM_r1str<- GBM(streams_per_week, shock = "rett", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 45, 95, +0.1), display = F)
summary(GBM_r1str)

pred_GBMr1str<- predict(GBM_r1str, newx=c(1:200))
pred_GBMr1str.inst<- make.instantaneous(pred_GBMr1str)

myplot(cumsum(streams_per_week.ts), type= "l", lwd = 8, ylab="Cumulative Streams",  xlim=c(1,200), ylim=c(0, max(pred_GBMr1str) * 1.1/1000))
mylines(pred_GBMr1str)

myplot(streams_per_week.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMr1str.inst)*1.1/1000))
mylines(pred_GBMr1str.inst)

###GBM With Exponential Shock
GBM_e1str<- GBM(streams_per_week, shock = "exp", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 75, +0.1, +0.1), display = F)
summary(GBM_e1str)

pred_GBMe1str<- predict(GBM_e1str, newx=c(1:200))
pred_GBMe1str.inst<- make.instantaneous(pred_GBMe1str)

myplot(cumsum(streams_per_week.ts), lwd = 8,ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0, max(pred_GBMe1str) * 1.1/1000))
mylines(pred_GBMe1str)

myplot(streams_per_week.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMe1str.inst)*1.1/1000))
mylines(pred_GBMe1str.inst)

#########################################
### GUSEO-GUIDOLIN MODEL
#########################################

###GGM -> usually performs well with qc=0.001, pc=0.01
GGM_str<- GGM(streams_per_week, prelimestimates=c(9.744394e+06, 0.001, 0.01, 9.411032e-04, 3.444991e-02), display = F)
summary(GGM_str)

pred_GGM_str<- predict(GGM_str, newx=c(1:200))
pred_GGM_str.inst<- make.instantaneous(pred_GGM_str)

myplot(streams_per_week.ts, xlim=c(1,200))
mylines(pred_GGM_str.inst)

###Analysis of residuals
res_GGM_str<- residuals(GGM_str)
acf<- acf(residuals(GGM_str))

#########################################
### SIMPLE EXPONENTIAL SMOOTHING 
#########################################

SES_str <- ses(streams_per_week.ts, h=50)
plot(SES_str)

#########################################
### HOLT'S METHODS 
#########################################

HOLT_str <- holt(streams_per_week, h = 50)
plot(HOLT_str)

H_DAMPED_str <- holt(streams_per_week, h = 50, damped = T)
plot(H_DAMPED_str)

#H_WINTERS_str <- HoltWinters(streams_per_week)

#########################################
### ARIMA
#########################################

myplot(streams_per_week.ts)
# The time plot shows some non-stationarity
# To address the non-stationarity, we will take a first difference of the data
streams_diff <- diff(streams_per_week.ts)
myplot(streams_diff)
Acf(streams_diff)
Pacf(streams_diff)
# Acf suggests MA(1), i.e., ARIMA(0,1,1)
# Pacf suggests AR(1), i.e., ARIMA(1,1,0)
# Below we try to fit both model

############################

# ARIMA(1,1,0)
arima1.str <- Arima(streams_per_week.ts, order=c(1,1,0))
plot(streams_per_week.ts)
lines(fitted(arima1.str), col="red")
# Checking residuals
res.a1 <- residuals(arima1.str)
plot(res.a1)
Acf(res.a1)
# Predictions
pred.a1 <- forecast(arima1.str)
plot(pred.a1)

############################

# ARIMA(0,1,1)
arima2.str <- Arima(streams_per_week.ts, order=c(0,1,1))
plot(streams_per_week.ts)
lines(fitted(arima2.str), col="blue")
# Checking residuals
res.a2 <- residuals(arima2.str)
plot(res.a2)
Acf(res.a2)
# Predictions
pred.a2 <- forecast(arima2.str)
plot(pred.a2)

############################
# AIC
AIC(arima1.str)
AIC(arima2.str)
# The best is the second model, i.e., ARIMA(0,1,1)
############################

# Fitting with auto.arima
auto.arima<- auto.arima(streams_per_week.ts)
auto.arima
# Autoarima confirms our choce, i.e., ARIMA(0,1,1)

autoplot(forecast(auto.arima))
checkresiduals(auto.arima)

#########################################
### MODELS COMPARISONS
#########################################
#pdf("models.pdf", width=10, height=6)
myplot(streams_per_week.ts, xlim=c(1,200))
mylines(pred.inst_streams, lwd=2)
mylines(pred_GBMr1str.inst, lwd=2, col="Red 1")
mylines(pred_GGM_str.inst, lwd=2, col="Slate Blue 1")
# Aggiungi una legenda
legend("topleft", 
       legend=c("Bass Model", "GBM rect", "GGM"),  # Nomi dei modelli
       col=c(SPOT_BLUE, "red", "slateblue1"),  # Colori corrispondenti
       lwd=c(1, 2, 2, 2),  # Spessori delle linee
       #bg="transparent",  # Sfondo trasparente
       ) 
#dev.off()
#########################################
#########################################
#########################################

#########################################

#########################################
### ALTRO...
#########################################

myplot <- function(data, type="l", lwd = 2, color=SPOT_GREEN, alpha = 0.5, xlab = "Weeks", ylab = "Streams (in k)", title = "Streams", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(0,max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim)
  # Add colored area below the curve
  polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.3), border = NA)
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.9)
  axis(2, las=1)
  }

mylines <- function(data, lwd=2, col=SPOT_BLUE){
  lines(data/1000, lwd=lwd, col=col)
  }

myplot(streams_per_week.ts)
mylines(fitted(fit_2))


