####SPOTIFY DATA ANALYSIS

###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(xts)
library(corrplot)
##############################################

#########################################
### ENVIRONMENT SET UP AND DATA LOADING
#########################################

SPOT_GREEN = "#1DB954"
SPOT_BLUE = "#040504"
SPOT_BLUE_2 = "#1072eb"
SPOT_BLUE_3 = "#d4e2fc"
SPOT_RED = adjustcolor("red", alpha.f = 0.75)

spotify <- read.csv("data/ch3f.csv")
playlist <- read.csv("data/playlist.csv")

## Set the date language to English
Sys.setlocale("LC_TIME", "en_US")

#########################################
### LOADING spotify DATASET
#########################################

## Checking NA
any(is.na(spotify))
## NA are present in column 4 and 5
## As we don't use those columns we'll remove them

## Turning Dates to the correct format
spotify$date <- as.Date(spotify$date, format = "%Y-%m-%d")

#########################################
### LOADING playlist DATASET
#########################################

## Checking NA
any(is.na(playlist))

## Turning Dates to the correct format
playlist$Date <- as.Date(playlist$Date, format = "%b %d, %Y")

## Selecting only needed rows
start = "2021-01-01"
end = "2023-10-18"
start_idx <- which(playlist$Date == as.Date(start))
end_idx <- which(playlist$Date == as.Date(end))
df_2 <- playlist[start_idx:end_idx, 1:3]

## Checking NA
any(is.na(df_2))

## Issue in row 789, i.e. date "2023-02-28" is missing in playlist dataset
## Adding the missing row with data of the previous line, i.e. 788
df_3 <- rbind(df_2[1:788, ], df_2[788, ], df_2[789:1020, ])
df_3$Date[789] <- as.Date("2023-02-28")

## Converting numeric values to integers
df_3$Playlist.Count <- as.numeric(gsub(",", "", df_3$Playlist.Count))
df_3$Playlist.Total.Reach <- as.numeric(gsub(",", "", df_3$Playlist.Total.Reach))

#########################################
### CONCATENATING THE 2 DATASETS
#########################################

## Concatenating spotify and playlist dataframes
data <- cbind(spotify, df_3[, 2:3])

#########################################
### TURNING DAILY DATA INTO WEEKLY DATA
#########################################

streams_xts <- xts(data$streams, order.by = data$date)
streams <- apply.weekly(streams_xts, FUN = sum)

listeners_xts <- xts(data$listeners, order.by = data$date)
listeners <- apply.weekly(listeners_xts, FUN = sum)

followers_xts <- xts(data$followers, order.by = data$date)
followers <- apply.weekly(followers_xts, FUN = mean)

plst.count_xts <- xts(data$Playlist.Count, order.by = data$date)
plst.count <- apply.weekly(plst.count_xts, FUN = mean)

plst.reach_xts <- xts(data$Playlist.Total.Reach, order.by = data$date)
plst.reach <- apply.weekly(plst.reach_xts, FUN = mean)

#########################################
### CREATING THE FINAL WEEKLY DATASET
#########################################

## Concatenating the columns containing weekly data
weekly.xts <- cbind(streams, listeners, followers, plst.count, plst.reach)
length(weekly.xts$streams)

## Removing the first and the last rows
weekly.xts <- weekly.xts[2:146]

## weekly.xts is an xts object
## we can use coredata() to turn it into a data.frame
weekly <- as.data.frame(coredata(weekly.xts))

#########################################
### DEFINING CUSTOM PLOT
#########################################

myplot <- function(data, type="l", lwd = 3, color=SPOT_BLUE_2, alpha = 0.5, xlab = "Weeks", ylab = "Streams (in k)", title = "Streams", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(min(data),max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim, axes=FALSE)
  # Add colored area below the curve
  polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.2), border = NA)
  # Add grid
  #grid(nx = 0, ny = 5, col = "#878787", lty = 1, lwd = 1, equilogs = TRUE)
  #axis(2, at = c(20, 40, 60, 80, 100, 120), tck = 1, lty = 1, col = "#878787", labels = NA)
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.8)
  axis(2, las=1, cex.axis=0.8, tck=1, col = adjustcolor(SPOT_BLUE, alpha.f = 0.3), lwd = 0, lwd.ticks = 1)
}
myplot.res <- function(data, type="p", lwd = 3, pch=16, cex=1, color=SPOT_BLUE_2, alpha = 0.5, xlab = "Weeks", ylab = "Residuals (in k)", title = "Residuals", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(min(data),max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, pch=pch, cex=cex, xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim, axes=FALSE)
  # Add colored area below the curve
  polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.2), border = NA)
  # Add grid
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.8)
  axis(2, las=1, cex.axis=0.8, tck=1, col = adjustcolor(SPOT_BLUE, alpha.f = 0.3), lwd = 0, lwd.ticks = 1)
}

mylines <- function(data, lwd=3, col=SPOT_BLUE){
  lines(data/1000, lwd=lwd, col=col)
}

#########################################
### EXPLORATORY DATA ANALYSIS AND LINEAR MODEL
#########################################

weeks <- as.Date(time(weekly.xts$streams), format = "%Y-%m-%d")

myplot(weekly$streams)
myplot(weekly$listeners, title = "Listeners", ylab = "Listeners (in k)")
myplot(weekly$followers, title = "Followers", ylab = "Followers (in k)")
myplot(weekly$plst.count, title = "Number of Playlists", ylab = "N. of Playlists (in k)")
myplot(weekly$plst.reach, title = "Potential Audience (from Private Playlists)", ylab = "Potential Audience (in k)")

# Calculate the correlation matrix 
cor_matrix = cor(weekly) 

# Create the correlogram 
corrplot(cor_matrix, type = "upper",  
         method = "color",
         col= colorRampPalette(c(SPOT_BLUE_2,"white", SPOT_GREEN))(10),
         addCoef.col = "black",  
         tl.col = "black", 
         tl.srt = 45)

#########################################
### LINEAR MODEL
#########################################

## Creating ts object
streams.ts <- ts(weekly$streams)

## Plotting weekly data
myplot(streams.ts)

## Fitting Linear Model
fit_1 <- tslm(streams.ts~trend)
summary(fit_1)

## Plotting Linear Model
myplot(streams.ts, title = "Linear Regression")
mylines(fitted(fit_1), col = 2)

## Residuals
res_1 <- residuals(fit_1)
myplot(res_1/1000,  ylab="Residuals (in k)", xlab="weeks", title = "Residuals", type = "p")
#abline(h=0, lty="dashed", col=SPOT_GREEN)

## Durbin Watson test for Autocorrelation
dwtest(fit_1)

## Plotting Correlogram
acf(streams.ts)

#########################################
### TSLM
#########################################

## Include the effect of Audience
fit_2 <- tslm(streams.ts ~ weekly$plst.reach)
summary(fit_2)

## Plotting the model
myplot(weekly$streams, type = "l", title = "TSLM")
mylines(fitted(fit_2), col=2)

## Residuals
res_2 <- residuals(fit_2)
myplot(res_2/1000, ylab="Residuals (in k)", xlab="weeks", title = "Residuals", type = "p")
#abline(h=0, lty="dashed", col=SPOT_GREEN)

## Durbin Watson test for Autocorrelation
dwtest(fit_2)

acf(res_2)

#########################################
### BASS MODEL
#########################################

## Fitting Bass Model
bm_streams <- BM(weekly$streams, display = F)
summary(bm_streams)

## Making Prediction With Bass Model
pred_bm_streams<- predict(bm_streams, newx=c(1:200))
pred.inst_streams<- make.instantaneous(pred_bm_streams)

myplot(cumsum(streams.ts), lwd = 8, ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0,10000))
mylines(pred_bm_streams, col="red")

myplot(streams.ts, xlim=c(1,200))
mylines(pred.inst_streams, col="red")

## Plotting Residuals
bm_streams_res <- residuals(bm_streams)
myplot(bm_streams_res/1000, ylab="Residuals (in k)", xlab="weeks", title = "Residuals", type = 'p')

#########################################
### GENERALIZED BASS MODEL
#########################################

###GBM With Rectangular Shock
GBM_r1str<- GBM(weekly$streams, shock = "rett", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 45, 95, +0.1), display = F)
summary(GBM_r1str)

pred_GBMr1str<- predict(GBM_r1str, newx=c(1:200))
pred_GBMr1str.inst<- make.instantaneous(pred_GBMr1str)

myplot(cumsum(streams.ts), type= "l", lwd = 8, ylab="Cumulative Streams",  xlim=c(1,200), ylim=c(0, max(pred_GBMr1str) * 1.1/1000))
mylines(pred_GBMr1str, col = "red")

myplot(streams.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMr1str.inst)*1.1/1000))
mylines(pred_GBMr1str.inst, col = "red")

###GBM With Exponential Shock
GBM_e1str<- GBM(weekly$streams, shock = "exp", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 75, +0.1, +0.1), display = F)
summary(GBM_e1str)

pred_GBMe1str<- predict(GBM_e1str, newx=c(1:200))
pred_GBMe1str.inst<- make.instantaneous(pred_GBMe1str)

myplot(cumsum(streams.ts), lwd = 8,ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0, max(pred_GBMe1str) * 1.1/1000))
mylines(pred_GBMe1str, col = "red")

myplot(streams.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMe1str.inst)*1.1/1000))
mylines(pred_GBMe1str.inst, col = "red")

#########################################
### GUSEO-GUIDOLIN MODEL
#########################################

###GGM -> usually performs well with qc=0.001, pc=0.01
GGM_str<- GGM(weekly$streams, prelimestimates=c(9.664377e+06, 0.001, 0.01, 9.777106e-04, 3.461131e-02), display = F)
summary(GGM_str)

pred_GGM_str<- predict(GGM_str, newx=c(1:200))
pred_GGM_str.inst<- make.instantaneous(pred_GGM_str)

myplot(streams.ts, xlim=c(1,200))
mylines(pred_GGM_str.inst, col = "red")

###Analysis of residuals
res_GGM_str<- residuals(GGM_str)
acf<- acf(residuals(GGM_str))

#########################################
### SIMPLE EXPONENTIAL SMOOTHING 
#########################################

SES_str <- ses(streams.ts, h=50)
plot(SES_str)

#########################################
### HOLT'S METHODS 
#########################################

HOLT_str <- holt(weekly$streams, h = 50)
plot(HOLT_str)

H_DAMPED_str <- holt(weekly$streams, h = 50, damped = T)
plot(H_DAMPED_str)

#H_WINTERS_str <- HoltWinters(streams)

#########################################
### ARIMA
#########################################

myplot(streams.ts)
# The time plot shows some non-stationarity
# To address the non-stationarity, we will take a first difference of the data
streams_diff <- diff(streams.ts)
myplot(streams_diff, title = "Streams (Differenced)")
Acf(streams_diff)
Pacf(streams_diff)
# Acf suggests MA(1), i.e., ARIMA(0,1,1)
# Pacf suggests AR(1), i.e., ARIMA(1,1,0)
# Below we try to fit both model

############################

# ARIMA(1,1,0)
arima1.str <- Arima(streams.ts, order=c(1,1,0))
myplot(streams.ts)
mylines(fitted(arima1.str), col=SPOT_RED, lwd = 3)
# Checking residuals
res.a1 <- residuals(arima1.str)
myplot.res(res.a1)

Acf(res.a1)
# Predictions
pred.a1 <- forecast(arima1.str)
myplot(streams.ts)
mylines(fitted(arima1.str), col=SPOT_RED)

############################

# ARIMA(0,1,1)
arima2.str <- Arima(streams.ts, order=c(0,1,1))
myplot(streams.ts)
mylines(fitted(arima2.str), col=SPOT_RED)
# Checking residuals
res.a2 <- residuals(arima2.str)
myplot.res(res.a2)
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
auto.arima<- auto.arima(streams.ts)
auto.arima
# Autoarima confirms our choce, i.e., ARIMA(0,1,1)

autoplot(forecast(auto.arima))
checkresiduals(auto.arima)

#########################################
### ARMAX
#########################################

armax1<- Arima(streams.ts, xreg = weekly$plst.reach, order = c(0,1,1))
myplot(streams.ts)
mylines(fitted(armax1), col = SPOT_RED, lwd=3)

# Checking residuals
res.a1 <- residuals(armax1)
myplot.res(res.a1)
Acf(res.a1)
# Predictions with external regressors
pred.a1 <- forecast(armax1, xreg = weekly$plst.reach, h=30)
plot(pred.a1)

#########################################
### GBM + ARIMA
#########################################

###GBM With Rectangular Shock
GBM_r1str<- GBM(weekly$streams, shock = "rett", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 45, 95, +0.1), display = F)
summary(GBM_r1str)

## GBM Fitted values
fitted_GBMr1str <- predict(GBM_r1str, newx=c(1:145))
fitted_GBMr1str.inst<- make.instantaneous(fitted_GBMr1str)

## Plot cumulative process
myplot(cumsum(streams.ts), type= "l", lwd = 8, ylab="Cumulative Streams",  xlim=c(1,200), ylim=c(0, max(pred_GBMr1str) * 1.1/1000))
mylines(pred_GBMr1str, col="red")

## Plot instantaneous process
myplot(streams.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMr1str.inst)*1.1/1000))
mylines(pred_GBMr1str.inst, col="red")

## Computing residuals
res.GBM_r1str <- streams.ts - fitted_GBMr1str.inst
myplot.res(res.GBM_r1str)
Acf(res.GBM_r1str)

## Fitting Arima to Residuals
arima.GBM <- auto.arima(res.GBM_r1str)
#plot(forecast(arima.GBM))
## ARIMA (2,0,0) chosen

## Plot Arima fitted values for residuals
myplot.res(res.GBM_r1str)
mylines(fitted(arima.GBM), col = rgb(1, 0, 0, alpha = 0.75), lwd=3)

## Plot GBM + Arima fitted values
myplot(streams.ts)
mylines(fitted_GBMr1str.inst + fitted(arima.GBM), col = rgb(1, 0, 0, alpha = 0.75), lwd=3)

## Predictions (for 30 days)
pred_GBMr1str <- predict(GBM_r1str, newx=c(1:175))
pred_GBMr1str.inst<- make.instantaneous(pred_GBMr1str)
pred.arima.GBM <- forecast(arima.GBM, h=30)$mean
pred.arima.GBM <- c(fitted(arima.GBM), pred.arima.GBM)

## Plot GBM + Arima predictions
myplot(streams.ts, xlim=c(1,175), ylim=c(1, max(pred_GBMr1str.inst)/1000), title = "GBM + ARIMA")
mylines(pred_GBMr1str.inst + pred.arima.GBM, col = rgb(1, 0, 0, alpha = 0.75), lwd=3)

#########################################
### MODELS COMPARISONS
#########################################
#pdf("models.pdf", width=10, height=6)
myplot(streams.ts, xlim=c(1,200))
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