####SPOTIFY DATA ANALYSIS

###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(xts)
library(corrplot)
library(TTR)
##############################################

#########################################
### ENVIRONMENT SET UP AND DATA LOADING
#########################################

SPOT_GREEN = "#1DB954"
SPOT_BLUE = "#040504"
SPOT_BLUE_2 = "#1072eb"
SPOT_BLUE_3 = "#d4e2fc"
SPOT_RED = adjustcolor("red", alpha.f = 0.7)
SPOT_RED_2 = "red"

spotify <- read.csv("data/ch3f.csv")
playlist <- read.csv("data/playlist.csv")
monthly_listeners <- read.csv("data/monthly_listeners.csv")

## Set the date language to English
Sys.setlocale("LC_TIME", "en_US")

#########################################
### LOADING montly_listeners DATASET
#########################################

## Turning Dates to the correct format
monthly_listeners$Date <- as.Date(monthly_listeners$Date, format = "%b %d, %Y")

## Selecting only needed rows and cols
end = "2023-10-18"
start_idx <- 1
end_idx <- which(monthly_listeners$Date == as.Date(end))
monthly_listeners <- monthly_listeners[start_idx:end_idx, 1:4]

## Express Monthly.Listeners variable in thousand
monthly_listeners$Monthly.Listeners <- monthly_listeners$Monthly.Listeners / 1000

## Checking NA
any(is.na(monthly_listeners))

## Extracting Releases
releases <- monthly_listeners[monthly_listeners$Releases != "", ]
releases

#########################################
#### Song Release Plot 2
d1 <- which(monthly_listeners$Date == as.Date("2020-12-31"))
d2 <- which(monthly_listeners$Date == as.Date("2021-12-17 "))
d3 <- 1229
d4 <- 1653

## Plot 0
#pdf("release 0.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
#dev.off()

## Plot 1
#pdf("release 1.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
lines(monthly_listeners$Date[1:d1], monthly_listeners$Monthly.Listeners[1:d1], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.9))
points(releases$Date[1:6], releases$Monthly.Listeners[1:6], pch=21, cex=1.5, col=SPOT_BLUE_2, bg= "white", lwd = 2)
#dev.off()

## Plot 2
#pdf("release 2.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
lines(monthly_listeners$Date[1:d1], monthly_listeners$Monthly.Listeners[1:d1], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[1:6], releases$Monthly.Listeners[1:6], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d1:d2], monthly_listeners$Monthly.Listeners[d1:d2], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.9))
points(releases$Date[7:8], releases$Monthly.Listeners[7:8], pch=21, cex=1.5, col=SPOT_BLUE_2, bg= "white", lwd = 2)
#dev.off()

## Plot 3
#pdf("release 3.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
lines(monthly_listeners$Date[1:d1], monthly_listeners$Monthly.Listeners[1:d1], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[1:6], releases$Monthly.Listeners[1:6], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d1:d2], monthly_listeners$Monthly.Listeners[d1:d2], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[7:8], releases$Monthly.Listeners[7:8], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d2:d3], monthly_listeners$Monthly.Listeners[d2:d3], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.9))
points(releases$Date[8:12], releases$Monthly.Listeners[8:12], pch=21, cex=1.5, col=SPOT_BLUE_2, bg= "white", lwd = 2)
#dev.off()

## Plot 4
#pdf("release 4.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
lines(monthly_listeners$Date[1:d1], monthly_listeners$Monthly.Listeners[1:d1], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[1:6], releases$Monthly.Listeners[1:6], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d1:d2], monthly_listeners$Monthly.Listeners[d1:d2], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[7:8], releases$Monthly.Listeners[7:8], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d2:d3], monthly_listeners$Monthly.Listeners[d2:d3], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[8:12], releases$Monthly.Listeners[8:12], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d3:d4], monthly_listeners$Monthly.Listeners[d3:d4], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.9))
points(releases$Date[13:15], releases$Monthly.Listeners[13:15], pch=21, cex=1.5, col=SPOT_BLUE_2, bg= "white", lwd = 2)
#dev.off()


## Plot 4b
date.1 <- which(monthly_listeners$Date == as.Date("2022-08-20"))
date.2 <- which(monthly_listeners$Date == as.Date("2022-12-31"))

plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=1.2, col=adjustcolor("black", alpha.f=0), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
lines(monthly_listeners$Date[1:d1], monthly_listeners$Monthly.Listeners[1:d1], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[1:6], releases$Monthly.Listeners[1:6], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d1:d2], monthly_listeners$Monthly.Listeners[d1:d2], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[7:8], releases$Monthly.Listeners[7:8], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d2:d3], monthly_listeners$Monthly.Listeners[d2:d3], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3))
points(releases$Date[8:12], releases$Monthly.Listeners[8:12], pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.3), bg= "white", lwd = 2)
lines(monthly_listeners$Date[d3:d4], monthly_listeners$Monthly.Listeners[d3:d4], type="l", lwd=2.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=0.9))
points(releases$Date[13:15], releases$Monthly.Listeners[13:15], pch=21, cex=1.5, col=SPOT_BLUE_2, bg= "white", lwd = 2)

segments(x0 = monthly_listeners$Date[date.1], y0 = 0, x1 = monthly_listeners$Date[date.1], y1 = monthly_listeners$Monthly.Listeners[date.1], col = adjustcolor(SPOT_BLUE_2, alpha.f = 0.6), lty = 2, lwd = 1.5)
segments(x0 = monthly_listeners$Date[date.2], y0 = 0, x1 = monthly_listeners$Date[date.2], y1 = monthly_listeners$Monthly.Listeners[date.2], col = adjustcolor(SPOT_BLUE_2, alpha.f = 0.6), lty = 2, lwd = 1.5)


## Plot 5
#pdf("release 5.pdf", width=10, height=6)
plot(monthly_listeners$Date, monthly_listeners$Monthly.Listeners, type="l", lwd=2, col=adjustcolor(SPOT_BLUE_2, alpha.f=1), xlab="Time", ylab="Monthly Listeners (in k)", main="Song Releases", xaxt=NULL, yaxt=NULL, bty="l", axes=T, ylim=c(min(monthly_listeners$Monthly.Listeners), 200))
points(releases$Date, releases$Monthly.Listeners, pch=21, cex=1.5, col=adjustcolor(SPOT_BLUE_2, alpha.f=1), bg= "white", lwd = 2)
#dev.off()


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

myplot <- function(data, type="l", lwd = 3, bty = "l", color=SPOT_BLUE_2, alpha = 0.5, xlab = "Weeks", ylab = "Listeners (in k)", title = "Listeners", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(min(data),max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, bty = "l", xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim, axes=T)
  # Add colored area below the curve
  polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.3), border = NA)
  # Add grid
  #grid(nx = 0, ny = 5, col = "#878787", lty = 1, lwd = 1, equilogs = TRUE)
  #axis(2, at = c(20, 40, 60, 80, 100, 120), tck = 1, lty = 1, col = "#878787", labels = NA)
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.8)
  axis(2, las=1, cex.axis=0.8)
  #axis(2, las=1, cex.axis=0.8, tck=1, col = adjustcolor(SPOT_BLUE, alpha.f = 0.3), lwd = 0, lwd.ticks = 1)
}

myplot.res <- function(data, type="p", lwd = 3, bty = "l", pch=16, cex=1.2, color=SPOT_BLUE_2, alpha = 0.5, xlab = "Weeks", ylab = "Residuals (in k)", title = "Residuals", xaxt="n", yaxt="n", xlim=c(1,length(weeks)), ylim=c(min(data),max(data))) {
  data = data / 1000
  # Plot the curve
  plot(data, type = type, col=color, lwd=lwd, bty = bty, pch=pch, cex=cex, xlab = xlab, ylab = ylab, main = title, xaxt=xaxt, yaxt=yaxt, xlim=xlim, ylim=ylim, axes=T)
  # Add colored area below the curve
  #polygon(c(time(data), rev(time(data))), c(rep(0, length(data)), rev(data)), col = adjustcolor(color, alpha.f = 0.2), border = NA)
  # Add grid
  # Customize x-axis labels
  l <- length(data) %/% 6
  date_idx <- c(1, l, l*2, l*3, l*4, l*5, length(data))
  date_labels <- format(weeks[date_idx], format = "%b '%y")
  axis(1, at=date_idx, labels=date_labels, cex.axis=0.8)
  axis(2, las=1, cex.axis=0.8)}

mylines <- function(data, lwd=3, col=2){
  lines(data/1000, lwd=lwd, col=col)
}

#########################################
### EXPLORATORY DATA ANALYSIS AND LINEAR MODEL
#########################################

weeks <- as.Date(time(weekly.xts$streams), format = "%Y-%m-%d")

#pdf("streams.pdf", width=10, height=6)
myplot(weekly$streams)
#dev.off()
#pdf("listeners.pdf", width=10, height=6)
myplot(weekly$listeners, title = "Listeners", ylab = "Listeners (in k)")
#dev.off()
#pdf("followers.pdf", width=10, height=6)
myplot(weekly$followers*1000, title = "Followers", ylab = "Followers")
#dev.off()
#pdf("plst.count.pdf", width=10, height=6)
myplot(weekly$plst.count*1000, title = "Number of Playlists", ylab = "N. of Playlists")
#dev.off()
#pdf("plst.reach.pdf", width=10, height=6)
myplot(weekly$plst.reach, title = "Potential Audience (from Private Playlists)", ylab = "Potential Audience (in k)")
#dev.off()


cor(weekly$streams, weekly$listeners)

## Correlation between Streams and Listeners is about 100%.
##
## The "monthly listeners" variable on Spotify for Artists represents 
## the moving average of an artist's daily listens over a month. This 
## value offers a more stable and representative view of the artist's audience, 
## smoothing out daily fluctuations and highlighting long-term trends.
###
##  Since we need to choose one, let's decide to analyze the variable "monthly listeners".

# Calculate the correlation matrix 
cor_matrix = cor(weekly[, 2:5])

# Create the correlogram
corrplot(cor_matrix, type = "upper",  
         method = "color",
         col= colorRampPalette(c(SPOT_BLUE_2,"white", SPOT_GREEN))(10),
         addCoef.col = "black",  
         tl.col = "black", 
         tl.srt = 45)


## Since the "followers" variable poses multicollinearity issues and represents a source 
## of streams that is not particularly relevant (only 7% of the streams comes from 
## the fan base, i.e., from "followers"), we decide not to include it in our analysis.

## The variables "playlist count" and "playlist reach" are highly correlated as they explain 
## each other. This leads to multicollinearity issues, prompting us to explore defining a new 
## variable through a transformation of these two.

########################################
### TRYING TO COMBINE "playlist count" AND "playlist reach"
###
## 1. Creating a new variable defined as the weekly variation in the number of playlists
## in which the artist is present
##
## at first we apply a moving average filter
plst.reach.ma <- TTR::SMA(weekly$plst.reach, n = 5, na.rm = TRUE)
plot(plst.reach.ma, type = "l")
lines(weekly$plst.reach, col = "blue")
## then we differenciate
diff.plst.reach <- diff(plst.reach.ma)
diff.plst.reach <- c(diff.plst.reach[1], diff.plst.reach)
plot(diff.plst.reach, type = "l", main = "Weekly Variation in Number of Playlist", ylab = "Playlists Variation")

## 2. Creating a new variable defined as the average number of followers per playlists
avg.reach <- weekly$plst.reach / weekly$plst.count

 

# Calculate the correlation matrix 
cor_matrix_2 = cor(cbind(weekly[, 2:5], avg.reach))

# Create the correlogram 
corrplot(cor_matrix_2, type = "upper",  
         method = "color",
         col= colorRampPalette(c(SPOT_BLUE_2,"white", SPOT_GREEN))(10),
         addCoef.col = "black",  
         tl.col = "black", 
         tl.srt = 45)

## After observing the correlation matrix, we decide to keep the variables 
## Playlist Count and Average Reach, which represent the quantity of playlists 
## and their average size, respectively.

# Calculate the correlation matrix 
cor_matrix_3 = cor(cbind(weekly[, c(2,4)], avg.reach))

# Create the correlogram
#pdf("Correlation matrix.pdf", width=6, height=6)
corrplot(cor_matrix_3,
         main = "Correlation Matrix",
         type = "upper",  
         method = "color",
         col= colorRampPalette(c("white", SPOT_BLUE_3, SPOT_BLUE_2))(30),
         addCoef.col = "black",  
         tl.col = "black", 
         tl.srt = 45,
         mar = c(2, 2, 2, 2))
#dev.off()

#########################################
### LINEAR MODEL
#########################################

## Creating ts object
listeners.ts <- ts(weekly$listeners)

## Plotting weekly data
myplot(listeners.ts)

## Fitting Linear Model
fit_1 <- tslm(listeners.ts~trend)
summary(fit_1)

## Plotting Linear Model

#pdf("tslm 1.pdf", width=10, height=6)
myplot(listeners.ts, title = "Linear Regression")
mylines(fitted(fit_1), col = 2)
#dev.off()

## Residuals
res_1 <- residuals(fit_1)
myplot.res(res_1)

## Durbin Watson test for Autocorrelation
dwtest(fit_1)

## Plotting Correlogram
acf(res_1, main = "Correlogram")

#########################################
### TSLM
#########################################

## Include the effect of Audience
fit_2 <- tslm(listeners.ts ~ trend + weekly$plst.count + avg.reach)
summary(fit_2)

## Plotting the model
pdf("tslm 2.pdf", width=10, height=6)
myplot(weekly$listeners, type = "l", title = "TSLM")
mylines(fitted(fit_2), col=2)
dev.off()

## Residuals
res_2 <- residuals(fit_2)
pdf("res 2.pdf", width=8, height=6)
myplot.res(res_2)
dev.off()

## Durbin Watson test for Autocorrelation
dwtest(fit_2)

pdf("acf res 2.pdf", width=8, height=6)
acf(res_2, bty = "l")
dev.off()
#########################################
### BASS MODEL
#########################################

## Fitting Bass Model
bm_streams <- BM(listeners.ts, display = F)
summary(bm_streams)

## Making Prediction With Bass Model
pred_bm_streams<- predict(bm_streams, newx=c(1:200))
pred.inst_streams<- make.instantaneous(pred_bm_streams)

myplot(cumsum(listeners.ts), lwd = 8, ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0,10000))
mylines(pred_bm_streams, col=2)

myplot(listeners.ts, xlim=c(1,200))
mylines(pred.inst_streams, col=2)

## Plotting Residuals
bm_streams_res <- residuals(bm_streams)
myplot.res(bm_streams_res)

#########################################
### GENERALIZED BASS MODEL
#########################################

###GBM With Rectangular Shock
GBM_r1str<- GBM(weekly$listeners, shock = "rett", nshock = 1, prelimestimates = c(6.195036e+06, 8.101652e-04, 3.891079e-02, 41, 83, +2), display = F)
summary(GBM_r1str)

pred_GBMr1str<- predict(GBM_r1str, newx=c(1:200))
pred_GBMr1str.inst<- make.instantaneous(pred_GBMr1str)

myplot(cumsum(listeners.ts), type= "l", lwd = 8, ylab="Cumulative Streams",  xlim=c(1,200), ylim=c(0, max(pred_GBMr1str) * 1.1/1000))
mylines(pred_GBMr1str, col = 2)

myplot(listeners.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMr1str.inst)*1.1/1000))
mylines(pred_GBMr1str.inst, col = 2)

###GBM With Exponential Shock
GBM_e1str<- GBM(listeners.ts, shock = "exp", nshock = 1, prelimestimates = c(6.195036e+06, 8.101652e-04, 3.891079e-02, 83, -0.08, +0.09), display = F)
summary(GBM_e1str)

pred_GBMe1str<- predict(GBM_e1str, newx=c(1:200))
pred_GBMe1str.inst<- make.instantaneous(pred_GBMe1str)

myplot(cumsum(listeners.ts), lwd = 8,ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0, max(pred_GBMe1str) * 1.1/1000))
mylines(pred_GBMe1str, col = 2)

myplot(listeners.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMe1str.inst)*1.1/1000))
mylines(pred_GBMe1str.inst, col = 2)

###GBM 2Exp shock
GBM_e2str<- GBM(listeners.ts, shock = "exp", nshock = 2, prelimestimates = c(6.195036e+06, 8.101652e-04, 3.891079e-02, 41, +0.1, +0.1, 83, -0.08, +0.09), display = F)
summary(GBM_e2str)

pred_GBMe2str<- predict(GBM_e2str, newx=c(1:200))
pred_GBMe2str.inst<- make.instantaneous(pred_GBMe2str)

myplot(cumsum(listeners.ts), lwd = 8,ylab="Cumulative Streams (in k)", xlim=c(1,200), ylim=c(0, max(pred_GBMe2str) * 1.1/1000))
mylines(pred_GBMe2str, col = 2)

myplot(listeners.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMe2str.inst)*1.1/1000))
mylines(pred_GBMe2str.inst, col = 2)

## R^2 tilde: per modelli GBM con shock exp
## R^2 tilde > 0.3: modello più complesso è significan
## conviene usarlo piuttosto che usare quello con un solo shock

r2_tilde <- (0.999691 - 0.997783)/(1 - 0.997783)
r2_tilde

#########################################
### GUSEO-GUIDOLIN MODEL
#########################################

###GGM -> usually performs well with qc=0.001, pc=0.01
GGM_str<- GGM(listeners.ts, prelimestimates=c(6.195036e+06, 0.001, 0.01, 8.101652e-04, 3.891079e-02), display = F)
summary(GGM_str)

pred_GGM_str<- predict(GGM_str, newx=c(1:200))
pred_GGM_str.inst<- make.instantaneous(pred_GGM_str)

myplot(listeners.ts, xlim=c(1,200))
mylines(pred_GGM_str.inst, col = 2)

###Analysis of residuals
res_GGM_str<- residuals(GGM_str)
acf<- acf(residuals(GGM_str))

#########################################
### SIMPLE EXPONENTIAL SMOOTHING 
#########################################

SES_str <- ses(listeners.ts, h=50)
plot(SES_str)

#########################################
### HOLT'S METHODS 
#########################################

HOLT_str <- holt(listeners.ts, h = 12)
plot(HOLT_str)

H_DAMPED_str <- holt(listeners.ts, h = 12, damped = T)
plot(H_DAMPED_str)

#H_WINTERS_str <- HoltWinters(streams)

#########################################
### ARIMA
#########################################

myplot(listeners.ts)
# The time plot shows some non-stationarity
# To address the non-stationarity, we will take a first difference of the data
streams_diff <- diff(listeners.ts)
myplot(streams_diff, title = "Streams (Differenced)")
Acf(streams_diff)
Pacf(streams_diff)
# Acf suggests MA(1), i.e., ARIMA(0,1,1)
# Pacf suggests AR(1), i.e., ARIMA(1,1,0)
# Below we try to fit both model

############################

# ARIMA(1,1,0)
arima1.str <- Arima(listeners.ts, order=c(1,1,0))
myplot(listeners.ts)
mylines(fitted(arima1.str), col=SPOT_RED, lwd = 3)
# Checking residuals
res.a1 <- residuals(arima1.str)
myplot.res(res.a1)

Acf(res.a1)
# Predictions
pred.a1 <- forecast(arima1.str)
myplot(listeners.ts)
mylines(fitted(arima1.str), col=SPOT_RED)

############################

# ARIMA(0,1,1)
arima2.str <- Arima(listeners.ts, order=c(0,1,1))
myplot(listeners.ts)
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
auto.arima<- auto.arima(listeners.ts)
auto.arima
# Autoarima confirms our choce, i.e., ARIMA(0,1,1)

autoplot(forecast(auto.arima))
checkresiduals(auto.arima)

#########################################
### ARMAX
#########################################

armax1<- Arima(listeners.ts, xreg = weekly$plst.reach, order = c(0,1,1))
myplot(listeners.ts)
mylines(fitted(armax1), col = SPOT_RED, lwd=3)

# Checking residuals
res.a1 <- residuals(armax1)
myplot.res(res.a1)
Acf(res.a1)
# Predictions with external regressors
pred.a1 <- forecast(armax1, xreg = weekly$plst.reach, h=30)
plot(pred.a1)

#########################################
### TSLM + ARIMA
#########################################

## Fitting Arima to Residuals
arima.tslm <- auto.arima(res_2)
#plot(forecast(arima.tslm))
## Arima (1,0,1) is chosen

## Plot Arima fitted values for residuals
myplot.res(res_2)
mylines(fitted(arima.tslm), col = rgb(1, 0, 0, alpha = 0.6), lwd=3)

## Plot GBM + Arima fitted values

#pdf("tslm + arima.pdf", width=10, height=6)
myplot(listeners.ts)
mylines(fitted(fit_2) + fitted(arima.tslm), col = 2, lwd=3)
#dev.off()

#########################################
### GBM + ARIMA
#########################################

###GBM With Rectangular Shock
GBM_r1str<- GBM(listeners.ts, shock = "rett", nshock = 1, prelimestimates = c(9.744394e+06, 9.411032e-04, 3.444991e-02, 45, 95, +0.1), display = F)
summary(GBM_r1str)

## GBM Fitted values
fitted_GBMr1str <- predict(GBM_r1str, newx=c(1:145))
fitted_GBMr1str.inst<- make.instantaneous(fitted_GBMr1str)

## Plot cumulative process
myplot(cumsum(listeners.ts), type= "l", lwd = 8, ylab="Cumulative Streams",  xlim=c(1,200), ylim=c(0, max(pred_GBMr1str) * 1.1/1000))
mylines(pred_GBMr1str, col="red")

## Plot instantaneous process
myplot(listeners.ts, xlim=c(1,200), ylim=c(1, max(pred_GBMr1str.inst)*1.1/1000))
mylines(pred_GBMr1str.inst, col="red")

## Computing residuals
res.GBM_r1str <- listeners.ts - fitted_GBMr1str.inst
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
myplot(listeners.ts)
mylines(fitted_GBMr1str.inst + fitted(arima.GBM), col = rgb(1, 0, 0, alpha = 0.75), lwd=3)

## Predictions (for 30 days)
pred_GBMr1str <- predict(GBM_r1str, newx=c(1:175))
pred_GBMr1str.inst<- make.instantaneous(pred_GBMr1str)
pred.arima.GBM <- forecast(arima.GBM, h=30)$mean
pred.arima.GBM <- c(fitted(arima.GBM), pred.arima.GBM)

## Plot GBM + Arima predictions
myplot(listeners.ts, xlim=c(1,175), ylim=c(1, max(pred_GBMr1str.inst)/1000), title = "GBM + ARIMA")
mylines(pred_GBMr1str.inst + pred.arima.GBM, col = rgb(1, 0, 0, alpha = 0.75), lwd=3)

#########################################
### MODELS COMPARISONS
#########################################
#pdf("models.pdf", width=10, height=6)
myplot(listeners.ts, xlim=c(1,200))
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



