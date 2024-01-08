####SPOTIFY DATA ANALYSIS

###Libraries##################################
library(readxl)
library(lmtest) 
library(forecast)
library(DIMORA)
library(fpp2)
library(xts)
library(corrplot)
library(zoo)
##############################################

#########################################
### ENVIRONMENT SET UP AND DATA LOADING
#########################################

SPOT_GREEN = "#1DB954"
SPOT_BLUE = "#040504"
SPOT_BLUE_2 = "#1072eb"
SPOT_BLUE_3 = "#d4e2fc"
SPOT_RED = adjustcolor("red", alpha.f = 0.75)

## Set the date language to English
Sys.setlocale("LC_TIME", "en_US")

#########################################
### LOADING playlists_per_song_1 DATASET
##
## This dataset contains information about playlists where 
## the artist's songs have been added and removed, i.e., 
## playlists where the artist has been featured in the past.
#########################################

pps.1 <- read.csv("data/playlists_per_song_1.csv")

#########################################
### LOADING playlists_per_song_2 DATASET
##
## This dataset provides information about playlists where
## the artist's songs have been added and are currently present.
#########################################

pps.2 <- read.csv("data/playlists_per_song_2.csv")

## Print column names
names(pps.1)
names(pps.2)

## Selecting only needed cols
pps.1 <- pps.1[, c(1,10,13,15,18,20)]
pps.2 <- pps.2[, c(1,10,13,15,18,20)]

## Turning Dates to the correct format
pps.1$Date.Added <- as.Date(pps.1$Date.Added, format = "%b %d, %Y")
pps.1$Date.Removed <- as.Date(pps.1$Date.Removed, format = "%b %d, %Y")
pps.2$Date.Added <- as.Date(pps.2$Date.Added, format = "%b %d, %Y")
pps.2$Date.Removed <- as.Date(pps.2$Date.Removed, format = "%b %d, %Y")

## Ordering rows
pps.1 <- pps.1[order(pps.1$Track.Name, pps.1$Date.Added),]
pps.2 <- pps.2[order(pps.2$Track.Name, pps.2$Date.Added),]

#########################################
### FULL PLAYLISTS
###
## Here we analyze all the playlists
#########################################

## Arranging data to build a new data.frame to count total number of playlist
pps.1_in <- pps.1[, c(3,6)]
pps.1_out <- pps.1[, c(4,6)]
pps.1_out$Followers <- -pps.1_out$Followers
pps.2_in <- pps.1[, c(3,6)]

## Changing cols name to make it consistent
names(pps.1_in)[names(pps.1_in) == "Date.Added"] <- "Date"
names(pps.1_out)[names(pps.1_out) == "Date.Removed"] <- "Date"
names(pps.2_in)[names(pps.2_in) == "Date.Added"] <- "Date"

## Creating the new data.frame
pps <- rbind(pps.1_in, pps.2_in, pps.1_out)

## Ordering row by date
pps <- pps[order(pps$Date), ]
pps <- data.frame(pps, row.names = NULL)

## Add a column for cumulative followers count
pps$Playlist_Reach <- cumsum(pps$Followers)

## Add a column for counting number of playlist
pps$Playlist_Count <- ifelse(pps$Followers < 0, -1, +1)
pps$Playlist_Count <- cumsum(pps$Playlist_Count)

Playlist_Count <- aggregate(Playlist_Count ~ Date, data = pps, FUN = max)
Playlist_Reach <- aggregate(Playlist_Reach ~ Date, data = pps, FUN = max)

#########################################
### Filling missing dates for Playlist_Count

# Trova la data minima e massima nel tuo data.frame
min_date <- as.Date(min(Playlist_Count$Date))
max_date <- as.Date(max(Playlist_Count$Date))

# Crea un nuovo data.frame con tutte le date comprese tra minima e massima
expanded_dates <- data.frame(Date = seq(min_date, max_date, by = "days"))

# Unisci il data.frame originale con quello contenente tutte le date
Playlist_Count_Full <- merge(expanded_dates, Playlist_Count, by = "Date", all.x = TRUE)

# Sostituisci i valori mancanti con l'ultimo valore noto
Playlist_Count_Full$Playlist_Count <- zoo::na.locf(Playlist_Count_Full$Playlist_Count)

plot(Playlist_Count_Full, type = "l", col = SPOT_BLUE_2, lwd = 2, main = "Playlist Count", ylab = "Number of playlists", xlab = "Time" )

#########################################
### 1000+ FOLLOWERS PLAYLISTS
###
## Here we analyze only larger playlists, i.e.
## those with more than 1000 followers
#########################################

## Selecting playlists with more than 1000 followers 
large.1 <- pps.1[pps.1$Followers > 500,]
large.2 <- pps.2[pps.1$Followers > 500,]

## Arranging data
large.1_in <- large.1[, c(3,6)]
large.1_out <- large.1[, c(4,6)]
large.1_out$Followers <- -large.1_out$Followers
large.2_in <- large.2[, c(3,6)]

## Changing cols name to make it consistent
names(large.1_in)[names(large.1_in) == "Date.Added"] <- "Date"
names(large.1_out)[names(large.1_out) == "Date.Removed"] <- "Date"
names(large.2_in)[names(large.2_in) == "Date.Added"] <- "Date"

## Creating the new data.frame
large <- rbind(large.1_in, large.2_in, large.1_out)

## Ordering row by date
large <- large[order(large$Date), ]
large <- data.frame(large, row.names = NULL)

## Add a column for cumulative followers count
large$Playlist_Reach <- cumsum(large$Followers)

## Add a column for counting number of playlist
large$Playlist_Count <- ifelse(large$Followers < 0, -1, +1)
large$Playlist_Count <- cumsum(large$Playlist_Count)

plst.count_large <- aggregate(Playlist_Count ~ Date, data = large, FUN = max)
plst.reach_large <- aggregate(Playlist_Reach ~ Date, data = large, FUN = max)

#########################################
### Filling missing dates for plst.count_large

# Trova la data minima e massima nel tuo data.frame
min_date <- as.Date(min(plst.count_large$Date))
max_date <- as.Date(max(plst.count_large$Date))

# Crea un nuovo data.frame con tutte le date comprese tra minima e massima
large.expanded_dates <- data.frame(Date = seq(min_date, max_date, by = "days"))

# Unisci il data.frame originale con quello contenente tutte le date
plst.count.large.full <- merge(large.expanded_dates, plst.count_large, by = "Date", all.x = TRUE)

# Sostituisci i valori mancanti con l'ultimo valore noto
plst.count.large.full$Playlist_Count <- zoo::na.locf(plst.count.large.full$Playlist_Count)

plot(plst.count.large.full, type = "l", col = SPOT_BLUE_2, lwd = 2, main = "Playlist Count", ylab = "Number of playlists", xlab = "Time" )
idx <- which(plst.count.large.full$Date == as.Date("2022-08-20"))
idx.2 <- which(plst.count.large.full$Date == as.Date("2022-12-31"))
plot(plst.count.large.full[(idx-20):(idx.2+20),], type = "l", col = SPOT_BLUE_2, lwd = 2, main = "Playlist Count", ylab = "Number of playlists", xlab = "Time" )
points(plst.count.large.full[idx,], col = "red", pch = 16)

plot(pps.1$Followers)

# Supponendo che pps.1$Followers sia il tuo array
followers <- pps.1$Followers

# Dividi in due classi
class1 <- followers[followers < 500]
class2 <- followers[followers >= 500]

# Creazione di un barplot
#pdf("Barplot Followers.pdf", width=6, height=6)
bp <-barplot(c(length(class1), length(class2)), names.arg = c("Less than 500 Followers", "500+ Followers"), 
        col = c(SPOT_BLUE_2, SPOT_BLUE_3), main = "Playlist Comparison: Below 500 Followers vs. 500 or More",  
        ylab = "Number of Playlists", xlab = NULL, ylim = c(0,1000))

# Aggiungi il numero sopra ogni barra
text(bp, c(length(class1), length(class2)), labels = c(length(class1), length(class2)), pos = 3, col = "black", cex = 0.9, font = 2)
#dev.off()

 #########################################
### FENOMENALE
###
## Here we analyze only playlists where the
## song "FENOMENALE" is present
#########################################

## Transforming data to extract playlist_reach 
## and playlist_count for the song "FENOMENALE"

## Selecting only those rows relative to the song "FENOMENALE"
fen.1 <- pps.1[pps.1$Track.Name == "FENOMENALE",]
fen.2 <- pps.2[pps.2$Track.Name == "FENOMENALE",]

## Selecting "FENOMENALE"'s playlists with more than 1000 followers
fen.1 <- fen.1[fen.1$Followers > 10000,]
fen.2 <- fen.2[fen.2$Followers > 10000,]

fen.1_in <- fen.1[, c(3,6)]
fen.1_out <- fen.1[, c(4,6)]
fen.1_out$Followers <- -fen.1_out$Followers
fen.2_in <- fen.2[, c(3,6)]

## Changing cols name to make it consistent
names(fen.1_in)[names(fen.1_in) == "Date.Added"] <- "Date"
names(fen.1_out)[names(fen.1_out) == "Date.Removed"] <- "Date"
names(fen.2_in)[names(fen.2_in) == "Date.Added"] <- "Date"

fen.plst <- rbind(fen.1_in, fen.2_in, fen.1_out)

## Ordering row by date
fen.plst <- fen.plst[order(fen.plst$Date), ]
fen.plst <- data.frame(fen.plst, row.names = NULL)

## Add a column for cumulative followers count
fen.plst$Playlist_Reach <- cumsum(fen.plst$Followers)

## Add a column for counting number of playlist
fen.plst$Playlist_Count <- ifelse(fen.plst$Followers < 0, -1, +1)
fen.plst$Playlist_Count <- cumsum(fen.plst$Playlist_Count)

fen.playlist_count <- aggregate(Playlist_Count ~ Date, data = fen.plst, FUN = max)
fen.playlist_reach <- aggregate(Playlist_Reach ~ Date, data = fen.plst, FUN = max)

#########################################
### Filling missing dates for fen.playlist_count

# Trova la data minima e massima nel tuo data.frame
min_date <- as.Date(min(fen.playlist_count$Date))
max_date <- as.Date(max(fen.playlist_count$Date))

# Crea un nuovo data.frame con tutte le date comprese tra minima e massima
fen.expanded_dates <- data.frame(Date = seq(min_date, max_date, by = "days"))

# Unisci il data.frame originale con quello contenente tutte le date
fen.playlist_count.full <- merge(fen.expanded_dates, fen.playlist_count, by = "Date", all.x = TRUE)

# Sostituisci i valori mancanti con l'ultimo valore noto
fen.playlist_count.full$Playlist_Count <- zoo::na.locf(fen.playlist_count.full$Playlist_Count)

plot(fen.playlist_count.full, type = "l", col = SPOT_BLUE_2, lwd = 2, main = "Playlist Count", ylab = "Number of playlists", xlab = "Time" )

