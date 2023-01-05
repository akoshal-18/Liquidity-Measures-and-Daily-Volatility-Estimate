---
  title: "FE 570 - Final Project"
author: "Arjun Koshal"
date: "11/17/2022"
output: pdf_document
---
  
library(tidyverse)
library(lubridate)
library(xts)
library(highfrequency)

q_data <- read.csv(file = '20211003-quote.csv')
t_data <- read.csv(file = '20211003-trade.csv')

q_data$timestamp <- gsub('D', ' ', q_data$timestamp)
t_data$timestamp <- gsub('D', ' ', t_data$timestamp)

names(q_data)[names(q_data) == "bidPrice"] <- "BID"
names(q_data)[names(q_data) == "askPrice"] <- "OFR"
names(q_data)[names(q_data) == "symbol"] <- "SYMBOL"

names(t_data)[names(t_data) == "bidPrice"] <- "BID"
names(t_data)[names(t_data) == "askPrice"] <- "OFR"
names(t_data)[names(t_data) == "symbol"] <- "SYMBOL"

q_data.xts <- xts(q_data[,], 
                  order.by=as.POSIXct(q_data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz="GMT"))


t_data.xts <- xts(t_data[,], 
                  order.by=as.POSIXct(t_data$timestamp, format = "%Y-%m-%d %H:%M:%OS", tz="GMT"))

tq_data <- matchTradesQuotes(t_data.xts, q_data.xts)

tq_data <- tq_data[tq_data$SYMBOL == "ETHUSD", ]

format(tq_data, tz="America/New_York")

head(tq_data)

save(tq_data, file = "tq_data.RData")

n.trades <- length(tq_data$SIZE)
n.trades

prices <- as.numeric(tq_data$PRICE)
asks <- as.numeric(tq_data$OFR)
bids <- as.numeric(tq_data$BID)
mids <- 0.5*bids + 0.5*asks

plot(prices, type="l", col="red", main="Trade Price and Mids", 
     xlab="Trade Count",
     ylab="Trade Price")
lines(mids, type="l", col="blue")

ask.SIZE <- as.numeric(tq_data$ASKSIZE)
bid.SIZE <- -1*as.numeric(tq_data$BIDSIZE)
LOB.imbalance <- ask.SIZE + bid.SIZE

plot(ask.SIZE,col="blue", type="h", 
     ylab="Trade Size", 
     xlab="Trade Count", main="Trade Size at Best Quote", ylim=c(-4000,4000))
lines(bid.SIZE, col="red", type="h")

liq.measures <- getLiquidityMeasures(tq_data)

qs <- mean(as.numeric(liq.measures$quotedSpread))
qs

es <- mean(as.numeric(liq.measures$effectiveSpread))
es

rs <- mean(na.omit(as.numeric(liq.measures$realizedSpread)))
rs

price_impact <- mean(na.omit(as.numeric(liq.measures$priceImpact)))
price_impact

tq_data_1 <- tq_data["T00:00/T01:00"]
tq_data_2 <- tq_data["T01:00/T02:00"]
tq_data_3 <- tq_data["T02:00/T03:00"]
tq_data_4 <- tq_data["T03:00/T04:00"]
tq_data_5 <- tq_data["T04:00/T05:00"]
tq_data_6 <- tq_data["T05:00/T06:00"]
tq_data_7 <- tq_data["T06:00/T07:00"]
tq_data_8 <- tq_data["T07:00/T08:00"]
tq_data_9 <- tq_data["T08:00/T09:00"]
tq_data_10 <- tq_data["T09:00/T10:00"]
tq_data_11 <- tq_data["T10:00/T11:00"]
tq_data_12 <- tq_data["T11:00/T12:00"]
tq_data_13 <- tq_data["T12:00/T13:00"]
tq_data_14 <- tq_data["T13:00/T14:00"]
tq_data_15 <- tq_data["T14:00/T15:00"]
tq_data_16 <- tq_data["T15:00/T16:00"]
tq_data_17 <- tq_data["T16:00/T17:00"]
tq_data_18 <- tq_data["T17:00/T18:00"]
tq_data_19 <- tq_data["T18:00/T19:00"]
tq_data_20 <- tq_data["T19:00/T20:00"]
tq_data_21 <- tq_data["T20:00/T21:00"]
tq_data_22 <- tq_data["T21:00/T22:00"]
tq_data_23 <- tq_data["T22:00/T23:00"]
tq_data_24 <- tq_data["T23:00/T23:59:45.352204000"]

liq.1 <- getLiquidityMeasures(tq_data_1)
liq.2 <- getLiquidityMeasures(tq_data_2)
liq.3 <- getLiquidityMeasures(tq_data_3)
liq.4 <- getLiquidityMeasures(tq_data_4)
liq.5 <- getLiquidityMeasures(tq_data_5)
liq.6 <- getLiquidityMeasures(tq_data_6)
liq.7 <- getLiquidityMeasures(tq_data_7)
liq.8 <- getLiquidityMeasures(tq_data_8)
liq.9 <- getLiquidityMeasures(tq_data_9)
liq.10 <- getLiquidityMeasures(tq_data_10)
liq.11 <- getLiquidityMeasures(tq_data_11)
liq.12 <- getLiquidityMeasures(tq_data_12)
liq.13 <- getLiquidityMeasures(tq_data_13)
liq.14 <- getLiquidityMeasures(tq_data_14)
liq.15 <- getLiquidityMeasures(tq_data_15)
liq.16 <- getLiquidityMeasures(tq_data_16)
liq.17 <- getLiquidityMeasures(tq_data_17)
liq.18 <- getLiquidityMeasures(tq_data_18)
liq.19 <- getLiquidityMeasures(tq_data_19)
liq.20 <- getLiquidityMeasures(tq_data_20)
liq.21 <- getLiquidityMeasures(tq_data_21)
liq.22 <- getLiquidityMeasures(tq_data_22)
liq.23 <- getLiquidityMeasures(tq_data_23)
liq.24 <- getLiquidityMeasures(tq_data_24)

qs.hr <- c()
qs.hr[1] <- mean(as.numeric(liq.1$quotedSpread))
qs.hr[2] <- mean(as.numeric(liq.2$quotedSpread))
qs.hr[3] <- mean(as.numeric(liq.3$quotedSpread))
qs.hr[4] <- mean(as.numeric(liq.4$quotedSpread))
qs.hr[5] <- mean(as.numeric(liq.5$quotedSpread))
qs.hr[6] <- mean(as.numeric(liq.6$quotedSpread))
qs.hr[7] <- mean(as.numeric(liq.7$quotedSpread))
qs.hr[8] <- mean(as.numeric(liq.8$quotedSpread))
qs.hr[9] <- mean(as.numeric(liq.9$quotedSpread))
qs.hr[10] <- mean(as.numeric(liq.10$quotedSpread))
qs.hr[11] <- mean(as.numeric(liq.11$quotedSpread))
qs.hr[12] <- mean(as.numeric(liq.12$quotedSpread))
qs.hr[13] <- mean(as.numeric(liq.13$quotedSpread))
qs.hr[14] <- mean(as.numeric(liq.14$quotedSpread))
qs.hr[15] <- mean(as.numeric(liq.15$quotedSpread))
qs.hr[16] <- mean(as.numeric(liq.16$quotedSpread))
qs.hr[17] <- mean(as.numeric(liq.17$quotedSpread))
qs.hr[18] <- mean(as.numeric(liq.18$quotedSpread))
qs.hr[19] <- mean(as.numeric(liq.19$quotedSpread))
qs.hr[20] <- mean(as.numeric(liq.20$quotedSpread))
qs.hr[21] <- mean(as.numeric(liq.21$quotedSpread))
qs.hr[22] <- mean(as.numeric(liq.22$quotedSpread))
qs.hr[23] <- mean(as.numeric(liq.23$quotedSpread))
qs.hr[24] <- mean(as.numeric(liq.24$quotedSpread))

plot(1:24, qs.hr, type="p", pch=20, col="blue", main="Quoted Spread", 
     xlab = "Hour", ylab = "Values", ylim = c(0,2.0))

es.hr <- c()
es.hr[1] <- mean(as.numeric(liq.1$effectiveSpread))
es.hr[2] <- mean(as.numeric(liq.2$effectiveSpread))
es.hr[3] <- mean(as.numeric(liq.3$effectiveSpread))
es.hr[4] <- mean(as.numeric(liq.4$effectiveSpread))
es.hr[5] <- mean(as.numeric(liq.5$effectiveSpread))
es.hr[6] <- mean(as.numeric(liq.6$effectiveSpread))
es.hr[7] <- mean(as.numeric(liq.7$effectiveSpread))
es.hr[8] <- mean(as.numeric(liq.8$effectiveSpread))
es.hr[9] <- mean(as.numeric(liq.9$effectiveSpread))
es.hr[10] <- mean(as.numeric(liq.10$effectiveSpread))
es.hr[11] <- mean(as.numeric(liq.11$effectiveSpread))
es.hr[12] <- mean(as.numeric(liq.12$effectiveSpread))
es.hr[13] <- mean(as.numeric(liq.13$effectiveSpread))
es.hr[14] <- mean(as.numeric(liq.14$effectiveSpread))
es.hr[15] <- mean(as.numeric(liq.15$effectiveSpread))
es.hr[16] <- mean(as.numeric(liq.16$effectiveSpread))
es.hr[17] <- mean(as.numeric(liq.17$effectiveSpread))
es.hr[18] <- mean(as.numeric(liq.18$effectiveSpread))
es.hr[19] <- mean(as.numeric(liq.19$effectiveSpread))
es.hr[20] <- mean(as.numeric(liq.20$effectiveSpread))
es.hr[21] <- mean(as.numeric(liq.21$effectiveSpread))
es.hr[22] <- mean(as.numeric(liq.22$effectiveSpread))
es.hr[23] <- mean(as.numeric(liq.23$effectiveSpread))
es.hr[24] <- mean(as.numeric(liq.24$effectiveSpread))

plot(1:24, es.hr, type="p", pch=20, col="blue", main="Effective Spread", 
     xlab = "Hour", ylab = "Values", ylim = c(0,2.0))

par(mar=c(5,4,0,2) + 0.1)
pr <- as.numeric(tq_data$PRICE)
dpr <- diff(pr)
covpr <- acf(dpr, lag.max=20, type="correlation", plot=FALSE)
plot(covpr, col="red", ylim = c(-0.05,1))
title("Autocorrelation of Price Changes",line=-1)

covpr <- acf(dpr, lag.max=20, type="covariance", plot=FALSE)
gamma0 <- sd(dpr)^2
gamma0
gamma1 <- covpr$acf[2]
gamma1
cparam <- sqrt(-covpr$acf[2])
cparam
sig2u <- gamma0 + 2*gamma1
sigu <- sqrt(sig2u)
sigu

par(mar=c(5,4,0,2) + 0.1)
tradeSigns <- getTradeDirection(tq_data)
acTS <- acf(tradeSigns, main="ACF trade signs")

p <- as.numeric(tq_data$PRICE)
dp <- diff(p)
deps <- diff(tradeSigns)
mids <- (as.numeric(tq_data$OFR) + as.numeric(tq_data$BID))/2
dm <- diff(mids)
(fit.lm <- lm(dp ~ dm + deps))
fit.lm$coeff[3]
title("ACF trade signs",line=-1)

realizedVar <- function(q){rCov(diff(p, lag=q, differences=1))/q}
rv_data <- NULL
for(q in 1:200){
  rv_data <- c(rv_data, realizedVar(q))
}

q5min <- n.trades*5/1440
q5min

rv5 = realizedVar(q5min)
rv5
sqrt(rv5)

rvRoll <- sig2u*n.trades
rvRoll
sigRoll <- sqrt(sig2u*n.trades)
sigRoll

plot(rv_data, type ="l", 
     main="Signature Plot for Prices + Roll")
abline(h=rv5,col="red")
abline(h=rvRoll,col="blue")