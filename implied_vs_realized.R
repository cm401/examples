library(downloader)
library(quantmod)
# download data from Bloomberg

# The strategy was presented by TradingTheOdds in this post. I had to replicate it for myself to be sure it worked as 
# advertised, but unless I have something horrendously incorrect, this strategy works…quite well. Here’s the strategy:
# [http://www.tradingtheodds.com/2014/11/ddns-volatility-risk-premium-strategy-revisited-2/]
#   
# Using the actual S&P 500 index, compute the two-day annualized historical volatility. Subtract that from the VXMT, 
# which is the six-month expected volatility of the S&P 500 (prior to 2008, use the actual VIX). Then, take the 5-day 
# SMA of that difference. If this number is above 0, go long XIV, otherwise go long VXX. In my replication, the 
# strategy uses market-on-close orders (AKA observe “near” the close, buy at the close), so the strategy should be 
# taken with a little bit of a grain of salt. 

download("https://dl.dropboxusercontent.com/s/jk6der1s5lxtcfy/XIVlong.TXT",
         destfile="longXIV.txt")

download("https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/VXXlong.TXT", 
         destfile="longVXX.txt") #requires downloader package

download("https://dl.dropboxusercontent.com/s/950x55x7jtm9x2q/vxmtdailyprices.csv", 
         destfile="vxmtdailyprices.csv") #requires downloader package

xiv <- xts(read.zoo("longXIV.txt", format="%Y-%m-%d", sep=",", header=TRUE))
vxx <- xts(read.zoo("longVXX.txt", format="%Y-%m-%d", sep=",", header=TRUE))
vxmt <- xts(read.zoo("vxmtdailyprices.csv", format="%Y-%m-%d", sep=",", header=TRUE))

getSymbols("^VIX", from="2004-03-29")

vixvxmt <- merge(Cl(VIX), Cl(vxmt))
vixvxmt[is.na(vixvxmt[,2]),2] <- vixvxmt[is.na(vixvxmt[,2]),1]

getSymbols("^GSPC", from="1990-01-01")
spyRets <- diff(log(Cl(GSPC)))

spyVol <- runSD(spyRets, n=2)
annSpyVol <- spyVol*100*sqrt(252)

vols <- merge(vixvxmt[,2], annSpyVol, join='inner')
vols$smaDiff <- SMA(vols[,1] - vols[,2], n=5)
vols$signal <- vols$smaDiff > 0
vols$signal <- lag(vols$signal, k = 1)

xivRets <- Return.calculate(Cl(xiv))
vxxRets <- Return.calculate(Cl(vxx))
stratRets <- vols$signal*xivRets + (1-vols$signal)*vxxRets
stratRets[is.na(stratRets)]<-0

plot(log(cumprod(1+stratRets)))

# Performance stats
charts.PerformanceSummary(stratRets)

stats <- data.frame(cbind(Return.annualized(stratRets)*100, 
                          maxDrawdown(stratRets)*100, 
                          SharpeRatio.annualized(stratRets)))

colnames(stats) <- c("Annualized Return", "Max Drawdown", "Annualized Sharpe")
stats$MAR <- as.numeric(stats[1])/as.numeric(stats[2])

vols$signal <- lag(vols$signal, k = 1)
vols$signal <- lag(vols$signal, k = 2)    # delay signal

charts.PerformanceSummary(stratRets)
