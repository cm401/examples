library("quantmod")
library("PerformanceAnalytics")

#Control variables for entering a trade
#Used to check for the level of contango / backwardation
#IF signal < signalLowLim then in contango and do a carry trade
#IF signal > signalUpperLim then in backwardation so do a reverse carry
#ELSE do nothing
signalLowLim <- 0.9
signalUpperLim <- 1.1

#Use volatility futures, shortdate vs medium dated
#VXX iPath S&P 500 VIX Short-Term Futures ETN (VXX)
#VXZ iPath S&P 500 VIX Mid-Term Futures ETN (VXZ)
symbolLst <- c("VXX","VXZ")

#Specify dates for downloading data, training models and running simulation
startDate = as.Date("2009-01-01") #Specify what date to get the prices from
hedgeTrainingStartDate = as.Date("2009-01-01") #Start date for training the hedge ratio
hedgeTrainingEndDate = as.Date("2009-05-01") #End date for training the hedge ratio
tradingStartDate = as.Date("2009-05-02") #Date to run the strategy from

### SECTION 1 - Download Data & Calculate Returns ###
#Download the data
symbolData <- new.env() #Make a new environment for quantmod to store data in
getSymbols(symbolLst, env = symbolData, src = "yahoo", from = startDate)

#Plan is to check for trade entry at the open, and exit the trade at the close
#So need to calculate the open to close return as they represens our profit or loss
#Calculate returns for VXX and VXZ
vxxRet <- (Cl(symbolData$VXX)/Op(symbolData$VXX))-1
colnames(vxxRet) <- "Ret"
symbolData$VXX <- cbind(symbolData$VXX,vxxRet)

vxzRet <- (Cl(symbolData$VXZ)/Op(symbolData$VXZ))-1
colnames(vxzRet) <- "Ret"
symbolData$VXZ <- cbind(symbolData$VXZ,vxzRet)

### SECTION 2 - Calculating the hedge ratio ###
#Want to work out a hedge ratio, so that we can remain Vega neutral (the futures contact are trading VEGA)
#Select a small amount of data for training the hedge model on
subVxx <- window(symbolData$VXX$Ret,start=hedgeTrainingStartDate ,end=hedgeTrainingEndDate)
subVxz <- window(symbolData$VXZ$Ret,start=hedgeTrainingStartDate ,end=hedgeTrainingEndDate)
datablock = na.omit(cbind(subVxx,subVxz))
colnames(datablock) <- c("VXX","VXZ")

#Simply linearly regress the returns of Vxx with Vxz
regression <- lm(datablock[,"VXZ"] ~ datablock[,"VXX"]) #Linear Regression

#Plot the regression
plot(x=as.vector(datablock[,"VXX"]), y=as.vector(datablock[,"VXZ"]), main=paste("Hedge Regression: XXZret =",regression$coefficient[2]," * RXXret + intercept"),
     xlab="Vxx Ret", ylab="Vxz ", pch=19)
abline(regression, col = 2 )
hedgeratio = regression$coefficient[2]


### SECTION 3 - Generate trading signals ###
#Generate Trading signal
#Check ratio to see if contango or backwarded volatility future
#If shortTermVega < midTermVega in contango so do carry trade
#If shortTermVega > midTermVega in backwardation so do reverse carry
#If VXX less than VXZ then want to short VXX and long VXZ

#Calculate the contango / backwardation signal
tSig <- Op(symbolData$VXX)/Op(symbolData$VXZ)
colnames(tSig) <- "Signal"

### SECTION 4 - Do the trading ###
#Generate the individual buy/sell signals for each of the futures contract
vxxSignal <- apply(tSig,1, function(x) {if(x<signalLowLim) { return (-1) } else { if(x>signalUpperLim) { return(1) } else { return (0) }}})
vxzSignal <- -1 * vxxSignal

#Strategy returns are simply the direction * the Open-to-Close return for the day
#Include the hedge ratio here so that we remain vega neutral
strategyReturns <- ((vxxSignal * symbolData$VXX$Ret) + hedgeratio * (vxzSignal * symbolData$VXZ$Ret) )
strategyReturns <- window(strategyReturns,start=tradingStartDate,end=Sys.Date(), extend = FALSE)
#Normalise the amount of money being invested on each trade so that we can compare to the S&P index later
strategyReturns <- strategyReturns * 1 / (1+abs(hedgeratio))
colnames(strategyReturns) <- "StrategyReturns"
#plot(cumsum(strategyReturns))

#SECTION 5
#### Performance Analysis ###

#Get the S&P 500 index data
indexData <- new.env()
startDate = as.Date("2009-01-01") #Specify what date to get the prices from
getSymbols("^GSPC", env = indexData, src = "yahoo", from = startDate)

#Calculate returns for the index
indexRet <- (Cl(indexData$GSPC)-lag(Cl(indexData$GSPC),1))/lag(Cl(indexData$GSPC),1)
colnames(indexRet) <- "IndexRet"
zooTradeVec <- cbind(as.zoo(strategyReturns),as.zoo(indexRet)) #Convert to zoo object
colnames(zooTradeVec) <- c("Vol Carry Trade","S&P500")
zooTradeVec <- na.omit(zooTradeVec)
#Lets see how all the strategies faired against the index
dev.new()
charts.PerformanceSummary(zooTradeVec,main="Performance of Volatility Carry Trade",geometric=FALSE)

#Lets calculate a table of montly returns by year and strategy
cat("Calander Returns - Note 13.5 means a return of 13.5%\n")
print(table.CalendarReturns(zooTradeVec))
#Calculate the sharpe ratio
cat("Sharpe Ratio")
print(SharpeRatio.annualized(zooTradeVec))
#Calculate other statistics
cat("Other Statistics")
print(table.CAPM(zooTradeVec[,"Vol Carry Trade"],zooTradeVec[,"S&P500"]))

dev.new()
#Lets make a boxplot of the returns
chart.Boxplot(zooTradeVec)

dev.new()
#Set the plotting area to a 2 by 2 grid
layout(rbind(c(1,2),c(3,4)))

#Plot various histograms with different overlays added
chart.Histogram(zooTradeVec, main = "Plain", methods = NULL)
chart.Histogram(zooTradeVec, main = "Density", breaks=40, methods = c("add.density", "add.normal"))
chart.Histogram(zooTradeVec, main = "Skew and Kurt", methods = c("add.centered", "add.rug"))
chart.Histogram(zooTradeVec, main = "Risk Measures", methods = c("add.risk"))
