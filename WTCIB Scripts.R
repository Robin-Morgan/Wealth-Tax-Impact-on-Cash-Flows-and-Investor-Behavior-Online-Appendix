library(ggplot2)
library(lubridate)
library(dplyr)

#note to user: first set wd

data <- read.csv("fred_monthly_195304_202012.csv", stringsAsFactors = FALSE)
data$OTR2 <- data$OTR/100
data$CGR2 <- data$CGR/100
class(data$Date)

data <- data[1:813,]

test <- data$Date
test2 <- as.Date(test, format = "%m/%d/%Y")

data$Date <- as.Date(data$Date, format="%m/%d/%Y")
summary(data$Date)

data$fixedforty <- 40
data$fixedfort2 <- data$fixedfort/100

data$WTR <- ((data$GS1/100)/(1+data$GS1/100))*data$CGR
data$WTR2 <- data$WTR/100

data$WTRF <- ((data$GS1/100)/(1+data$GS1/100))*data$fixedforty
data$WRTF2 <- data$WTRF/100

data$WTRReal <- ((data$FII5/100)/(1+data$FII5/100))*data$CGR
data$WTRReal2 <- ((data$FII5/100)/(1+data$FII5/100))*data$fixedforty

data$ITREquivTo3percWT <- 100*((0.03*(1+data$GS1/100))/(data$GS1/100))

plot(data$Date, data$WTR, type="l", ylim=c(0,6), main= "Figure 1: Wealth Taxes Equivalent to Top CG Rates for 1953-2020", ylab="Wealth Tax Rates (%)",
     xlab="", yaxs="i")
lines(data$Date, data$WTRF, type="l", col="red")
abline(h=c(1,2,3,4,5,6), col=rgb(0,0,0,0.15) )
legend("topright",legend=c("Equivalent to Historic CG Rates","Equivalent to 40% CG Rate"),
       text.col=c("black","red"),col=c("black","red"), lty=c(1,1))
title(sub = "Source: Federal Reserve Bank of St. Louis. Estimates calculated using 
      Nominal 1-Year Treasury  yields                                                                         .", line = 3.5)

xlab.ticks <- seq(as.Date("2003-1-1"), as.Date("2021-1-1"), by = "years")


plot(data$Date, data$WTRReal, type="l", ylim=c(-1,1.5), xlim=c(data$Date[598], data$Date[813]),main= "Figure 2: Wealth Taxes Equivalent to Top CG Rates for 2003-2020", ylab="Wealth Tax Rates (%)",
     xlab="", yaxs="i", xaxt="n")
lines(data$Date, data$WTRReal2, type="l", col="red")
abline(h=c(-1,-0.5,0,0.5,1,2,3,4,5,6), col=rgb(0,0,0,0.15) )
abline(h=0, col=rgb(0,0,0,0.3) )
axis(1, xlab.ticks, format(xlab.ticks, "%Y"), cex.axis = 1)
legend("topright",legend=c("Equivalent to Historic CG Rates","Equivalent to 40% CG Rate"),
       text.col=c("black","red"),col=c("black","red"), lty=c(1,1))
title(sub = "Source: Federal Reserve Bank of St. Louis. Estimates calculated using  
      Real 5-Year Treasury  yields (inflation-indexed Treasuries).                             ", line = 3.5)

plot(data$Date, data$ITREquivTo3percWT, type="l", ylim=c(0,3300), main= "Figure 3: Income Tax Rates Equivalent to a 3% Wealth Tax Rate for 1953-2020", ylab="Wealth Tax Rates (%)",
     xlab="", yaxs="i")
abline(h=100, col=rgb(0,0,0,0.2) )
title(sub = "Source: Federal Reserve Bank of St. Louis. Estimates calculated using
      Nominal 1-Year Treasury  yields.                                                                        ", line = 3.5)

