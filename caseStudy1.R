################################################################################
##                           casestudy1.R                                     ##
################################################################################
## In this script, we use sensibank data to answer the following:             ##
##  - Identifying locations where co-branded card is not being used as often  ##
##  - Identifying potential high-income areas                                 ##
##  - Identifying stores where their co-branded card usage has gone down over ##
##    time                                                                    ##
##  - Anything else that can help Sensibank evaluate the success of their     ##
##    co-branded card                                                         ##
################################################################################


## Libraries
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)

############################################################################
## Identifying locations where co-branded card is not being used as often ##
############################################################################

## First, get an idea with some quick visuals with primary dataset (for ease)

par(mfrow = c(3,2)) ## Set layout of plot panels

for (i in 1:5) {
  with(filter(sensibank,Store.Location == locations[i]), { ## Subset data based on Store Location
    plot(Transaction.Date, Number.of.Bank.Card.Transactions, cex = 0.1, 
         main = locations[i], col = alpha('blue', 0.3), xlab = 'Year', ylab = 'Number of Transactions') ## Plot Bank Card Transactions
    points(Transaction.Date, Number.of.Off.Card.Transactions, col = alpha('red', 0.3), cex = 0.1)  ## Add Off Card Transactions
    abline(lm(Number.of.Bank.Card.Transactions~Transaction.Date), col = 'blue', lwd = 2) ## Add linear fit line for bank card
    abline(lm(Number.of.Off.Card.Transactions~Transaction.Date), col = 'red', lwd = 2) ## Add linear fit line for off card
  })
}
plot(1, type = 'n', axes = FALSE, xlab = NA, ylab = NA) ## Add plot space for legend
with(sensibank, legend('center', inset = 0, 
                       legend = c("Bank Card Transactions", "Off Card Transactions"), 
                       lty = 1, lwd = 2, col = c('blue', 'red'), bty = 'n')) ## Add legend
## Findings:
## Not being used as often: University Avenue
## Trending towards not being used as often: Airport, King Street
## Relatively Similar, but trending slightly in favor: Big Street, Mount Road
## Would be useful to have a more in depth plot that contains percent increase/decrease over time

dev.copy(png, file = './figures/location_transactions.png') ## Copy plot to PNG file
dev.off()

## Create such a ggplot which includes better visuals and percent increase/decrease

## To do so, need to use tidy dataset from "transformData.R"
source("transformData.R", local = TRUE)

## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedat <- ddply(sensiData,.(Store.Location, Card.Type),function(df)  ## Split sensi dataset by store location and transaction type and apply function
  data.frame(slope=format(signif(coef(lm(Total.Transactions~Transaction.Date,data=df))[2],2), ## Record slope
                          scientific=-2), y = ifelse(coef(lm(Total.Transactions~Transaction.Date,data=df))[2]<0,  ## Record y coord location
             min(predict(lm(Total.Transactions~Transaction.Date,data=df))),
             max(predict(lm(Total.Transactions~Transaction.Date,data=df))))))
slopedat <- slopedat %>%
  mutate(slope = as.numeric(slope)*100) %>% ## Transform slope into percentage
  mutate(slope = as.character(slope)) ## Transform back to character

## Plot both data and slope data
transactionPlot <- ggplot(sensiData, aes(Transaction.Date, Total.Transactions)) ## Plot Date vs Transactions
tplot <- transactionPlot + geom_point(aes(color = Card.Type), alpha = 0.1) + ## Add two sets of points based on transaction type
  facet_grid(Store.Location~.) + coord_cartesian(ylim = c(0,200)) + ## Divide into multiple facets/panels based on store location
  geom_smooth(aes(color = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
  labs(x = "Date", y = "Number of Transactions", color = "Type of Card") + ## Add labels
  geom_text(data=slopedat,aes(x=as.Date('2018-01-01'),y=y,label=paste0(slope, '%')), cex = 4)  ## Annotate linear fit lines
tplot ## Show plot
dev.copy(png, file = './figures/location_transactions2.png', width  = 1000, height = 1000)  ## Copy to PNG file
dev.off()

##################################################################################
## Identifying stores where their co-branded card usage has gone down over time ##
##################################################################################

## This is also shown by the last plot:
## - Airport -1.8%
## - King Street -5.2%

## Could also be interesting to look at only the last year of data, as some
## locations seem to dip up

startDate <- max(sensiData$Transaction.Date) - 365  ## Find the last date in data and go back one year
startDate

sensiData2017 <- sensiData[sensiData$Transaction.Date > startDate,]

## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedat2017 <- ddply(sensiData2017,.(Store.Location, Card.Type),function(df)  ## Split sensi dataset by store location and transaction type and apply function
  data.frame(slope=format(signif(coef(lm(Total.Transactions~Transaction.Date,data=df))[2],2), ## Record slope
                          scientific=-2), y = ifelse(coef(lm(Total.Transactions~Transaction.Date,data=df))[2]<0,  ## Record y coord location
                                                     min(predict(lm(Total.Transactions~Transaction.Date,data=df))),
                                                     max(predict(lm(Total.Transactions~Transaction.Date,data=df))))))
slopedat2017 <- slopedat2017 %>%
  mutate(slope = as.numeric(slope)*100) %>% ## Transform slope into percentage
  mutate(slope = as.character(slope)) ## Transform back to character

## Plot both data and slope data
transactionPlot <- ggplot(sensiData2017, aes(Transaction.Date, Total.Transactions)) ## Plot Date vs Transactions
tplot <- transactionPlot + geom_point(aes(color = Card.Type), alpha = 0.1) + ## Add two sets of points based on transaction type
  facet_grid(Store.Location~.) + coord_cartesian(ylim = c(0,200)) + ## Divide into multiple facets/panels based on store location
  geom_smooth(aes(color = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
  labs(x = "Date", y = "Number of Transactions", color = "Type of Card") + ## Add labels
  geom_text(data=slopedat2017,aes(x=as.Date('2017-11-01'),y=y,label=paste0(slope, '%')), cex = 4)  ## Annotate linear fit lines
tplot ## Show plot
dev.copy(png, file = './figures/location_transactions2017.png', width  = 1000, height = 1000)  ## Copy to PNG file
dev.off()

## This shows that in the last year there have been the following changes:
## - Airport is making a small increase
## - King Street seems to be leveling out from decrease over time (perhaps time to input resources into this location to return to earlier performance)
## - Big Street performing better than in the past
## - Mount Road and University Avenue still performing relatively the same

#############################################
## Identifying potential high-income areas ##
#############################################

## First, get an idea with some quick visuals

par(mfrow = c(3,2)) ## Set layout of plot panels

for (i in 1:5) {
  with(filter(sensibank,Store.Location == locations[i]), { ## Subset data based on Store Location
    plot(Transaction.Date, Bank.Card.Spend, cex = 0.1, 
         main = locations[i], col = alpha('blue', 0.3), xlab = 'Year', ylab = 'Spending') ## Plot Bank Card Transactions
    points(Transaction.Date, Off.Card.Spend, col = alpha('red', 0.3), cex = 0.1)  ## Add Off Card Transactions
    abline(lm(Bank.Card.Spend~Transaction.Date), col = 'blue', lwd = 2) ## Add linear fit line for bank card
    abline(lm(Off.Card.Spend~Transaction.Date), col = 'red', lwd = 2) ## Add linear fit line for off card
  })
}
plot(1, type = 'n', axes = FALSE, xlab = NA, ylab = NA) ## Add plot space for legend
with(sensibank, legend('center', inset = 0, 
                       legend = c("Bank Card", "Off Card"), 
                       lty = 1, lwd = 2, col = c('blue', 'red'), bty = 'n')) ## Add legend
dev.copy(png, file = './figures/location_spend.png', width  = 1000, height = 1000)  ## Copy to PNG file
dev.off()
## Observation: May be potential for higher-income at University Avenue

## Need to decide some metrics for potential income gain:
## - average transaction amount (potential for greater spend with more transactions)
## - decrease in spend over time (potential for return to larger spend per day)
## - 
