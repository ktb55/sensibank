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
source("slopedata.R")
slopedat <- slopedata(sensiData, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)

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


## Chart data
source("range11.R")
transactionsVsDate <- slopedat %>%
  select(-y) %>%
  rename(Total.Increase = slope) %>%
  mutate(Total.Increase = as.numeric(Total.Increase), Relative.Total.Increase = round(range11(Total.Increase)*100, digits = 0))


##################################################################################
## Identifying stores where their co-branded card usage has gone down over time ##
##################################################################################

## This is also shown by the last plot:
## - Airport -1.8%
## - King Street -5.2%

## Could also be interesting to look at only the last year of data, as some
## locations seem to dip up

year = 365
lastDate <- max(sensiData$Transaction.Date)  ## Find the last date in data and go back one year
lastDate

sensiData2017 <- sensiData[sensiData$Transaction.Date > '2016-12-31' ,]
sensiData2016 <- sensiData[sensiData$Transaction.Date > '2015-12-31' & sensiData$Transaction.Date >= '2016-12-31',]
sensiData2015 <- sensiData[sensiData$Transaction.Date > '2014-12-31' & sensiData$Transaction.Date >= '2015-12-31',]
sensiData2014 <- sensiData[sensiData$Transaction.Date > '2013-12-31' & sensiData$Transaction.Date >= '2014-12-31',]
sensiData2013 <- sensiData[sensiData$Transaction.Date > '2012-12-31' & sensiData$Transaction.Date >= '2013-12-31',]
sensiData2012 <- sensiData[sensiData$Transaction.Date > '2011-12-31' & sensiData$Transaction.Date >= '2012-12-31',]
sensiData2011 <- sensiData[sensiData$Transaction.Date <= '2011-12-21',]


## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedat2017 <- slopedata(sensiData2017, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2016 <- slopedata(sensiData2016, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2015 <- slopedata(sensiData2015, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2014 <- slopedata(sensiData2014, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2013 <- slopedata(sensiData2013, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2012 <- slopedata(sensiData2012, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)
slopedat2011 <- slopedata(sensiData2011, .(Store.Location, Card.Type), Total.Transactions~Transaction.Date)


## Plot both data and slope data for 2017
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

## Add to chart data

transactionsVsDate <- transactionsVsDate %>%
  mutate(Increase.2017 = round(as.numeric(slopedat2017$slope), digits = 2)) %>%
  mutate(Relative.Increase.2017 = round(range11(Increase.2017)*100, digits = 0)) %>%
  mutate(Increase.2016 = round(as.numeric(slopedat2016$slope), digits = 2)) %>%
  mutate(Relative.Increase.2016 = round(range11(Increase.2016)*100, digits = 0)) %>%
  mutate(Increase.2015 = round(as.numeric(slopedat2015$slope), digits = 2)) %>%
  mutate(Relative.Increase.2015 = round(range11(Increase.2015)*100, digits = 0)) %>%
  mutate(Increase.2014 = round(as.numeric(slopedat2014$slope), digits = 2)) %>%
  mutate(Relative.Increase.2014 = round(range11(Increase.2014)*100, digits = 0)) %>%
  mutate(Increase.2013 = round(as.numeric(slopedat2013$slope), digits = 2)) %>%
  mutate(Relative.Increase.2013 = round(range11(Increase.2013)*100, digits = 0)) %>%
  mutate(Increase.2012 = round(as.numeric(slopedat2012$slope), digits = 2)) %>%
  mutate(Relative.Increase.2012 = round(range11(Increase.2012)*100, digits = 0)) %>%
  mutate(Increase.2011 = round(as.numeric(slopedat2011$slope), digits = 2)) %>%
  mutate(Relative.Increase.2011 = round(range11(Increase.2011)*100, digits = 0)) %>%
  mutate(Store.Location = factor(Store.Location))


relativeTransactions <- transactionsVsDate %>%
  select(colnames(transactionsVsDate)[c(1:2, seq(6,18, by = 2))]) %>%
  gather(Year, Relative.Increase, -colnames(transactionsVsDate)[1:2]) %>%
  mutate(Year = as.numeric(gsub("Relative.Increase.","", Year)))

## Plot relative increases
relativePlot <- ggplot(relativeTransactions, aes(Year, Relative.Increase)) ## Plot Date vs Transactions
rplot <- relativePlot + geom_line(aes(linetype = Card.Type, color = Store.Location)) + 
  labs(x = "Year", y = "Relative Increase", color = "Store Location", linetype = "Card Type") ## Add labels
rplot ## Show plot
dev.copy(png, file = './figures/relative_increases.png')  ## Copy to PNG file
dev.off()

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
## - average spend amount (potential for greater spend with more transactions)
## - decrease in spend over time (potential for return to larger spend per day)
## - high spend, but decrease in transactions over time (potential to return to higher transactions, thus higher income)
## - higher spend on off card
## - high spend on off card, with high transactions on bank card (potentional to get higher spend on bank card)

## Add column to data for average spend based on transaction number - sensiDataAvg

## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedatavg <- slopedata(sensiDataAvg, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)

## Plot both data and slope data
avgPlot <- ggplot(sensiDataAvg, aes(Transaction.Date, Average.Spend)) ## Plot Date vs Transactions
avgplot <- avgPlot + geom_point(aes(color = Card.Type), alpha = 0.1) + ## Add two sets of points based on transaction type
  facet_grid(Store.Location~.) + #coord_cartesian(ylim = c(0,200)) + ## Divide into multiple facets/panels based on store location
  geom_smooth(aes(color = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
  labs(x = "Date", y = "Average Spend", color = "Type of Card") + ## Add labels
  geom_text(data=slopedatavg,aes(x=as.Date('2018-02-01'),y=y,label=paste0(slope, '%')), cex = 4)  ## Annotate linear fit lines
avgplot ## Show plot
dev.copy(png, file = './figures/location_spendAverage.png', width  = 1000, height = 1000)  ## Copy to PNG file
dev.off()

## Observations:
## - All locations average a high spend on bank card than off card
## - This means that locations that are under-performing in total spend, are lacking more in transactions than expenditure
## - This shows a need to focus efforts more on transactions in specific locations
## - The last year shows interesting changes in average spend

## Again, would be interesting to view last year of spending and average spending

sensiDataAvg2017 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2016-12-31' ,]

## Create data frame to hold slope and y coord location for annotating linear fit lines later
slopedatavg2017 <- slopedata(sensiDataAvg2017, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)

## Plot both data and slope data for 2017
avgPlot2017 <- ggplot(sensiDataAvg2017, aes(Transaction.Date, Average.Spend)) ## Plot Date vs Transactions
avgplot2017 <- avgPlot2017 + geom_point(aes(color = Card.Type), alpha = 0.1) + ## Add two sets of points based on transaction type
  facet_grid(Store.Location~.) + #coord_cartesian(ylim = c(0,200)) + ## Divide into multiple facets/panels based on store location
  geom_smooth(aes(color = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
  labs(x = "Date", y = "Average Spend", color = "Type of Card") + ## Add labels
  geom_text(data=slopedatavg2017,aes(x=as.Date('2017-11-01'),y=y,label=paste0(slope, '%')), cex = 4)  ## Annotate linear fit lines
avgplot2017 ## Show plot
dev.copy(png, file = './figures/location_spendAverage2017.png', width  = 1000, height = 1000)  ## Copy to PNG file
dev.off()

## Shows a more dramatic rise in average spending in the last year

## Chart data

sensiDataAvg2016 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2015-12-31' & sensiDataAvg$Transaction.Date >= '2016-12-31',]
sensiDataAvg2015 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2014-12-31' & sensiDataAvg$Transaction.Date >= '2015-12-31',]
sensiDataAvg2014 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2013-12-31' & sensiDataAvg$Transaction.Date >= '2014-12-31',]
sensiDataAvg2013 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2012-12-31' & sensiDataAvg$Transaction.Date >= '2013-12-31',]
sensiDataAvg2012 <- sensiDataAvg[sensiDataAvg$Transaction.Date > '2011-12-31' & sensiDataAvg$Transaction.Date >= '2012-12-31',]
sensiDataAvg2011 <- sensiDataAvg[sensiDataAvg$Transaction.Date <= '2011-12-21',]

slopedatavg2016 <- slopedata(sensiDataAvg2016, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)
slopedatavg2015 <- slopedata(sensiDataAvg2015, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)
slopedatavg2014 <- slopedata(sensiDataAvg2014, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)
slopedatavg2013 <- slopedata(sensiDataAvg2013, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)
slopedatavg2012 <- slopedata(sensiDataAvg2012, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)
slopedatavg2011 <- slopedata(sensiDataAvg2011, .(Store.Location, Card.Type), Average.Spend~Transaction.Date)

avgspendVsDate <- slopedatavg %>%
  select(-y) %>%
  rename(Total.Increase = slope) %>%
  mutate(Total.Increase = as.numeric(Total.Increase), Relative.Total.Increase = round(range11(Total.Increase)*100, digits = 0))

avgspendVsDate <- avgspendVsDate %>%
  mutate(Increase.2017 = round(as.numeric(slopedatavg2017$slope), digits = 2)) %>%
  mutate(Relative.Increase.2017 = round(range11(Increase.2017)*100, digits = 0)) %>%
  mutate(Increase.2016 = round(as.numeric(slopedatavg2016$slope), digits = 2)) %>%
  mutate(Relative.Increase.2016 = round(range11(Increase.2016)*100, digits = 0)) %>%
  mutate(Increase.2015 = round(as.numeric(slopedatavg2015$slope), digits = 2)) %>%
  mutate(Relative.Increase.2015 = round(range11(Increase.2015)*100, digits = 0)) %>%
  mutate(Increase.2014 = round(as.numeric(slopedatavg2014$slope), digits = 2)) %>%
  mutate(Relative.Increase.2014 = round(range11(Increase.2014)*100, digits = 0)) %>%
  mutate(Increase.2013 = round(as.numeric(slopedatavg2013$slope), digits = 2)) %>%
  mutate(Relative.Increase.2013 = round(range11(Increase.2013)*100, digits = 0)) %>%
  mutate(Increase.2012 = round(as.numeric(slopedatavg2012$slope), digits = 2)) %>%
  mutate(Relative.Increase.2012 = round(range11(Increase.2012)*100, digits = 0)) %>%
  mutate(Increase.2011 = round(as.numeric(slopedatavg2011$slope), digits = 2)) %>%
  mutate(Relative.Increase.2011 = round(range11(Increase.2011)*100, digits = 0)) %>%
  mutate(Store.Location = factor(Store.Location))

relativeSpend <- avgspendVsDate %>%
  select(colnames(avgspendVsDate)[c(1:2, seq(6,18, by = 2))]) %>%
  gather(Year, Relative.Increase, -colnames(avgspendVsDate)[1:2]) %>%
  mutate(Year = as.numeric(gsub("Relative.Increase.","", Year)))

## Plot relative increases
relativePlot <- ggplot(relativeSpend, aes(Year, Relative.Increase)) ## Plot Date vs Transactions
rplot <- relativePlot + geom_line(aes(linetype = Card.Type, color = Store.Location)) + 
  labs(x = "Year", y = "Relative Increase", color = "Store Location", linetype = "Card Type") ## Add labels
rplot ## Show plot
dev.copy(png, file = './figures/relative_spend_increases.png')  ## Copy to PNG file
dev.off()


#########################################################################
## Anything else that can help Sensibank evaluate the success of their ##
## co-branded card                                                     ##
#########################################################################

## Plot correlations

## Average Spend vs Type of Day
dayvsspend <- ggplot(sensiDataAvg, aes(Average.Spend)) ## Plot Date vs Transactions
dayVsSpend <- dayvsspend + geom_histogram(aes(fill = Day.Type), color = "black") + 
  labs(x = "Average Spend", fill = "Type of Day") ## Add labels
dayVsSpend ## Show plot
dev.copy(png, file = './figures/average_spend_day_type.png')  ## Copy to PNG file
dev.off()

## Average Spend vs Type of Day - split by card type
dayvsspendcard <- ggplot(sensiDataAvg, aes(Average.Spend)) ## Plot Date vs Transactions
dayVsSpendCard <- dayvsspendcard + geom_histogram(aes(fill = Day.Type), color = "black") + 
  facet_grid(Card.Type~.) + ## Split by card type
  labs(x = "Average Spend", fill = "Type of Day") ## Add labels
dayVsSpendCard ## Show plot
dev.copy(png, file = './figures/average_spend_day_type_card.png')  ## Copy to PNG file
dev.off()
## Spend higher on average for bank card, however, higher spending differential 
## between holiday and work day for off card --> perhaps can focus on raising 
## holiday spending on co-branded card.

## Card Type vs Transactions
cardvstrans <- ggplot(sensiDataAvg, aes(Total.Transactions)) ## Plot Date vs Transactions
cardVStrans <- cardvstrans + geom_histogram(aes(fill = Card.Type), color = "black") + 
  labs(x = "Total Transactions", fill = "Card Type") ## Add labels
cardVStrans ## Show plot
dev.copy(png, file = './figures/total_trans_card_type.png')  ## Copy to PNG file
dev.off()

## Card Type vs Spend
cardvsspend <- ggplot(sensiDataAvg, aes(Total.Spend)) ## Plot Date vs Transactions
cardVSspend <- cardvsspend + geom_histogram(aes(fill = Card.Type), color = "black") + 
  labs(x = "Total Spend", fill = "Card Type") ## Add labels
cardVSspend ## Show plot
dev.copy(png, file = './figures/total_spend_card_type.png')  ## Copy to PNG file
dev.off()

## Card Type vs Avg Spend
cardvsavgspend <- ggplot(sensiDataAvg, aes(Total.Spend)) ## Plot Date vs Transactions
cardVSavgspend <- cardvsavgspend + geom_histogram(aes(fill = Card.Type), color = "black") + 
  labs(x = "Average Spend", fill = "Card Type") ## Add labels
cardVSavgspend ## Show plot
dev.copy(png, file = './figures/average_spend_card_type.png')  ## Copy to PNG file
dev.off()

