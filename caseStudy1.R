## Libraries
library(scales)
library(forecast)
library(tsutils)
library(lattice)
library(ggplot2)
library(tidyr)
library(dplyr)

## Identifying locations where co-branded card is not being used as often

## First, get an idea with some quick visuals
par(mfrow = c(3,2))

for (i in 1:5) {
  with(filter(sensibank,Store.Location == locations[i]), {
    plot(Transaction.Date, Number.of.Bank.Card.Transactions, cex = 0.1, 
         main = locations[i], col = alpha('blue', 0.3), xlab = 'Year', ylab = 'Number of Transactions')
    points(Transaction.Date, Number.of.Off.Card.Transactions, col = alpha('red', 0.3), cex = 0.1)
    abline(lm(Number.of.Bank.Card.Transactions~Transaction.Date), col = 'blue', lwd = 2)
    abline(lm(Number.of.Off.Card.Transactions~Transaction.Date), col = 'red', lwd = 2)
  })
}
plot(1, type = 'n', axes = FALSE, xlab = NA, ylab = NA)
with(sensibank, legend('center', inset = 0, 
                       legend = c("Bank Card Transactions", "Off Card Transactions"), 
                       lty = 1, lwd = 2, col = c('blue', 'red'), bty = 'n'))
## Not being used as often: University Avenue
## Trending towards not being used as often: Airport, King Street
## Relatively Similar, but trending slightly in favour: Big Street, Mount Road
dev.copy(png, file = './figures/location_transactions.png')
dev.off()

sensibank_colnames <- colnames(sensibank)[c(1:3,6:7)]
sensi <- gather(sensibank, Transaction.Type, Number.of.Transactions,-sensibank_colnames)
sensi <- mutate(sensi,Transaction.Type = factor(Transaction.Type, labels = c("Bank Card", "Off Card")))

transactionPlot <- ggplot(sensi, aes(Transaction.Date, Number.of.Transactions))
transactionPoints <- transactionPlot + geom_point(aes(color = Transaction.Type), alpha = 0.05) 
transactionPanels <- transactionPoints +  facet_grid(Store.Location~.) + coord_cartesian(ylim = c(0,200))
transactionTrends <- transactionPanels + geom_smooth(aes(color = Transaction.Type), method = 'lm', size = 2) 
transactionTrends + labs(x = "Date", y = "Number of Transactions", color = "Type of Transaction")
dev.copy(png, file = './figures/location_transactions2.png', width  = 1000, height = 1000)
dev.off()
