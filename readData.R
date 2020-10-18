################################################################################
##                             readData.R                                     ##
################################################################################
## Reads in sensibank csv and assigns appropriate classes to columns          ##
################################################################################

## Libraries
library(dplyr)

## Read in data
sensibank <- read.csv("./data/sensibank-aggregate-data.csv", header = TRUE) %>%
                select(-Weekday) %>%
                rename(Day.Type = Working.Day) %>%
                mutate(Transaction.Date = as.Date(Transaction.Date), 
                       Day.Type = factor(Day.Type, levels = c('H', 'W'), 
                                            labels = c('Holiday', 'Work Day')),
                       Number.of.Bank.Card.Transactions = as.integer(Number.of.Bank.Card.Transactions),
                       Off.Card.Spend = as.numeric(Off.Card.Spend))
                


