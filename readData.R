################################################################################
##                             readData.R                                     ##
################################################################################
## Reads in sensibank csv and assigns appropriate classes to columns          ##
################################################################################

## Libraries
library("dplyr")

## Read in data
sensibank <- read.csv("./data/sensibank-aggregate-data.csv", header = TRUE) %>%
                select(-Weekday) %>%
                mutate(Transaction.Date = as.Date(Transaction.Date), 
                       Working.Day = factor(Working.Day, levels = c('H', 'W'), 
                                            labels = c('Holiday', 'Work Day')))

