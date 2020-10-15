################################################################################
##                             explore.R                                      ##
################################################################################
## Explore sensibank data and data checking.                                  ##
################################################################################

## Look at different columns
summary(sensibank)

## Check for missing values
any(is.na(sensibank)) # FALSE

## Check for typos
locations <- unique(sensibank$Store.Location) # Five distinct locations

## Check for outliers/erroneous data
par(mfrow = c(2,2))
hist(sensibank$Number.of.Bank.Card.Transactions)
hist(sensibank$Number.of.Off.Card.Transactions)
hist(sensibank$Bank.Card.Spend)
hist(sensibank$Off.Card.Spend)
## All datapoints > 0 and within reasonable limits
