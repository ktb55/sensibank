################################################################################
##                           transformData.R                                  ##
################################################################################
## Transform data to make tidier dataset.                                     ##
################################################################################

## Gather transaction columns
sensibank_colnames <- colnames(sensibank)[c(1:3)] ## Get column names that will not be gathered
sensiTransactions <- sensibank[c(1:5)] %>% ## Take out spend columns
  gather(Card.Type, Total.Transactions,-sensibank_colnames) %>% ## Create new dataset with transactions gathered
  mutate(Card.Type = factor(Card.Type, labels = c("Bank", "Off"))) ## Change transaction type into factor class

## Gather spend columns
sensiSpend <- sensibank[,c(1:3,6:7)] %>% ## Take out transaction columns
  gather(Card.Type, Total.Spend,-sensibank_colnames) %>% ## Create new dataset with transactions gathered
  mutate(Card.Type = factor(Card.Type, labels = c("Bank", "Off"))) ## Change transaction type into factor class


##  Inner join data frames together
sensiData <- merge(sensiTransactions,sensiSpend, by = c("Store.Location", "Transaction.Date", "Day.Type", "Card.Type")) %>%
  arrange(Store.Location, Transaction.Date)

## Check for NAs
any(is.na(sensiData)) # Returns FALSE, thus no NAs
