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
any(is.na(sensiData)) ## Returns FALSE, thus no NAs

## Add column to data for average spend based on transaction number
any(sensiData$Total.Transactions == 0) ## TRUE, some total transactions = 0
sum(sensiData$Total.Spend[which(sensiData$Total.Transactions == 0)]) ## Output = 0, when trans = 0, spend = 0
sensiDataAvg <- mutate(sensiData, Average.Spend = ifelse(Total.Transactions == 0, 0, round(Total.Spend/Total.Transactions, digits = 2)))

any(is.na(sensiDataAvg)) # Returns FALSE

## Create data frame to check correlations by setting factor columns to numeric
sensiDataCorr <- sensiDataAvg %>%
  select(-Transaction.Date) %>%
  mutate(Store.Location = factor(Store.Location)) %>%
  mutate(Store.Location = as.numeric(Store.Location), Day.Type = as.numeric(Day.Type), Card.Type = as.numeric(Card.Type))
cor(sensiDataCorr) 

## Shows correlation between:
## - Store Location and Transactions
## - Store Location and Spend
## - Day Type and Average Spend
## - Card Type and Transactions
## - Card Type and Spend
## - Card Type and Average Spend

saveRDS(sensiDataAvg, file = "./sensibank_dashboard/sensiDataAvg.rds")
