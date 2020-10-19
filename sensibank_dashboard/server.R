
library(shinydashboard)
library(ggplot2)
library(dplyr)
sensiDataAvg <- readRDS(file = "sensiDataAvg.rds")
library(plyr)


shinyServer(function(input, output) {
    ## Dashboard Tab
    
    output$transCardPlot <- renderPlot(
        ggplot(sensiDataAvg, aes(Total.Transactions)) +## Plot Date vs Transactions
            geom_histogram(aes(fill = Card.Type), color = "black") + 
            labs(x = "Total Transactions", fill = "Card Type") ## Add labels
    )
    
    output$transDayPlot <- renderPlot(
        ggplot(sensiDataAvg, aes(Total.Transactions)) +## Plot Date vs Transactions
            geom_histogram(aes(fill = Day.Type), color = "black") + 
            labs(x = "Total Transactions", fill = "Day Type") ## Add labels
    )
    
    output$spendCardPlot <- renderPlot(
        ggplot(sensiDataAvg, aes(Total.Spend)) +## Plot Date vs Transactions
            geom_histogram(aes(fill = Card.Type), color = "black") + 
            labs(x = "Total Transactions", fill = "Card Type") ## Add labels
    )
    
    output$spendDayPlot <- renderPlot(
        ggplot(sensiDataAvg, aes(Total.Spend)) +## Plot Date vs Transactions
            geom_histogram(aes(fill = Day.Type), color = "black") + 
            labs(x = "Total Transactions", fill = "DayType") ## Add labels
    )

    ## Transaction Tab
    df <- reactive({
        df <- sensiDataAvg[sensiDataAvg$Store.Location == input$transLoc[1], ]
        if(input$transLoc > 1){
            for (i in 2:length(input$transLoc)) {
                tmp <- sensiDataAvg[sensiDataAvg$Store.Location == input$transLoc[i], ]
                df <- rbind(df, tmp)
            }  
        }
        if(length(input$transCard) == 1){
            df <- df[df$Card.Type == input$transCard[1],]
        }
        df <- df[df$Transaction.Date >= input$transDates[1] & df$Transaction.Date <= input$transDates[2],]
        df

    })
    
    slope <- reactive({
        slopeData <- ddply(df(),.(Store.Location, Card.Type),function(df)  ## Split sensi dataset by store location and transaction type and apply function
            data.frame(slope=format(signif(coef(lm(Total.Transactions~Transaction.Date,data=df))[2],2), ## Record slope
                                    scientific=-2), y = ifelse(coef(lm(Total.Transactions~Transaction.Date,data=df))[2]<0,  ## Record y coord location
                                                               min(predict(lm(Total.Transactions~Transaction.Date,data=df))),
                                                               max(predict(lm(Total.Transactions~Transaction.Date,data=df))))))
        slopeData<- slopeData %>%
            select(Store.Location, Card.Type, slope) %>%
            mutate(slope = as.numeric(slope)*100) %>% ## Transform slope into percentage
            mutate(slope = as.character(slope)) %>% ## Transform back to character 
            arrange(desc(slope))
        
        slopeData
    
    })
    
    output$transactionPlot <- renderPlot(
        
       ggplot(df(), aes(Transaction.Date, Total.Transactions)) + ## Plot Date vs Transactions
           geom_smooth(aes(color = Store.Location, linetype = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
           labs(x = "Date", y = "Number of Transactions", color = "Store Location", linetype = "Card Type"),  ## Add labels
    )
    
    output$increaseTable <- renderTable(slope()[slope()$slope > 0,], colnames = FALSE)
    
    output$decreaseTable <- renderTable(slope()[slope()$slope <= 0,], colnames = FALSE)
    
    
    ## Spend Tab
    
    dfSpend <- reactive({
        df <- sensiDataAvg[sensiDataAvg$Store.Location == input$spendLoc[1], ]
        if(input$spendLoc > 1){
            for (i in 2:length(input$spendLoc)) {
                tmp <- sensiDataAvg[sensiDataAvg$Store.Location == input$spendLoc[i], ]
                df <- rbind(df, tmp)
            }  
        }
        if(length(input$spendCard) == 1){
            df <- df[df$Card.Type == input$spendCard[1],]
        }
        df <- df[df$Transaction.Date >= input$spendDates[1] & df$Transaction.Date <= input$spendDates[2],]
        df
        
    })
    
    slopeSpend <- reactive({
        
        if(input$typeSpend == "Total"){
            slopeData <- ddply(dfSpend(),.(Store.Location, Card.Type),function(dfSpend)  ## Split sensi dataset by store location and transaction type and apply function
                data.frame(slope=format(signif(coef(lm(Total.Spend~Transaction.Date,data=dfSpend))[2],2), ## Record slope
                                        scientific=-2), y = ifelse(coef(lm(Total.Spend~Transaction.Date,data=dfSpend))[2]<0,  ## Record y coord location
                                                                   min(predict(lm(Total.Spend~Transaction.Date,data=dfSpend))),
                                                                   max(predict(lm(Total.Spend~Transaction.Date,data=dfSpend))))))
        }else{
            slopeData <- ddply(dfSpend(),.(Store.Location, Card.Type),function(dfSpend)  ## Split sensi dataset by store location and transaction type and apply function
                data.frame(slope=format(signif(coef(lm(Average.Spend~Transaction.Date,data=dfSpend))[2],2), ## Record slope
                                        scientific=-2), y = ifelse(coef(lm(Average.Spend~Transaction.Date,data=dfSpend))[2]<0,  ## Record y coord location
                                                                   min(predict(lm(Average.Spend~Transaction.Date,data=dfSpend))),
                                                                   max(predict(lm(Average.Spend~Transaction.Date,data=dfSpend))))))
        }
        
        slopeData<- slopeData %>%
            select(Store.Location, Card.Type, slope) %>%
            mutate(slope = as.numeric(slope)*100) %>% ## Transform slope into percentage
            mutate(slope = as.character(slope)) %>% ## Transform back to character 
            arrange(slope)
        
        slopeData
    })
    
    output$spendPlot <- renderPlot(
        if(input$typeSpend == "Total"){
            ggplot(dfSpend(), aes(Transaction.Date, Total.Spend)) + ## Plot Date vs Transactions
                geom_smooth(aes(color = Store.Location, linetype = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
                labs(x = "Date", y = "Total Spending", color = "Store Location", linetype = "Card Type")  ## Add labels 
        }else{
            ggplot(dfSpend(), aes(Transaction.Date, Average.Spend)) + ## Plot Date vs Transactions
                geom_smooth(aes(color = Store.Location, linetype = Card.Type), method = 'lm', size = 2) + ## Add linear fit line and confidence interval (will not be seen as large point/not too much variance)
                labs(x = "Date", y = "Average Spending", color = "Store Location", linetype = "Card Type")  ## Add labels
        }
        
    )
    
    output$increaseSpendTable <- renderTable(slopeSpend()[slopeSpend()$slope > 0,], colnames = FALSE)
    
    output$decreaseSpendTable <- renderTable(slopeSpend()[slopeSpend()$slope <= 0,], colnames = FALSE)
    
    ## Data Tab
    output$mytable = DT::renderDataTable({
        sensiDataAvg
    })

    })
