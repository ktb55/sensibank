# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library(shinydashboard)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Transactions", icon = icon("receipt"), tabName = "transactions"),
        menuItem("Spending", icon = icon("dollar-sign"), tabName = "spending"),
        menuItem("Data", icon = icon("database"), tabName = "data")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                    box(
                        width = 6, status = "info", solidHeader = TRUE,
                        title = "Transactions vs Card Type",
                        plotOutput("transCardPlot")
                    ),
                    box(
                        width = 6, status = "info", solidHeader = TRUE,
                        title = "Transactions vs Day Type",
                        plotOutput("transDayPlot")
                    ),
                    box(
                        width = 6, status = "info", solidHeader = TRUE,
                        title = "Spending vs Card Type",
                        plotOutput("spendCardPlot")
                    ),
                    box(
                        width = 6, status = "info", solidHeader = TRUE,
                        title = "Spending vs Day Type",
                        plotOutput("spendDayPlot")
                    )
                )
        ),
        
        tabItem(tabName = "transactions",
                fluidRow(
                    box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Total Transactions by Date",
                        fluidRow(
                            column(2, selectInput("transLoc", "Locations", locations, multiple = TRUE, selected = locations)),
                            column(2, selectInput("transCard", "Card Type", c("Bank", "Off"), multiple = TRUE, selected = c("Bank", "Off"))),
                            column(3, dateRangeInput("transDates", "Date Range", firstDate, lastDate, min = firstDate, max = lastDate))
                            
                        ),
                        plotOutput("transactionPlot")
                        #plotOutput("transPlot")
                    ),
                    box(
                        width = 4, status = "info", solidHeader = TRUE,
                        title = "Increase Over Time",
                        tableOutput("increaseTable")
                    ),
                    
                    box(
                        width = 4, status = "info", solidHeader = TRUE,
                        title = "Decrease Over Time",
                        tableOutput("decreaseTable")
                    )
                    
                )
        ),
        
        tabItem(tabName = "spending",
                fluidRow(
                    box(
                        width = 8, status = "info", solidHeader = TRUE,
                        title = "Spending by Date",
                        fluidRow(
                            column(2, selectInput("spendLoc", "Locations", locations, multiple = TRUE, selected = locations)),
                            column(2, selectInput("spendCard", "Card Type", c("Bank", "Off"), multiple = TRUE, selected = c("Bank", "Off"))),
                            column(3, dateRangeInput("spendDates", "Date Range", firstDate, lastDate, min = firstDate, max = lastDate)),
                            column(2, selectInput("typeSpend", "Type of Spending", c("Total", "Average"), selected = "Total"))
                            
                        ),
                        plotOutput("spendPlot")
                    ),
                    box(
                        width = 4, status = "info", solidHeader = TRUE,
                        title = "Increase Over Time",
                        tableOutput("increaseSpendTable")
                    ),
                    
                    box(
                        width = 4, status = "info", solidHeader = TRUE,
                        title = "Decrease Over Time",
                        tableOutput("decreaseSpendTable")
                    )
                    
                )
        ),
        
        tabItem(tabName = "data",
                DT::dataTableOutput("mytable")
        )
    )
)

dashboardPage(
    dashboardHeader(title = "Sensibank"),
    sidebar,
    body
)
