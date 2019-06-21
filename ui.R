library(shiny)
library(shinydashboard)

# Define UI for random distribution application 
shinyUI(dashboardPage(
  
  dashboardHeader(title = "Stock Visualization"),
  
  dashboardSidebar(
    
    div(class="info-card",
        img(src="Profile Pic (Sample).jpg", class="profile-image rounded-circle", alt="Jessie"),
        div(class="info-card-text",
            h5("Jessie"),
            h5("New York, US")),
        img(src="stock_image.png", class="cover", alt="cover")
    ),
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Stock Price History", tabName = "StockPriceHistory"),
      menuItem("Stock Price Forecast", tabName = "StockPriceForecast"),
      menuItem("Portfolio Management", tabName = "PortfolioManagement")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")
    ),
    tabItems(
      # Page 1: Introduction  
      tabItem(tabName = "Introduction",
              div(align = "center",
              img(src="Profile Pic (Sample).jpg",class="profile-image", alt="Jessie")
              ),
              h2(align = "center","Introduction"),
              h5("[testing]\n testing testing]"),
              h3(strong("Stock price trend")),
              h5("[testing testing testing]"),
              h3(strong("Stock price forecast")),
              h5("[testing testing testing]"),
              h3(strong("Portfolio management")),
              h5("[testing testing testing]")
      ),
      # Page 2: Stock History 
      tabItem(tabName = "StockPriceHistory",
              h1(align = "center",
                 "Stock Price History"),
              fluidRow(
                box(width = 3,align = "center",
                    textInput("TichkeInput", "Input Ticker:",value = "AAL"),
                    dateRangeInput("date", strong("Select Data Range:"), 
                                   start = "2013-02-12", end = "2013-04-14",
                                   min = "2007-01-01", max = "2017-07-31"),
                    checkboxGroupInput(inputId = "showfiger", label = "Show History For:",inline = TRUE,selected="open",choices=c("open","close","high","low")),
                    actionButton("go","Go")
                ),
                
                box(width = 9,
                    plotOutput("plot", height = 500, click='pl_click')
                )
              )
      ),
      # Page 3: Stock Forecast 
      tabItem(tabName = "StockPriceForecast",
              h1(align = "center",
                 "Stock Price Forecast"),
              fluidRow(
                box(width = 3,
                    textInput("TichkeInput_For", "Input Ticker:",value = "AAL"),
                    checkboxGroupInput(inputId = "ForMath", label = "Prediction model:",inline = FALSE,selected="linear",choices=c("linear","SVM","ARIMA")),
                    actionButton("go_for","See My Forecast")
                ),
                
                box(width = 9,
                    plotOutput("plot_for", height = 500, click='pl_for_click')
                )
              )
      ),
      # Page 4: Portfolio Management 
      tabItem(tabName = "PortfolioManagement", 
              h1(align = "center",
                 "Portfolio Management"),
              fluidRow(
                box(width = 3,align = "center",
                    fluidRow(
                      box(width = 6,
                    textInput("Stock_1", "Stock:",value = "AAL")
                      ),
                    box(width = 6,
                        numericInput("Weight_1", "Weight:",value = 0.2)
                    )
                    ),
                    fluidRow(
                      box(width = 6,
                          textInput("Stock_2", "Stock:",value = "APA")
                      ),
                      box(width = 6,
                          numericInput("Weight_2", "Weight:",value = 0.1)
                      )
                    ),
                    fluidRow(
                      box(width = 6,
                          textInput("Stock_3", "Stock:",value = "DAL")
                      ),
                      box(width = 6,
                          numericInput("Weight_3", "Weight:",value = 0.3)
                      )
                    ),
                    fluidRow(
                      box(width = 6,
                          textInput("Stock_4", "Stock:",value = "GS")
                      ),
                      box(width = 6,
                          numericInput("Weight_4", "Weight:",value = 0.2)
                      )
                    ),
                    fluidRow(
                      box(width = 6,
                          textInput("Stock_5", "Stock:",value = "WYNN")
                      ),
                      box(width = 6,
                          numericInput("Weight_5", "Weight:",value = 0.2)
                      )
                    ),
                    
                    
                    
                    dateRangeInput("date_mana", strong("Select Data Range:"), 
                                   start = "2013-02-12", end = "2013-04-14",
                                   min = "2007-01-01", max = "2017-07-31"),
                    actionButton("go_mana","Build My Portfolio")
                ),
                
                box(width = 9,
                    plotOutput("plot_mana", height = 500, click='pl_mana_click')
                )
              )
      )
      
      
      
      
    )
    
  )
  
  
  
))