library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for random distribution application 
shinyUI(dashboardPage(
  
  dashboardHeader(title = "Stock Visualization"),
  
  # dashboard sidebar 
  dashboardSidebar(
    
    div(class="info-card",
        
        div(class="info-card-text",
            h5("Mengran Jessie Cui"),
            h5("NYC Data Science Academy"),
            h5("New York, US")),
        img(src="stock_image.png", class="cover", alt="cover")
    ),
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction"),
      menuItem("Stock Price History", tabName = "StockPriceHistory"),
      menuItem("Stock Price Forecast", tabName = "StockPriceForecast"),
      menuItem("Stock Volume", tabName = "StockVolume"),
      menuItem("Portfolio Management", tabName = "PortfolioManagement")
    )
  ),
  
  # dashboard body 
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      
      
      # Page 1: Introduction  
      tabItem(tabName = "Introduction",
              # div(align = "center",
             
              # ),
              h2(align = "center","Stock Visualization in Time Series"),
              h3("This is a stock price and portfolio return visualization exercise, using the daily stock price and trading volume of all companies listed on SP500 from February 2013 to February 2018. I will use Shiny to display historical stock price trend, showcase stock price forecast, and visualize portfolio return based on different weights and 5 stocks of choice. "),
              
              h2(strong("Stock price trend")),
              h3("In this section, users are able to input the ticker of the company of their choice, select range of dates, and the type of price (open price / close price / high price / low price). The app will display the price using line chart to visualize the trend of stock price during the given time period."),

              h2(strong("Stock price forecast")),
              h3("In this section, users are able to input the ticker of the company of their choice, and visualize how far/close classical stock price forecasting methods, such as linear regression and SVM (support vector machine), predict the daily stock price from March 2017 to February 2018, using the historical daily price from February 2013 to March 2017 as training data."),
              
              h2(strong("Stock volume")),
              h3("In this section, users are able to visualize the historical monthly trading volume of each stock that has been listed on SP 500 from February 2013 to February 2018. Monthly trading volume heat indice is calculated as the total trading volume of a given stock divided by the total trading volume of all SP500 stocks of that month. "),
              
              h2(strong("Portfolio management")),
              h3("In this section, users are able to build a portfolio consisting for 5 stocks of their choice, and assign different weights to each stock. Users are also able to select range of dates. The app will display the hypothetical portfolio return of the 5 stocks during the given time frame."),
              h3("Please note: this project is a live project that is still under construction, improvement, and updates as of this time.")
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
                    checkboxGroupInput("showfiger", "Show history for",  selected = "open", choices = c("open", "high", "low", "close"))),
                box(width = 9,
                    plotlyOutput("line_plot"),
                    plotlyOutput("hist_plot"),
                    plotlyOutput("bar_plot"),
                    dataTableOutput("summary")
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
                    selectizeInput(inputId = "ForMath", label = "Prediction model:",selected="Linear",choices=c("Linear","SVM"))
                ),
                
                box(width = 9,
                    plotlyOutput("plot_for"),
                    dataTableOutput("quality")
                )
              )
      ),
      
      
      # Page 4: Stock Volume 
      tabItem(tabName = "StockVolume",
              # h2(align = "center","Stock Volume Heatmap")
              fluidRow(column(
                width = 12,
                align = 'center',
                box(
                  title = strong('Stock Volume Heatmap'),
                  status = 'primary',
                  solidHeader = T,
                  width = NULL,
                  img(src = 'gplot.png',
                      width = 1350, align = "center")
                )))),
              
   
      # Page 5: Portfolio Management 
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
                                   min = "2007-01-01", max = "2017-07-31")
                ),
                
                box(width = 9,
                    plotlyOutput("plot_mana")
                )
              )
      )
      
      
      
      
    )
    
  )
  
  
  
))

