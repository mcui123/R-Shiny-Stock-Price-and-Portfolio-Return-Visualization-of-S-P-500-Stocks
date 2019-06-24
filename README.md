# NYCDSA

This is a stock price and portfolio return visualization exercise, using the 
daily stock price of all companies listed on SP500 from Feb 2013 to Feb 2018. 


I will use Shiny to 
(1) Display historical stock price trend [Stock History]
(2) Showcase stock price estimation [Stock Forecast], 
(3) Visualize stock volume using heatmap, and 
(4) Visualize portfolio return based on different weights and 5 stocks of choice [Portfolio Management] 



This is a stock price and portfolio return visualization exercise, using the daily stock price and trading volume of all companies listed on SP500 from February 2013 to February 2018. I will use Shiny to display historical stock price trend, showcase stock price forecast, and visualize portfolio return based on different weights and 5 stocks of choice.

[Stock price trend]
In this section, users are able to input the ticker of the company of their choice, select range of dates, and the type of price (open price / close price / high price / low price). The app will display the price using line chart to visualize the trend of stock price during the given time period.

[Stock price forecast]
In this section, users are able to input the ticker of the company of their choice, and visualize how far/close classical stock price forecasting methods, such as linear regression and ARIMA, predict the daily stock price from March 2017 to February 2018, using the historical daily price from February 2013 to March 2017 as training data. ARIMA stands for Auto Regressive Integrated Moving Average, which is a classic model for time series forecasting, especially for stock price.

[Stock volume]
In this section, users are able to visualize the historical monthly trading volume of each stock that has been listed on SP 500 from February 2013 to February 2018. Monthly trading volume heat indice is calculated as the total trading volume of a given stock divided by the total trading volume of all SP500 stocks of that month.

[Portfolio management]
In this section, users are able to build a portfolio consisting for 5 stocks of their choice, and assign different weights to each stock. Users are also able to select range of dates. The app will display the hypothetical portfolio return of the 5 stocks during the given time frame.

Please note: this project is a live project that is still under construction, improvement, and updates as of this time.
