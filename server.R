library(shiny)
library(e1071)
library(forecast)
library(dplyr)
library(ggplot2)
library(plotly)
library(rowr)
library(janitor) # This package is using for clean names, transfering all names into lowercase with underline

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
 # # prepare the data for stock history plotting
  # df <- read.csv("all_stocks_5yr.csv")
  # 
  # df <- df %>%
  #   clean_names() %>%
  #   mutate(date = as.Date(date,"%m/%d/%Y")) %>%
  #   mutate(increase  = ifelse(close >= open, 1, -1))
  # 
  # df_open <- df %>% select(date, open, volume, name, increase) %>% rename(value = open) %>% mutate(label = "open")
  # df_close <- df %>% select(date, close, volume, name, increase) %>% rename(value = close )%>% mutate(label = "close")
  # df_high <- df %>% select(date, high, volume, name, increase) %>% rename(value = high) %>% mutate(label = "high")
  # df_low <- df %>% select(date, low, volume, name, increase) %>% rename(value = low) %>% mutate(label = "low")
  # 
  # df <- rbind(df_open, df_close, df_high, df_low)
  # saveRDS(df, "df.rda")

  df <- readRDS("df.rda")

  #  Page 2: Stock History 
  data_select_Input <- reactive({
    data_select_Input <- df %>% 
     filter(date >= as.Date(input$date[1])) %>% 
     filter(date <= as.Date(input$date[2]))%>% 
     filter(name %in% input$TichkeInput) %>% 
     filter(label %in% input$showfiger)
    })
  
  
  # Stock Price History
  
  ## plot the line chart 
  output$line_plot <- renderPlotly({
    data_plot_line <- data_select_Input()
  plot_ly(data = data_plot_line,
          x = ~date,
          y = ~value,
          color = ~label,
          type = 'scatter', 
          mode = 'lines',
          hoverinfo = "text",
          text = ~c(paste(paste("Date:", date),'<br>',
                          paste('Price Type:', as.character(label)), '<br>',
                          paste('Value:', as.numeric(value)), '<br>',
                          paste("Volume:", format(round(volume), big.mark = ","))))) %>% 
    # add_trace(y = ~value, mode = 'markers') %>% 
    layout(title = paste("From", format(input$date[1]), "to", format(input$date[2]), "Stock Price History"),
           xaxis = list(title = "Date", tickfont = list(size = 9)),
           yaxis = list(title = "Stock Price"),
           legend = list(orientation = "h", y = -0.2))
          # barmode = "stack",
          # dragmode = "select")
  })
  
  
  # hist plot
  output$hist_plot <- renderPlotly({
    data_plot_line <- data_select_Input()
    plot_ly(data = data_plot_line,
            x = ~date,
            y = ~value,
            color = ~label,
            type = "bar",
            hoverinfo = "text",
            text = ~c(paste(paste("Date:", date),'<br>',
                            paste('Price Type:', as.character(label)), '<br>',
                            paste('Value:', as.numeric(value)), '<br>',
                            paste("Volume:", format(round(volume), big.mark = ","))))) %>% 
      layout(# title = paste("From", format(input$date[1]), "to", format(input$date[2]), "Stock Price History"),
             xaxis = list(title = "Date", tickfont = list(size = 9)),
             yaxis = list(title = "Stock Price"),
             legend = list(orientation = "h", y = -0.2))
    # barmode = "stack",
    # dragmode = "select")
  })
  
  
  
  
  # heatmap plot
  aa <- reactive({
    data_plot_line <- df %>% 
      filter(date >= as.Date(input$date[1])) %>% 
      filter(date <= as.Date(input$date[2]))%>% 
      filter(name %in% input$TichkeInput) 
    date_data <- as.data.frame(seq.Date(from = input$date[1], to = input$date[2], by =1 ))
    names(date_data) <- "date"
    full_data <- left_join(date_data, data_plot_line, by = c("date"))
    full_data[is.na(full_data)] <- 0
    data_list <- full_data %>% 
      as.data.frame %>% 
      split(c("open", "high", "low", "close"))
    
    bar_data_1 <- data.frame()
    bar_data_2 <- data.frame()
    bar_data_3 <- data.frame()
    bar_data_4 <- data.frame()
    if("open" %in% input$showfiger){
      bar_data_1 <- if(!is.null(data_list$open)){
        as.data.frame(as.data.frame(data_list$open)[,5])
      }else{
        NULL
      } 
    }else{
      NULL
    }

    if("close" %in% input$showfiger){
      bar_data_2 <- if(!is.null(data_list$close)){
        as.data.frame(as.data.frame(data_list$close)[,5])
      }else{
        NULL
      }
    }else{
      NULL
    }

    if("high" %in% input$showfiger){
      bar_data_3 <- if(!is.null(data_list$high)){
        as.data.frame(as.data.frame(data_list$high)[,5])
      }else{
        NULL
      }
    }else{
      NULL
    }
    
    if("low" %in% input$showfiger){
      bar_data_4 <- if(!is.null(data_list$low)){
        as.data.frame(as.data.frame(data_list$low)[,5])
      }else{
        NULL
      }
    }else{
      NULL
    }
  
    bar_data_1 <- ifelse(is.null(bar_data_1), NA, bar_data_1)
    bar_data_2 <- ifelse(is.null(bar_data_2), NA, bar_data_2)
    bar_data_3 <- ifelse(is.null(bar_data_3), NA, bar_data_3)
    bar_data_4 <- ifelse(is.null(bar_data_4), NA, bar_data_4)
    
    bar_data <- as.data.frame(cbind(bar_data_1, bar_data_2, bar_data_3, bar_data_4))
    names(bar_data) <- c("open", "close", "high", "low")
    bar_data <- as.matrix(t(as.matrix(bar_data)))

    bar_data
  })
  output$bar_plot <- renderPlotly({
    plot_data <- aa()
    plot_ly(z = plot_data,
            colors = c(""),
            type = 'heatmap',
            hoverinfo = F
            ) %>% 
      add_trace(showscale = FALSE)  %>% 
      add_trace(showlegend = FALSE) %>% 
      layout(title = paste("Stock history price changes, Rise (red) and Fall (blue)"),
             xaxis = list(title = "Date", tickfont = list(size = 9)),
             yaxis = list(title = "Open, Close, High, Low"))
  })
  
  
  
  
  
  ## plot the metric table
  summary_table <- reactive({
    md <- df %>% 
      filter(date >= as.Date(input$date[1])) %>% 
      filter(date <= as.Date(input$date[2]))%>% 
      filter(name %in% input$TichkeInput)
    
    day_counts <- md %>% 
      summarise(SP500_days = n_distinct(date))
    
    max_close_price_1 <- md %>% 
      filter(label == "close") %>%
      summarise(a = max(value, na.rm = T))
    
    min_close_price_1 <- md %>% 
      filter(label == "close")  %>%  
      summarise(a = min(value, na.rm = T))
    
    average_close_price_1 <- md %>% 
      filter(label == "close")  %>%  
      summarise(a = mean(value, na.rm = T))
    
    summary_table <- data.frame(number_of_days_SP500 = day_counts[1,1], max_close_price = max_close_price_1[1,1], 
                                min_close_price =min_close_price_1[1,1],
                                average_close_price = average_close_price_1[1,1])
    summary_table <- as.data.frame((summary_table))
  })
  
  output$summary <- renderDataTable({
    req(summary_table())
    
    summary_table()
  }, options = list(scrollX = TRUE))
  
  
  # Page 3:Stock Forecast 
  data_select_forecast <- reactive({
    data_select_forecast <- df %>% 
      mutate(rn =  date - min(date,na.rm= T)) %>% 
      filter(label %in% "close") %>% 
      filter(name %in% input$TichkeInput_For)  
    data_select_forecast %>% na.omit()
  })

  
  data_all <- reactive({
    data_all <- data_select_forecast()
    linear_model <- lm(data_all$value ~ data_all$volume + data_all$rn)
    result_linear <- as.data.frame(linear_model$fitted.values)
    names(result_linear) <- "linear"
    
    svm_model <- svm(value ~ volume + rn, data = data_all)
    result_svm <- as.data.frame(predict(svm_model, data_all %>% select(volume, rn) %>% mutate_all(as.numeric) %>% na.omit()))
    names(result_svm) <- "svm"
    data_all <- cbind(data_all, result_svm, result_linear)
    data_all

  })
  
    output$plot_for <- renderPlotly({
      forecast_data <- data_all()
      forecast_data_1 <- forecast_data %>% 
        filter(date < as.Date("2017-03-01")) %>% 
        mutate(linear = NA,
               svm = NA)
      forecast_data_2 <- forecast_data %>% 
        filter(date >= as.Date("2017-03-01"))
      forecast_data <- rbind(forecast_data_1, forecast_data_2)
      
      if(input$ForMath == "Linear"){
        plot_ly(data = forecast_data,
                x = ~ date,
                y = ~ value,
                name = "Actual",
                type = 'scatter', 
                mode = 'lines',
                hoverinfo = "text",
                text = ~c(paste(paste("Date:", date),'<br>',
                                #paste('Price Type:', as.character(label)), '<br>',
                                paste("Price:", round(value, digits = 2))))) %>% 
          add_trace(y = ~linear, name = "Linear", mode = "lines" ) %>% 
          layout(title = paste("From", format(input$date[1]), "to", format(input$date[2]), "Forecast"),
                 xaxis = list(title = "Date", tickfont = list(size = 9)),
                 yaxis = list(title = "Price"))
      }else if("SVM" == input$ForMath){
          plot_ly(data = forecast_data,
                  x = ~ date,
                  y = ~ value,
                  name = "Actual",
                  type = 'scatter',
                  mode = 'lines',
                  hoverinfo = "text",
                  text = ~c(paste(paste("Date:", date),'<br>',
                                  #paste('Price Type:', as.character(label)), '<br>',
                                  paste("Price:", round(value, digits = 2))))) %>%
            add_trace(y = ~ svm, name = "SVM", mode = "lines") %>%
            layout(title = paste("From", format(input$date[1]), "to", format(input$date[2]), "Forecast"),
                   xaxis = list(title = "Date", tickfont = list(size = 9)),
                   yaxis = list(title = "Price"))
      }
    })
    quality_table <- reactive({
      forecast_data <- data_all()
      forecast_data_1 <- forecast_data %>% 
        filter(date < as.Date("2017-03-01")) %>% 
        mutate(linear = NA,
               svm = NA)
      forecast_data_2 <- forecast_data %>% 
        filter(date >= as.Date("2017-03-01"))
      forecast_data <- rbind(forecast_data_1, forecast_data_2)
      
      if("Linear" == input$ForMath){
        q_data <- forecast_data %>% 
          mutate(error = linear - value,
                 error_square = (linear-value)^2)
        mae <- mean(abs(q_data$error), na.rm = T)
        rmse <- sqrt(mean(q_data$error_square, na.rm = T))
        data_quality = data.frame(Method = "Linear", MAE = as.numeric(round(mae, digits = 4)), RMSE = as.numeric(round(rmse, digits = 4)))
      }else if ("SVM" == input$ForMath){
        q_data <- forecast_data %>% 
          mutate(error = svm - value,
                 error_square = (svm-value)^2)
        mae <- mean(abs(q_data$error), na.rm = T)
        rmse <- sqrt(mean(q_data$error_square, na.rm = T))
        data_quality = data.frame(Method = "SVM", MAE = as.numeric(round(mae, digits = 4)), RMSE = as.numeric(round(rmse, digits = 4)))
      }
      data_quality 
      print(data_quality)
    })
    output$quality <- renderDataTable({
      req(quality_table())
      
      quality_table()
    }, options = list(scrollX = TRUE))

  # data <- read.csv("all_stocks_5yr.csv")
  # data_select_Input_for <- reactive({
  #   temp <- subset(data, as.Date(date,"%m/%d/%Y") >= as.Date("2013-01-01") & as.Date(date,"%m/%d/%Y") < as.Date("2017-03-01"))
  #   subset(temp, Name == input$TichkeInput_For)
  # })
  # data_select_Input_real <- reactive({
  #   temp <- subset(data, as.Date(date,"%m/%d/%Y") >= as.Date("2017-03-01") & as.Date(date,"%m/%d/%Y") <= as.Date("2018-03-01"))
  #   subset(temp, Name == input$TichkeInput_For)
  # })
  # c_temp_2 <- reactive({
  #   paste0(input$ForMath)
  # })
  # 
  # cords_for <- reactiveValues(xy=NULL)
  # observeEvent(  ## Don't use observe 
  #   input$pl_for_click,
  #   {
  #     if(!is.null(input$pl_for_click)){
  #       cords_for$xy <- input$pl_for_click[c('x', 'y')]
  #     }
  #   })
  # 
  # observeEvent(input$go_for, {
  #   
  #   data_select_for<-data_select_Input_for()
  #   data_select_real<-data_select_Input_real()
  #   ymax<-max(data_select_real$close)
  #   ymin<-min(data_select_real$close)
  #   c_2<-c_temp_2()
  #   
  #   line_x=1:length(data_select_for$close)
  #   for(n in 1:length(c_2))
  #     switch(c_2[n],
  #            linear = {line_model <- lm(formula = data_select_for$close~line_x)$coefficients},
  #            # SVM = {svm_model<-svm(line_x, data_select_for$close,type = "eps-regression")
  #            #        svm_pre<-predict(svm_model,seq(length(data_select_for$close)+1,length(data_select_for$close)+length(data_select_real$close),1))},
  #            ARIMA = {a2<-arima(data_select_for$close,order=c(0,1,0), seasonal=list(order=c(1,1,0), period=100))
  #            a3<-forecast(a2,length(data_select_real$close))})
  #   
  #   
  #   output$plot_for <- renderPlot({
  #     plot(data_select_real$close,type="l",xaxt="n",ylim=c(ymin-10, ymax+10),ylab='',xlab='',lty=1,col="black", axes = T, lwd = 2)
  #     
  #     Date_show <- format(as.Date(data_select_real$date,"%m/%d/%Y"), "%m/%d/%y")
  #     axis(1,labels=Date_show,at=1:length(data_select_real$date),las=1, tick = F)
  #     title(main='',ylab='Price',xlab='')
  #     for(n in 1:length(c_2))
  #       switch(c_2[n],
  #              linear = lines(seq(1,length(data_select_real$close),1),line_model[1]+line_model[2]*seq(length(data_select_for$close)+1,length(data_select_for$close)+length(data_select_real$close),1),lty=2,col="red", lwd = 2),
  #              # SVM = lines(seq(1,length(data_select_real$close),1),svm_pre,lty=3,col="green", lwd = 2),
  #              ARIMA = lines(seq(1,length(data_select_real$close),1),a3[["mean"]],lty=2,col="blue", lwd = 2))
  #     
  #     
  #     c_col_2<-c("black")
  #     for(n in 1:length(c_2))
  #       c_col_2<-switch(c_2[n],
  #                       linear = c(c_col_2,c("red")),
  #                       SVM = c(c_col_2,c("green")),
  #                       ARIMA = c(c_col_2,c("blue")))
  #     c_lty_2<-c(1)
  #     for(n in 1:length(c_2))
  #       c_lty_2<-switch(c_2[n],
  #                       linear = c(c_lty_2,c(2)),
  #                       SVM = c(c_lty_2,c(3)),
  #                       ARIMA = c(c_lty_2,c(2)))
  #     c_temp_2<-c("real")
  #     c_temp_2<-c(c_temp_2,c_2)
  #     legend("topright",c_temp_2,lty=c_lty_2,col=c_col_2)
  #     
  #     xy <- cords_for$xy
  #     if(!is.null(xy)){
  #       temp <- round(xy[['x']])
  #       xy_x <- as.character(Date_show[temp])
  #       xy_y <- round(xy[['y']],3)
  #       xy_final <- c(xy_x,xy_y)
  #       #text(xy, labels=paste(as.list(xy_final), collapse=', '), xpd=TRUE, adj=c(0.5,-2))
  #       legend(xy,paste(as.list(xy_final), collapse=', '))
  #     } 
  #     
  #   })
  # })
  
  
  # Page 4: Portfolio Management 
  date_data <- reactive({
    as.data.frame(seq.Date(from = input$date_mana[1], to = input$date_mana[2], by =1 ))
  })
  
  data_select_port <- reactive({
    date_data_port <- date_data()
    names(date_data_port) <- "date"
    data_select_port <- df %>% 
      filter(date >= as.Date(input$date_mana[1])) %>% 
      filter(date <= as.Date(input$date_mana[2]))%>% 
      filter(label %in% "close") %>% 
      right_join(date_data_port)
    data_select_port
  })
  
  data_port <- reactive({
    
    data_port_1 <- data_select_port() %>% 
      filter(name %in% input$Stock_1) %>% 
      mutate(return = (value/lag(value) - 1)) %>% 
      mutate(weight = as.numeric(input$Weight_1))
    data_port_1[is.na(data_port_1)] <- 0

    data_port_2 <- data_select_port() %>% 
      filter(name %in% input$Stock_2) %>% 
      mutate(return = (value/lag(value) - 1)) %>% 
      mutate(weight = as.numeric(input$Weight_2))
    data_port_2[is.na(data_port_2)] <- 0

    data_port_3 <- data_select_port() %>% 
      filter(name %in% input$Stock_3) %>% 
      mutate(return = (value/lag(value) - 1)) %>% 
      mutate(weight = as.numeric(input$Weight_3))
    data_port_3[is.na(data_port_3)] <- 0

    data_port_4 <- data_select_port() %>% 
      filter(name %in% input$Stock_4) %>% 
      mutate(return = (value/lag(value) - 1)) %>% 
      mutate(weight = as.numeric(input$Weight_4))
    data_port_4[is.na(data_port_4)] <- 0

    data_port_5 <- data_select_port() %>% 
      filter(name %in% input$Stock_5) %>% 
      mutate(return = (value/lag(value) - 1)) %>% 
      mutate(weight = as.numeric(input$Weight_5))
    data_port_5[is.na(data_port_5)] <- 0
    
    total_data <-  rbind(data_port_1, data_port_2, data_port_3, data_port_4, data_port_5)
    total_data
  })
  
  
  total_data <- reactive({
    total_data_1 <- data_port() %>% 
      mutate(grade = as.numeric(return) * as.numeric(weight)*100) %>% 
      group_by(date) %>% 
      summarise(grade = sum(grade, na.rm = T)) %>% 
      ungroup()
    total_data_1
  })
  
  
  output$plot_mana <- renderPlotly({
    mana_data <- total_data()
    plot_ly(data = mana_data,
            x = ~ date,
            y = ~ grade,
           # color = ~label,
            type = 'scatter', 
            mode = 'lines',
            hoverinfo = "text",
            text = ~c(paste(paste("Date:", date),'<br>',
                            #paste('Price Type:', as.character(label)), '<br>',
                            paste("Value:", round(grade, digits = 2), "%")))) %>% 
      # add_trace(y = ~value, mode = 'markers') %>% 
      layout(title = paste("From", format(input$date[1]), "to", format(input$date[2]), "Portfolio"),
             xaxis = list(title = "Date", tickfont = list(size = 9)),
             yaxis = list(title = "Portfolio return in %"))
    # barmode = "stack",
    # dragmode = "select")
  })
  
  
  # data_select_Input_man <- reactive({
  #   subset(data, as.Date(date,"%m/%d/%Y") >= as.Date(input$date_mana[1]) & as.Date(date,"%m/%d/%Y") <= as.Date(input$date_mana[2]))
  # })
  # 
  # cords_mana <- reactiveValues(xy=NULL)
  # observeEvent(  ## Do not use observe 
  #   input$pl_mana_click,
  #   {
  #     if(!is.null(input$pl_mana_click)){
  #       cords_mana$xy <- input$pl_mana_click[c('x', 'y')]
  #     }
  #   })
  # observeEvent(input$go_mana, {
  #   data_select_Input_man <- data_select_Input_man()
  #   data_select_return_man1<-diff(subset(data_select_Input_man, Name == input$Stock_1)$close)
  #   data_select_return_man2<-diff(subset(data_select_Input_man, Name == input$Stock_2)$close)
  #   data_select_return_man3<-diff(subset(data_select_Input_man, Name == input$Stock_3)$close)
  #   data_select_return_man4<-diff(subset(data_select_Input_man, Name == input$Stock_4)$close)
  #   data_select_return_man5<-diff(subset(data_select_Input_man, Name == input$Stock_5)$close)
  #   #ymax<-max(data_select_real$open)
  #   #ymin<-min(data_select_real$open)
  #   data_return=data_select_return_man1*input$Weight_1+data_select_return_man2*input$Weight_2+data_select_return_man3*input$Weight_3+data_select_return_man4*input$Weight_4+data_select_return_man5*input$Weight_5
  #   
  #   
  #   
  #   
  #   date_show<-subset(data_select_Input_man, Name == input$Stock_1)$date
  #   date_show<-date_show[-1]
  #   
  #   output$plot_mana <- renderPlot({
  #     
  #     plot(data_return,type="l",xaxt="n",ylab='return',xlab='',lty=1.5,col="steelblue3", axes = T, lwd = 2)
  #     Date_show <- format(as.Date(date_show,"%m/%d/%Y"), "%m/%d/%y")
  #     axis(1,labels=Date_show,at=1:length(date_show),las=1, tick = F)
  #     #lines(seq(1,length(data_select_real$open),1),line_model[1]+line_model[2]*seq(length(data_select_for$open)+1,length(data_select_for$open)+length(data_select_real$open),1))
  #     
  #     xy <- cords_mana$xy
  #     if(!is.null(xy)){
  #       temp <- round(xy[['x']])
  #       xy_x <- as.character(Date_show[temp])
  #       xy_y <- round(xy[['y']],3)
  #       xy_final <- c(xy_x,xy_y)
  #       #text(xy, labels=paste(as.list(xy_final), collapse=', '), xpd=TRUE, adj=c(0.5,-2))
  #       legend(xy,paste(as.list(xy_final), collapse=', '))
  #     } 
  #   })
  #   
  #   
  # })
  
  
  
})