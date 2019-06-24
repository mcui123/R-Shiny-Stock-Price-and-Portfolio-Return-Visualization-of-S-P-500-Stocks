library(shiny)
library(e1071)
library(forecast)
library(ggplot2)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  
  data <- read.csv("all_stocks_5yr.csv")
  
  #  Page 2: Stock History 
  data_select_Input <- reactive({
    temp <- subset(data, as.Date(date,"%m/%d/%Y") >= as.Date(input$date[1]) & as.Date(date,"%m/%d/%Y") <= as.Date(input$date[2]))
    subset(temp, Name == input$TichkeInput)
    })
  c_temp <- reactive({
    paste0(input$showfiger)
  })
  cords <- reactiveValues(xy=NULL)
  observeEvent(  ## Don't use observe here 
    input$pl_click,
    {
      if(!is.null(input$pl_click)){
        cords$xy <- input$pl_click[c('x', 'y')]
      }
    })
  observeEvent(input$go, {
    data_select<-data_select_Input()
    ymax<-max(data_select$high)
    ymin<-min(data_select$low)
    c<-c_temp()
    output$plot <- renderPlot({
      switch(c[1],
       open = plot(data_select$open,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=1,col="black", axes = T, lwd = 2),
       high = plot(data_select$high,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=2,col="red", axes = T, lwd = 2),
       low = plot(data_select$low,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=3,col="blue", axes = T, lwd = 2),
       close = plot(data_select$close,type="l",xaxt="n",ylim=c(ymin, ymax),ylab='',xlab='',lty=1,col="green", axes = T, lwd = 2))
      
      Date_show <- format(as.Date(data_select$date,"%m/%d/%Y"), "%m/%d/%y")
      
      
      axis(1,labels=Date_show,at=1:length(data_select$date),las=1, tick = F)
      title(main='',ylab='Price',xlab='')
      
      for(n in 1:length(c))
        if(n==1){}
        else{
          switch(c[n],
                 open = lines(data_select$open,type="l",xaxt="n",ylab='',xlab='',lty=1,col="black", lwd = 2),
                 high = lines(data_select$high,type="l",xaxt="n",ylab='',xlab='',lty=2,col="red", lwd = 2),
                 low = lines(data_select$low,type="l",xaxt="n",ylab='',xlab='',lty=3,col="blue", lwd = 2),
                 close = lines(data_select$close,type="l",xaxt="n",ylab='',xlab='',lty=1,col="green", lwd = 2))
        }
      c_col<-c("")
      for(n in 1:length(c))
        c_col<-switch(c[n],
               open = c(c_col,c("black")),
               high = c(c_col,c("red")),
               low = c(c_col,c("blue")),
               close = c(c_col,c("green")))
      c_col<-c_col[-1]
      c_lty<-c(1)
      for(n in 1:length(c))
        c_lty<-switch(c[n],
                      open = c(c_lty,c(1)),
                      high = c(c_lty,c(2)),
                      low = c(c_lty,c(3)),
                      close = c(c_lty,c(1)))
      c_lty<-c_lty[-1]
      legend("topright",c,lty=c_lty,col=c_col)
      
      xy <- cords$xy
      if(!is.null(xy)){
        temp <- round(xy[['x']])
        xy_x <- as.character(Date_show[temp])
        xy_y <- round(xy[['y']],3)
        xy_final <- c(xy_x,xy_y)
        
        #text(xy, labels=paste(as.list(xy_final), collapse=', '), xpd=TRUE, adj=c(0.5,-2))
        legend(xy,paste(as.list(xy_final), collapse=', '))
      } 
       
    })
  })
  
  
  # Page 3:Stock Forecast 
  
  data_select_Input_for <- reactive({
    temp <- subset(data, as.Date(date,"%m/%d/%Y") >= as.Date("2013-01-01") & as.Date(date,"%m/%d/%Y") < as.Date("2017-03-01"))
    subset(temp, Name == input$TichkeInput_For)
  })
  data_select_Input_real <- reactive({
    temp <- subset(data, as.Date(date,"%m/%d/%Y") >= as.Date("2017-03-01") & as.Date(date,"%m/%d/%Y") <= as.Date("2018-03-01"))
    subset(temp, Name == input$TichkeInput_For)
  })
  c_temp_2 <- reactive({
    paste0(input$ForMath)
  })
  
  cords_for <- reactiveValues(xy=NULL)
  observeEvent(  ## Don't use observe 
    input$pl_for_click,
    {
      if(!is.null(input$pl_for_click)){
        cords_for$xy <- input$pl_for_click[c('x', 'y')]
      }
    })
  
  observeEvent(input$go_for, {
    
      data_select_for<-data_select_Input_for()
      data_select_real<-data_select_Input_real()
      ymax<-max(data_select_real$close)
      ymin<-min(data_select_real$close)
      c_2<-c_temp_2()
      
      line_x=1:length(data_select_for$close)
      for(n in 1:length(c_2))
        switch(c_2[n],
               linear = {line_model <- lm(formula = data_select_for$close~line_x)$coefficients},
               # SVM = {svm_model<-svm(line_x, data_select_for$close,type = "eps-regression")
               #        svm_pre<-predict(svm_model,seq(length(data_select_for$close)+1,length(data_select_for$close)+length(data_select_real$close),1))},
               ARIMA = {a2<-arima(data_select_for$close,order=c(0,1,0), seasonal=list(order=c(1,1,0), period=100))
                        a3<-forecast(a2,length(data_select_real$close))})


    output$plot_for <- renderPlot({
      plot(data_select_real$close,type="l",xaxt="n",ylim=c(ymin-10, ymax+10),ylab='',xlab='',lty=1,col="black", axes = T, lwd = 2)
      
      Date_show <- format(as.Date(data_select_real$date,"%m/%d/%Y"), "%m/%d/%y")
      axis(1,labels=Date_show,at=1:length(data_select_real$date),las=1, tick = F)
      title(main='',ylab='Price',xlab='')
      for(n in 1:length(c_2))
        switch(c_2[n],
               linear = lines(seq(1,length(data_select_real$close),1),line_model[1]+line_model[2]*seq(length(data_select_for$close)+1,length(data_select_for$close)+length(data_select_real$close),1),lty=2,col="red", lwd = 2),
               # SVM = lines(seq(1,length(data_select_real$close),1),svm_pre,lty=3,col="green", lwd = 2),
               ARIMA = lines(seq(1,length(data_select_real$close),1),a3[["mean"]],lty=2,col="blue", lwd = 2))
      
      
      c_col_2<-c("black")
      for(n in 1:length(c_2))
        c_col_2<-switch(c_2[n],
                      linear = c(c_col_2,c("red")),
                      SVM = c(c_col_2,c("green")),
                      ARIMA = c(c_col_2,c("blue")))
      c_lty_2<-c(1)
      for(n in 1:length(c_2))
        c_lty_2<-switch(c_2[n],
                        linear = c(c_lty_2,c(2)),
                        SVM = c(c_lty_2,c(3)),
                        ARIMA = c(c_lty_2,c(2)))
      c_temp_2<-c("real")
      c_temp_2<-c(c_temp_2,c_2)
      legend("topright",c_temp_2,lty=c_lty_2,col=c_col_2)
      
      xy <- cords_for$xy
      if(!is.null(xy)){
        temp <- round(xy[['x']])
        xy_x <- as.character(Date_show[temp])
        xy_y <- round(xy[['y']],3)
        xy_final <- c(xy_x,xy_y)
        #text(xy, labels=paste(as.list(xy_final), collapse=', '), xpd=TRUE, adj=c(0.5,-2))
        legend(xy,paste(as.list(xy_final), collapse=', '))
      } 

    })
  })
  
  
  # Page 4: Portfolio Management 
  data_select_Input_man <- reactive({
    subset(data, as.Date(date,"%m/%d/%Y") >= as.Date(input$date_mana[1]) & as.Date(date,"%m/%d/%Y") <= as.Date(input$date_mana[2]))
  })

  cords_mana <- reactiveValues(xy=NULL)
  observeEvent(  ## Do not use observe 
    input$pl_mana_click,
    {
      if(!is.null(input$pl_mana_click)){
        cords_mana$xy <- input$pl_mana_click[c('x', 'y')]
      }
    })
  observeEvent(input$go_mana, {
    data_select_Input_man <- data_select_Input_man()
    data_select_return_man1<-diff(subset(data_select_Input_man, Name == input$Stock_1)$close)
    data_select_return_man2<-diff(subset(data_select_Input_man, Name == input$Stock_2)$close)
    data_select_return_man3<-diff(subset(data_select_Input_man, Name == input$Stock_3)$close)
    data_select_return_man4<-diff(subset(data_select_Input_man, Name == input$Stock_4)$close)
    data_select_return_man5<-diff(subset(data_select_Input_man, Name == input$Stock_5)$close)
    #ymax<-max(data_select_real$open)
    #ymin<-min(data_select_real$open)
    data_return=data_select_return_man1*input$Weight_1+data_select_return_man2*input$Weight_2+data_select_return_man3*input$Weight_3+data_select_return_man4*input$Weight_4+data_select_return_man5*input$Weight_5


    
    
    date_show<-subset(data_select_Input_man, Name == input$Stock_1)$date
    date_show<-date_show[-1]
    
    output$plot_mana <- renderPlot({
   
      plot(data_return,type="l",xaxt="n",ylab='return',xlab='',lty=1.5,col="steelblue3", axes = T, lwd = 2)
      Date_show <- format(as.Date(date_show,"%m/%d/%Y"), "%m/%d/%y")
      axis(1,labels=Date_show,at=1:length(date_show),las=1, tick = F)
      #lines(seq(1,length(data_select_real$open),1),line_model[1]+line_model[2]*seq(length(data_select_for$open)+1,length(data_select_for$open)+length(data_select_real$open),1))
      
      xy <- cords_mana$xy
      if(!is.null(xy)){
        temp <- round(xy[['x']])
        xy_x <- as.character(Date_show[temp])
        xy_y <- round(xy[['y']],3)
        xy_final <- c(xy_x,xy_y)
        #text(xy, labels=paste(as.list(xy_final), collapse=', '), xpd=TRUE, adj=c(0.5,-2))
        legend(xy,paste(as.list(xy_final), collapse=', '))
      } 
    })
    
    
  })
  
  
  
})