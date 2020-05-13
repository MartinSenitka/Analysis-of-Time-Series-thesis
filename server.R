library(shiny)
library(forecast)
library(fma)
library(caTools)
library(ggplot2)
library(minpack.lm)
library(tseries)
library(MLmetrics)
library(hydroGOF)
library(Metrics)
library(plotly)
library(dplyr)
library(devtools)

data(airpass, package="fma")
DData<-data.frame(Time=seq(1,144),airpass)
shinyServer(function(input,output,session){
    MyData <- reactive({
        if(input$RD1==1){
            inFile<-input$file1
            if (is.null(inFile))
                return(NULL)
            read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
        }else{
            DData
        }
    })
    
    
    
    
    
    Col<-reactive({input$Col})
    Start<-reactive({input$Start})
    End<-reactive({input$End})
    Fre<-reactive({input$freq})
    Hstar<-reactive({input$Starth})
    Hend<-reactive({input$Endh})
    
    output$summary<-renderPrint({
        summary(MyData()[,Col()])
    })
    output$table<-renderDataTable({
        MyData()
    })
    
    #PLOT POVODNYCH DAT AN PRVEJ STRANE
    
    
    output$PlotG<-renderPlotly({
        
        if(is.null(MyData())!=T){

          
         x<-MyData()[,1]
         y<-MyData()[,Col()]
          fig <- plot_ly( x = ~x, y = ~y, type = 'scatter', mode = 'markers')
          if(input$RD1==2){
            fig <- plot_ly( x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers')
          }
          
          fig
          
          
           }
        
        
        
        
    })
    
    
    
    
    
    #Machine Leardning
    
    output$MLPlot<-renderPlotly({
      
      req(MyData())
      set.seed(2)
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      
      
      models <- lapply(paste(names(MyData()), '~ .'), 
                       function(f){ lm(as.formula(f), data = MyData()) })
      Prediction <- predict(models[[Col()]])
      
      
      plot_ly(x = ~x) %>% 
          add_markers(y = ~y, name='Data') %>% 
          add_markers(x= ~x, y= Prediction, name="Fit")%>%
          layout(showlegend = F)

      
      

      
    })
    
    output$MlFormula<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      fit <- lm(y~x) 
      coeficients <- coef(fit)
      
      models <- lapply(paste(names(MyData()), '~ .'), 
                       function(f){ lm(as.formula(f), data = MyData()) })
      Prediction <- predict(models[[Col()]])
      
      regressor = lm(formula = y ~ ., 
                     data=MyData())
      
      
      model_equation <- function(model, ...) {
        format_a<- list(...)
        
        model_coeff <- model$coefficients
        format_a$x <- abs(model$coefficients)
        model_c_s <- sign(model_coeff)
        model_c_p <- case_when(model_c_s == -1 ~ " - ",
                               model_c_s == 1 ~ " + ",
                               model_c_s == 0 ~ " + ")
        model_eqn <- paste(strsplit(as.character
                                    (model$call$formula), "~")[[2]], 
                           "=",
                           paste(if_else(model_coeff[1]<0, "- ", ""),
                                 do.call(format, format_a)[1],
                                 paste(model_c_p[-1],
                                       do.call(format, format_a)[-1],
                                       " * ",
                                       names(model_coeff[-1]),
                                       sep = "", collapse = ""),
                                 sep = ""))
        return(model_eqn)
      }
      
      formula <- model_equation(regressor)
      formula
      
    })
    
    output$MachineLearningTable = renderTable({
        digits = 6
        req(MyData())
        set.seed(2)

        

        models <- lapply(paste(names(MyData()), '~ .'), 
                         function(f){ lm(as.formula(f), data = MyData()) })
        Prediction <- predict(models[[Col()]])
        
        
        
        predict_df = data.frame(Class = Prediction)
        output_df = cbind(predict_df)
        
        Matrix<- cbind(MyData()[],Prediction)
        return(Matrix)
        
        
    },        digits = 5
    )
    

    
    # ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA
    
    
    output$Plot11<- renderPlot({ #FORECASTING PLOT
        
        
        
        if(input$Data==1){
           
            mymodel<-auto.arima(AirPassengers)
            myforecast<-forecast(mymodel, level=input$CI5, h=(Hend()-Hstar())*Fre())
            plot(myforecast)
            
        }
        
        if(input$Data==2){
           
            
            dataAsTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
            Sampling<-window(dataAsTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
            outputOfData<-window(dataAsTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
           
            
            forecastMyData<-auto.arima(Sampling)
            
            plot(forecast(forecastMyData,level = input$CI5,h=length(outputOfData)))
            lines(fitted(forecastMyData),col="red",lty=2)
            lines(outputOfData,col="green",lty=2)
            
            
        }
        
        
    })
    output$Plot12 <- renderPlotly({#ACCURACY BAR PLOT
        
        
        if(input$Data==1){
            
            mymodel<-auto.arima(AirPassengers)
            
        }
        
        if(input$Data==2){
            
            dataAsTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
            Sampling<-window(dataAsTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
            outputOfData<-window(dataAsTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
            
            
            forecastMyData<-auto.arima(Sampling)
            
            plot(forecast(forecastMyData,level = input$CI5,h=length(outputOfData)))
            lines(fitted(forecastMyData),col="red",lty=2)
            lines(outputOfData,col="green",lty=2)
            
        }
        
        
        sum<-summary(mymodel)
        
        #MAPE
        MAPE<-sum[[5]]
        
        
        #ME MEan error
        ME<-sum[[1]]
        
        #MAE
        MAE<-sum[[3]]
        
        #MSE
        MSE<-sum[[2]]
        
        
        p <- plot_ly(
            x = c("Mape", "ME", "MAE", "MSE"),
            y = c(MAPE, ME, MAE, MSE),
            name = "Bar plot",
            type = "bar"
        )
        
        p
        
        
        
       
    })
    output$accu6 <- renderTable({ #ACCURACY TABLE
        
  
        if(input$Data==1){
          
       
          
          mymodel<-auto.arima(AirPassengers)
            
        }
        
        if(input$Data==2){
          
          dataAsTS<-ts(MyData()[,Col()],start=Start(),frequency = Fre())
          Sampling<-window(dataAsTS,start = c(Start(),1),end=c((Hstar()-1),Fre()))
          outputOfData<-window(dataAsTS,start=c(Hstar(),1),end = c(Hend(),Fre()))
          
          
          forecastMyData<-auto.arima(Sampling)
          
         
        }
        
        sum<-summary(mymodel)
        
        #MAPE
        MAPE<-sum[[5]]
        
        
        #ME MEan error
        ME<-sum[[1]]
        
        #MAE
        MAE<-sum[[3]]
        
        #MSE
        MSE<-sum[[2]]
        
        
        datatable<-cbind(MAPE,ME,MAE,MSE)
        
        return(datatable)
        
        #return(summary(mymodel))
        
        
        
    })
    
    # ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA ARIMA
    
    
    
    #LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF 
    output$LinearPlot<-renderPlotly({
        

        
        ###NEZUZENE
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        fit <- lm(y~x) 
          plot_ly(x = ~x) %>% 
            add_markers(y = ~y, name='Data') %>% 
            add_lines(x= ~x, y= fitted(fit), name="Fit")%>%
            layout(showlegend = F)
          
      
        
        
    })
    output$LinearFormula<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      fit <- lm(y~x) 
      coeficients <- coef(fit)
      
      model_equation <- function(model, ...) {
        format_args <- list(...)
        
        model_coeff <- model$coefficients
        format_args$x <- abs(model$coefficients)
        model_coeff_sign <- sign(model_coeff)
        model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                        model_coeff_sign == 1 ~ " + ",
                                        model_coeff_sign == 0 ~ " + ")
        model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], 
                           "=",
                           paste(if_else(model_coeff[1]<0, "- ", ""),
                                 do.call(format, format_args)[1],
                                 paste(model_coeff_prefix[-1],
                                       do.call(format, format_args)[-1],
                                       " * ",
                                       names(model_coeff[-1]),
                                       sep = "", collapse = ""),
                                 sep = ""))
        return(model_eqn)
      }
      
      formula <- model_equation(fit, digits = 4, trim = TRUE)
      formula
      
    })
    
    output$LinearPrediction<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      fit <- lm(y~x) 
      coeficients <- coef(fit)
      xyz<-input$yearL
      premenna<-as.numeric(xyz)
      
      formula <- coeficients[1]+coeficients[2]*premenna
      formula
    })
    
    output$LinearAccuracyBar <- renderPlotly({#ACCURACY BAR PLOT 
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        model <- lm(y~x) 
        b<-model$coefficients               
        fy<-model$fitted.values  
        xl<-colnames(MyData)[1]
        
        #MAPE
        MAPE<-round(MAPE(fy,y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fy,y),digits=4))
        
        #MAE
        MAE<-round(MAE(fy,y),digits=4)
        
        #MSE
        MSE<-round(MSE(fy,y),digits=4)
        
        #datatable<-cbind(MAPE,ME,MAE,MSE)
        
        #barplot(datatable)
        
        
        
        p <- plot_ly(
            x = c("Mape", "ME", "MAE", "MSE"),
            y = c(MAPE, ME, MAE, MSE),
            name = "Bar plot",
            type = "bar"
        )
        
        p
    })
    output$LinearAccuracyTable <- renderTable({ #ACCURACY TABLE
        
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        model <- lm(y~x) 
        b<-model$coefficients               
        fy<-model$fitted.values  
        
        #MAPE
        MAPE<-round(MAPE(fy,y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fy,y),digits=4))
        
        #MAE
        MAE<-round(MAE(fy,y),digits=4)
       
        #MSE
        MSE<-round(MSE(fy,y),digits=4)
        
        datatable<-cbind(MAPE,ME,MAE,MSE)
        
         return(datatable)
        
        
    })
    
    
    #LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF LINEAR GRAF 
    
    
    #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL 

    output$ExponentialPlot<-renderPlotly({
     
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        n=length(y)
        m=n/3
        
        s1=sum(y[1:m])
        s2=sum(y[(m+1):(2*m)])
        s3=sum(y[((2*m)+1):(3*m)])
        
        b1=((s3-s2)/(s2-s1))^(1/m)
        b0=((b1-1)*(s2-s1))/(b1*(b1^m-1)^2)
        c=(s1-((b0*b1*((b1^m)-1))/(b1-1)))/m 
        
        mod<-nls(y~c+a*b^x,start=list(a=b0,b=b1))
        plot_ly(x = ~x) %>% 
          add_markers(y = ~y, name='Data') %>% 
          add_lines(x= ~x, y= fitted(mod), name="Fit")%>%
          layout(showlegend = F)
        
        
    })
    output$ExponentionalFormula<-renderText({
  
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      n=length(y)
      m=n/3
      
      s1=sum(y[1:m])
      s2=sum(y[(m+1):(2*m)])
      s3=sum(y[((2*m)+1):(3*m)])
      
      b1=((s3-s2)/(s2-s1))^(1/m)
      b0=((b1-1)*(s2-s1))/(b1*(b1^m-1)^2)
      c=(s1-((b0*b1*((b1^m)-1))/(b1-1)))/m
      
      formula <- paste("y=", round(c,digits=5),"+", round(b0,digits=5), "*" ,round(b1,digits=5), "^x",sep = "")
      formula
      
    })
    
    output$ExponentionalPrediction<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      fit <- glm(log(y) ~ x)
      b<-fit$coefficients    
      b0<-exp(b[1])                                        
      b1<-b[2] 
      m<-nls(y~a*exp(b*x),start=list(a=b0,b=b1))
      #m<-nls(y~a*b^x,start=list(a=b0,b=b1))
      coeficients <- coef(m)
      xyz<-input$yearE
      premenna<-as.numeric(xyz)
      
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      n=length(y)
      m=n/3
      
      s1=sum(y[1:m])
      s2=sum(y[(m+1):(2*m)])
      s3=sum(y[((2*m)+1):(3*m)])
      
      b1=((s3-s2)/(s2-s1))^(1/m)
      b0=((b1-1)*(s2-s1))/(b1*(b1^m-1)^2)
      c=(s1-((b0*b1*((b1^m)-1))/(b1-1)))/m
      
      xyz<-input$yearE
      premenna<-as.numeric(xyz)
      
      formula <- c+b0*b1^premenna
      formula
    })
    
    output$ExponentialAccuracyBar <- renderPlotly({#ACCURACY BAR PLOT
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        model <- glm(log(y) ~ x)
        b<-model$coefficients               
        fz<-model$fitted.values                              
        b0<-exp(b[1])                                        
        b1<-b[2] 
        fy<-exp(fz)                                          
        m<-nls(y~a*exp(b*x),start=list(a=b0,b=b1))
        
        
        n=length(y)
        m=n/3
        
        s1=sum(y[1:m])
        s2=sum(y[(m+1):(2*m)])
        s3=sum(y[((2*m)+1):(3*m)])
        
        b1=((s3-s2)/(s2-s1))^(1/m)
        b0=((b1-1)*(s2-s1))/(b1*(b1^m-1)^2)
        c=(s1-((b0*b1*((b1^m)-1))/(b1-1)))/m
       
        mod<-nls(y~c+a*b^x,start=list(a=b0,b=b1))
        
        
        #MAPE
        MAPE<-round(MAPE(fitted(mod),y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fitted(mod),y),digits=4))
        
        #MAE
        MAE<-round(MAE(fitted(mod),y),digits=4)
        
        #MSE
        MSE<-round(MSE(fitted(mod),y),digits=4)
        

        p <- plot_ly(
            x = c("Mape", "ME", "MAE", "MSE"),
            y = c(MAPE, ME, MAE, MSE),
            name = "Bar plot",
            type = "bar"
        )
        
        p
        
    })
    output$ExponentialAccuracyTable <- renderTable({ #ACCURACY TABLE
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        model <- glm(log(y) ~ x)
        b<-model$coefficients               
        fz<-model$fitted.values                              
        b0<-exp(b[1])                                        
        b1<-b[2] 
        fy<-exp(fz)                                          
        m<-nls(y~a*exp(b*x),start=list(a=b0,b=b1))
        
        n=length(y)
        m=n/3
        
        
        s1=sum(y[1:m])
        s2=sum(y[(m+1):(2*m)])
        s3=sum(y[((2*m)+1):(3*m)])
        
        b1=((s3-s2)/(s2-s1))^(1/m)
        b0=((b1-1)*(s2-s1))/(b1*(b1^m-1)^2)
        c=(s1-((b0*b1*((b1^m)-1))/(b1-1)))/m
        
        mod<-nls(y~c+a*b^x,start=list(a=b0,b=b1))
        
        
        #MAPE
        MAPE<-round(MAPE(fitted(mod),y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fitted(mod),y),digits=4))
        
        #MAE
        MAE<-round(MAE(fitted(mod),y),digits=4)
        
        #MSE
        MSE<-round(MSE(fitted(mod),y),digits=4)
        
        datatable<-cbind(MAPE,ME,MAE,MSE)
        
        return(datatable)
    })
    #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL #EXPONENTIONAL 
    
    
    #QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATICQUADRATIC QUADRATIC
    
    output$QuadraticPlot<-renderPlotly({
        
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        x2<-x^2
        fit <- lm(y ~ x+I(x^2))
        
        coeficients <- coef(fit)
        
        
        plot_ly(x = ~x) %>% 
            add_markers(y = ~y, name='Data') %>% 
            add_lines(x= ~x, y= fitted(fit), name="Fit")%>%
            layout(showlegend = F)
        
    })
    output$QuadraticFormula<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      x2<-x^2
      fit <- lm(y ~ x+I(x^2))
      coeficients <- coef(fit)
      
      model_equation <- function(model, ...) {
        format_args <- list(...)
        
        model_coeff <- model$coefficients
        format_args$x <- abs(model$coefficients)
        model_coeff_sign <- sign(model_coeff)
        model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                        model_coeff_sign == 1 ~ " + ",
                                        model_coeff_sign == 0 ~ " + ")
        model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                           "=",
                           paste(if_else(model_coeff[1]<0, "- ", ""),
                                 do.call(format, format_args)[1],
                                 paste(model_coeff_prefix[-1],
                                       do.call(format, format_args)[-1],
                                       " * ",
                                       names(model_coeff[-1]),
                                       sep = "", collapse = ""),
                                 sep = ""))
        return(model_eqn)
      }
      
      formula <- model_equation(fit, digits = 4, trim = TRUE)
      formula

    })
    output$QuadraticPrediction<-renderText({
      
      x<-MyData()[,1]
      y<-MyData()[,Col()]
      x2<-x^2
      fit <- lm(y ~ x+I(x^2))
      coeficients <- coef(fit)
      xyz<-input$yearQ
      premenna<-as.numeric(xyz)
      
      formula <- coeficients[1]+coeficients[2]*premenna+coeficients[3]*premenna^2
      formula
    })
    output$QuadraticAccuracyBar <- renderPlotly({#ACCURACY BAR PLOT
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        x2<-x^2
        #quadratic.model <- lm(y ~ x2)
        quadratic.model <- lm(y ~ x+I(x^2))
        
        
        b<-quadratic.model$coefficients               
        fy<-quadratic.model$fitted.values                           
        b0<-b[1]                                       
        b1<-b[2] 
        c1<-b[3]
        predictedcounts <- predict(quadratic.model)
        
        #MAPE
        MAPE<-round(MAPE(fy,y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fy,y),digits=4))
        
        #MAE
        MAE<-round(MAE(fy,y),digits=4)
        
        #MSE
        MSE<-round(MSE(fy,y),digits=4)
        
     
        
        p <- plot_ly(
            x = c("Mape", "ME", "MAE", "MSE"),
            y = c(MAPE, ME, MAE, MSE),
            name = "Bar plot",
            type = "bar"
        )
        
        p
        
        
    })
    output$QuadraticAccuracyTable <- renderTable({ #ACCURACY TABLE
        x<-MyData()[,1]
        y<-MyData()[,Col()]
        x2<-x^2
        #quadratic.model <- lm(y ~ x2)
        quadratic.model <- lm(y ~ x+I(x^2))
        
        
        b<-quadratic.model$coefficients               
        fy<-quadratic.model$fitted.values                           
        b0<-b[1]                                       
        b1<-b[2] 
        c1<-b[3]
        predictedcounts <- predict(quadratic.model)

        #MAPE
        MAPE<-round(MAPE(fy,y)*100,digits = 4)
        
        
        #ME MEan error
        ME<-abs(round(me(fy,y),digits=4))
        
        #MAE
        MAE<-round(MAE(fy,y),digits=4)
        
        #MSE
        MSE<-round(MSE(fy,y),digits=4)
        
        datatable<-cbind(MAPE,ME,MAE,MSE)
        
    })
    
    #QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATIC QUADRATICQUADRATICQUADRATICQUADRATICQUADRATICQUADRATIC
    
    
})


