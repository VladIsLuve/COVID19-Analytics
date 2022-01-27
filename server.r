library(shiny)
library(COVID19)
library(lubridate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tibble)
library(tseries)
library(lattice)

download <- function(country, window_size)
{
  country_data<-covid19(country,level =1,start=today() - days(window_size + 1))
  country_data <- country_data[,2:6]
  country$date <- as.Date(country_data$date,"%m/%d/%y")
  return (country_data)
}


server<-shinyServer(function(input, output) {
    output$cases_plot <- renderPlot({
      len_date <- input$past
      len_for <- input$forecast
      Russia <- download("Russia", len_date) 
      USA <- download("USA", len_date)
      Italy <- download("Italy", len_date)
      France <- download("France", len_date)
      min_date = min(length(Russia$confirmed),length(USA$confirmed),
                     length(Italy$confirmed),length(France$confirmed))
      date <- NULL
      if (length(Russia$confirmed) == min_date){
        date <- as.Date(Russia$date[-1],"%m/%d/%y")
      } else if (length(USA$confirmed) == min_date){
        date <- as.Date(USA$date[-1],"%m/%d/%y")
      } else if (length(Italy$confirmed) == min){
        date <- as.Date(Italy$date[-1],"%m/%d/%y")
      } else if (length(France$confirmed) == min){
        date <- as.Date(France$date[-1],"%m/%d/%y")
      }
      
      countries <- data.frame(id = date)
      
      if ("Russia" %in% input$Country) {
        dailyRussia <-list()
        dailyRussia$confirmed <- diff(Russia$confirmed)[1:length(date)]
        countries$Russia <- dailyRussia$confirmed
        
      } 
      
      if ("USA" %in% input$Country) {
        dailyUSA <-list()
        dailyUSA$confirmed <- diff(USA$confirmed)[1:length(date)]
        countries$USA <- dailyUSA$confirmed
        
      }
      
      if ("Italy" %in% input$Country) {
        dailyItaly <-list()
        dailyItaly$confirmed <- diff(Italy$confirmed)[1:length(date)]
        countries$Italy <- dailyItaly$confirmed
        
      }
      
      if ("France" %in% input$Country) {
        dailyFrance <-list()
        dailyFrance$confirmed <- diff(France$confirmed)[1:length(date)]
        countries$France <- dailyFrance$confirmed
        
      }
      
      
      if (length(countries) == 1) {
      } else {
      
        if (input$real_for == "Real data only") {
          y_lab <- "Cases"
          if (input$abs_rel == "Relative"){
            y_lab <- "Cases, % of population"
            if ("Russia" %in% colnames(countries)){
              countries$Russia <- countries$Russia/146748590*100  
            }
            if ("USA" %in% colnames(countries)){
              countries$USA <- countries$USA/335193238*100 
            }
            if ("Italy" %in% colnames(countries)){
              countries$Italy <- countries$Italy/60317000*100  
            }
            if ("France" %in% colnames(countries)){
              countries$France <- countries$France/67081000*100  
            }
          }
        df <- melt(countries ,  id.vars = 'id' )
      
        options(scipen = 999)
        
        
        ggplot(data = df, aes(x = id, y = value, fill = variable, color = variable)) +
          geom_line() +
          geom_point()+
          labs(color = "Country")+
          xlab("Date")+
          ylab(y_lab)+
          guides(fill=FALSE)+
          theme(legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                axis.title = element_text(size = 12),
                axis.text = element_text(size = 12))# +
          #geom_text(aes(label = floor(value / 1000)), 
          #                        vjust = 5, nudge_y = 2000)
        
        
        } else{
          
          
          HWmodel <- "additive"
          if (input$model != "Additive") {
            HWmodel <- "mult"
          } 
          
          future <- future <- tail(date + days(len_for + 1), len_for+1)
          pred_countries <- future
          
          if ("Russia" %in% input$Country) {
            past_Russia <- ts(data.frame(date, dailyRussia), frequency = 7)
            past_Russia[!is.finite(past_Russia)] <- 0
            model_Russia <- HoltWinters(past_Russia,seasonal = HWmodel)
            pred_Russia <- predict(model_Russia,n.ahead = len_for)
            pred_Russia <- as.integer(round(pred_Russia))
            pred_countries <- cbind(pred_countries, pred_Russia)
            colnames(pred_countries)[length(colnames(pred_countries))] <- "Russia"
          } 
          
          if ("USA" %in% input$Country) {
            past_USA <- ts(data.frame(dailyUSA), frequency = 7)
            past_USA[!is.finite(past_USA)] <- 0
            model_USA <- HoltWinters(past_USA,seasonal = HWmodel)
            pred_USA <- predict(model_USA,n.ahead = len_for)
            pred_USA <- as.integer(round(pred_USA))
            pred_countries <- cbind(pred_countries, pred_USA)
            colnames(pred_countries)[length(colnames(pred_countries))] <- "USA"
          }
          
          if ("Italy" %in% input$Country) {
            past_Italy <- ts(data.frame(dailyItaly), frequency = 7)
            past_Italy[!is.finite(past_Italy)] <- 0
            model_Italy <- HoltWinters(past_Italy,seasonal = HWmodel)
            pred_Italy <- predict(model_Italy,n.ahead = len_for)
            pred_Italy <- as.integer(round(pred_Italy))
            pred_countries <- cbind(pred_countries, pred_Italy)
            colnames(pred_countries)[length(colnames(pred_countries))] <- "Italy"
          }
          
          if ("France" %in% input$Country) {
            past_France <- ts(data.frame(dailyFrance), frequency = 7)
            past_France[!is.finite(past_France)] <- 0
            model_France <- HoltWinters(past_France,seasonal = HWmodel)
            pred_France <- predict(model_France,n.ahead = len_for)
            pred_France <- as.integer(round(pred_France))
            pred_countries <- cbind(pred_countries, pred_France)
            colnames(pred_countries)[length(colnames(pred_countries))] <- "France"
          }
          
          colnames(pred_countries)[1] <- "id"
          pred_countries <- data.frame(pred_countries)
          pred_countries$id <- as.Date("1970-01-01", format = "%Y-%m-%d")+pred_countries[,1] 
          countries <- rbind(countries, pred_countries[1,])
          #pred_countries <- join(pred_countries, countries)
          #countries <- join(countries,pred_countries)
          #countries <- rbind(countries, pred_countries)
          countries$pred <- FALSE
          pred_countries$pred <- TRUE
          
          countries <- rbind(countries, pred_countries)
          
          y_lab <- "Cases"
          if (input$abs_rel == "Relative"){
            y_lab <- "Cases, % of population"
            if ("Russia" %in% colnames(countries)){
              countries$Russia <- countries$Russia/146748590*100  
            }
            if ("USA" %in% colnames(countries)){
              countries$USA <- countries$USA/335193238*100 
            }
            if ("Italy" %in% colnames(countries)){
              countries$Italy <- countries$Italy/60317000*100  
            }
            if ("France" %in% colnames(countries)){
              countries$France <- countries$France/67081000*100  
            }
          }
          
          df <- melt(countries,  id.vars = c('id','pred') )
          
          
          options(scipen = 999)
          ggplot(data = df, aes(x = id, y = value, fill = variable, color = variable )) +
            geom_line(aes(linetype = pred)) +
            geom_vline(xintercept = today(), 
                       color = "red",
                       lty = 5) +
            geom_point()+
            labs(color = "Country")+
            guides(fill=FALSE, linetype = FALSE) +
            xlab("Date")+
            ylab(y_lab)+
            annotate("text", x = today(), y = 0, label = "Today", color = 'black')+
            theme(legend.title = element_text(size = 16),
                  legend.text = element_text(size = 14),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12))
          
            
        }
      }
      
      })
  
})
