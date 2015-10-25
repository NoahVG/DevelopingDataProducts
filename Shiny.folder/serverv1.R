library(shiny)

stocks.name<- function(){
 ## data from :  http://pages.swcp.com/stocks/#historical%20data 
  stocks_data<-read.table("S.P.500_data.txt",sep=",")
  colnames(stocks_data)<-c("Date","Ticker","Open","High","Low","Close","Volume") 
  stocks_data$Date<- as.Date(as.character(stocks_data$Date), "%Y%m%d")
  
  stocks_data$Percent.Change<-((stocks_data$High-stocks_data$Low)/stocks_data$Open)
  
  #get company names + match with ticker symbols
  library(RCurl)
#   company_names1 <- "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents.csv"
#   company_names2<-getURL(company_names1)
#   company_names3<-read.csv(textConnection(company_names2))
# write.csv(company_names3,"company_names3",row.names = FALSE)
company_names3<-read.csv("company_names3",header=TRUE)
  stocks_data$Company.Names<-company_names3[match(stocks_data$Ticker,company_names3$Symbol),"Name"]
  
  #stocks_data<-sort(unique(as.character(stocks_data$Company.Names)))
return(stocks_data)
  }

#stocks_data[,"Date"]<- as.POSIXct(stocks_data[,"Date"])

library(plyr)
library(dplyr)
averages <- aggregate(Close~Date,data=stocks_data, FUN=mean)
names(averages)[names(averages)=="Close"] <- "Average"
#cdata<-summaryBy(Close~Date,data=stocks_data,FUN=mean)
#by_date<-group_by(stocks_data,Date)
#averages<-summarise(by_date,Average = mean(Close))
stocks_data<- rbind.fill(stocks_data,averages)
stocks_data$Company.Names<-as.character(stocks_data$Company.Names)
for (i in 1:nrow(stocks_data)){
  if(!is.na(stocks_data[i,"Average"])){
    stocks_data[i,"Company.Names"]<-"Average" 
    stocks_data[i,"Close"]<-stocks_data[i,"Average"]
  }
}
stocks_data$Company.Names<-as.factor(stocks_data$Company.Names)


by_company<-group_by(stocks_data,Company.Names)
by_company2<- data.frame(summarise(by_company, min = min(Date),max=max(Date)))
min.val<-by_company[match(paste0(by_company2$Company.Names,by_company2$min),
                          paste0(by_company$Company.Names,by_company$Date)),"Close"]
max.val<-by_company[match(paste0(by_company2$Company.Names,by_company2$max),
                          paste0(by_company$Company.Names,by_company$Date)),"Close"]

by_company2<-cbind(by_company2,min.val,max.val)
names(by_company2)[4]<-"Min.Val"
names(by_company2)[5]<-"Max.Val"
by_company2$Change<-by_company2$Max.Val - by_company2$Min.Val
by_company2$Perc.Change<-(by_company2$Max.Val - by_company2$Min.Val)/by_company2$Min.Val


library(rCharts)


shinyServer(function(input,output,session){
  ########
  output$stockA <- renderUI({ 
    selectInput("stockA", "Select your first stock to compare:",choices=stocks.name() )
  })
  ##########
  output$stockB <- renderUI({ 
    selectInput("stockB", "Select your second stock to compare:",choices=stocks.name() )
  })
  
  output$stockC <- renderUI({ 
    selectInput("stockC", "Select your third stock to compare:",choices=stocks.name())
  })
  


  
  output$StockPlot<-renderChart2({
    plot1<-nPlot(Close~Date,data=stocks_data[1:400,],group='Company.Names',type='lineChart')
    plot1$yAxis( axisLabel = "Closing Stock Price Over Time", width = 40 )
    plot1$xAxis(tickFormat = "#!function(d) {return d3.time.format('%m-%d-%Y')(new Date( d*1000-(86400/2)));}!#") 
     
    #plot1$set(width=1500)
    return(plot1)
  })
  
#######
observeEvent(input$submitButton, {
stocks_data<-subset(stocks_data, Company.Names %in% input$stockA | Company.Names %in% input$stockB | Company.Names %in% input$stockC | 
                      Company.Names=="Average")

by_company2<-subset(by_company2, Company.Names %in% input$stockA | Company.Names %in% input$stockB | Company.Names %in% input$stockC | 
                      Company.Names=="Average")


  ####### 
  output$StockPlot<-renderChart2({
    plot1<-nPlot(Close~Date,data=stocks_data ,group='Company.Names',type='lineChart')
    plot1$xAxis(tickFormat = "#!function(d) {return d3.time.format('%m-%d-%Y')(new Date( d*1000-(86400/2)));}!#")     #plot1$xAxis(tickFormat="#!function(d) {return d3.time.format('%d-%m-%Y')(new Date( d * 86400000 ));}!#")
    plot1$yAxis( axisLabel = "Closing Stock Price Over Time", width = 40 )
    #plot1$xAxis(rotateLabels=-90)
    return(plot1)
  })
  #####
  output$ChangePlot<-renderChart2({
    plot2<- nPlot(Change~Company.Names,data=by_company2,type='discreteBarChart')
    plot2$yAxis( axisLabel = "Actual Change", width = 40 )
   # plot2$set(width=500)
    #plot2$yAxis( tickFormat = "#!d3.format('$')!#" )
    return(plot2)
  })
output$ChangePercPlot<-renderChart2({
  plot3<- nPlot(Perc.Change~Company.Names,data=by_company2,type='discreteBarChart')
  plot3$yAxis( axisLabel = "Percent Change", width = 40 )
  #plot3$set(width=500)
  plot3$yAxis( tickFormat = "#!d3.format('%')!#" )
  return(plot3)
})
  })
}
)
