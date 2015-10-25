# #library(shiny)
# 
# 
#  ## data from :  http://pages.swcp.com/stocks/#historical%20data 
#   stocks_data<-read.table("S.P.500_data.txt",sep=",")
#   colnames(stocks_data)<-c("Date","Ticker","Open","High","Low","Close","Volume") 
#   stocks_data$Date<- as.Date(as.character(stocks_data$Date), "%Y%m%d")
# 
#   
#   #get company names + match with ticker symbols
#   library(RCurl)
# #   company_names1 <- "http://data.okfn.org/data/core/s-and-p-500-companies/r/constituents.csv"
# #   company_names2<-getURL(company_names1)
# #   company_names3<-read.csv(textConnection(company_names2))
# # write.csv(company_names3,"company_names3",row.names = FALSE)
# company_names3<-read.csv("company_names3",header=TRUE)
#   stocks_data$Company.Names<-company_names3[match(stocks_data$Ticker,company_names3$Symbol),"Name"]
#   
#   #stocks_data<-sort(unique(as.character(stocks_data$Company.Names)))


#stocks_data[,"Date"]<- as.POSIXct(stocks_data[,"Date"])

library(plyr)
library(dplyr)
# averages <- aggregate(Close~Date,data=stocks_data, FUN=mean)
# names(averages)[names(averages)=="Close"] <- "Average"
# #cdata<-summaryBy(Close~Date,data=stocks_data,FUN=mean)
# #by_date<-group_by(stocks_data,Date)
# #averages<-summarise(by_date,Average = mean(Close))
# stocks_data<- rbind.fill(stocks_data,averages)
# stocks_data$Company.Names<-as.character(stocks_data$Company.Names)
# for (i in 1:nrow(stocks_data)){
#   if(!is.na(stocks_data[i,"Average"])){
#     stocks_data[i,"Company.Names"]<-"Average" 
#     stocks_data[i,"Close"]<-stocks_data[i,"Average"]
#   }
# }
# stocks_data$Company.Names<-as.factor(stocks_data$Company.Names)
# 
# 
# #by_company<-aggregate(Date~Company.Names,data=stocks_data,FUN=c(min,max))
# by_company<-data.frame(1:501,Company.Names="",Company.Names2="",Min.Value="",Max.Value="",stringsAsFactors = FALSE)
# y=0
# for (i in 1:nrow(stocks_data)){
#   if(stocks_data[i,"Date"]=="2009-08-21"){
#     y=y+1
#     by_company[y,"Company.Names"]<-as.character(stocks_data[i,"Company.Names"])
#     by_company[y,"Min.Value"]<-as.numeric(as.character(stocks_data[i,"Close"]))
#   }
#   if(stocks_data[i,"Date"]=="2010-08-20"){
#     by_company[y,"Company.Names2"]<-as.character(stocks_data[i,"Company.Names"])
#     by_company[y,"Max.Value"]<-as.numeric(as.character(stocks_data[i,"Close"]))
#   }
# }
# 
# by_company$Change<-as.numeric(by_company$Max.Value) - as.numeric(by_company$Min.Value)
# 
# by_company$Perc.Change<-(as.numeric(by_company$Max.Value) - as.numeric(by_company$Min.Value))/as.numeric(by_company$Min.Value)

# write.csv(stocks_data,"stocks_data.csv",row.names = FALSE)
# write.csv(by_company,"by_company.csv",row.names = FALSE)

stocks_data<-read.table("stocks_data.csv",header = TRUE,sep=",")
stocks_data$Date<-as.Date(as.character(stocks_data$Date))
by_company<-read.table("by_company.csv",header=TRUE,sep=",")


stocks.name<- reactive({
  return(unique(as.character(stocks_data$Company.Names)))
})


library(rCharts)


shinyServer(function(input,output,session){
  ########
  output$stockA <- renderUI({ 
    selectInput("stockA", "Select your first stock to compare:",choices=stocks.name()  )
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
    plot1$xAxis(tickFormat = "#!function(d) {return d3.time.format('%m-%d-%Y')(new Date( d*86400000)) ;}!#") 
     
    #plot1$set(width=1500)
    return(plot1)
  })
  
#######
observeEvent(input$submitButton, {
stocks_data<-subset(stocks_data, Company.Names %in% input$stockA | Company.Names %in% input$stockB | Company.Names %in% input$stockC | 
                      Company.Names=="Average")

by_company<-subset(by_company, Company.Names %in% input$stockA | Company.Names %in% input$stockB | Company.Names %in% input$stockC | 
                      Company.Names=="Average")


  ####### 
  output$StockPlot<-renderChart2({
    plot1<-nPlot(Close~Date,data=stocks_data ,group='Company.Names',type='lineChart')
    plot1$xAxis(tickFormat = "#!function(d) {return d3.time.format('%m-%d-%Y')(new Date( d*86400000));}!#")   
    #plot1$xAxis(rotateLabels=-90)
    return(plot1)
  })
  #####
  output$ChangePlot<-renderChart2({
    plot2<- nPlot(Change~Company.Names,data=by_company,type='discreteBarChart')
    plot2$yAxis( axisLabel = "Actual Change", width = 40 )
   # plot2$set(width=500)
    #plot2$yAxis( tickFormat = "#!d3.format('$')!#" )
    return(plot2)
  })
output$ChangePercPlot<-renderChart2({
  plot3<- nPlot(Perc.Change~Company.Names,data=by_company,type='discreteBarChart')
  plot3$yAxis( axisLabel = "Percent Change", width = 40 )
  #plot3$set(width=500)
  plot3$yAxis( tickFormat = "#!d3.format('%')!#" )
  return(plot3)
})
  })
}
)
