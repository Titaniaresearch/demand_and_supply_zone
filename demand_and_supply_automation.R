library(smartapi)
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)


login_params = list(api_key = 'LPUVlRxd')

login_object = create_connection_object(login_params)

session_data <- generate_session(login_object,"J95213","startteja123")

nifty_50_data <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Nifty50_Stocks.csv")

supply_zone_detection <- function(df,stock,df_supply_and_demand){
  # browser()
  if(nrow(df) >= 3){
    for (ind in 2:(nrow(df)-1)) {
      # browser()
      if(df[ind-1,'Open'] < df[ind-1,'Close'] && ##Green Candle
         (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5 *(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im Balance Candle
         (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
         (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
         (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
      ){
        # browser()
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Supply Reversal Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        # df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
        # df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        df_supply_and_demand[ind,'zone_1'] = df[ind,'Low']
        df_supply_and_demand[ind,'zone_2'] = df[ind,'High']
        
        if(df[ind+1,'Open'] > df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
        
      }
      else if((df[ind-1,'Open'] > df[ind-1,'Close']) && # Red Candle
              (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
              
              (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
              (df[ind+1,'Open'] > df[ind+1,'Close']) && # Red Candle
              (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
              
      ){
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Supply Continuous Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        # df_supply_and_demand[ind,'zone_1'] = round(min(df[ind,'Open'],df[ind,'Close']),2)
        # df_supply_and_demand[ind,'zone_2'] = round(max(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        df_supply_and_demand[ind,'zone_1'] = df[ind,'Low']
        df_supply_and_demand[ind,'zone_2'] = df[ind,'High']
        
        
        if(df[ind+1,'Open'] < df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
        
      }
      # print(ind)
    }
    
  }
  
  return(df_supply_and_demand)
}

demand_zone_detection <- function(df,stock,df_supply_and_demand){
  if(nrow(df) >= 3){
    for (ind in 2:(nrow(df)-1)) {
      # browser()
      if((df[ind-1,'Open'] > df[ind-1,'Close']) &&
         (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) &&
         (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
         (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
         (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))
      ){
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Demand Reversal Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        # df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
        # df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        df_supply_and_demand[ind,'zone_1'] = df[ind,'High']
        df_supply_and_demand[ind,'zone_2'] = df[ind,'Low']
        
        if(df[ind+1,'Open'] > df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
        
      }
      else if((df[ind-1,'Open'] < df[ind-1,'Close']) && # Green Candle
              (abs(df[ind-1,'Open'] - df[ind-1,'Close']) > 0.5*(df[ind-1,'High'] - df[ind-1,'Low'])) && #Im-Balance Candle
              (abs(df[ind,'Open'] - df[ind,'Close']) <= 0.3*(df[ind,'High'] - df[ind,'Low'])) &&
              (df[ind+1,'Open'] < df[ind+1,'Close']) && # Green Candle
              (abs(df[ind+1,'Open'] - df[ind+1,'Close']) > 0.5*(df[ind+1,'High'] - df[ind+1,'Low']))  #Im-Balance Candle
              
      ){
        df_supply_and_demand[ind,'stock'] = stock
        df_supply_and_demand[ind,'pattern'] = "Demand Continuous Pattern"
        df_supply_and_demand[ind,'Date'] = df[ind,'Date']
        # df_supply_and_demand[ind,'zone_1'] = round(max(df[ind,'Open'],df[ind,'Close']),2)
        # df_supply_and_demand[ind,'zone_2'] = round(min(df[ind-1,'High'],df[ind,'High'],df[ind+1,'High']),2)
        
        df_supply_and_demand[ind,'zone_1'] = df[ind,'High']
        df_supply_and_demand[ind,'zone_2'] = df[ind,'Low']
        
        if(df[ind+1,'Open'] > df[ind,'Open']){
          df_supply_and_demand[ind,'strength'] = "Strong"
        }else{
          df_supply_and_demand[ind,'strength'] = "Normal"
        }
      }
      # print(ind)
    }
  }
  return(df_supply_and_demand)
}



nifty_df <- read.csv("~/Desktop/Demand_And_supply_zone/Nifty50_Stocks.csv")
df_supply_and_demand_final = data.frame(stock=character(),pattern=character(),strength=character(),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)


stock <- "TCS.NS"

print(stock)

response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=5d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))

stock_timestamp <- response_data$chart$result[[2]][[1]]

Close <- response_data$chart$result[[3]]$quote[[1]]$close

High <- response_data$chart$result[[3]]$quote[[1]]$high

Low <- response_data$chart$result[[3]]$quote[[1]]$low

Open <- response_data$chart$result[[3]]$quote[[1]]$open

Volume <- response_data$chart$result[[3]]$quote[[1]]$volume

# final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
# if(input$candle_stick_range == "1d" && !(as.character(wday(Sys.Date(), label = TRUE)) %in% c("Sat","Sun")) && hour(Sys.time()) >= 9 && hour(Sys.time()) < 16){

# colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")

final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
if(typeof(final_data$V1) == "list"){
  final_data <- final_data[-c(which(final_data$Close == "NULL")),]
  new_stock_timestamp <- unlist(final_data$V1)
  Close <- unlist(final_data$Close)
  High <- unlist(final_data$High)
  Open <- unlist(final_data$Open)
  Low <- unlist(final_data$Low)
  Volume <- unlist(final_data$Volume)
  
  final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
  
  final_data$dates <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
  
  final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
}else{
  final_data$dates <- as.POSIXct(final_data$V1, origin="1970-01-01")
  
  final_data <- final_data %>% select(dates, Open, High, Low, Close,Volume)
}
# browser()

data <- as.data.frame(final_data)

# print(tail(data,5))

if(nrow(data) >= 3){
  
  df_supply_and_demand = data.frame(stock=character(),pattern=character(),strength=character(0),Date=as.POSIXct(character()),zone_1=numeric(),zone_2=numeric(),stringsAsFactors=FALSE)
  
  
  # supply_zone_df = supply_zone_detection(df,stock)
  
  supply_zone_df = supply_zone_detection(data,stock,df_supply_and_demand)
  
  df_supply_and_demand_final <- rbind(df_supply_and_demand_final, supply_zone_df)

  
  demand_zone_df = demand_zone_detection(data,stock,df_supply_and_demand)
  
  df_supply_and_demand_final <- rbind(df_supply_and_demand_final, demand_zone_df)

}

df_supply_and_demand_final = df_supply_and_demand_final[!rowSums((is.na(df_supply_and_demand_final))),]

df_supply_and_demand_final$date_col = as.Date(df_supply_and_demand_final$Date)

print(df_supply_and_demand_final)

increment = 1
Signal_df = data.frame("Strategy"=character(), "Stock"=character(),"Signal"=character(),"Datetime"=as.POSIXct(character()),"Value"=numeric(),"StopLoss"=numeric(),"Target"=numeric())

for(index in 1:nrow(df_supply_and_demand_final)){
  # if(df_supply_and_demand_final[index,'date_col'] == Sys.Date()){
  if(df_supply_and_demand_final[index,'date_col'] == Sys.Date()-1){
    # print(df_supply_and_demand_final[index,])
    
    if(df_supply_and_demand_final[index,'pattern'] == "Supply Continuous Pattern" | df_supply_and_demand_final[index,'pattern'] == "Supply Reversal Pattern"){
      Signal_df[increment,"Strategy"] <- "Supply_and_Demand"
      Signal_df[increment,"Stock"]=stock
      Signal_df[increment,"Signal"]="SELL"
      Signal_df[increment,"Datetime"]=df_supply_and_demand_final[index,'Date']
      Signal_df[increment,"Value"]=df_supply_and_demand_final[index,'zone_1']
      Signal_df[increment,"StopLoss"]=df_supply_and_demand_final[index,'zone_2']
      target = abs(df_supply_and_demand_final[index,'zone_1'] - df_supply_and_demand_final[index,'zone_2'])
      Signal_df[increment,"Target"]= df_supply_and_demand_final[index,'zone_1'] - target
      
      increment = increment + 1
      
    }else if(df_supply_and_demand_final[index,'pattern'] == "Demand Continuous Pattern" | df_supply_and_demand_final[index,'pattern'] == "Demand Reversal Pattern"){
      Signal_df[increment,"Strategy"] <- "Supply_and_Demand"
      Signal_df[increment,"Stock"]=stock
      Signal_df[increment,"Signal"]="BUY"
      Signal_df[increment,"Datetime"]=df_supply_and_demand_final[index,'Date']
      Signal_df[increment,"Value"]=df_supply_and_demand_final[index,'zone_1']
      Signal_df[increment,"StopLoss"]=df_supply_and_demand_final[index,'zone_2']
      target = abs(df_supply_and_demand_final[index,'zone_2'] - df_supply_and_demand_final[index,'zone_2'])
      Signal_df[increment,"Target"]= df_supply_and_demand_final[index,'zone_1'] + target
      
      increment = increment + 1
    }

  }
}


temp_df <- Signal_df

Signal_df <- temp_df

if(nrow(Signal_df) > 0){
  # Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
  # Signal_df$Datetime <- Signal_df$Datetime
  Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
  stop_loss <- 1
  target <- 0.5
  
  Capital <- 1000
  
  Signal_df$StopLoss <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
  Signal_df$Target <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
  
  Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  
  Signal_df <-Signal_df[order(Signal_df$Datetime),]
  
  row.names(Signal_df) <- 1:nrow(Signal_df)
  
  print(Signal_df)
  
  
  for(i in 1:nrow(Signal_df)){
    # if(as.numeric(difftime(Sys.time(), Signal_df[i,4], units ="mins")) <= 10){
      print(Signal_df[i,2])
      
      # login_params = list(api_key = 'LPUVlRxd')
      # 
      # login_object = create_connection_object(login_params)
      # 
      # session_data <- generate_session(login_object,"J95213","startteja123")
      
      limit_price = 0
      
      
      
      trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$TradingSymbol
      symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(Signal_df[i,2]),]$Symbol.Token
      
      transaction_type = as.character(Signal_df[i,3])
      exchange_counter = "NSE"
      order_type = "LIMIT"
      product_type = "INTRADAY"
      duration_day = "DAY"
      stoploss = as.numeric(Signal_df[i,6])
      
      squareoff = as.numeric(Signal_df[i,7])
      # qty = Signal_df[i,8]
      qty = as.numeric(Signal_df[i,8])
      limit_price = as.numeric((Signal_df[i,5]))
      
      # print(session_data)
      print(trading_symbol)
      print(symbol_token)
      print(transaction_type)
      print(order_type)
      print(product_type)
      print(duration_day)
      print(limit_price)
      print(squareoff)
      print(stoploss)
      print(qty)
      
      order_place <- place_order(object = session_data,
                                 variety= "NORMAL",
                                 tradingsymbol= as.character(trading_symbol),
                                 symboltoken= as.character(symbol_token),
                                 transactiontype= as.character(transaction_type),
                                 exchange= as.character(exchange_counter),
                                 ordertype= as.character(order_type),
                                 producttype= as.character(product_type),
                                 duration= as.character(duration_day),
                                 price= as.numeric(limit_price),
                                 squareoff= as.numeric(squareoff),
                                 stoploss= as.numeric(stoploss),
                                 quantity= as.numeric(qty)
      )
      
      # order_place <- place_order(object = session_data,
      #                            variety= "NORMAL",
      #                            tradingsymbol= "BANDHANBNK-EQ",
      #                            symboltoken= "2263",
      #                            transactiontype= "BUY",
      #                            exchange= "NSE",
      #                            # ordertype= "MARKET",
      #                            ordertype = "LIMIT",
      #                            producttype= "INTRADAY",
      #                            duration= "DAY",
      #                            price= 300,
      #                            # price = "377",
      #                            squareoff= "400",
      #                            stoploss= "360",
      #                            quantity= 100000
      # )
      
      # if(order_place > 0){
      #   if(transaction_type == "BUY"){
      #     transaction_type = "SELL"
      #   }else{
      #     transaction_type = "BUY"
      #   }
      #   
      #   order_place <- place_order(object = session_data,
      #                              variety= "NORMAL",
      #                              tradingsymbol= as.character(trading_symbol),
      #                              symboltoken= as.character(symbol_token),
      #                              transactiontype= transaction_type,
      #                              exchange= "NSE",
      #                              ordertype = "LIMIT",
      #                              producttype= "INTRADAY",
      #                              duration= "DAY",
      #                              price= as.numeric(squareoff),
      #                              squareoff= 0,
      #                              stoploss= 0,
      #                              quantity= qty
      #   )
      #   print(order_place)
      # }
      
      # print(order_place)
      
      
    # }
  }
}





