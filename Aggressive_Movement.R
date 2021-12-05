library(smartapi)
library("RJSONIO")
library(magrittr)
library(dplyr)
library(lubridate)
library("sqldf")
require(sqldf)


login_params = list(api_key = 'LPUVlRxd')

login_object = create_connection_object(login_params)

session_data <- generate_session(login_object,"J95213","startteja123")

get_candle_data(object = session_data,exchange = "NSE",symboltoken="17963",interval="ONE_MINUTE",fromdate="2021-05-01 09:00",todate=Sys.Date())



order_place <- place_order(object = session_data,
                           variety= "STOPLOSS",
                           tradingsymbol= "ITC-EQ",
                           exchange= "NSE",
                           quantity= 323,
                           transactiontype= "BUY",
                           ordertype = "STOPLOSS_LIMIT",
                           producttype= "INTRADAY",
                           price= "210",
                           triggerprice = "209",
                           symboltoken= "1660",
                           duration= "DAY"
                           
)

nifty_50_data <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Nifty50_Stocks.csv")
# stocks <- c("TCS.NS","BAJFINANCE.NS","BAJAJFINSV.NS","%5ENSEBANK","%5ENSEI")

stocks <- c("BAJFINANCE.NS")

generate_orders <- function(stock){
  
  increment = 1
  Signal_df = data.frame("Strategy"=character(0), "Stock"=character(0),"Signal"=character(0),"Datetime"=character(0),"Value"=character(0))
  
  response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=5m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
  # response_data <- fromJSON(paste("https://query1.finance.yahoo.com/v8/finance/chart/",stock,"?region=IN&lang=en-IN&includePrePost=false&interval=15m&range=1d&corsDomain=in.finance.yahoo.com&.tsrc=financet", sep=""))
  
  stock_timestamp <- response_data$chart$result[[1]]$timestamp
  Close <- response_data$chart$result[[1]]$indicators$quote[[1]]$close
  High <- response_data$chart$result[[1]]$indicators$quote[[1]]$high
  Low <- response_data$chart$result[[1]]$indicators$quote[[1]]$low
  Open <- response_data$chart$result[[1]]$indicators$quote[[1]]$open
  Volume <- response_data$chart$result[[1]]$indicators$quote[[1]]$volume
  final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),Close,High,Low,Open,Volume))
  
  # browser()
  
  if(nrow(final_data) == 0){
    stock_timestamp <- response_data$chart$result[[2]][[1]]
    Close <- response_data$chart$result[[3]]$quote[[1]]$close
    High <- response_data$chart$result[[3]]$quote[[1]]$high
    Low <- response_data$chart$result[[3]]$quote[[1]]$low
    Open <- response_data$chart$result[[3]]$quote[[1]]$open
    Volume <- response_data$chart$result[[3]]$quote[[1]]$volume
    
    
    final_data <- as.data.frame(cbind(as.POSIXct(stock_timestamp, origin="1970-01-01"),as.numeric(unlist(Close)),as.numeric(unlist(High)),as.numeric(unlist(Low)),as.numeric(unlist(Open)),as.numeric(unlist(Volume))))
  }
  
  
  colnames(final_data) <- c("V1","Close","High","Low","Open","Volume")
  
  if(typeof(final_data$V1) == "list"){
    final_data <- final_data[-c(which(final_data$Close == "NULL")),]
    new_stock_timestamp <- unlist(final_data$V1)
    Close <- unlist(final_data$Close)
    High <- unlist(final_data$High)
    Open <- unlist(final_data$Open)
    Low <- unlist(final_data$Low)
    Volume <- unlist(final_data$Volume)
    
    final_data <- data.frame(new_stock_timestamp,Close,High,Low,Open,Volume)
    
    final_data$Date <- as.POSIXct(final_data$new_stock_timestamp, origin="1970-01-01")
    
    final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
  }else{
    final_data$Date <- as.POSIXct(final_data$V1, origin="1970-01-01")
    
    final_data <- final_data %>% select(Date, Open, High, Low, Close,Volume)
  }
  final_data <- na.omit(final_data)
  # browser()
  # print(final_data)
  
  # if(minute(final_data[nrow(final_data),]$dates) %% 5 == 0){
  #   final_data <- final_data
  # }else{
  #   final_data <- final_data[1:nrow(final_data)-1,]
  # }
  
  final_data$Call <- ""
  
  satisfied_df = data.frame(Date = character(0),Open = numeric(0),High=numeric(0),Low = numeric(0),Close = numeric(0),Volume = numeric(0),Call=character(0))
  
  data <- as.data.frame(final_data)
  
  # print(data)
  
  for(index in 2:nrow(data)){
    previous_range = data[index-1,"High"] - data[index-1,"Low"]
    current_range = data[index,"High"] - data[index,"Low"]
    

    if(current_range >= 1.8*(previous_range)){
      # print(data[index,])
      
      temp_comb = cbind(final_data[index,],final_data[index-1,])
      colnames(temp_comb) = c("current_date","current_open","current_high","current_low","current_close","current_volume","current_call","previous_date","previous_open","previous_high","previous_low","previous_close","previous_volume","previous_call")
      
      # print(temp_comb)
      
      temp_comb_final = temp_comb %>% select(current_date,previous_open,previous_high,previous_low,previous_close,previous_volume,current_call)
      
      colnames(temp_comb_final) <- c("Date","Open","High","Low","Close","Volume","Call")
      
      satisfied_df = rbind(satisfied_df,temp_comb_final)
      rownames(satisfied_df) <- 1:nrow(satisfied_df)
      if(data[index,"Close"] > data[index,"Open"]){
        satisfied_df[nrow(satisfied_df),"Call"] <- "BUY"
      }else{
        satisfied_df[nrow(satisfied_df),"Call"] <- "SELL"
      }
      
      
    }
    
  }
  
  # print(satisfied_df)
  
  if(nrow(satisfied_df) == 0){
    next
  }
  else{
    
    for(j in 1:nrow(satisfied_df)){
      
      time_min = format(as_datetime(as.character(as_datetime(satisfied_df[j,"Date"]) + hm("5:30")),tz="Asia/Kolkata"), format="%H:%M:%S")
      # print(time_min)
      # browser()
      if(time_min <= "15:10:00"){
        Signal_df[increment,"Strategy"] <- "Agg_Movement"
        Signal_df[increment,"Stock"]=stock
        Signal_df[increment,"Signal"]=satisfied_df[j,"Call"]
        Signal_df[increment,"Datetime"]=satisfied_df[j,"Date"]
        # Signal_df[increment,"Value"]=satisfied_df[j,"Close"]
        
        if(satisfied_df[j,"Call"] == "BUY"){
          Signal_df[increment,"Value"] = satisfied_df[j,"High"]
          Signal_df[increment,"StopLoss"] = satisfied_df[j,"Low"]
          
          target = satisfied_df[j,"High"] - satisfied_df[j,"Low"]
          
          Signal_df[increment,"Target"] = satisfied_df[j,"High"] + target
          
        }else{
          Signal_df[increment,"Value"] = satisfied_df[j,"Low"]
          Signal_df[increment,"StopLoss"] = satisfied_df[j,"High"]
          target = satisfied_df[j,"High"] - satisfied_df[j,"Low"]
          
          Signal_df[increment,"Target"] = satisfied_df[j,"Low"] - target
          
          
        }
        
        increment = increment + 1
      }else{
        next
      }
      
    }
  }
  
  return(Signal_df)
}


generate_params <- function(Signal_df){
  Signal_df$Datetime <- as.POSIXct(as.numeric(as.character(Signal_df$Datetime)),origin="1970-01-01")
  Signal_df$Datetime <- Signal_df$Datetime
  Signal_df$Value <- round(as.numeric(Signal_df$Value),2)
  Signal_df$StopLoss <- round(as.numeric(Signal_df$StopLoss),2)
  Signal_df$Target <- round(as.numeric(Signal_df$Target),2)
  
  stop_loss <- 1
  target <- 0.5
  
  Capital <- 100
  
  # Signal_df$StopLoss <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value-((stop_loss*Signal_df$Value)/100),((stop_loss*Signal_df$Value)/100)+Signal_df$Value)
  # Signal_df$Target <- ifelse(Signal_df$Signal == "BUY",Signal_df$Value+((target*Signal_df$Value)/100),Signal_df$Value-((target*Signal_df$Value)/100))
  
  Signal_df$Qty <- round(abs((20/100)*Capital/(Signal_df$Target - Signal_df$StopLoss)),0)
  
  Signal_df <-Signal_df[order(Signal_df$Datetime),]
  
  row.names(Signal_df) <- 1:nrow(Signal_df)
  
  # print(Signal_df)
  
  Signal_df <- as.data.frame(Signal_df %>%
                               group_by(Strategy,Stock) %>%
                               mutate(exec_rnk = order(order(Datetime, decreasing=FALSE))))
  Signal_df$order_place <- 0
  Signal_df$order_id <- 0
  Signal_df$target_order_id <- 0
  Signal_df$cancel_order_id <- 0
  Signal_df$conclusion <- ""
  
  return(Signal_df)
}


execute_orders <- function(order_data,execute_df){
  
  #######     Iterate over the Signals Data    ##########
  # browser()
  for(i in 1:nrow(execute_df)){
    
    # browser()
    limit_price = 0
    
    trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[i,2]),]$TradingSymbol
    symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[i,2]),]$Symbol.Token
    
    
    
    ##########   Get the current Order parameters   #################
    transaction_type = as.character(execute_df[i,3])
    exchange_counter = "NSE"
    order_type = "LIMIT"
    product_type = "INTRADAY"
    duration_day = "DAY"
    stoploss = as.numeric(execute_df[i,6])
    
    squareoff = as.numeric(execute_df[i,7])
    # qty = execute_df[i,8]
    qty = as.numeric(execute_df[i,8])
    limit_price = as.numeric((execute_df[i,5]))
    
    # browser()
    
    ##########   Execute the firt order   ###################3
    if(execute_df[i,"exec_rnk"] == 1 && (as.numeric(difftime(Sys.time(), execute_df[i,"Datetime"], units ="mins")) <= 15)){
      if(execute_df[i,"order_place"] == 0){
        print("Hello")
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
        
        Sys.sleep(2)
        
        execute_df[i,"order_id"] <- order_place
        execute_df[i,"order_place"] <- 1
        execute_df[i,"conclusion"] <- "Pending"
      }
    }else if(i > 1){
      #########   Start Checking from the second order execution ##############
      if(execute_df[i,"order_place"] == 0){
        
        # browser()
        print("before order book")
        my_orders <- order_book(session_data)
        
        Sys.sleep(1)
        
        order_data = data.frame(variety = as.character(),
                                ordertype=as.character(),
                                producttype=as.character(),
                                duration=as.character(),
                                price = as.character(),
                                triggerprice = as.character(),
                                quantity = as.character(),
                                disclosedquantity = as.character(),
                                squareoff = as.character(),
                                stoploss = as.character(),
                                trailingstoploss = as.character(),
                                tradingsymbol = as.character(),
                                transactiontype = as.character(),
                                exchange = as.character(),
                                symboltoken = as.character(),
                                ordertag = as.character(),
                                instrumenttype = as.character(),
                                strikeprice = as.character(),
                                optiontype = as.character(),
                                expirydate = as.character(),
                                lotsize = as.character(),
                                cancelsize = as.character(),
                                averageprice = as.character(),
                                filledshares = as.character(),
                                unfilledshares = as.character(),
                                orderid = as.character(),
                                text = as.character(),
                                status = as.character(),
                                updatetime = as.character(),
                                exchorderupdatetime = as.character(),
                                fillid = as.character(),
                                filltime = as.character(),
                                parentorderid = as.character()
        )
        
        if(length(my_orders$data) > 0){
          for(ind in 1:length(my_orders$data)){
            
            order_data[ind,"variety"] = my_orders$data[[ind]]$variety
            order_data[ind,"ordertype"] = my_orders$data[[ind]]$ordertype
            order_data[ind,"producttype"] = my_orders$data[[ind]]$producttype
            order_data[ind,"duration"] = my_orders$data[[ind]]$duration
            order_data[ind,"price"] = my_orders$data[[ind]]$price
            order_data[ind,"triggerprice"] = my_orders$data[[ind]]$triggerprice
            order_data[ind,"quantity"] = my_orders$data[[ind]]$quantity
            order_data[ind,"disclosedquantity"] = my_orders$data[[ind]]$disclosedquantity
            order_data[ind,"squareoff"] = my_orders$data[[ind]]$squareoff
            order_data[ind,"stoploss"] = my_orders$data[[ind]]$stoploss
            order_data[ind,"trailingstoploss"] = my_orders$data[[ind]]$trailingstoploss
            order_data[ind,"tradingsymbol"] = my_orders$data[[ind]]$tradingsymbol
            order_data[ind,"transactiontype"] = my_orders$data[[ind]]$transactiontype
            order_data[ind,"exchange"] = my_orders$data[[ind]]$exchange
            order_data[ind,"symboltoken"] = my_orders$data[[ind]]$symboltoken
            order_data[ind,"ordertag"] = my_orders$data[[ind]]$ordertag
            order_data[ind,"instrumenttype"] = my_orders$data[[ind]]$instrumenttype
            order_data[ind,"strikeprice"] = my_orders$data[[ind]]$strikeprice
            order_data[ind,"optiontype"] = my_orders$data[[ind]]$optiontype
            order_data[ind,"expirydate"] = my_orders$data[[ind]]$expirydate
            order_data[ind,"lotsize"] = my_orders$data[[ind]]$lotsize
            order_data[ind,"cancelsize"] = my_orders$data[[ind]]$cancelsize
            order_data[ind,"averageprice"] = my_orders$data[[ind]]$averageprice
            order_data[ind,"filledshares"] = my_orders$data[[ind]]$filledshares
            order_data[ind,"unfilledshares"] = my_orders$data[[ind]]$unfilledshares
            order_data[ind,"orderid"] = my_orders$data[[ind]]$orderid
            order_data[ind,"text"] = my_orders$data[[ind]]$text
            order_data[ind,"status"] = my_orders$data[[ind]]$status
            order_data[ind,"updatetime"] = my_orders$data[[ind]]$updatetime
            order_data[ind,"exchorderupdatetime"] = my_orders$data[[ind]]$exchorderupdatetime
            order_data[ind,"fillid"] = my_orders$data[[ind]]$fillid
            order_data[ind,"filltime"] = my_orders$data[[ind]]$filltime
            order_data[ind,"parentorderid"] = my_orders$data[[ind]]$parentorderid
            
          }
        }
        
        
        
        ########## Current Execution details ###########
        current_stock_df <- execute_df[execute_df$Stock == execute_df[i,"Stock"],]
        # print(current_stock_df)
        
        ###########  Previous Order Details ###########
        prev_order = current_stock_df[current_stock_df$exec_rnk==(i-1),]
        
        rownames(prev_order) <- 1
        # print(prev_order)
        prev_order_id <- prev_order[1,"order_id"]
        
        # print(prev_order_id)
        
        last_order <- order_data[order_data$orderid == as.character(prev_order_id),]
        
        # print((as.numeric(difftime(Sys.time(), Signal_df[(i-1),"Datetime"], units ="mins"))))
        
        print(last_order)
        
        if(nrow(last_order) > 0 ){
          
          if(last_order$status == "complete"){
            # if(last_order$status == "rejected"){
            
            ########      Placing the Target Order      ########
            
            if(prev_order[1,"target_order_id"] == 0){
              browser()
              prev_generation <- execute_df[(i-1),]
              
              print(prev_generation)
              
              prev_trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[(i-1),2]),]$TradingSymbol
              prev_symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[(i-1),2]),]$Symbol.Token
              
              prev_transaction_type = as.character(execute_df[(i-1),3])
              prev_exchange_counter = "NSE"
              prev_order_type = "LIMIT"
              prev_product_type = "INTRADAY"
              prev_duration_day = "DAY"
              prev_stoploss = as.numeric(execute_df[(i-1),6])
              
              prev_squareoff = as.numeric(execute_df[(i-1),7])
              # qty = Signal_df[i,8]
              prev_qty = as.numeric(execute_df[(i-1),8])
              prev_limit_price = as.numeric((execute_df[(i-1),5]))
              
              if(prev_transaction_type == "BUY"){
                prev_transaction_type = "SELL"
              }else{
                prev_transaction_type = "BUY"
              }
              
              print("placing target")
              order_place <- place_order(object = session_data,
                                         variety= "NORMAL",
                                         tradingsymbol= as.character(prev_trading_symbol),
                                         symboltoken= as.character(prev_symbol_token),
                                         transactiontype= prev_transaction_type,
                                         exchange= "NSE",
                                         ordertype = "LIMIT",
                                         producttype= "INTRADAY",
                                         duration= "DAY",
                                         price= as.numeric(prev_squareoff),
                                         squareoff= 0,
                                         stoploss= 0,
                                         quantity= prev_qty
              )
              Sys.sleep(2)
              
              # print(order_place)
              
              execute_df[(i-1),"target_order_id"] <- order_place
              execute_df[(i-1),"target_place"] <- 1
              execute_df[(i-1),"conclusion"] <- "Target Pending"
            }
            
            
          }
          if(last_order$status == "open" || (as.numeric(difftime(Sys.time(), execute_df[(i-1),"Datetime"], units ="mins")) <= 15)){
            print("placing cancel")
            cancel_id = cancel_order(object = session_data,
                                     orderid = as.character(prev_order_id),
                                     variety = "NORMAL")
            
            Sys.sleep(2)
            print(cancel_id)
            execute_df[(i-1),"cancel_order_id"] <- cancel_id
            execute_df[(i-1),"conclusion"] <- "Cancelled Order"
          }
          
        
      }
      
      
      
    }else{
      curr_order = current_stock_df[current_stock_df$exec_rnk==(i),]
      
      rownames(curr_order) <- 1
      # print(prev_order)
      curr_order_id <- curr_order[1,"order_id"]
      
      curr_order_df <- order_data[order_data$orderid == as.character(curr_order_id),]
      
      print(curr_order_df)
      
      if(curr_order_df$status == "complete"){
        
        curr_generation <- execute_df[i,]
        
        # print(prev_generation)
        
        curr_trading_symbol = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[(i),2]),]$TradingSymbol
        curr_symbol_token = nifty_50_data[nifty_50_data$Yahoo.Symbol==as.character(execute_df[(i),2]),]$Symbol.Token
        
        curr_transaction_type = as.character(execute_df[i,3])
        curr_exchange_counter = "NSE"
        curr_order_type = "LIMIT"
        curr_product_type = "INTRADAY"
        curr_duration_day = "DAY"
        curr_stoploss = as.numeric(execute_df[i,6])
        
        curr_squareoff = as.numeric(execute_df[i,7])
        # qty = Signal_df[i,8]
        curr_qty = as.numeric(execute_df[i,8])
        curr_limit_price = as.numeric((execute_df[i,5]))
        
        if(curr_transaction_type == "BUY"){
          curr_transaction_type = "SELL"
        }else{
          curr_transaction_type = "BUY"
        }
        # browser()
        print("palcing target order")
        print(curr_generation)
        print(curr_generation[i,"target_order_id"])
        if(curr_generation[i,"target_order_id"] == 0){
          order_place <- place_order(object = session_data,
                                     variety= "NORMAL",
                                     tradingsymbol= as.character(curr_trading_symbol),
                                     symboltoken= as.character(curr_symbol_token),
                                     transactiontype= curr_transaction_type,
                                     exchange= "NSE",
                                     ordertype = "LIMIT",
                                     producttype= "INTRADAY",
                                     duration= "DAY",
                                     price= as.numeric(curr_squareoff),
                                     squareoff= 0,
                                     stoploss= 0,
                                     quantity= curr_qty
          )
          Sys.sleep(2)
          
          print(order_place)
          
          execute_df[i,"target_order_id"] <- order_place
          execute_df[i,"target_place"] <- 1
          execute_df[i,"conclusion"] <- "Target Pending"
          
        }
        
        
      }
      
      
    }
      
      if(execute_df[i,"order_place"] == 0 && (as.numeric(difftime(Sys.time(), execute_df[i,"Datetime"], units ="mins")) <= 15)){
        
        ########  Placing the current order ############
        print("placing current orders")
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
        Sys.sleep(2)
        
        execute_df[i,"order_id"] <- order_place
        execute_df[i,"order_place"] <- 1
        execute_df[i,"conclusion"] <- "Pending"
        
        execute_df
        
      }else{
        execute_df[i,"conclusion"] <- "Skip as time past"
      }
        
    }else{
      execute_df[i,"conclusion"] <- "Skip as time past"
      }
      
      
  }
  
  return(execute_df)
  
}

# nifty_50_data <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Nifty50_Stocks.csv")



stocks <- c("BAJFINANCE.NS","TCS.NS","GRASIM.NS","TITAN.NS")

stocks <- c("BAJFINANCE.NS")

signals_export <- data.frame("Strategy" = character(),"Stock"=character(), "Signal"=character(),"Datetime"=as.POSIXct(character()),"Value"=character(),"StopLoss"=character(),"Target"=character(),"Qty"=character(), "exec_rnk"=character(), "order_place"=character(),"order_id"=character(),"target_order_id"=character(),"cancel_order_id"=character(),"conclusion"=character())

for(stk in 1:length(stocks)){
  
  print(stocks[stk])
  
  
  Signal_df <- generate_orders(stocks[stk])

# if(nrow(Signal_df)> 0 ){
  Signal_df <- generate_params(Signal_df)

  # print(Signal_df)
  
  Sys.sleep(1)
  my_orders <- order_book(session_data)

  order_data = data.frame(variety = as.character(),
                          ordertype=as.character(),
                          producttype=as.character(),
                          duration=as.character(),
                          price = as.character(),
                          triggerprice = as.character(),
                          quantity = as.character(),
                          disclosedquantity = as.character(),
                          squareoff = as.character(),
                          stoploss = as.character(),
                          trailingstoploss = as.character(),
                          tradingsymbol = as.character(),
                          transactiontype = as.character(),
                          exchange = as.character(),
                          symboltoken = as.character(),
                          ordertag = as.character(),
                          instrumenttype = as.character(),
                          strikeprice = as.character(),
                          optiontype = as.character(),
                          expirydate = as.character(),
                          lotsize = as.character(),
                          cancelsize = as.character(),
                          averageprice = as.character(),
                          filledshares = as.character(),
                          unfilledshares = as.character(),
                          orderid = as.character(),
                          text = as.character(),
                          status = as.character(),
                          updatetime = as.character(),
                          exchorderupdatetime = as.character(),
                          fillid = as.character(),
                          filltime = as.character(),
                          parentorderid = as.character()
  )


  if(length(my_orders$data) > 0){
    for(ind in 1:length(my_orders$data)){

      order_data[ind,"variety"] = my_orders$data[[ind]]$variety
      order_data[ind,"ordertype"] = my_orders$data[[ind]]$ordertype
      order_data[ind,"producttype"] = my_orders$data[[ind]]$producttype
      order_data[ind,"duration"] = my_orders$data[[ind]]$duration
      order_data[ind,"price"] = my_orders$data[[ind]]$price
      order_data[ind,"triggerprice"] = my_orders$data[[ind]]$triggerprice
      order_data[ind,"quantity"] = my_orders$data[[ind]]$quantity
      order_data[ind,"disclosedquantity"] = my_orders$data[[ind]]$disclosedquantity
      order_data[ind,"squareoff"] = my_orders$data[[ind]]$squareoff
      order_data[ind,"stoploss"] = my_orders$data[[ind]]$stoploss
      order_data[ind,"trailingstoploss"] = my_orders$data[[ind]]$trailingstoploss
      order_data[ind,"tradingsymbol"] = my_orders$data[[ind]]$tradingsymbol
      order_data[ind,"transactiontype"] = my_orders$data[[ind]]$transactiontype
      order_data[ind,"exchange"] = my_orders$data[[ind]]$exchange
      order_data[ind,"symboltoken"] = my_orders$data[[ind]]$symboltoken
      order_data[ind,"ordertag"] = my_orders$data[[ind]]$ordertag
      order_data[ind,"instrumenttype"] = my_orders$data[[ind]]$instrumenttype
      order_data[ind,"strikeprice"] = my_orders$data[[ind]]$strikeprice
      order_data[ind,"optiontype"] = my_orders$data[[ind]]$optiontype
      order_data[ind,"expirydate"] = my_orders$data[[ind]]$expirydate
      order_data[ind,"lotsize"] = my_orders$data[[ind]]$lotsize
      order_data[ind,"cancelsize"] = my_orders$data[[ind]]$cancelsize
      order_data[ind,"averageprice"] = my_orders$data[[ind]]$averageprice
      order_data[ind,"filledshares"] = my_orders$data[[ind]]$filledshares
      order_data[ind,"unfilledshares"] = my_orders$data[[ind]]$unfilledshares
      order_data[ind,"orderid"] = my_orders$data[[ind]]$orderid
      order_data[ind,"text"] = my_orders$data[[ind]]$text
      order_data[ind,"status"] = my_orders$data[[ind]]$status
      order_data[ind,"updatetime"] = my_orders$data[[ind]]$updatetime
      order_data[ind,"exchorderupdatetime"] = my_orders$data[[ind]]$exchorderupdatetime
      order_data[ind,"fillid"] = my_orders$data[[ind]]$fillid
      order_data[ind,"filltime"] = my_orders$data[[ind]]$filltime
      order_data[ind,"parentorderid"] = my_orders$data[[ind]]$parentorderid

    }
  }


  temp_signals_df <- read.csv("~/Desktop/Reddy_Stocks_Application/data/Aggressive_Movement.csv")
  temp_signals_df <- temp_signals_df %>% select(-X)
  temp_signals_df$Datetime <- as.POSIXct(temp_signals_df$Datetime, origin="1970-01-01")
  # temp_signals_df <- temp_signals_df[-c(1:nrow(temp_signals_df)),]

  final_signals_df <- sqldf("select COALESCE(tsd.Strategy,sd.Strategy) as Strategy,
                                  COALESCE(tsd.Stock,sd.Stock) as Stock,
                                  COALESCE(tsd.Signal,sd.Signal) as Signal,
                                  COALESCE(tsd.Datetime,sd.Datetime) as Datetime,
                                  COALESCE(tsd.Value,sd.Value) as Value,
                                  COALESCE(tsd.StopLoss,sd.StopLoss) as StopLoss,
                                  COALESCE(tsd.Target,sd.Target) as Target,
                                  COALESCE(tsd.Qty,sd.Qty) as Qty,
                                  COALESCE(tsd.exec_rnk,sd.exec_rnk) as exec_rnk,
                                  COALESCE(cast(tsd.order_place as numeric),cast(sd.order_place as numeric) )as order_place,
                                  COALESCE(cast(tsd.order_id as numeric),cast(sd.order_id as numeric)) as order_id,
                                  COALESCE(cast(tsd.target_order_id as numeric),cast(sd.target_order_id as numeric)) as target_order_id,
                                  COALESCE(cast(tsd.cancel_order_id as numeric),cast(sd.cancel_order_id as numeric)) as cancel_order_id,
                                  COALESCE(tsd.conclusion,sd.conclusion) as conclusion

                           from Signal_df sd
                           left join temp_signals_df tsd on sd.Strategy = tsd.Strategy and sd.Stock = tsd.Stock and sd.Datetime = tsd.Datetime
                          ")
  # print("final data signals")
  # print(final_signals_df)
  
  earlier_signals <- temp_signals_df
  
  Sys.sleep(2)
  
  tryCatch(
    {

      Signal_df <- execute_orders(order_data,final_signals_df)
    },
    error=function(cond) {
      print(cond)
      print("inside error")
      Signal_df <- earlier_signals
    },
    warning=function(cond) {
      print(cond)
      print("inside warning")
    },
    finally={
      # print(colnames(Signal_df))
      # print(Signal_df)
      Signal_df <- Signal_df %>% select(c("Strategy","Stock","Signal","Datetime","Value","StopLoss","Target","Qty","exec_rnk","order_place","order_id","target_order_id","cancel_order_id","conclusion"))
      # print(colnames(signals_export))
      # print(signals_export)
      
      # signals_export$target_place <- 0
      signals_export <- rbind(signals_export,Signal_df)
      # print("signals export")
      # print(signals_export)
    }
  )
# }



# }


}


# temp_signals_df

write.csv(signals_export,"~/Desktop/Reddy_Stocks_Application/data/Aggressive_Movement.csv")
print("exported successfully")

