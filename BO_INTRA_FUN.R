require("pbapply"); require("dplyr"); require("highfrequency"); require("quantmod"); require("data.table")
require("RobinHood");require("lubridate");require("timeDate");require("jsonlite");require("httr")
PASS <- new.env()
assign("username","*******************",envir = PASS)
assign("password","*******************",envir = PASS)
# time difference between current time zone and NY-time
tmDIFF = round(as.numeric(difftime(Sys.time(),
                                   lubridate::force_tz(with_tz(Sys.time(),tz="America/New_York")),
                                   units = "hours")),0)
# BEST COMBINATIONS FOR INDICES
BEST <- rbind(
as.data.frame(cbind("QQQ",9,50,7)),
as.data.frame(cbind("SPY",15,39,20)),
as.data.frame(cbind("DIA",9,38,20)),
as.data.frame(cbind("IWM",9,19,20)))
# as.data.frame(cbind("OEF",9,50,9))
colnames(BEST) <- c("Symbol","lookBackDays","volDays","holdBars")
# ***********************************************************************************************
### wrapper to get historical closes
# barChart API : https://www.barchart.com/ondemand/free-market-data-api
histData = function(symbol, MINUTES)
{
  # establish RH connection
  # create a connection with Robin Hood: disable Two-Factor Auth on App
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # get raw historical data via RH api
  data = get_historicals(RH, symbol, interval = paste0(MINUTES,'minute'), 
                         span = 'week', tz=Sys.timezone()) 
  # subset the relevant columns only
  data = data[,1:6]
  # timestamps -> as.character
  data[,"begins_at"]<- sapply(data[,"begins_at"],as.character)
  # OHLCV -> as.numeric
  data[,2:6]<- sapply(data[,2:6],as.numeric)
  # format column names
  colnames(data) <- c("timestamp","Open","Close","High","Low","Volume")
  # fix timestamps
  data$timestamp <- as.POSIXct(as.character(data[,"timestamp"]), 
                               tz=Sys.timezone(), format="%Y-%m-%d %H:%M:%S")
  # fix column names -> quantmod friendly
  colnames(data)[2:6] <- paste0(as.character(symbol),".",names(data))[2:6]
  # convert to xts
  intradata = xts(data[,c(2:6)], order.by=data[,"timestamp"])
  # in case we have missing bars
  dat = intradata
  dat = make.index.unique(dat)
  if(MINUTES < 240)
  {
    # convert bars
    dat = to.period(x=dat,period="minutes",k=MINUTES,indexAt = "lastof",name = symbol)
    colnames(dat) = names(data)[2:6]
    # MKTM == "T06:00/T13:00"
    MKTM = paste0("T0",9+as.numeric(tmDIFF),":30/T",16+as.numeric(tmDIFF),":00")
    dat = dat[MKTM] %>% suppressWarnings()
  }
  # only weekdays
  dat <- dat[isBizday(as.timeDate(index(dat)), 
                      holidays=holidayNYSE(year=as.numeric(format(Sys.time(),"%Y"))),
                      wday = 1:5)]
#  print(tail(OHLCV(dat),5));cat("\n")
  dat
}
# ***********************************************************************************************
# find breakouts
breakOut = function(df, lookBackDays, volDays, closeAtHigh=FALSE)
{
  # add MAX price column
  df$MAX = na.omit(apply(coredata(OHLC(df)),1, max))
  
  # rolling Volume Average
  df$VoAvg <- rollmean(x=Vo(df),k = volDays, align = "right")
  
  tmp = merge(Cl(df), df$MAX)
  # rollapply function every step ('by') & window is width
  tmp <- rollapply(data=tmp, width=lookBackDays,by=1,align = 'right',by.column = FALSE, 
                   FUN=function(x){
                     LAST <- as.numeric(last(Cl(x))) # last is the last/latest price in nth place 
                     bo <- nrow(x[LAST >= x$MAX,])   # compare how many times the lastest price >= MAX column
                   })
  # add column to data
  df$nDaysHigher <- as.numeric(coredata(tmp))
  # complete cases
  df <- na.omit(df)
  
  print(tail(df,3));cat("\n")
  # subset breakout days
  if(closeAtHigh == TRUE)
  {
    breakOutDates =  df[which(df$nDaysHigher == lookBackDays & as.numeric(last(Vo(df))) > as.numeric(last(df$VoAvg))),]
  }
  
  if(closeAtHigh == FALSE)
  {
    # stocks not closing at the Highs of the day will return lookBackDays - 1 
    breakOutDates = rbind(df[which(df$nDaysHigher == lookBackDays & Vo(df) > df$VoAvg),],
                          df[which(df$nDaysHigher == (lookBackDays-1) & Vo(df) > df$VoAvg),]) %>% 
      make.index.unique(drop=TRUE, fromLast = TRUE)
  }

  print(tail(breakOutDates,2));cat("\n")
  # all time Highs
  breakOutDates
}
addCloseBar = function(bOuts,symbol,barInterval,holdBars)
{
  # extract dates
  BO = index(bOuts)
  # sequence of dates in 'barInterval' span
  RANGE = seq.POSIXt(as.POSIXct("2021-01-01 09:30:00")+hours(tmDIFF),
                     as.POSIXct("2025-12-31 16:00:00")+hours(tmDIFF), 
                     by = paste(barInterval, "min"))
  # ficticious xts / makes time handling easier
  RANGE = xts(rep(NA,length(RANGE)),order.by = RANGE)
  # adjust market time range: "T06:30/T13:00"
  MKTM = paste0("T0",9+as.numeric(tmDIFF),":30/T",16+as.numeric(tmDIFF),":00")
  RANGE = RANGE[MKTM] %>% suppressWarnings()
  # after creating the time sequence -> delete weekends/holidays based on the NYSE calendar
  RANGE = RANGE[isBusinessDay(calendar = "UnitedStates/NYSE",
                              dates = as.Date(index(RANGE),format="%Y-%m-%d %H:%M:%S" )),]
  # extract timestamps only
  RANGE = index(RANGE)
  # get closing Bars 
  ENDS = lapply(as.list(1:length(BO)), function(ii){
    # subset starting date only
    START = BO[ii]
    # locate bar in RANGE
    LOC = which(as.character(RANGE) == as.character(START))
    # Ending BAr
    closeAt = RANGE[LOC+holdBars]
    as.data.frame(closeAt)
  })
  # convert bOuts to df
  bts = as.data.frame(cbind(paste(symbol),coredata(round(bOuts,2)), paste(BO)))
  colnames(bts)[1] = "Symbol"
  colnames(bts)[10] = "openAt"
  # row bind dates/times
  #bts$closeAt <- rbindlist(ENDS)
  bts <- cbind(bts,as.data.frame(do.call(rbind,ENDS)))
  # format colnames
  colnames(bts)[2:6] = c("Open","Close","High","Low","Volume")
  
  bts
}
# ***********************************************************************************************
### RobinHood fractional order requirements
# temporary patch for fractional shares
# readMe: https://github.com/JestonBlu/RobinHood/issues/101
# **********************************************************
stk_order = function (RH, symbol, type, time_in_force, trigger, 
                      price, stop_price = NA, quantity, side) 
{
  check_rh(RH)
  if (!type %in% c("market", "limit")) 
    stop("type must be 'market' or 'limit'")
  if (!time_in_force %in% c("gfd", "gtc", "ioc", "opg")) 
    stop(" time_in_fore must be one of 'gfd', 'gtc', 'ioc', 'opg'")
  if (!trigger %in% c("immediate", "stop")) 
    stop("trigger must be 'immediate' or 'stop'")
  if (trigger == "stop" & is.na(stop_price) == TRUE) 
    stop("stop price cant be null if trigger == 'stop'")
  if (!side %in% c("buy", "sell")) 
    stop("side must be 'buy' or 'sell'")
  if (is.na(stop_price) == TRUE) 
    stop_price <- ""
  quantity <- as.character(quantity)
  price <- as.character(price)
  instrument_url <- paste(api_endpoints(endpoint = "quotes"), 
                          symbol, sep = "")
  instrument <- api_quote(RH, instrument_url)
  instrument_id <- instrument$instrument
  orders <- api_orders(RH = RH, action = "order", instrument_id = instrument_id, 
                       symbol = symbol, type = type, time_in_force = time_in_force, 
                       trigger = trigger, price = price, stop_price = stop_price, 
                       quantity = quantity, side = side)
  return(orders)
}
api_endpoints <- function(endpoint, source = "equity") {
  
  api.endpoint <- list(
    # RobinHood endpoints
    url                = "https://api.robinhood.com/",
    accounts           = "accounts/",
    ach_transfers      = "ach/transfers/",
    ach_relationships  = "ach/relationships/",
    ach_schedules      = "ach/deposit_schedules/",
    forex              = "marketdata/forex/quotes/",
    fundamentals       = "fundamentals/?symbols=",
    historicals        = "quotes/historicals/",
    markets            = "markets/",
    marketdata_options = "marketdata/options/",
    options            = "options/",
    option_positions   = "options/positions/",
    option_orders      = "options/orders/",
    option_instruments = "options/instruments/",
    orders             = "orders/",
    portfolios         = "portfolios/",
    positions          = "positions/",
    quotes             = "quotes/?symbols=",
    tags               = "midlands/tags/tag/",
    instruments        = "instruments/",
    token              = "oauth2/token/",
    revoke_token       = "oauth2/revoke_token/",
    user               = "user/",
    watchlist          = "watchlists/",
    # Nummus endpoints
    url_nummus         = "https://nummus.robinhood.com/",
    accounts_crypto    = "accounts/",
    currency_pairs     = "currency_pairs/",
    holdings_crypto    = "holdings/",
    orders_crypto      = "orders/",
    portfolios_crypto  = "portfolios/"
  )
  
  x <- which(names(api.endpoint) == endpoint)
  
  if (source == "equity") {
    endpoint <- paste(api.endpoint$url, as.character(api.endpoint[x]), sep = "")
  }
  
  if (source == "crypto") {
    endpoint <- paste(api.endpoint$url_nummus, as.character(api.endpoint[x]), sep = "")
  }
  
  
  return(endpoint)
}
api_quote <- function(RH, symbols_url) {
  
  # URL and token
  url <- symbols_url
  token <- paste("Bearer", RH$tokens.access_token)
  
  # GET call
  dta <- GET(url,
             add_headers("Accept" = "application/json",
                         "Content-Type" = "application/json",
                         "Authorization" = token))
  
  # format return
  dta <- mod_json(dta, "fromJSON")
  dta <- as.data.frame(dta$results)
  
  # Check if api did not return any results
  if (nrow(dta) == 0) stop("Symbol not found")
  
  dta <- dta %>%
    dplyr::mutate_at(c("ask_price", "bid_price", "last_trade_price",
                       "last_extended_hours_trade_price",
                       "previous_close", "adjusted_previous_close"), as.numeric) %>%
    dplyr::mutate_at("previous_close_date", lubridate::ymd) %>%
    dplyr::mutate_at("updated_at", lubridate::ymd_hms)
  
  
  return(dta)
}
api_orders <- function(RH, action, status_url = NULL, cancel_url = NULL, instrument_id = NULL, 
                       symbol = NULL, type = NULL,time_in_force = NULL, trigger = NULL, price = NULL, 
                       stop_price = NULL, quantity = NULL,side = NULL, page_size = NULL) {
  
  
  if (action == "order") {
    
    url <- api_endpoints("orders")
    token <- paste("Bearer", RH$tokens.access_token)
    
    detail <- data.frame(account = RH$url.account_id,
                         instrument = instrument_id,
                         symbol = symbol,
                         type = type,
                         time_in_force = time_in_force,
                         trigger = trigger,
                         price = price,
                         stop_price = stop_price,
                         quantity = quantity,
                         side = side,
                         client_id = RH$api_client_id)
    
    # If trigger = "stop" then stop_price must be included, otherwise it must be excluded
    if (trigger == "immediate") {
      detail <- detail[, c("account", "instrument", "symbol", "type", "time_in_force",
                           "trigger", "price", "quantity", "side", "client_id")]
    }
    
    dta <- POST(url = url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token),
                body = mod_json(detail, type = "toJSON"))
    
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename URLs
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
    dta$updated_at <-  lubridate::ymd_hms(dta$updated_at)
    dta$last_transaction_at <-  lubridate::ymd_hms(dta$last_transaction_at)
    dta$created_at <-  lubridate::ymd_hms(dta$created_at)
    dta$fees <- as.numeric(dta$fees)
    dta$cumulative_quantity <- as.numeric(dta$cumulative_quantity)
    dta$stop_price <- as.numeric(dta$stop_price)
    dta$reject_reason <- as.numeric(dta$reject_reason)
    dta$price <- as.numeric(dta$price)
    dta$average_price <- as.numeric(dta$average_price)
    dta$quantity <- as.numeric(dta$quantity)
    
    return(dta)
    
  }
  
  
  if (action == "status") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(status_url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.list(dta)
    
    # Rename urls
    names(dta)[names(dta) %in% c("url", "cancel")] <- c("status_url", "cancel_url")
    
  }
  
  
  if (action == "cancel") {
    
    # Token
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- POST(cancel_url,
                add_headers("Accept" = "application/json",
                            "Content-Type" = "application/json",
                            "Authorization" = token))
    
    # Format return
    dta <- mod_json(dta, "fromJSON")
    
  }
  
  
  if (action == "history") {
    
    url <- paste(api_endpoints("orders"), "?page_size=", page_size, sep = "")
    token <- paste("Bearer", RH$tokens.access_token)
    
    # GET call
    dta <- GET(url,
               add_headers("Accept" = "application/json",
                           "Content-Type" = "application/json",
                           "Authorization" = token))
    
    # format return
    dta <- mod_json(dta, "fromJSON")
    dta <- as.data.frame(dta$results)
    
  }
  
  return(dta)
  
  
}
check_rh <- function(RH) {
  
  # Check if RH is the correct class
  if (class(RH) != "RobinHood") stop("RH must be class RobinHood, see RobinHood()")
  
}
mod_json <- function(x, type) {
  
  if (type == "toJSON") {
    x <- x %>% jsonlite::toJSON()
    x <- substr(x, 2, nchar(x) - 1)
    return(x)
  }
  
  if (type == "fromJSON") {
    x <- jsonlite::fromJSON(rawToChar(x$content))
    return(x)
  }
  
}
# **********************************************************
### send order to Open 
sendOrdersMKT = function(toOpen, maxAlloc){
  require("RobinHood")
  # create a connection with Robin Hood: disable Two-Factor Auth on App
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # *********************************************************************
  # check to see if any stocks that we need to open are already in our portfolio
  # Returns a data frame of stock ownership positions
  positions = get_positions(RH)  # get current positions
  positions = as.data.frame(positions)
  # check if positions exists
  if(nrow(positions) >0)
  {
    # don't send orders if the symbol is in our portfolio
    newOrders = setdiff(toOpen$Symbol,positions$symbol)               # returns stocks not in our portfolio
    newOrders = rbindlist(lapply(as.list(newOrders),function(x) toOpen[toOpen$Symbol == x,])) 
  }
  if(nrow(positions) == 0){
    newOrders = toOpen
  }
  #**********************************************************************
  #  SEND ORDERS
  #**********************************************************************
  if(nrow(newOrders) == 1)
  {
    security     = paste(newOrders$Symbol[1])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
  }
  if(nrow(newOrders) == 2)
  {
    
    security     = paste(newOrders$Symbol[1])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[2])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
  }
  if(nrow(newOrders) == 3)
  {
    
    security     = paste(newOrders$Symbol[1])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[2])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[3])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
  }
  if(nrow(newOrders) == 4)
  {
    
    security     = paste(newOrders$Symbol[1])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[2])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[3])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
    
    security     = paste(newOrders$Symbol[4])
    ACT          = "buy"
    lmtPRC       = RobinHood::get_quote(RH,security)$last_trade_price
    securityShrs = round(maxAlloc/lmtPRC,2)
    
    x = stk_order(RH = RH,symbol = security,type = "market",     
                  time_in_force = "gfd",trigger = "immediate",  
                  price = lmtPRC,quantity = securityShrs, side  = ACT)        
    
  }

  logout(RH)
  # ************************************************************************
}
### send orders to Close
closePOS = function(bar2Cl)
{
  require("RobinHood")
  # create a connection with Robin Hood: disable Two-Factor Auth on App
  RH = RobinHood(username = PASS$username, password = PASS$password)
  # *********************************************************************
  # Returns a data frame of stock ownership positions
  positions = get_positions(RH)  # get current positions
  positions = as.data.frame(positions)
  #positions = positions[!(positions$sectype=="CASH"),]
  NROWS  = ifelse(length(positions)==0,0,nrow(positions))
  if(NROWS > 0)
  {
    # subset to check if tickers are in the portfolio
    newOrders = rbindlist(lapply(as.list(bar2Cl$Symbol),function(x) positions[positions$symbol == x,])) 
    if(nrow(newOrders) == 0){newOrders <- NULL}
  }else{
    newOrders = NULL
  }
  if(is.null(newOrders)){
    cat("\n NOTHING TO CLOSE!!!\n")
  }
  #**********************************************************************
  #  SEND ORDERS
  #**********************************************************************
  if(!is.null(newOrders))
  {
    ii = 0 
    if(nrow(newOrders) == 1)
    {
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT)     
    }
    if(nrow(newOrders) == 2)
    {
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      
    }
    if(nrow(newOrders) == 3)
    {
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      
    }
    if(nrow(newOrders) == 4)
    {
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      ii = ii+1
      security     = paste(newOrders$symbol[ii])
      securityShrs = as.numeric(newOrders$quantity[ii])
      ACT          = "sell"
      lmtPRC       = as.numeric(RobinHood::get_quote(RH,symbol=security,limit_output = TRUE)$last_trade_price)
      x = stk_order(RH = RH,symbol = security,type = "market",     
                    time_in_force = "gfd",trigger = "immediate",  
                    price = lmtPRC,quantity = securityShrs, side  = ACT) 
      
    }
    
  }
  logout(RH)
  #*********************************************************************************************
}
# **********************************************************
### DETERMINE TRADING DAY: (local time)
DAYTODAY = function()
{
  NOW <- Sys.time()
  # if the time now is past the market close but less than midnight -> trading day will be the next day
  if(NOW > as.POSIXct(paste0(Sys.Date(), " 13:00:00")) & NOW < as.POSIXct(paste0(Sys.Date(), " 23:59:59")))
  {
    daytoday <- format(Sys.Date()+1, "%Y%m%d")
  }else{
    # otherwise TODAY is the trading day 
    daytoday <- format(Sys.Date(), "%Y%m%d")
  }
}
### SLEEP UNTIL MARKET OPENS 
SLEEEP = function(xx){
  ttt <- tmz[xx] - Sys.time()
  HMS <- attr(ttt,"units")
  tt <- as.numeric(ttt)
  if(HMS == "hours")
  {
    print(paste0("Will now sleep for: ",tt , " hours"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60*60)
  }
  if(HMS == "mins")
  {
    print(paste0("Will now sleep for: ",tt , " minutes"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt*60)
  }
  if(HMS == "secs")
  {
    print(paste0("Will now sleep for: ",tt , " seconds"));cat("\n")
    print(paste0("STARTING AT: ",tmz[xx]));cat("\n")
    Sys.sleep(tt)
  }  
}
## Create sequence of times
getTMZ = function(TF)
{
  if(TF < 240)
  {
    # determines the trading day to start
    daytoday <- DAYTODAY()
    # IT WILL MAKE A DECISION AT THE CLOSE OF THE FIRST BAR! 
    START <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 09:30:10")) + hours(tmDIFF)
    END <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:00:10")) + hours(tmDIFF)
    # the following line will determine the start times for the algo
    tmz <- seq(START,END, by=paste0("",TF," min"))
    # MAKE A DECISION RIGHT BEFORE THE CLOSE 
    tmz[(length(tmz)-1)] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 15:59:30")) + hours(tmDIFF)
    # ALGO STOP TIME
    tmz[(length(tmz))] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:00:30")) + hours(tmDIFF)
  }
  if(TF==240)
  {
    # determines the trading day to start
    daytoday <- DAYTODAY()
    # IT WILL MAKE A DECISION AT THE CLOSE OF THE FIRST BAR! 
    START <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 13:00:00")) + hours(tmDIFF)
    MID   <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:00:00")) + hours(tmDIFF)
    END <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 17:00:00")) + hours(tmDIFF)
    # TMZ
    tmz = c(START,MID,END)
    # MAKE A DECISION RIGHT BEFORE THE CLOSE 
    tmz[(length(tmz)-1)] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 15:59:30")) + hours(tmDIFF)
    # ALGO STOP TIME
    tmz[(length(tmz))] <- as.POSIXct(paste0(as.Date(daytoday,format="%Y%m%d"), " 16:00:30")) + hours(tmDIFF)
  }
  tmz
}
