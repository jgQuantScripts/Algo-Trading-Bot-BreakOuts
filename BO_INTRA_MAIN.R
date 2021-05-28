source("BO_INTRA_FUN.R")
### get Time Sequences for Algo
tmz = getTMZ(TF=5)
# ***********************************************************************************************
# THE FOLLOWING LINE ENSURES THAT THE ALGO START TIMES ARE GREATER THAN THE CURRENT TIME
# **** IF THE "SCAN" BLOCK STOP/FAILS - RE-RUN LINES: 12-43 *****
tmz <- tmz[tmz>Sys.time()]
# ***********************************************************************************************
#                               ALGO START
# ***********************************************************************************************
SLEEEP(1)
SCAN <- pblapply(as.list(2:length(tmz)), function(xx){
  source("BO_INTRA_FUN.R")
  # get data and breakouts
  data = pblapply(as.list(BEST$Symbol), function(x){
    # subset BEST data per symbol
    sub   = subset(BEST, BEST$Symbol == x)
    # formatting columns
    sub$lookBackDays = as.numeric(sub$lookBackDays)
    sub$volDays      = as.numeric(sub$volDays)
    sub$holdBars     = as.numeric(sub$holdBars)
    # get breakouts
    df    = try(histData(symbol=x, MINUTES = 5))
    if(nrow(df) > 0){
    bOuts = breakOut(df=df,lookBackDays=sub$lookBackDays,volDays=sub$volDays,closeAtHigh = FALSE)
    opcl  = addCloseBar(bOuts=bOuts,symbol = x,barInterval = 5, holdBars = sub$holdBars)
    colnames(opcl)[ncol(opcl)] <- "closeAt"
    }else{
      opcl <- NULL
    }
    opcl
  })
  # remove empty lists
  data <- data[lapply(data,length)>0]
  # row bind data
  data = do.call(rbind,data)
  # round bar to the nearest minute - RH bars are indexed at the START
  thisBAR = as.character(as.POSIXct(round_date(tmz[xx]-minutes(10), paste0(1," minutes"))))
  # find any matching bars
  OPEN = subset(data, data$openAt == thisBAR)
  # send market orders in Fractional Shares
  if(nrow(OPEN) > 0){sendOrdersMKT(toOpen = OPEN, maxAlloc = 50)}
  # check if any Closing Cases
  CLOSE = subset(data, data$closeAt == thisBAR)
  # close positions
  if(nrow(CLOSE) > 0){closePOS(bar2Cl = CLOSE)}
  # Print OPENED POSITIONS
  if(nrow(OPEN) > 0){print(OPEN);cat("\n")}
  
  SLEEEP(xx)
})

