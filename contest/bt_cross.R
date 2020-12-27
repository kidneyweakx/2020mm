setwd("D://workspace//R//Course//2020mm//contest//")
rm(list=ls(all=T));gc()
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,tidyquant, reticulate)
# python anaconda 位置 (修改cloud成你的環境，conda為ana/miniconda路徑)
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda") 
# 加入 API (reticulate Python module)
max <- import("max.client") # Python方法
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
# 要到的data轉換成dataframe
r2df <- function(l){
  data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
             stringsAsFactors=FALSE)}
trange <- function(x){(Sys.time() - x) %>% as.numeric() %>% round()}
market <- client$get_public_all_markets() %>% r2df %>% 
  filter(X6 == "usdt")
limit <- 10000; period <- c(1,5,15,30,60,120,240,360,720,1440) # 時間 (分鐘)
market <- market$X1

for(mk in c(1:8)){
  k <-  client$get_public_k_line(market[mk], 
                                 toString(limit), 
                                 toString(period[2]),
                                 toString(trange(days(754)))) %>%  r2df()
  colnames(k) <- c("date", "open", "high", "low", "close", "volume")
  stockData <- k %>% mutate(date=as_datetime(date)) 
  stockData <- stockData %>%
    tq_mutate(select = close, mutate_fun = BBands, n=30, sd=2) %>% 
    tq_mutate(select = close, mutate_fun = MACD) %>% 
    tq_mutate(select = close, mutate_fun = momentum, n =10) %>% 
    tq_mutate(select = close, mutate_fun = SMA, n =10) %>% 
    tq_mutate(select = c(close,high,low), mutate_fun = ATR)
# ================================================
  # 雙MA策略 (MA-Cross)
  inSiteTable <- stockData %>%
    filter(lag(dn) > lag(close) & dn  <=  close |  # 下往上穿越下線：可能短期會反轉
             lag(mavg) > lag(close) & mavg <= close  #下往上穿越中線：可能會加速向上
    ) %>%
    select(inDate=date, buyPrice=close)
#=================================================
  outSiteTable <- stockData %>%
    mutate(MAF= SMA(close, 5),
           MAS= SMA(close, 15)) %>%
    filter(MAF<=MAS,lag(MAF)>lag(MAS)) %>%
    select(outDate=date, sellPrice=close)
  # outSiteTable <- stockData %>%
  #   filter(lag(up) < lag(close) & up >= close |# 上往下跌破上線：暗示上漲趨勢結束
  #            lag(mavg) < lag(close) & mavg >= close# 由上往下跌破中線：可能會下跌
  #            ) %>%
  #   select(outDate=date, sellPrice=close)
  # ==============================================
  tradeDetailTable <- NULL   
  
  for(ix in 1:nrow(inSiteTable)){
    # 目前的進場日期
    inDate <- inSiteTable$inDate[ix] 
    # 找尋進場日期往後最近的出場位置
    outSite <- which(outSiteTable$outDate>inDate)[1]  
    # 防呆機制，如果進場日期在資料尾端，有可能發生資料不足找不到出場位置的狀況
    if(length(outSite)>0){                            
      # 將該筆進場資訊與對應的出場資訊合併，並儲存至交易明細表內
      tradeDetailTable <- bind_rows(tradeDetailTable, 
                                    bind_cols(inSiteTable[ix,],
                                              outSiteTable[outSite,]))
      outSiteTable <- outSiteTable[-outSite,]
    }
    
  }
  # 手續費
  buyCostR <- 0.0015   # 買入交易成本 
  sellCostR <- 0.0015  # 賣出交易成本
  tradeDetailTable <- tradeDetailTable %>%
    mutate(# 計算報酬率
      ret=sellPrice*(1-sellCostR)/(buyPrice*(1+buyCostR))-1,
      # 計算持有日數
      holdtimes=difftime(outDate,inDate,units = "hours")) %>% na.omit() 
  meanRet <- mean(tradeDetailTable$ret) # 平均報酬率
  sdRet <- sd(tradeDetailTable$ret) # 報酬率標準差
  tradeNums <- nrow(tradeDetailTable) # 交易次數
  winRatio <- sum(as.numeric(tradeDetailTable$ret>0))/tradeNums # 勝率
  maxRet <- max(tradeDetailTable$ret) # 最大報酬率
  minRet <- min(tradeDetailTable$ret) # 最小報酬率
  avgHoldtimes <- mean(tradeDetailTable$holdtimes) # 平均持有時間
  
  # 列印出回測績效
  cat(paste0(min(stockData$date),"到", max(stockData$date),"\n",
             "*********",market[mk],"策略回測績效*********\n",
             "平均報酬率: ",round(meanRet*100,2)," %\n",
             "交易次數: ",tradeNums," 次\n",
             "勝率: ",round(winRatio*100,2)," %\n",
             "報酬率標準差: ",round(sdRet*100,2)," %\n",
             "最大報酬率: ",round(maxRet*100,2)," %\n",
             "最小報酬率: ",round(minRet*100,2)," %\n",
             "平均持有間距: ",round(avgHoldtimes,2),"小時 \n"))
}