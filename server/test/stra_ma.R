rm(list=ls(all=T))
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, reticulate, telegram.bot)
if(!require(binancer)) {
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("daroczig/binancer")
}
setwd("C://Users//kidneyweak//Workspace//R//2020mm//contest//")
## ===============================================================
market = "BCHUSDT"
USDTvol = 10
## ===============================================================
# Setting Telegram Bot
## file.edit(path.expand(file.path("~", ".Renviron")))
## example. R_TELEGRAM_BOT_RTelegramBot=TOKEN
bot <- Bot(token = bot_token("RTelegramBot"))

sendmsg <- function(texts="*懶狗測試交易囉!*"){
  bot$sendMessage(Sys.getenv("TG_GROUP_ID"),
                  text = texts,
                  parse_mode = "Markdown"
  )}
sendpic <- function(path="./tmp.png"){
  bot$sendPhoto(Sys.getenv("TG_GROUP_ID"),
                photo = path
  )}
## ===============================================================
# Setting MAX trading Robot
## python anaconda location
use_condaenv("cloud",conda = "C:\\Users\\kidneyweak\\anaconda3\\condabin\\conda")
## MAX API(Python)
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
## get kbound from max
market <- 'btctwd'; limit <- 10000; # make it bigger that it not limit
period <- c(1,5,15,30,60,120,240,360,720,1440,4320,10080) # 時間 (分鐘)
trange <- function(x){(Sys.time() - x) %>% as.numeric() %>% round()}
r2df <- function(l){
  data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
             stringsAsFactors=FALSE)}
## ===============================================================
getk <- function(time=15){
  k <- client$get_public_k_line('btcusdt', 
                                toString(limit), 
                                toString(time)) %>%  r2df()
  colnames(k) <- c("timestamp", "open", "high", "low", "close", "volume")
  df <- k %>% mutate(timestamp=as_datetime(timestamp)) 
  return(df)
}
## ===============================================================
sendmsg()
i=0
buyprice = 0
while(1){
  k <- getk()
  k <- k %>% tq_mutate(select = c(close),
                       mutate_fun = SMA,
                       n = 5) %>%
    rename(ma5 = SMA) %>%
    # 計算20個單位簡單移動平均線參數
    tq_mutate(select = c(close),
              mutate_fun = SMA,
              n = 15) %>%
    rename(ma15 = SMA) %>% 
    tq_mutate(select = c(close),
              mutate_fun = SMA,
              n = 120) %>%
    rename(ma120 = SMA) %>%
    tq_mutate(select = c(close),
              mutate_fun = BBands)
  if( last(k$open) > last(k$close) &  last(k$dn)>=last(k$close) & i<1){
    # 進場
    sendmsg(paste("*懶狗*交易進場時間:",Sys.time()))
    sendmsg(paste("*懶狗*交易價格:",last(k$close)))
    buyprice = last(k$close)
    end <- max(k$timestamp)
    start <- end - hours(3)
    k %>% ggplot(aes(x = timestamp %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 5, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 15, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr) yMA5 bMA15"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    i=i+1
    Sys.sleep(5)
    #up<= close,MAF<=MAS,lag(MAF)>lag(MAS)
  }else if( last(k$close) >last(k$ma120) & i<1){
    # 進場
    sendmsg(paste("*懶狗*交易進場時間:",Sys.time()))
    sendmsg(paste("*懶狗*交易價格:",last(k$close)))
    buyprice = last(k$close)
    end <- max(k$timestamp)
    start <- end - hours(3)
    k %>% ggplot(aes(x = timestamp %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 5, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 15, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr) yMA5 bMA15"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    i=i+1
    Sys.sleep(5)
    #up<= close,MAF<=MAS,lag(MAF)>lag(MAS)
  }else if(last(k$up) <= last(k$close) & (last(k$ma5) <= last(k$ma15)) & nth(k$ma5,-2)>nth(k$ma15,-2) &i>0){
    #  出場
    print(last(k$close))
    sendmsg(paste("*懶狗*交易出場時間:",Sys.time()))
    sendmsg(paste("*懶狗*交易價格:",last(k$close)))
    end <- max(k$timestamp)
    start <- end - hours(3)
    k %>% ggplot(aes(x = timestamp %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 5, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 10, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    i=i-1
    break
  }else if(last(k$close) >= buyprice*1.05  &i>0){
    #  出場
    print(last(k$close))
    sendmsg(paste("*懶狗*交易出場時間:",Sys.time()))
    sendmsg(paste("*懶狗*交易價格:",last(k$close)))
    end <- max(k$timestamp)
    start <- end - hours(3)
    k %>% ggplot(aes(x = timestamp %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 5, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 10, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    i=i-1
    break
  }else if(last(k$close) <= buyprice*0.97  &i>0){
    #  出場停損
    print(last(k$close))
    sendmsg(paste("*懶狗*交易出場時間:",Sys.time()))
    sendmsg(paste("*懶狗*交易價格:",last(k$close)))
    end <- max(k$timestamp)
    start <- end - hours(3)
    k %>% ggplot(aes(x = timestamp %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 5, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 10, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    i=i-1
    break
  } else{
    Sys.sleep(5)
    #sendmsg(paste("*有內鬼終止交易*",Sys.time()))
  }
}
