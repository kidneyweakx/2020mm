rm(list=ls(all=T))
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, reticulate, telegram.bot)
setwd("C://Users//kidneyweak//Workspace//R//2020mm//contest//")
## ===============================================================
# Setting Telegram Bot
## file.edit(path.expand(file.path("~", ".Renviron")))
## example. R_TELEGRAM_BOT_RTelegramBot=TOKEN
bot <- Bot(token = bot_token("RTelegramBot"))

sendmsg <- function(texts="*懶狗該交易囉!*"){
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
clientx <- max$Client("","")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
## get kbound from max
market <- 'btctwd'; limit <- 1000; # make it bigger that it not limit
period <- c(1,5,15,30,60,120,240,360,720,1440,4320,10080) # 時間 (分鐘)
r2df <- function(l){
  data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
             stringsAsFactors=FALSE)}
## ===============================================================
getk <- function(market,time=5, limit){
  k <- clientx$get_public_k_line(market, 
                                toString(limit), 
                                toString(time)) %>%  r2df()
  colnames(k) <- c("date", "open", "high", "low", "close", "volume")
  df <- k %>% mutate(date=as_datetime(date)) 
  return(df)
}
## =============買入設定=============
market = "ethusdt"; spendusdt = 10 # 花費USDT量
ms="usdt";mb="eth";hold = 0
# amount = spendusdt / ticker # 算出amount
## ==================================
sendmsg()
ticker = client$get_public_all_tickers()[[tolower(market)]]$buy %>% as.numeric()
ticker = ticker*2
while(1){
  tryCatch(k <- getk(market,5,1000),error = function(e) {
    k <- k[,1:6]; sendmsg("getK error!!!!")})
  k <- k %>% tq_mutate(select = c(close),
                       mutate_fun = SMA,
                       n = 10) %>%
    rename(MAF = SMA) %>%
    tq_mutate(select = c(close),
              mutate_fun = SMA,
              n = 30) %>%
    rename(MAS = SMA) %>% 
    tq_mutate(select = c(close),
              mutate_fun = SMA,
              n = 120) %>%
    rename(MA120 = SMA) 
  
  if(last(k$MA120) >= last(k$close) & hold==0){
    print("進場")
    # 進場
    # ==========交易============
    ticker = client$get_public_all_tickers()[[tolower(market)]]$buy %>% as.numeric()
    amount = spendusdt / ticker
    trade <- client$set_private_create_order(toString(market),'buy', toString(amount), toString(ticker))
    hold = 1
    sendmsg(paste("*懶狗交易錄*\n 交易時間:",Sys.time(),
                  "\n 價格:",last(k$close),"\n 買入價格:",ticker,"\n 交易量:",amount))
    # =======繪圖==========
    end <- max(k$date)
    start <- end - hours(3)
    k %>% ggplot(aes(x = date %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 10, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 30, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    Sys.sleep(3)
    #===========================
  } else if((last(k$MAF) <= last(k$MAS)) & nth(k$MAF, -2) > nth(k$MAS, -2)  & hold == 1 ){
    print("出場")
    #  出場
    sticker = client$get_public_all_tickers()[[tolower(market)]]$sell %>% as.numeric()
    samount =  client$get_private_account_balance(mb)$balance
    strade <- client$set_private_create_order(toString(market),'sell', toString(samount), toString(sticker))
    hold = 0
    sendmsg(paste("*懶狗交易錄[出場]*\n 交易時間:",Sys.time(),
                  "\n 價格:",last(k$close),"\n 出場價格:",sticker,"\n 交易量:",samount))
    # =================================
    end <- max(k$date)
    start <- end - hours(3)
    k %>% ggplot(aes(x = date %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 10, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 30, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 10, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    break;
    # Sys.sleep(3)
  } else if((last(k$close)<=ticker*0.97) & hold==1){ # 停損
    #  停損出場
    sticker = client$get_public_all_tickers()[[tolower(market)]]$sell %>% as.numeric()
    samount =  client$get_private_account_balance(mb)$balance
    strade <- client$set_private_create_order(toString(market),'sell', toString(samount), toString(sticker))
    hold = 0
    sendmsg(paste("*懶狗交易錄[停損]*\n 交易時間:",Sys.time(),
                  "\n 價格:",last(k$close),"\n 出場價格:",sticker,"\n 交易量:",samount))
    # =================================
    end <- max(k$date)
    start <- end - hours(3)
    k %>% ggplot(aes(x = date %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 3) +
      geom_ma(ma_fun = SMA, n = 10, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 30, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 10, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(3hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    sendpic()
    break;
    # Sys.sleep(3)
  } else {
    Sys.sleep(5)
    #sendmsg(paste("*有內鬼終止交易*",Sys.time()))
  }
}
client$set_private_cancel_order(trade$id)
