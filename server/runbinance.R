rm(list=ls(all=T))
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, reticulate, telegram.bot)
if(!require(binancer)) {
  if(!require(devtools)) install.packages("devtools")
  devtools::install_github("daroczig/binancer")
  library(binancer)
}
setwd("D://workspace//R//Course//2020mm//contest//")
## ===============================================================
# Setting Telegram Bot
file.edit(path.expand(file.path("~", ".Renviron")))
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
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
## MAX API(Python)
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
## ===============================================================
r2df <- function(l){
  data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
             stringsAsFactors=FALSE)}
## ===============================================================
getk <- function(market,time=5, limit){
  k <- client$get_public_k_line(market, 
                                toString(limit), 
                                toString(time)) %>%  r2df()
  colnames(k) <- c("date", "open", "high", "low", "close", "volume")
  df <- k %>% mutate(date=as_datetime(date)) 
  return(df)
}
getbk <- function(market,time=5,limit){
  B <- binance_klines(toupper(market), paste0(time,'m'),limit = limit)
  B <- B[,c(1:6)]
  colnames(B) <- c("date","bopen","bhigh","blow","bclose","bvolume")
  return(B)
}
## ===============================================================
# 委託單送出可以卡著直到完成的function
chkstate <- function(id){
  # client$get_private_order_detail(id)[['state']] == 'wait' # 等待
  while(client$get_private_order_detail(id)[['state']] == 'wait'){
    Sys.sleep(0.5)
  }
  client$get_private_order_detail(id)[['state']]
  print("完成委託單")
}
# =============買入設定=============
## ===============================================================
# sendmsg()
# =============買入設定=============
market = "bchusdt"; spendusdt = 10 # 花費USDT量
# amount = spendusdt / ticker # 算出amount
# ===============================
while(1){
  k <- getk(market,1,500)
  bk <- getbk(market,1,500)
  k <- left_join(k, bk, by="date")
  # 計算MA
  k <- k %>% tq_mutate(select = c(close),
                       mutate_fun = SMA,
                       n = 10) %>%
    rename(ma10 = SMA) 
  
    if((last(k$close)*1.0015 < last(k$bclose)*0.999) ){
      # 進場
      # client$set_private_create_order('btcusdt', 'buy', 'amount', 'price', stop='', _type='market') # 交易 API
      
      sendmsg(paste("*懶狗*幣安比他高:", Sys.time()))
      amount = spendusdt / last(k$close)
      trade <- client$set_private_create_order(toString(market),'buy', toString(amount), toString(last(k$close)))
      chkstate()
      sendmsg("*懶狗*開始賣剛剛那個")
      amt <- client$get_private_account_balance('bch')$balance
      trade <- client$set_private_create_order(toString(market),'sell', toString(amt), toString(last(k$bclose)))
      chkstate()
      sendmsg("*懶狗*完成一次垃圾套利")
      break
    } else{
    Sys.sleep(5)
    #sendmsg(paste("*有內鬼終止交易*",Sys.time()))
  }
}
