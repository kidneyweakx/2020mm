# ----edit by UTF-8----
setwd("D://workspace//R//Course//2020mm//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate) # 載入PKG
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
bot <- Bot(token = bot_token("RTelegramBot"))
sendmsg <- function(texts="*懶狗該交易囉!*"){
  bot$sendMessage(Sys.getenv("TG_ME"), text = texts); print(texts) }
noti <- 0
while(1) {
  chis <- client$get_private_order_history("maxusdt")
  if(length(chis)>0){
    noti <- 0;Sys.sleep(30)
  } else if(noti==0&length(chis)==0){
    ubal <- client$get_private_account_balance("usdt")$balance %>% as.numeric()
    mbal <- client$get_private_account_balance("max")$balance %>% as.numeric()
    if(ubal >0.99){
      subal <- floor(ubal);print(ubal);noti <- 1
      sendmsg(paste("該花掉usdt",ubal,"了"))
      Sys.sleep(120)
    }else if(mbal > 0.99){
      smbal <- floor(mbal);print(mbal);noti <- 1
      sendmsg(paste("該花掉max",mbal,"了"))
      Sys.sleep(120)
    }
  } else{
    Sys.sleep(30)
  }
}