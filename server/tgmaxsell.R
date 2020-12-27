# ----edit by UTF-8----
setwd("~//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate,telegram.bot) # 載入PKG
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
bot <- Bot(token = bot_token("RTelegramBot"))
updater <- Updater(token = bot_token("RTelegramBot"))


sendmsg <- function(texts="*懶狗該交易囉!*"){
  bot$sendMessage(Sys.getenv("TG_ME"), text = texts); print(texts) }

max <- function(bot, update, args){
  print("開始探測")
  
  if (length(args) > 1){
    tryCatch(odr <- client$get_private_order_detail(args[1]), error=function(e) {
      print(e);odr <- NULL})
    noti <- 0
    if(!is.null(odr)){
      while(1){
        odr <- client$get_private_order_detail(args[1])
        if(odr$market != "maxusdt"){
          bot$sendMessage(chat_id = update$message$chat_id, text="非MAX");break;}
        
        if(odr$state == "wait"){
          bot$sendMessage(chat_id = update$message$chat_id, text=paste(odr$market, odr$side,"尚未成交"))
          Sys.sleep(3)
        }else if(odr$state == "done"&noti==0){
          if(args[2] %>% as.numeric() >= odr$price){
            pr <- args[2]; noti <- 1
            mbal <- client$get_private_account_balance("max")$balance %>% as.numeric()
            bot$sendMessage(chat_id = update$message$chat_id, text=paste("[掛單]賣價",pr,"買價",mbal))
            # tryCatch(client$set_private_create_order(toString("maxusdt"), # 市場
            #                                          toString("sell"), # side
            #                                          toString(mbal), # 量
            #                                          toString(pr)),# 價格
            #          error=function(e){
            #            print(e); bot$sendMessage(chat_id = cid,text="交易失敗")} )
            break;
          }else{
              bot$sendMessage(chat_id = update$message$chat_id, text=paste("[掛單]狀態", odr$state))
              break;
            }
          
        }
      }
    }else{
      bot$sendMessage(chat_id = update$message$chat_id, text="輸入正確單號 價格 /help")
    }
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入單號 /help")
  }
}

help <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, text="/max 單號 賣價 來檢測 \n去http://140.117.71.98:8787/暫停")
}
# Create error callback
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}
filter_user <- as.BaseFilter(function(message) 
  message$from_user  %in% Sys.getenv("TG_ME"))
updater <- updater + 
  CommandHandler("max", max, pass_args = T, filter_user)+ 
  CommandHandler("help", help, pass_args = F, filter_user)
updater$dispatcher$add_error_handler(error_callback)
updater$start_polling()
sendmsg("壞了 去http://140.117.71.98:8787/")
# 復原用
# updater <- Updater(token = bot_token("RTelegramBot"))
# updater$dispatcher$add_error_handler(error_callback)
# updater$start_polling()
