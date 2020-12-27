setwd(~\)
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, reticulate, telegram.bot)
max <- import("max.client")
client <- max$Client("","")
wclient <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
sclient <- max$Client(Sys.getenv("shu_MAX_API_KEY"),Sys.getenv("shu_MAX_API_SECRET"))
lclient <- max$Client(Sys.getenv("lin_MAX_API_KEY"),Sys.getenv("lin_MAX_API_SECRET"))
#
updater <- Updater(token = bot_token("RTelegramBot"))
r2df <- function(l) {
  if(length(l)!=0){
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                     stringsAsFactors=FALSE);
    name <- names(l[[1]]);colnames(df) <- name[1:ncol(df)];
    return(df)} }

getk <- function(bot, update, args){
  if(length(args)>0){
    period <- c(1,5,15,30,60,120,240,360,720,1440)
    market <- args[1]; 
    time <- ifelse(!is.na(args[2]) & args[2] %in% period,args[2],5)
    hr <- ifelse(!is.na(args[3]),strtoi(args[3]),6);print(hr)
    k <- client$get_public_k_line(market, 
                                  toString(1000), 
                                  toString(time)) %>%  r2df()
    colnames(k) <- c("date", "open", "high", "low", "close", "volume")
    k <- k %>% mutate(date=as_datetime(date))
    end <- max(k$date); start <- end - hours(hr)
    k %>% ggplot(aes(x = date %>% as_datetime, y = close, 
                     open = open, high = high,
                     low = low, close = close)) +
      geom_barchart(colour_up = "firebrick3", colour_down = "chartreuse3",
                    fill_up  = "firebrick3", fill_down  = "chartreuse3", size = 2) +
      geom_ma(ma_fun = SMA, n = 10, color = "yellow", linetype = 7,size = 1) +
      geom_ma(ma_fun = SMA, n = 30, color = "blue", linetype = 7, size = 1) +
      geom_bbands(ma_fun = SMA, sd = 2, n = 20, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(",hr,"hr)"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    bot$sendPhoto(update$message$chat_id,
                  photo = "./tmp.png"
    )
  }
}

# sell
sell <- function(bot, update, args){
  if (length(args) > 2L){
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton("YES",
                               callback_data = paste("sell",args[1],args[2],args[3],sep=" ")),
          InlineKeyboardButton("NO")
        )
      )
    )
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("sell",args[1],args[2],args[3]),
                    reply_markup = IKM) 
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="請輸入幣種 價格 量")
  }
}

buy <- function(bot, update, args){
  if (length(args) > 2L){
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton("YES",
                               callback_data = paste("buy",args[1],args[2],args[3],sep=" ")),
          InlineKeyboardButton("NO")
        )
      )
    )
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("buy",args[1],args[2],args[3]),
                    reply_markup = IKM) 
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="請輸入幣種 價格 量")
  }
}
answer_cb <- function(bot, update) {
  data <- update$callback_query$data
  print(data)
  m <- client$get_public_all_markets() %>% map(1) %>% unlist()
  if(word(data,2) %in% m){
    price <- client$get_public_all_tickers()[[tolower(word(data, 2))]] %>% as.numeric()
    tpr <- word(data,3) %>% as.numeric()
    if(tpr > price[5]*0.95 & tpr < price[6]*1.05 ){
      cid <- update$callback_query$message$chat$id
      bot$sendMessage(chat_id = cid, 
                      text = paste("幣種:", word(data,2),"價格:",word(data,3),"量:",word(data,4)))
      if(cid==Sys.getenv("TG_ME")){
        client <- wclient
        bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                        text = paste0(client$get_private_member_me()$email))
        # tryCatch(client$set_private_create_order(toString(word(data,2)), # 市場
        #                                          toString(word(data,1)), # side
        #                                          toString(word(data,4)), # 量
        #                                          toString(word(data,3))),# 價格
        #          error=function(e){
        #            bot$sendMessage(chat_id = update$callback_query$message$chat$id,text=e) } )
      }else if(cid==Sys.getenv("TG_SHU")){
        client <- sclient
        bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                        text = paste0(client$get_private_member_me()$email))
        # tryCatch(client$set_private_create_order(toString(word(data,2)), # 市場
        #                                          toString(word(data,1)), # side
        #                                          toString(word(data,4)), # 量
        #                                          toString(word(data,3))),# 價格
        #          error=function(e){
        #            bot$sendMessage(chat_id = update$callback_query$message$chat$id,text=e) } )
      }else if(cid==Sys.getenv("TG_LIN")){
        client <- lclient
        bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                        text = paste0(client$get_private_member_me()$email))
        # tryCatch(client$set_private_create_order(toString(word(data,2)), # 市場
        #                                          toString(word(data,1)), # side
        #                                          toString(word(data,4)), # 量
        #                                          toString(word(data,3))),# 價格
        #          error=function(e){
        #            bot$sendMessage(chat_id = update$callback_query$message$chat$id,text=e) }) 
      }
      
      
    }else{
      bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                      text = "價格錯囉")
    }
    
  }else{
    
  }
  
  
}
filter_user <- as.BaseFilter(function(message) 
  message$from_user  %in% c(Sys.getenv("TG_ME"),Sys.getenv("TG_SHU"),Sys.getenv("TG_LIN")))
updater <- updater + CallbackQueryHandler(answer_cb)
updater <- updater + 
  CommandHandler("buy", buy, pass_args = TRUE,filter_user) +
  CommandHandler("sell", sell, pass_args = TRUE,filter_user) +
  CommandHandler("getk", getk, pass_args = TRUE,filter_user)
# Create error callback
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}
# Register it to the updater's dispatcher
updater$dispatcher$add_error_handler(error_callback)
updater$start_polling()