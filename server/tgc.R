setwd("~/")
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidyquant, reticulate, telegram.bot)
max <- import("max.client")
client <- max$Client("","")
wclient <- max$Client(Sys.getenv("weak_MAX_API_KEY"),Sys.getenv("weak_MAX_API_SECRET"))
sclient <- max$Client(Sys.getenv("shu_MAX_API_KEY"),Sys.getenv("shu_MAX_API_SECRET"))
lclient <- max$Client(Sys.getenv("lin_MAX_API_KEY"),Sys.getenv("lin_MAX_API_SECRET"))
# file.edit(path.expand(file.path("~", ".Renviron")))
# example. R_TELEGRAM_BOT_RTelegramBot=TOKEN
updater <- Updater(token = bot_token("RTelegramBot"))
# useful function
r2df <- function(l) {
  if(length(l)!=0){
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                     stringsAsFactors=FALSE);
    name <- names(l[[1]]);colnames(df) <- name[1:ncol(df)];
    return(df)} }

chkclient <- function(cid) {
  if(cid==Sys.getenv("TG_WEAK")){ return (wclient)
  }else if(cid==Sys.getenv("TG_SHU")){ return (sclient)
  }else if(cid==Sys.getenv("TG_LIN")){ return (lclient)
  }else {return (client)}
}
# action
getk <- function(bot, update, args){
  m <- client$get_public_all_markets() %>% map(1) %>% unlist()
  if(length(args)>0){
    print("gk")
    period <- c(1,5,15,30,60,120,240,360,720,1440)
    market <- ifelse(args[1]%in%m,args[1],"btcusdt"); 
    time <- ifelse(!is.na(args[2]) & args[2] %in% period,args[2],5)
    hr <- ifelse(!is.na(args[3]),strtoi(args[3]),6);
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
      geom_bbands(ma_fun = SMA, sd = 2, n = 30, linetype = 5) + # 布林通道
      labs(title = paste(market, "Line Chart(",hr,"hr) MA黃=10, MA藍=30 BB[sd2/n30]"),
           y = "Closing Price", x = "") +
      coord_x_datetime(xlim=c(start, end)) +
      theme_tq()
    ggsave("tmp.png")
    bot$sendPhoto(update$message$chat_id,
                  photo = "./tmp.png"
    )
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入市場 /help")
  }
}
# vol
cvol <- function(bot, update, args){
  m <- client$get_public_all_markets() %>% map(1) %>% unlist()
  if (length(args) > 0){
    print("可兌量")
    market <- ifelse(args[1]%in%m,args[1],"btcusdt");
    amt <- ifelse(!is.na(args[2]),as.numeric(args[2]),10)
    price <- client$get_public_all_tickers()[[tolower(market)]] %>% as.numeric()
    vol=amt/price[7]
    bot$sendMessage(chat_id = update$message$chat_id, 
                    text=paste("在",market,"\n",amt,"USDT 可換\n",vol,"\n 價格:",price[7]))
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入市場 USDT量 /help")
  }
}
# pr
cpr <- function(bot, update, args){
  m <- client$get_public_all_markets() %>% map(1) %>% unlist()
  if (length(args) > 0){
    print("現價")
    market <- ifelse(args[1] %in% m,args[1],"btcusdt");
    pb <- client$get_public_all_tickers()[[market]]$buy %>% as.numeric()
    ps <- client$get_public_all_tickers()[[market]]$sell %>% as.numeric()
    po <- client$get_public_all_tickers()[[market]]$open %>% as.numeric()
    ph <- client$get_public_all_tickers()[[market]]$high %>% as.numeric()
    pl <- client$get_public_all_tickers()[[market]]$low %>% as.numeric()
    pr <- client$get_public_all_tickers()[[market]]$last %>% as.numeric()
    bot$sendMessage(chat_id = update$message$chat_id, 
                    text=paste("在",market,"=> 現價:",pr,
                               "\n open:",po,"buy:",pb,"sell:",ps,
                               "\n high:",ph,"low:",pl))
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入市場 /help")
  }
}
# order history
ohis <- function(bot, update) {
  print("訂單紀錄")
  m <- client$get_public_all_markets() %>% r2df
  client <- chkclient(update$message$chat_id)
  orhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
    client$get_private_order_history(m$id[i]) %>% r2df }))
  if(!is.null(orhis)){
    orhis <- orhis[c(1:4,7,12:13)]
    colnames(orhis) <- c("id","side","odr_type","price","market","vol","rvol")
    orstr <- do.call(paste,lapply(1:nrow(orhis),function(t){
      usdpr <- as.numeric(orhis$rvol[t])*as.numeric(orhis$price[t])
      paste("[",orhis$side[t],"]",orhis$market[t],"=> id:",orhis$id[t],
            "\n價:",orhis$price[t],"量:",orhis$rvol[t],"USDT:",usdpr,
            "\n===================\n")
    }))
    orstr <- paste("[現有訂單]\n", orstr)
    }else{
      orstr <- paste("現在沒有任何訂單，趕快去下單吧")
    }
    
  bot$sendMessage(chat_id = update$message$chat_id, text=orstr)
  
}
# trade history
this <- function(bot, update, args) {
  print("交易紀錄")
  m <- client$get_public_all_markets() %>% r2df
  client <- chkclient(update$message$chat_id)
  trhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
    client$get_private_trade_history(m$id[i]) %>% r2df }))
  trhis <- trhis[order(desc(trhis$id)),]
  if (length(args) > 0){
    lx <- ifelse(args[1] != "all", ifelse(nrow(trhis)>=5, 5, nrow(trhis)), nrow(trhis))
  }else {
    lx <- ifelse(nrow(trhis)>=5, 5, nrow(trhis))
  }
  trstr <- do.call(paste,lapply(1:lx,function(t){
    paste("[",trhis$side[t],"]",trhis$market[t],"=> id:",trhis$id[t],
          "\n價:",trhis$price[t],"量:",trhis$volume[t],"總價:",trhis$funds[t],
          "\n===================\n")
  }))
  trstr <- paste("[交易紀錄]\n", trstr)
  bot$sendMessage(chat_id = update$message$chat_id, text=trstr)
}
# cancel by id
canid  <- function(bot, update, args){
  m <- client$get_public_all_markets() %>% r2df()
  if (length(args) > 0){
      client <- chkclient(update$message$chat_id)
      orhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
        client$get_private_order_history(m$id[i]) %>% r2df }))
      
      if(args[1] %in% orhis$id){
        client$set_private_cancel_order(args[1])
        bot$sendMessage(chat_id = update$message$chat_id, text=paste("取消",args[1],"成功"))
        print(paste("取消",args[1]))
      }else{
        bot$sendMessage(chat_id = update$message$chat_id, text="沒有這筆訂單")
      }
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入訂單ID /help")
  }
}
# cancel 1
can1 <- function(bot, update, args){
  print("取消")
  m <- client$get_public_all_markets() %>% map(1) %>% unlist()
  if (length(args) > 0){
    if(args[1] %in% m){
      client <- chkclient(update$message$chat_id)
      orhis <- client$get_private_order_history(args[1]) %>% r2df()
      if(length(orhis)>0){
        client$set_private_cancel_order(orhis$id[nrow(orhis)])
        canid <- orhis$id[nrow(orhis)]
        bot$sendMessage(chat_id = update$message$chat_id, text=paste("取消",canid,"成功"))
        print(paste("取消",canid))
      }else{
        bot$sendMessage(chat_id = update$message$chat_id, text="沒有訂單")
      }
    }else{
      bot$sendMessage(chat_id = update$message$chat_id, text="輸錯市場了")
    }
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="輸入市場 /help")
  }
}
# price
pricex <- function(bot, update){
  print("pricex")
  m <- client$get_public_all_markets() %>% r2df
  m <- m %>% filter(quote_unit=="usdt");st <- ""
  for(i in c(1:nrow(m))) {
    pb <- client$get_public_all_tickers()[[m$id[i]]]$buy %>% as.numeric()
    ps <- client$get_public_all_tickers()[[m$id[i]]]$sell %>% as.numeric()
    po <- client$get_public_all_tickers()[[m$id[i]]]$open %>% as.numeric()
    ph <- client$get_public_all_tickers()[[m$id[i]]]$high %>% as.numeric()
    pl <- client$get_public_all_tickers()[[m$id[i]]]$low %>% as.numeric()
    pr <- client$get_public_all_tickers()[[m$id[i]]]$last %>% as.numeric()
    st <- paste(st,m$base_unit[i],"=> 現價:",pr,
                "\n open:",po,"buy:",pb,"sell:",ps,
                "\n high:",ph,"low:",pl,
                "\n===================\n")
  }
  bot$sendMessage(chat_id = update$message$chat_id, text=st)
}
# profit
profit <- function(bot, update){
  print("profit")
  m <- client$get_public_all_markets() %>% r2df
  m <- m %>% filter(quote_unit=="usdt");
  client <- chkclient(update$message$chat_id)
  st <- "";get <- 0;
  for(i in c(1:nrow(m))) {
    amt <- client$get_private_account_balance(m$base_unit[i])$balance %>% as.numeric()
    lamt <- client$get_private_account_balance(m$base_unit[i])$locked %>% as.numeric()
    nowp <- client$get_public_all_tickers()[[m$id[i]]]$buy %>% as.numeric()
    acamt <- (amt+lamt)*nowp
    get <- get+acamt
    st <- paste(st,m$base_unit[i],"=> 餘額:",amt+lamt,
          "\n 可用:",amt,"鎖倉:",lamt,"\n現價:",acamt,
          "\n===================\n")
  }
  amt <- client$get_private_account_balance("usdt")$balance %>% as.numeric()
  lamt <- client$get_private_account_balance("usdt")$locked %>% as.numeric()
  assets <- get+amt+lamt
  st <- paste0(st,"USDT => 餘額: ",amt+lamt,
              "\n可用: ",amt,"鎖倉: ",lamt,"\n現價: ",acamt,
              "\n===================\n")
  st <- paste0(st,"總資產: ",assets," USDT\n獲利: ",(assets/60 -1)*100,"%")
  bot$sendMessage(chat_id = update$message$chat_id, text=st)
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
    usdpr <- as.numeric(args[2]) * as.numeric(args[3])
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("sell \n市場:",args[1],"\n價格:",args[2],
                                 "\n量:",args[3],"\n獲得USDT:",usdpr),
                    reply_markup = IKM) 
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="請輸入幣種 價格 量 /help")
  }
}
# ubuy
ubuy <- function(bot, update, args){
  if (length(args) > 2L){
    amt <- as.numeric(args[3])/as.numeric(args[2])
    IKM <- InlineKeyboardMarkup(
      inline_keyboard = list(
        list(
          InlineKeyboardButton("YES",
                               callback_data = paste("buy",args[1],args[2],amt,sep=" ")),
          InlineKeyboardButton("NO")
        )
      )
    )
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("buy\n市場",args[1],"\n價格:",args[2],"\n量:",amt),
                    reply_markup = IKM) 
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="請輸入幣種 價格 USDT量 /help")
  }
}
# buy
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
                    text = paste("buy \n市場",args[1],"\n價格:",args[2],"\n量:",args[3]),
                    reply_markup = IKM) 
  }else{
    bot$sendMessage(chat_id = update$message$chat_id, text="請輸入幣種 價格 量 /help")
  }
}
# call_back
answer_cb <- function(bot, update) {
  data <- update$callback_query$data; print(data)
  cid <- update$callback_query$message$chat$id
  bot$deleteMessage(cid, update$callback_query$message$message_id)
  m <- client$get_public_all_markets() %>% r2df
  m <- m %>% filter(quote_unit=="usdt");
  if(word(data,2) %in% m$id){
    price <- client$get_public_all_tickers()[[tolower(word(data, 2))]] %>% as.numeric()
    tpr <- word(data,3) %>% as.numeric()
    avol <- word(data,4) %>% as.numeric()
    if(tpr > price[5]*0.95 & tpr < price[6]*1.05 ){
      client <- chkclient(cid)
      if(word(data,1)=="buy"){
        bal <- client$get_private_account_balance("usdt")$balance %>% as.numeric();print(bal)
      }else if(word(data,1)=="sell"){
        bal <- client$get_private_account_balance(unlist(strsplit(word(data,2),"usdt")))$balance %>% as.numeric()
      }
     
      if(tpr*avol <= bal & tpr*avol >= 9 &word(data,1)=="buy"| 
         avol <= bal & tpr*avol >=9 & word(data,1)=="sell"){
      bot$sendMessage(chat_id = cid, 
                      text = paste("幣種:", word(data,2),"價格:",word(data,3),"量:",word(data,4),
                                   "\nUSDT量:",tpr*avol))
      bot$sendMessage(chat_id = cid,text=client$get_private_member_me()$email)
      bot$answerCallbackQuery(callback_query_id = update$callback_query$id,
                              text = paste("交易成功"))
      tryCatch(client$set_private_create_order(toString(word(data,2)), # 市場
                                               toString(word(data,1)), # side
                                               toString(word(data,4)), # 量
                                               toString(word(data,3))),# 價格
               error=function(e){
                 print(e); bot$sendMessage(chat_id = cid,text="交易失敗")} )
      }else{
        bot$sendMessage(chat_id = cid,text = "餘額不足或不足額交易")
      }
    }else{
      bot$sendMessage(chat_id = cid, text = "價格錯囉")
    }
  }else{
    bot$sendMessage(chat_id = cid, text = "沒有這種市場或Level-1拉基無法交易台幣")
  }
}
# help
help <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id, 
                  text=paste("括弧內資訊為可不打",
                    "危險指令(private)",
                    "/buy  買 內容: 市場 價格 量",
                    "/ubuy 買 內容: 市場 價格 USDT量" ,
                    "/sell 賣 內容: 市場 價格 量",
                    "/can1 取消最新一筆 內容: 市場",
                    "/canid 取消訂單ID 內容ID",
                    "正常指令(public)",
                    "/getk k線 內容: 市場 (k時間:5) (小時:6)",
                    "/cvol USDT可換量 內容: 市場 (USDT:10)",
                    "/cpr  現在價格 內容: 市場",
                    "/profit 獲利和餘額",
                    "/pricex 所有價格(跑很久不要常按)",
                    "/this 交易歷史(未加all為最新5筆)",
                    "/ohis 訂單歷史",
                    "/help 求助",sep="\n"))
}
# tg setting
filter_user <- as.BaseFilter(function(message) 
  message$from_user  %in% c(Sys.getenv("TG_WEAK"),Sys.getenv("TG_SHU"),Sys.getenv("TG_LIN")))
updater <- updater + CallbackQueryHandler(answer_cb)
updater <- updater + 
  CommandHandler("buy", buy, pass_args = TRUE, filter_user) +
  CommandHandler("ubuy", ubuy, pass_args = TRUE, filter_user) +
  CommandHandler("sell", sell, pass_args = TRUE, filter_user) +
  CommandHandler("getk", getk, pass_args = TRUE, filter_user) +
  CommandHandler("cvol", cvol, pass_args = TRUE, filter_user) +
  CommandHandler("cpr" , cpr,  pass_args = TRUE, filter_user) +
  CommandHandler("can1", can1, pass_args = TRUE, filter_user) +
  CommandHandler("canid", canid, pass_args = TRUE, filter_user)+
  CommandHandler("profit", profit, pass_args = FALSE, filter_user) +
  CommandHandler("pricex", pricex, pass_args = FALSE, filter_user) +
  CommandHandler("this", this, pass_args = TRUE, filter_user) +
  CommandHandler("ohis", ohis, pass_args = FALSE, filter_user) +
  CommandHandler("help", help, pass_args = FALSE, filter_user) +
  CommandHandler("start", help, pass_args = FALSE, filter_user) 
# Create error callback
error_callback <- function(bot, error) {
  warning(simpleWarning(conditionMessage(error), call = "Updates polling"))
}
# Register it to the updater's dispatcher
updater$dispatcher$add_error_handler(error_callback)
updater$start_polling()
bot <- Bot(token = bot_token("RTelegramBot"))
updates <- bot$getUpdates()
bot$sendMessage(Sys.getenv("TG_WEAK"), text = "*懶狗通知*壞掉了要重開")