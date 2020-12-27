# ----edit by UTF-8----
setwd("D://workspace//R//Course//2020mm//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate, telegram.bot) # 載入PKG
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
# token
bot <- Bot(token = bot_token("RTelegramBot"))
updater <- Updater(token = bot_token("RTelegramBot"))



m <- client$get_public_all_markets() %>% map(1)# %>% unlist()
m <- m[str_detect(m, "usdt")] %>% unlist()

mIKM <- function(ll,cbdata){
  inline_keyboard = list()
  tmp <- list()
  for(i in c(1:length(ll))) {
    tmp <- append(tmp,list(InlineKeyboardButton(ll[i],callback_data = cbdata)))
    if(i%% 3==0){
      inline_keyboard = append(inline_keyboard,list(tmp)); tmp <- list()
    }
  }
  return(InlineKeyboardMarkup(inline_keyboard))
}


buy <- function(bot, update, args){
  IKM <- mIKM(m)
  
  if (length(args > 0L)){
    argstr <- strsplit(args, " ")
    vol <- argstr[1]
    pr <- argstr[2]
    curry <- argstr[3]
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = paste("Vol:",vol,"Pr:",pr,"幣別:",curry),
                    reply_markup = IKM
    ) 
  }
}
s=0
answer_cb <- function(bot, update) {
  client$get_private_account_balance
  data <- update$callback_query$data
  s <- update
  # Send Custom Keyboard
  bot$sendMessage(chat_id = update$callback_query$message$chat$id, 
                  text = paste0("Hello"))
  
  
  bot$answerCallbackQuery(callback_query_id = update$callback_query$id,
                          text = paste("Answer recorded:", data),
                          )
}
updater <- updater + CallbackQueryHandler(answer_cb)
updater <- updater + CommandHandler("buy", buy, pass_args = TRUE)
updater$start_polling()
