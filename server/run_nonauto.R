# ----edit by UTF-8----
# @author kidneyweak
# @title 手動交易
# @description test buy MAX (10 usdt)
setwd("D://workspace//R//Course//2020mm//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate) # 載入PKG
# reticulate 是R call Python function的package
# 使用前需下載 anaconda或mimiconda
# 如無下載套件會自動讓你下載

# *****重要*****
## python anaconda location (需自行修改)
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")

# Setting MAX trading Robot
## MAX API(Python)
# 如果懶得做 直接將
# + `file.edit(path.expand(file.path("~", ".Renviron")))`
# + 加入環境變數範例: `MAX_API_KEY="yourkey"`
# + 加入後**重新開啟** Rstudio
max <- import("max.client")
# client <- max$Client("MAX_API_KEY","MAX_API_SECRET") # setting your key
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
# ==================================
# 委託單送出可以卡著直到完成的function
chkstate <- function(id){
  # client$get_private_order_detail(id)[['state']] == 'wait' # 等待
  while(client$get_private_order_detail(id)[['state']] == 'wait'){
    Sys.sleep(0.5)
  }
  client$get_private_order_detail(id)[['state']]
  print("Done!!!")
}
# =============買入設定=============
market = "ethusdt"; spendusdt = 10 # 花費USDT量
# 價格 有 high low buy sell 可以選
ticker = client$get_public_all_tickers()[[tolower(market)]]$buy %>% as.numeric()
amount = spendusdt / ticker # 算出amount
#client$get_private_account_balance("max")$balance
# ===============================
#  # client$set_private_create_order('btcusdt', 'buy', 'amount', 'price', stop='', _type='market') 
trade <- client$set_private_create_order(toString(market), # 市場
                                'buy',                     # side
                                toString(amount),          # 量
                                toString(ticker))          # 價格
#383897626 #383897626
# client$get_private_order_history('maxusdt')
# id <- client$get_private_order_history(market)[[1]]$id
id <- toString(trade$id) # 交易ID(用於)取消訂單用
# 訂單成交否 ?
client$get_private_order_detail(id)[['state']] != 'wait'
# 卡住直到訂單完成
# 可ctrl + c 暫停並取消訂單
chkstate(id)

# ==================相當重要====================
# client$set_private_cancel_order(id) # 取消訂單
# ==============================================
market = "maxusdt"
sticker = 0.129#client$get_public_all_tickers()[[tolower(market)]]$high %>% as.numeric()
samount = 77#client$get_private_account_balance('max')$balance %>% as.numeric()
strade <- client$set_private_create_order(toString(market),'sell', toString(samount), toString(sticker))
# 剩下餘額 自己更換市場

# 383891263
market = "ethusdt"
sticker = 580#client$get_public_all_tickers()[[tolower(market)]]$high %>% as.numeric()
samount = client$get_private_account_balance('eth')$balance %>% as.numeric()
strade <- client$set_private_create_order(toString(market),'sell', toString(samount), toString(sticker))
# 剩下餘額 自己更換市場
client$get_private_account_balance('usdt')$balance
client$get_private_account_balance('max')$balance

## =============自己設定區=============
market="ethusdt"; ms="usdt" ;mb="eth"; hold = 0
bot <- Bot(token = bot_token("RTelegramBot"))

sendmsg <- function(texts="*懶狗該交易囉!*"){
  bot$sendMessage(Sys.getenv("TG_GROUP_ID"),
                  text = texts,
                  parse_mode = "Markdown"
  )}
# sticker = 596
# samount =  client$get_private_account_balance(mb)$balance
# strade <- client$set_private_create_order(toString(market),'sell', toString(samount), toString(sticker))
if (!client$get_private_order_history("ethusdt")[[1]]$state == "wait") {
  sendmsg(paste("*懶狗交易錄[出場]*\n 交易時間:",Sys.time(),
                "\n 價格:",596,"\n 出場價格:",
                client$get_private_order_history("ethusdt")[[1]]$price,
                "\n 交易量:","0.01681"))
}
