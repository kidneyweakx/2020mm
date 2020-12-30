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

# Setting MAX trading Robot
## MAX API(Python)
# 如果懶得做 直接將
# + `file.edit(path.expand(file.path("~", ".Renviron")))`
# + 加入環境變數範例: `MAX_API_KEY="yourkey"`
# + 加入後**重新開啟** Rstudio
max <- import("max.client")
client <- max$Client("MAX_API_KEY","MAX_API_SECRET") # setting your key

# =============買入設定=============
market = "maxusdt"; spendusdt = 10 # 花費USDT量
# 價格 有 high low buy sell 可以選
ticker = client$get_public_all_tickers()[[tolower(market)]]$sell %>% as.numeric()
ticker = 0.127
amount = spendusdt / ticker # 算出amount
# ===============================
#  # client$set_private_create_order('btcusdt', 'buy', 'amount', 'price', stop='', _type='market') 
trade <- client$set_private_create_order(toString(market), # 市場
                                'buy',                     # side
                                toString(amount),          # 量
                                toString(ticker))          # 價格

# client$get_private_order_history('maxusdt')
# id <- client$get_private_order_history(market)[[1]]$id
id <- toString(trade$id) # 交易ID(用於)取消訂單用
# 訂單成交否 ?
client$get_private_order_detail(id)[['state']] != 'wait'
# 卡住直到訂單完成
# 可ctrl + c 暫停並取消訂單

# ==================相當重要====================
# client$set_private_cancel_order(id) # 取消訂單
# ==============================================

# 剩下餘額 自己更換市場
client$get_private_account_balance('usdt')$balance
client$get_private_account_balance('max')$balance
