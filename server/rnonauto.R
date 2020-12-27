# ----edit by UTF-8----
setwd("D://workspace//R//Course//2020mm//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate) # 載入PKG
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))


# =============買入設定=============
market = "maxusdt"; spendusdt = 20#client$get_private_account_balance("usdt")$balance %>% as.numeric()#10 # 花費USDT量
# 價格 有 high low buy sell 可以選
ticker = 0.128#client$get_public_all_tickers()[[tolower(market)]]$buy %>% as.numeric()
amount = spendusdt / ticker # 算出amount
# ===============================
# client$set_private_create_order('btcusdt', 'buy', 'amount', 'price', stop='', _type='market') 
trade <- client$set_private_create_order(toString(market), # 市場
                                'buy',                     # side
                                toString(amount),          # 量
                                toString(ticker))          # 價格

# client$get_private_order_history('maxusdt')
# id <- client$get_private_order_history(market)[[1]]$id
id <- toString(trade$id) # 交易ID(用於)取消訂單用
client$get_private_order_detail(id)[['state']] != 'wait' # 訂單成交否 ?

# =============賣出設定=============
smarket = "maxusdt"
sticker = 0.129#client$get_public_all_tickers()[[tolower(smarket)]]$high %>% as.numeric()
samount = 238.596#client$get_private_account_balance('max')$balance
strade <- client$set_private_create_order(toString(smarket),
                                          'sell', 
                                          toString(samount),
                                          toString(sticker))
# sid <- client$get_private_order_history(smarket)[[3]]$id
sid <- toString(strade$id) # 交易ID(用於)取消訂單用
client$get_private_order_detail(sid)$state

# ==================相當重要====================
client$set_private_cancel_order(id) # 取消訂單
# client$set_private_cancel_order(sid)# 取消訂單
# ==============================================

# 剩下餘額 自己更換市場
client$get_private_account_balance('usdt')$balance
client$get_private_account_balance('max')$balance
client$get_private_account_balance('xrp')$balance


# =============交易清單========================
m <- client$get_public_all_markets() %>% r2df
m <- m %>% filter(quote_unit=="usdt");get <- 0
r2df <- function(l) {
  if(length(l)!=0){
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                   stringsAsFactors=FALSE);
    name <- names(l[[1]]);colnames(df) <- name[1:ncol(df)];
    return(df)} }
trhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
  client$get_private_trade_history(m$id[i]) %>% r2df }))
orhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
  client$get_private_order_history(m$id[i]) %>% r2df }))
