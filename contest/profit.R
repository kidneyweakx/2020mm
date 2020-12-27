# ----edit by UTF-8----
setwd("D://workspace//R//Course//2020mm//contest//") # setting your workspace
rm(list=ls(all=T)) # 清空資料
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, reticulate) # 載入PKG
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
max <- import("max.client")
client <- max$Client(Sys.getenv("MAX_API_KEY"),Sys.getenv("MAX_API_SECRET"))
# r2df
r2df <- function(l) {
  if(length(l)!=0){
    df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T),
                     stringsAsFactors=FALSE);
    name <- names(l[[1]]);colnames(df) <- name[1:ncol(df)];
    return(df)} }
# list(如果有TWD交易把filter去掉)
m <- client$get_public_all_markets() %>% r2df
m <- m %>% filter(quote_unit=="usdt");get <- 0
trhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
  client$get_private_trade_history(m$id[i]) %>% r2df }))
orhis <- do.call(rbind,lapply(c(1:nrow(m)), function(i){ 
  client$get_private_order_history(m$id[i]) %>% r2df }))

camt <- function(m){
  amt <- client$get_private_account_balance(m)$balance %>% as.numeric()
  amt2 <- client$get_private_account_balance(m)$locked %>% as.numeric()
  return(amt+amt2)
}
for(i in c(1:nrow(m))) {
  amt <- camt(m$base_unit[i])
  nowp <- client$get_public_all_tickers()[[m$id[i]]]$buy %>% as.numeric()
  acamt <- amt*nowp
  get <- get+acamt
  print(paste(m$base_unit[i],acamt))
}
assets <- get+camt("usdt")
cat(" 獲利趴數:",(assets/60 -1)*100,"%\n", 
    "交易次數:", nrow(trhis),"筆\n",
    "目前掛單:", nrow(orhis),"筆")
