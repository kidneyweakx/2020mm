# 前置作業
rm(list=ls(all=T))
knitr::opts_chunk$set(comment = NA)
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse,lubridate)
# 做一間公司
c_scale <- read.csv("data/公司規模.csv", header = TRUE, sep=",",stringsAsFactors = FALSE) %>% as_tibble()
odr_all <-  dir("data/委託檔") %>% substr(1,4)

set.seed(111)
# from here 卑鄙之術
# 讀取成交檔和委託檔
odr_tail <- c("odr01_03.csv","odr04_06.csv","odr07_09.csv","odr10_12.csv")
company = toString(c_scale$code[1])
Odr <-  read.csv(paste("data/委託檔/",
                       company, odr_tail[2],sep=""), 
                 header = TRUE, sep=",",stringsAsFactors = FALSE) %>% as_tibble()
# 處理時間
Odr <- Odr %>% mutate(OdrTimes = (ymd_hms(OdrDate*1000000+OdrTime/100)+dmilliseconds(OdrTime%%100*10)),
                      OdrDate=ymd(OdrDate))
# 智障之術
buylist <- data.frame()
selllist <- data.frame()
spread <- data.frame()
Odr <- Odr %>%  na.omit()

for(i in 1:nrow(Odr)){
  X=0
  pio <- interval(ymd(Odr$OdrDate[i])+hours(9), # 開盤
                  ymd(Odr$OdrDate[i])+hours(13)+minutes(30)) # 收盤
  if(Odr$OdrTimes[i] > Odr$OdrDate[i]) { # 處理一天
    if(Odr$BuySell[i]=="S"){
      selllist= rbind(selllist,Odr[Odr$OdrDate == Odr$OdrDate[i],][i,])
    }else{
      buylist = rbind(buylist,Odr[Odr$OdrDate == Odr$OdrDate[i],][i,])  # buy
      }
    if(Odr$OdrTimes[i] %within% pio){ # 開收盤期間
      while(min(buylist[buylist$OdrDate == Odr$OdrDate[i] & buylist$ChOdrShr > 0,]$OdrPr) <=
            max(selllist[selllist$OdrDate == Odr$OdrDate[i] & selllist$ChOdrShr > 0,]$OdrPr) )  {
        bestBPr <- min(buylist[buylist$OdrDate == Odr$OdrDate[i] & buylist$ChOdrShr > 0,]$OdrPr)
        bestSPr <- max(selllist[selllist$OdrDate == Odr$OdrDate[i] & selllist$ChOdrShr > 0,]$OdrPr)
        OdrShr <- sum(buylist[buylist$OdrDate == Odr$OdrDate[i] &
                                buylist$ChOdrShr > 0 &
                                buylist$OdrPr == bestBPr,]$ChOdrShr) -
          sum(selllist[selllist$OdrDate == Odr$OdrDate[i]&
                          selllist$ChOdrShr > 0 &
                          selllist$OdrPr == bestSPr,]$ChOdrShr)

          buylist[buylist$OdrDate == Odr$OdrDate[i] &
                    buylist$ChOdrShr>0 & buylist$OdrPr == bestBPr,]$ChOdrShr <- 0
          selllist[selllist$OdrDate == Odr$OdrDate[i] &
                     selllist$ChOdrShr>0 & selllist$OdrPr == bestSPr,]$ChOdrShr <- 0
        if(OdrShr>0){
          buylist[buylist$OdrDate == Odr$OdrDate[i] &
                    buylist$OdrPr == bestBPr,]$ChOdrShr[1] <- OdrShr
        } else if(OdrShr<0){
          selllist[selllist$OdrDate == Odr$OdrDate[i] &
                     selllist$OdrPr == bestSPr,]$ChOdrShr[1] <- OdrShr
        } else{}

        X=X+1
      }
      if(X>1){
        sl5 <- selllist %>% filter(OdrDate == Odr$OdrDate[i],
                                   ChOdrShr > 0) %>%
          arrange(desc(OdrPr)) %>% select(OdrPr) %>% unique() %>% head(5)
        bl5 <- buylist %>% filter(OdrDate == Odr$OdrDate[i],
                                 ChOdrShr > 0) %>%
          arrange(OdrPr) %>% select(OdrPr) %>% unique() %>% head(5)

        spread=rbind(spread,data.frame(time=Odr$OdrTimes[i],
                                       sl5=sl5[5,],sl4=sl5[4,],sl3=sl5[3,],sl2=sl5[2,],sl1=sl5[1,],
                                       bl5=bl5[5,],bl4=bl5[4,],bl3=bl5[3,],bl2=bl5[2,],bl1=bl5[1,]))

      }else{}
    }else{}
  }else{}
}

