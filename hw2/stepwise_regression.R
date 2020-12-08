rm(list=ls());gc()
library(dplyr)
library(tidyverse)

#-----------------------逐步回歸程式碼範例-----------------------
#colume要進行逐步回歸的變數欄位
reg <- data %>% 
  select(colume)

#將變數轉為numeric型態 
reg <- sapply(reg,as.numeric) %>% as_tibble()
#NA值補0(因有NA值，下面無法跑)
reg[is.na(reg)] <- 0

# 1.建立空的線性迴歸(只有截距項)
#Y:預測變數
null = lm(Y ~ 1, data = reg)  
full = lm(Y ~ ., data = reg,na.rm=T)# 建立上界，也就是完整的線性迴歸

#2. 逐步回歸類型 : forward,backward,both
forward_lm_1 <- step(null, scope = list(upper=full), direction="forward")
summary(forward_lm_1)