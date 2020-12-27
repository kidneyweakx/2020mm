rm(list = ls());gc()

library(lubridate)
library(tidyverse)
library(data.table)
library(quantmod)

TW_2330 <- read.csv("data/成交檔/2330mth04_06.csv")

#波動性&流動性

vol<- TW_2330 %>%  
  filter(TW_2330$BuySell=='B')

new_vol <- vol %>% 
  filter(vol$MthTime<=13300000) %>% 
  separate(MthTime,into=c('h',"m",'s','ss'),sep=c(-6,-4,-2)) %>% 
  select(MthDate,h,m,s,ss,MthPr,MthShr)

new_variable <- new_vol %>% 
  mutate(h=h %>% as.numeric(),
         m=m %>% as.numeric(),
         s=s %>% as.numeric(),
         ss=ss %>% as.numeric(),
         group=(h-9)*4+m%/%15) %>% 
  group_by(group,MthDate) %>% 
  mutate(n = row_number(),
         volume_sum=sum(MthShr)/1000) %>% 
  slice(1,n()) %>% 
  mutate(logret=abs(log(MthPr)-log(lag(MthPr,1))),
         ami=logret/volume_sum) %>% 
  na.omit() %>% 
  group_by(group) 

vol_everyday <- vol %>%
  select(MthDate,MthShr) %>% 
  group_by(MthDate) %>% 
  mutate(daily_vol=sum(MthShr)) %>% 
  select(-MthShr) %>% distinct()

lag_vol_everyday <- lag(vol_everyday,1)
colnames(lag_vol_everyday) <- c('MthDate','lag_daily_vol')

ami_var_daily <-new_variable %>%
  select(MthDate,logret,ami) %>% 
  arrange(MthDate) %>% 
  group_by(MthDate) %>% 
  summarize(ami_med=median(ami,na.rm = T),
            ret_var=var(logret,na.rm = T))


variable_combine <- left_join(vol_everyday,ami_var_daily)
total_variable_combine <- left_join(variable_combine,lag_vol_everyday)

lm(total_variable_combine$daily_vol~
     lag(total_variable_combine$daily_vol,1)+
     total_variable_combine$ami_med+
     total_variable_combine$ret_var)  

#第三題
#都算給個固定時間點前幾分鐘的平均價
#9點15分價格
price_15min <- vol %>% 
  filter(vol$MthTime<=9150000,vol$MthTime>=9130000) %>% 
  select(MthDate,MthTime,MthPr) %>% 
  group_by(MthDate) %>%
  mutate(mean_price=mean(MthPr)) %>% 
  select(mean_price) %>%
  distinct()

#9點半價格
price_30min <- vol %>% 
  filter(vol$MthTime<=9300000,vol$MthTime>=9130000) %>% 
  select(MthDate,MthTime,MthPr) %>% 
  group_by(MthDate) %>%
  mutate(mean_price=mean(MthPr)) %>% 
  select(mean_price) %>%
  distinct()


#開盤價
price_open <-vol %>% 
  filter(vol$MthTime<=9001000) %>% 
  select(MthDate,MthTime,MthPr) %>% 
  group_by(MthDate) %>%
  mutate(mean_price=mean(MthPr)) %>% 
  select(mean_price) %>%
  distinct()
view(price_open)

#收盤價
price_close <- vol %>% 
  filter(vol$MthTime<=14300000,vol$MthTime>=14290000) %>% 
  select(MthDate,MthTime,MthPr) %>% 
  group_by(MthDate) %>%
  mutate(mean_price=mean(MthPr)) %>% 
  select(mean_price) %>%
  distinct()
view(price_close)

#各價格整合
all_price <- cbind(price_open, price_close$mean_price, 
                   price_15min$mean_price, price_30min$mean_price)
colnames(all_price) <- c("MthDate","open","close","fir_15min","fir_30min")
#有些用 %>% 連結的指令需要先group_by才能執行
all_price_1 <- all_price %>% 
  arrange(MthDate) %>% 
  mutate(group="1")%>%
  group_by(group) %>%
  mutate(diff_day=log(lead(open,1)/close),
         diff_15min=log((fir_15min)/lag(fir_15min,1)),
         diff_30min=log(lead(fir_30min)/lag(fir_30min,1))) %>%
  na.omit()

#第四題
pattern_15min <-ggplot(all_price_1, aes(x = MthDate, y = fir_15min)) +
  geom_line()+
  xlab("Date")+
  ylab("ret")+
  ggtitle("medium_logret")