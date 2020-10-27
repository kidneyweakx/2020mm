if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, reticulate)
# python anaconda 位置
use_condaenv("cloud",conda = "D:\\app\\Anaconda3\\condabin\\conda")
# bs4 <- import("bs4")$BeautifulSoup
# req <- import("requests")
# pd <- import("pandas",as = "pd")

# df <- do.call(rbind,lapply(1:length(code),function(cc){
#   url <- paste0("https://stock.wearn.com/income.asp?kind=",code[cc])
#   res <- req$get(url)$content %>%
#     bs4("html.parser")
#   pddf <- pd$read_html(toString(res$find("table")))
#   df <- do.call(rbind,lapply(2:89,function(i){
#     data.frame(
#       "日期"=pddf[[1]][0][i],
#       "營收(仟元)"=pddf[[1]][1][i],
#       "營業毛利率"=pddf[[1]][2][i],
#       "營業利益率"=pddf[[1]][3][i],
#       "稅前純益率"=pddf[[1]][4][i],
#       "稅後純益率"=pddf[[1]][5][i],
#       "稅後EPS"=pddf[[1]][6][i],
#       "StkNo"=code[cc]
#     )
#   }))
#   return(df)
# }))
# save(df, file = "cccmoney.rdata")