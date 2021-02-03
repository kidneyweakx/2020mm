# 微結構課堂作業
Market MicroStructure Homework in NSYSU(National Sun Yat-sen University)

---

> 此專案內容多於半夜爆肝完成，如有任何不妥的內容，或違反著作權的部分請[聯繫我](mailto:kidneyweakx@gmail.com)刪除

### 使用工具與服務
1. [R 4.0.3](https://cran.r-project.org/bin/windows/base/) + [Rstudio 1.3.1093](https://rstudio.com/products/rstudio/download-server/)
2. [Miniconda](https://docs.conda.io/en/latest/miniconda.html)
3. [R套件下載工具 pacman](https://cran.r-project.org/web/packages/pacman/index.html)
4. [R Python串接介面 reticulate](https://cran.r-project.org/web/packages/reticulate/index.html)
5. [R tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)
6. [R telegram.bot](https://cran.r-project.org/web/packages/telegram.bot/index.html)
7. [telegram](https://telegram.org/)

### 課堂作業內容
該部分由分析台股2018年成交檔資料而成
- [HW1.](./hw1) 隱含交易成本與日內波動性、流動性
- [HW2.](./hw2) 報酬率與量
- [HW3.](./hw3) Kyle Lambda
- [HW4.](./hw4) VPIN
- [HW5.](./hw5) OIB,Order Flow Autocorrelation andVariance Ratio

### 期末報告及競賽
*by. kidneyweakx*
+ [contest](./contest)包含虛擬貨幣交易競賽回測程式，運用tidyquant建立簡單的回測工具，及計算大略的交易回報
+ [server](./server)內含有自動交易的R file，source後即可自動執行策略，策略執行也是使用tidyquant策略執行也是使用tidyquant
+ [final](./final)內部是幣安和MAX交易所採取價差策略(運用交易所價格差)來做回測及分析

### 小專案
[Trading telegram Bot](./server/tgc.R) 該檔案可以用於台灣MAX交易所交易的BOT，內含[說明文件](./server/tgc.pdf)

### Special Thanks
+ 資管系 [陳聖勳](https://github.com/kidneyweakx) 負責撰寫期末報告及課堂作業程式
+ 資管系 [李昀澍](https://github.com/Luv-Puff) 負責撰寫課堂作業及許多理論知識程式
+ 資管系 [林岳賢](https://github.com/bgt582165) 負責提供理論公式及將結果轉換成pdf檔 