# Asset Pricing Homework 1
#'@title Predict China's Stock Market Return
#'@author [Hang Cheng]
#'@date 2024-04-15
#'@description This Code is used for 2024 Spring Asset Pricing Course at DUFE
#'@Website http://chenghang.work

# Prepare -------------------------------------------------------------------------
# Load Packages
Sys.setlocale ( category = 'LC_ALL' , locale = 'English_United States.1252' )
library('tidyverse')
library('readxl')
library('lubridate')
library('forecast')
library('data.table')
library('sandwich')
library('lmtest')
library('stargazer')
library('timeDate')
library('tseries')
library('openxlsx')
library('plyr')
library('dplyr')

# Excess Return -------------------------------------------------------------------
# Stock Market Return: https://share.weiyun.com/q3kuM3Xu
# Market type is 21=沪深A股和创业板
# Trdmnt [交易月份] - 以YYYY-MM表示
# Cmretwdos [考虑现金红利再投资的综合月市场回报率(流通市值加权平均法)]
# Cmretwdeq [考虑现金红利再投资的综合月市场回报率(等权平均法)]
# Update to 2023

Marketret_mon_stock <- read.xlsx('D:/Academic/academic/Assetpricing/Input/Market_Return/TRD_Cnmont2023.xlsx',sheet = 1) %>%
  data.table() %>%
  .[c(-1,-2),] %>%
  .[as.numeric(Markettype) == 21] %>%
  .[ , c ( "Trdmnt" , "Cmretwdos" , 'Cmretwdeq' ) ] %>%
  na.omit ( ) %>%
  dplyr::rename(month = Trdmnt , MarketR = Cmretwdos , MarketR_e = Cmretwdeq) %>%
  .[, ':='(month = as.yearmon(month) ,
           MarketR = as.numeric(MarketR) ,
           MarketR_e = as.numeric(MarketR_e))]

# Risk free rate: https://share.weiyun.com/1kGOsysR
# Nrrdata [无风险利率(%)] -
# Nrrdaydt [日度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为日度数据。计算公式为：(POWER(1+Nrrdata/100,1/365)-1)*100
# Nrrwkdt [周度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为周度数据。计算公式为：(POWER(1+Nrrdata/100,1/52)-1)*100
# Nrrmtdt [月度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为月度数据。计算公式为：(POWER(1+Nrrdata/100,1/12)-1)*100

riskfree <- read.xlsx('D:/Academic/academic/Assetpricing/Input/Market_Return/TRD_Nrrate2023.xlsx',sheet = 1) %>% data.table() %>% .[c(-1,-2),]
riskfree_day <- riskfree[, c(2 , 4)] %>%
  dplyr::rename(Day = Clsdt , rfday = Nrrdaydt) %>%
  .[, ':='(rfday = as.numeric(rfday) / 100 , Day = as.Date(Day))]
riskfree_mon <- riskfree[, c(2 , 6)] %>%
  dplyr::rename(Day = Clsdt , rf = Nrrmtdt) %>%
  data.table() %>%
  .[, ':='(month = as.yearmon(Day))] %>%
  .[, .(rfmonth = mean(as.numeric(rf) / 100)) , by = 'month']

# merge risk free rato into return data: market ret - risk free rate = excess return
Marketret_mon_stock <- Marketret_mon_stock %>%
  merge(riskfree_mon , by = 'month' , all.x = T) %>%
  na.omit() %>%
  .[, ':='(ret = MarketR - rfmonth , ret_e = MarketR_e - rfmonth)]

# compound 3 6 12 months return
Marketret_mon_stock <- Marketret_mon_stock %>%
  ddply(.(month) , function(x) {
    ifelse (x$month <= '2023-10' ,
            x$marketret3 <- prod (1 + Marketret_mon_stock[month >= x$month & month <= (x$month + 2 / 12)]$MarketR) -
              prod (1 + Marketret_mon_stock[month >= x$month & month <= (x$month + 2 / 12)]$rfmonth) , x$marketret3 <- NA)
    ifelse (x$month <= '2023-07' ,
            x$marketret6 <- prod (1 + Marketret_mon_stock[month >= x$month & month <= x$month + 5 / 12]$MarketR) -
              prod (1 + Marketret_mon_stock[month >= x$month & month <= x$month + 5 / 12]$rfmonth) , x$marketret6 <- NA)
    ifelse (x$month <= '2023-01' ,
            x$marketret12 <- prod (1 + Marketret_mon_stock[month >= x$month & month <= x$month + 11 / 12]$MarketR) -
              prod (1 + Marketret_mon_stock[month >= x$month & month <= x$month + 11 / 12]$rfmonth) , x$marketret12 <- NA)
    return (x)
  } , .progress = 'text') %>%
  data.table ()

# Predictor: Price-Dividend Ratio ------------------------------------------------

# Market Value: https://share.weiyun.com/SwvqbO9O
ret_day_ST <- readRDS('D:/Academic/academic/Assetpricing/Output/ret_day_ST2023.RDS')

# Dividend: https://share.weiyun.com/YZcttlFH
# CSMAR 专题研究系列 股利政策数据库
# Stkcd [证券代码] - 上海A股以上交所公布的证券代码为准，深圳A股以深交所公布的证券代码前加上00
# Disttyp [分配类型] - CA=现金红利，SD=送红股，RO=配股，DS=拆细，GQ=股权分置
# Exdistdt [除权(息)日期] - 以YYYY-MM-DD表示
# Annodt [公告日期] - 以YYYY-MM-DD表示。
# Paydt [支付日期] - 如果分配类型是CA，指现金红利实际到帐日期。如果分配类型是SD、RO、DS、GQ指增加的股票实际上市流通日
# Amount [分配比率] - 以每股作为分配基准单位， 如果分配类型是CA，计量货币为人民币元，计量单位为元，如果分配类型是SD,RO或DS,计量单位为股
# Roprc [配股价格] - 除RO外其他分配类型为0，计量货币为人民币元, 计量单位为元

Dividend <- read.xlsx('D:/Academic/academic/Assetpricing/Input/Dividend/DPR_Acptl202312.xlsx') %>% data.table() %>%
  .[,c('Stkcd','Disttyp','Exdistdt','Amount')] %>% na.omit() %>%
  .[substring(Stkcd,1,1) == '0' | substring(Stkcd,1,1) == '3' | substring(Stkcd,1,1) == '6'] %>%    # 保留上海、深圳、创业板
  .[Disttyp == 'CA'] %>%  #现金分红
  dplyr::rename(Day = Exdistdt) %>%
  .[,':='(month = as.yearmon(Day))] %>% .[month <= '2023-12']

ret_day_ST[,':='(month = as.yearmon(Day))]
ret_day_ST[,':='(yuemo = ifelse(Day == max(Day),1,0)),by = c('Stkcd','month')]

# merge size and dividend
Dividend <- Dividend %>% merge(ret_day_ST[,c('Stkcd','month','CirculationValue',
                                             'A_float_shares','Clsprc','yuemo')] %>% .[yuemo == 1],
                               by = c('Stkcd','month'),all = T) %>% data.table() %>% setorder(Stkcd,month)

Dividend_market <- Dividend[,.(p = sum(as.numeric(CirculationValue),na.rm = T),
                        d = sum( as.numeric(Amount) * as.numeric(A_float_shares), na.rm = T)),by = 'month'] %>%
  setorder(month)
Dividend_market[,':='(d1 = dplyr::lag(d),d2 = dplyr::lag(d,n = 2L),
               d3 = dplyr::lag(d,n = 3L),d4 = dplyr::lag(d,n = 4L),
               d5 = dplyr::lag(d,n = 5L),d6 = dplyr::lag(d,n = 6L),
               d7 = dplyr::lag(d,n = 7L),d8 = dplyr::lag(d,n = 8L),
               d9 = dplyr::lag(d,n = 9L),d10 = dplyr::lag(d,n = 10L),
               d11 = dplyr::lag(d,n = 11L))] %>%
  .[,':='(sum_d = (d + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 + d11))]

Price_dividend_mon <- Dividend_market[,.(pd = log( p / sum_d)),by = 'month']

# IPOFDR -------------------------------------------------------------------------

IPO <- read.xlsx('D:/Academic/academic/Assetpricing/Input/QX_IPO.xlsx') %>%
  data.table() %>% .[c(-1,-2),]

# Choose Symbol begin with 0,3,6 but not 688
IPO <- IPO[grepl('^[036]',Symbol) & !grepl('688',Symbol)]
IPO[,':='(month = as.yearmon(as.Date(ListedDate)),
          Day = as.Date(ListedDate),
          Stkcd = as.character(Symbol))]

# Double Check 了IPO数据中和ret_day数据中的首日收益率是一样的

# ret_day数据中计算每一只股票自从上市以来第一天的收益率
# 如果后续涨停则累计计算，没有涨停则停止计算

ret_day_ST[,':='(Return_1 = as.numeric(Return_1),
                 Return_2 = as.numeric(Return_2))]

# 定义涨停的逻辑，这里假设是Return_2大于9.9%为涨停
# 请根据实际情况调整
limit_up_threshold <- 0.099

# 计算每只股票自上市以来直到第一个非涨停日的累计收益
# 使用data.table的by参数进行分组，并使用cumprod计算累计收益
IPOR <- ret_day_ST[, .(Cum_Return = {
  # 确定是否为涨停日
  limit_up <- Return_2 >= limit_up_threshold #仅考虑价格变动的收益率
  # 获取第一个非涨停日的索引
  first_non_limit_up <- which(!limit_up)[1]
  cum_return <- cumprod(1 + Return_2 * limit_up) - 1
  # 如果缺失代表一直在涨停，则返回数据中的最后一个累计收益率（这种情况只可能是2023年底最后一天上市）
  if (is.na(first_non_limit_up)) {
    cum_return[length(cum_return)]
    }
  #如果是第一天没涨停则返回第一天收益率
  else if (first_non_limit_up == 1) {
    Return_2[first_non_limit_up]
  }
  #如果是正常的非第一日没涨停日，返回上一个累计收益率
  else {
    cum_return[first_non_limit_up - 1]
  }
}
  ), keyby = .(Stkcd)] %>%
  merge(IPO, by = 'Stkcd',all.y = T) %>%
  .[,.(Stkcd,month,Day,Cum_Return)]

IPOR.month <- IPOR[,.(Cum_Return = mean(Cum_Return, na.rm = T)), keyby = .(month)] %>%
  .[,':='(month = as.yearmon(month))]

# Merge Data -------------------------------------------------------------

IPOR.month <- IPOR.month %>%
  merge(Price_dividend_mon, by = 'month', all = T) %>%
  .[,':='(ipor = ifelse(is.na(Cum_Return), 0, Cum_Return))] %>%
  .[,.(month,ipor,pd)]

ts <- merge(Marketret_mon_stock,IPOR.month,by = 'month',all.x = T) %>%
  .[month >= '2000-01'] %>%
  .[,':='(l.pd = shift(pd,type = 'lag'),
          l.ipor = shift(ipor,type = 'lag'))]

fwrite(ts,file = 'ts.csv')


# Regression ----------------------------------------------------------------

m1 <- lm(ret ~ l.pd, data = ts)
m2 <- lm(ret ~ l.ipor,data = ts)
m1nw <- coeftest(m1,vcov = NeweyWest(m1,lag = 6, prewhite = FALSE))
m2nw <- coeftest(m2,vcov = NeweyWest(m2,lag = 6, prewhite = FALSE))
stargazer(m1nw,m2nw,
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'scriptsize',no.space = T)

m1 <- lm(marketret3 ~ l.pd, data = ts)
m2 <- lm(marketret3 ~ l.ipor,data = ts)
m1nw <- coeftest(m1,vcov = NeweyWest(m1,lag = 6, prewhite = FALSE))
m2nw <- coeftest(m2,vcov = NeweyWest(m2,lag = 6, prewhite = FALSE))
stargazer(m1nw,m2nw,
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'scriptsize',no.space = T)

m1 <- lm(marketret6 ~ l.pd, data = ts)
m2 <- lm(marketret6 ~ l.ipor,data = ts)
m1nw <- coeftest(m1,vcov = NeweyWest(m1,lag = 6, prewhite = FALSE))
m2nw <- coeftest(m2,vcov = NeweyWest(m2,lag = 6, prewhite = FALSE))
stargazer(m1nw,m2nw,
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'scriptsize',no.space = T)

m1 <- lm(marketret12 ~ l.pd, data = ts)
m2 <- lm(marketret12 ~ l.ipor,data = ts)
m1nw <- coeftest(m1,vcov = NeweyWest(m1,lag = 6, prewhite = FALSE))
m2nw <- coeftest(m2,vcov = NeweyWest(m2,lag = 6, prewhite = FALSE))
stargazer(m1nw,m2nw,
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'scriptsize',no.space = T)