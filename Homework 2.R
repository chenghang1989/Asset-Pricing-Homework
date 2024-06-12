#'@Code Replicate the size and value in China
#'@author Cheng Hang
#'@Date 2024/06/12

# Prepare--------------------------------------------------------------------------
# Load Packages
Sys.setlocale ( category = 'LC_ALL' , locale = 'English_United States.1252' ) # 如果不你使用Pycharm不需要添加这行代码
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

# 个股月收益率 -----------------------------------------------------------------------

# Risk free rate https://share.weiyun.com/1kGOsysR
# Nrrdata [无风险利率(%)] -
# Nrrdaydt [日度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为日度数据。计算公式为：(POWER(1+Nrrdata/100,1/365)-1)*100
# Nrrwkdt [周度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为周度数据。计算公式为：(POWER(1+Nrrdata/100,1/52)-1)*100
# Nrrmtdt [月度化无风险利率(%)] - 根据复利计算方法，将年度的无风险利率转化为月度数据。计算公式为：(POWER(1+Nrrdata/100,1/12)-1)*100

riskfree <- read.xlsx('D:/Academic/academic/Assetpricing/Input/Market_Return/TRD_Nrrate2023.xlsx',sheet = 1) %>%
  data.table() %>%
  .[c(-1,-2),]
riskfree_day <- riskfree[,c(2,4)] %>%
  dplyr::rename(Day = Clsdt,rfday = Nrrdaydt) %>%
  .[,':='(rfday = as.numeric(rfday) / 100,Day = as.Date(Day))]
riskfree_mon <- riskfree[,c(2,6)] %>%
  dplyr::rename(Day = Clsdt,rf = Nrrmtdt) %>%
  data.table() %>%
  .[,':='(month = as.yearmon(Day))] %>%
  .[,.(rfmonth = mean(as.numeric(rf) / 100)),by = 'month']

# Individual stock monthly return: https://share.weiyun.com/ciQh8gct
# Stkcd [证券代码] - 以上交所、深交所公布的证券代码为准
# Trdmnt [交易月份] - 以YYYY-MM表示
# Opndt [月开盘日期] - 月第一个交易日。以DD表示，为OPNPRC的所在日，“DD”＝本月无交易
# Mopnprc [月开盘价] - 月第一个交易日的开盘价。
# Clsdt [月收盘日期] - 月最后一个交易日。以DD表示，为CLSPRC的所在日，“DD”＝本月无交易
# Mclsprc [月收盘价] - 月最后一个交易日的收盘价。
# Mnshrtrd [月个股交易股数] - 月内该股票的交易数量。计算公式为：月开盘日期与交易日期期间内日成交量之和。
# Mnvaltrd [月个股交易金额] - 月内该股票的交易金额。计算公式为：月开盘日期与交易日期期间内日成交额之和。A股以人民币元计，上海B以美元计，深圳B以港币计。
# Msmvosd [月个股流通市值] - 个股的流通股数与月收盘价的乘积。计算公式为：个股的流通股数与月收盘价的乘积。 A股以人民币元计，上海B以美元计，深圳B以港币计，注意单位是千
# Msmvttl [月个股总市值] - 个股的发行总股数与月收盘价的乘积。计算公式为：个股的发行总股数与月收盘价的乘积，A股以人民币元计，上海B股以美元计，深圳B股以港币计，注意单位是千
# Ndaytrd [月交易天数] - 计算公式为：月内实际交易的天数之和。
# Mretwd [考虑现金红利再投资的月个股回报率] - 字段说明见说明书“周、月、年个股回报率的计算方法”。
# Mretnd [不考虑现金红利再投资的月个股回报率] - 字段说明见说明书“周、月、年个股回报率的计算方法”。
# Markettype [市场类型] - 1=上证A股市场 (不包含科创板），2=上证B股市场，4=深证A股市场（不包含创业板）
# 8=深证B股市场，16=创业板， 32=科创板，64=北证A股市场。
# Capchgdt [最新股本变动日期] - 上市公司最近一次股本发生变化的日期
# Ahshrtrd_M [月盘后成交总量] - 月科创板股票盘后总成交量。计算公式为：月开盘日期与交易日期期间内日盘后成交量之和。
# Ahvaltrd_M [月盘后成交总额] - 月科创板股票盘后总成交额。计算公式为：月开盘日期与交易日期期间内日盘后成交额之和。

ret_mon <- read.xlsx('D:/Academic/academic/Assetpricing/Input/Individual Return/TRD_Mnth199012-202312.xlsx') %>%
  data.table() %>%
  .[c(-1,-2),c('Stkcd','Trdmnt','Ndaytrd','Msmvosd','Msmvttl','Mretwd','Markettype')] %>%
  dplyr::rename(month = Trdmnt,Freq = Ndaytrd,floatingvalue = Msmvosd,totalvalue = Msmvttl,Return = Mretwd) %>%
  .[,':='(floatingvalue = as.numeric(floatingvalue) * 1000, # 千元计价
		  totalvalue    = as.numeric(totalvalue) * 1000,
		  Return        = as.numeric(Return),
		  month         = as.yearmon(month),
		  Freq          = as.numeric(Freq),
		  Markettype    = as.numeric(Markettype))] %>%
  .[Markettype == 1 |
	  Markettype == 4 |
	  Markettype == 16] %>%   # 未保留科创板 日涨跌幅20%
  merge(riskfree_mon,by = 'month',all.x = T) %>%
  setorder(Stkcd,month)

# 将每只股票都补齐所有交易月份序列 方便滞后生成下一个月的收益率
ret_mon <- ret_mon %>%
  ddply(.(Stkcd),function(x){
	minmonth   <- min(x$month)
	test       <- merge(x,unique(ret_mon$month) %>%
	  data.table() %>%
	  dplyr::rename(month = '.') %>%
	  .[month >= minmonth],by = 'month',all.y = T)
	test$Stkcd <- unique(x$Stkcd)
	return(test)
  },.progress = 'text') %>%
  data.table() %>%
  .[,':='(ipomonth = min(month)),by = 'Stkcd'] %>%
  .[month >= ipomonth] %>%
  setorder(Stkcd,month) %>%
  .[,':='(ret        = Return - rfmonth,
		  Rank       = rank(month),
		  next_ret   = shift(Return - rfmonth,type = 'lead',n = 1L),
		  next_ret2  = shift(Return - rfmonth,type = 'lead',n = 2L),
		  next_ret3  = shift(Return - rfmonth,type = 'lead',n = 3L),
		  next_ret4  = shift(Return - rfmonth,type = 'lead',n = 4L),
		  next_ret5  = shift(Return - rfmonth,type = 'lead',n = 5L),
		  next_ret6  = shift(Return - rfmonth,type = 'lead',n = 6L),
		  next_ret7  = shift(Return - rfmonth,type = 'lead',n = 7L),
		  next_ret8  = shift(Return - rfmonth,type = 'lead',n = 8L),
		  next_ret9  = shift(Return - rfmonth,type = 'lead',n = 9L),
		  next_ret10 = shift(Return - rfmonth,type = 'lead',n = 10L),
		  next_ret11 = shift(Return - rfmonth,type = 'lead',n = 11L),
		  next_ret12 = shift(Return - rfmonth,type = 'lead',n = 12L),
		  sizef      = log(floatingvalue),
		  sizet      = log(totalvalue)),by = 'Stkcd'] %>%
  .[,c('Stkcd','month','Rank','Freq','floatingvalue','totalvalue','sizef','sizet','Return','rfmonth',
	   'ret','next_ret','next_ret2','next_ret3','next_ret4','next_ret5','next_ret6','next_ret7',
	   'next_ret8','next_ret9','next_ret10','next_ret11','next_ret12')]

# 市盈率 ------------------------------------------------------------
# 公告日期: https://share.weiyun.com/n0QsM3L1
# Stkcd [证券代码] - 以沪深交易所公布的最新证券代码为准
# Accper [统计截止日期] - 以YYYY-MM-DD列示，部分缺少在相应位置上以00表示，如1993年12月某日表示为1993-12-00
# Annodt [报告公布日期] - 以YYYY-MM-DD列示，部分缺少在相应位置上以00表示，如1993年12月某日表示为1993-12-00

account_date <- read.xlsx('Input/Report_Date/IAR_Rept2023.xlsx') %>%
  data.table() %>%
  .[c(-1,-2),c('Stkcd','Accper','Annodt')] %>%
  rename(report_date = Annodt) %>%
  .[,':='(Accper      = as.Date(Accper),
		  report_date = as.Date(report_date))] %>%
  .[,':='(Q = as.yearqtr(Accper))]

# 利润表: https://share.weiyun.com/D4Zptdam
# B002000000 净利润
# F020101 非经常损益
Profit <- read.xlsx("D:/Academic/academic/Assetpricing/Input/Profit/FS_Comins2023.xlsx") %>%
  data.table() %>%
  .[c(-1,-2),c('Stkcd','Accper','Typrep','B002000000')] %>%
  na.omit() %>%
  .[Typrep == 'A'] %>%
  .[,-3] %>%
  .[,Accper := as.Date(Accper)] %>%
  merge(read.xlsx('D:/Academic/academic/Assetpricing/Input/Non_recurring_gains_and_losses/FI_T22023.xlsx') %>% # 非经常性损益: https://share.weiyun.com/pwnvjDKb
		  data.table() %>%
		  .[c(-1,-2),c('Stkcd','Accper','F020101')] %>%
		  .[,Accper := as.Date(Accper)],by = c('Stkcd','Accper'),all.x = T) %>%
  rename(Q = Accper) %>%
  .[month(Q) > 1] %>%
  .[,':='(Q = as.yearqtr(as.Date(Q)))] %>%
  setorder(Stkcd,Q)
# 非常重要 该数据在2024.2.27日仍然没有修改
Profit[Stkcd == '600015' & Q == '2015 Q1']$F020101 <- as.numeric(Profit[Stkcd == '600015' & Q == '2015 Q1']$F020101) / 10^5
Profit <- Profit %>%
  .[,':='(earnings = ifelse(is.na(F020101) == F,as.numeric(B002000000) - as.numeric(F020101),as.numeric(B002000000)))] %>%
  merge(account_date[,c('Stkcd','Q','report_date')],by = c('Stkcd','Q'),all.x = T) %>%
  .[,':='(report_date = as.yearmon(as.Date(report_date)))]

# 发现的问题 2023.12.20
Profit[Stkcd == '601939' & Q == '2016 Q1']$report_date <- as.yearmon('2016-04')

# 数据中有很多报告日期缺失的情况 没有完全复查
# 例如 001872 招商港口 多期报告日期缺失 原因是 前身是 前代码为深交所：000022.SZ 和 200022.SZ
# 上市地：深圳证券交易所
# 证券代码：000022/200022 证券简称：深赤湾 A/深赤湾 B
# 拟变更后的证券代码：001872/201872 拟变更后的证券简称：招商港口/招港 B

Profit <- Profit %>%
  ddply(.(Stkcd,Q),function(x){
	if (year(x$Q) < 2002) {
	  x$Q_earnings <- NA
	}
	else {
	  if (quarter(x$Q) == 1) {
		x$Q_earnings <- x$earnings
	  }
	  else {
		if (nrow(Profit[Stkcd == x$Stkcd & Q == (x$Q - 1 / 4)]) == 1) {
		  x$Q_earnings <- Profit[Stkcd == x$Stkcd & Q == x$Q]$earnings - Profit[Stkcd == x$Stkcd & Q == (x$Q - 1 / 4)]$earnings
		}
		else {
		  x$Q_earnings <- NA
		}
	  }
	}
	return(x)
  },.progress = 'text') %>%
  data.table()


EP_individual_mon <- ret_mon[month >= '1991-01-01' & is.na(Return) == F] %>%
  ddply(.(Stkcd,month),function(x){
	
	test <- Profit[report_date <= x$month & Stkcd == x$Stkcd]
	
	if (nrow(test) >= 1) {
	  
	  t0 <- max(test$Q)
	  t1 <- max(test$Q) - 1 / 4
	  t2 <- max(test$Q) - 2 / 4
	  t3 <- max(test$Q) - 3 / 4
	  
	  recent_earnings <- test[Q == t0]$earnings
	  
	  if (x$month >= '2003-05') {
		if (nrow(Profit[Stkcd == x$Stkcd & Q == t1] == 1) &
		  nrow(Profit[Stkcd == x$Stkcd & Q == t2] == 1) &
		  nrow(Profit[Stkcd == x$Stkcd & Q == t3] == 1) &
		  nrow(Profit[Stkcd == x$Stkcd & Q == t0] == 1)) {
		  
		  matchearnings <- Profit[Stkcd == x$Stkcd & Q == t1]$Q_earnings +
			Profit[Stkcd == x$Stkcd & Q == t2]$Q_earnings +
			Profit[Stkcd == x$Stkcd & Q == t3]$Q_earnings +
			Profit[Stkcd == x$Stkcd & Q == t0]$Q_earnings
		}
		else {
		  matchearnings <- NA
		}
	  }
	  else {
		if (quarter(t0) == 4) {
		  matchearnings <- test[Q == t0]$earnings
		}
		if (quarter(t0) == 3) {
		  if (nrow(test[Q == t3] == 1)) {
			matchearnings <- test[Q == t3]$earnings
		  }
		  else {
			matchearnings <- NA
		  }
		}
		if (quarter(t0) == 2) {
		  if (nrow(test[Q == t2] == 1)) {
			matchearnings <- test[Q == t2]$earnings
		  }
		  else {
			matchearnings <- NA
		  }
		}
		if (quarter(t0) == 1) {
		  if (nrow(test[Q == t1] == 1)) {
			matchearnings <- test[Q == t1]$earnings
		  }
		  else {
			matchearnings <- NA
		  }
		}
	  }
	}
	else {
	  matchearnings   <- NA
	  recent_earnings <- NA
	}
	return(data.table(matchearnings,recent_earnings))
  },.progress = 'text') %>%
  data.table()

# The colume named 'earnings' is the sum of recent four quarters earnings
# 'recent_earnings' is the earnings in recetnt report

EP_individual_mon <- EP_individual_mon %>%
  merge(ret_mon[,c('Stkcd','month','floatingvalue','totalvalue')],by = c('Stkcd','month'),all.y = T) %>%
  merge(ret_day[yuemo == 1] %>% .[,c('Stkcd','month','ratio','Clsprc')],
		by = c('Stkcd','month'),all.x = T) %>%
  .[,':='(ep        = matchearnings * as.numeric(ratio) / as.numeric(floatingvalue),
		  ep_recent = recent_earnings / as.numeric(totalvalue))]

# EP_individual_mon <- readRDS('D:/Academic/academic/Assetpricing/Output/EP_individual_mon2023.RDS')

# Size and Value Factors -------------------------------------------------------------------------------
# Data Filter:
# we exclude stocks that have become public within the past six months.
# less than 120 days of trading records during the past 12 months or
# less than 15 days of trading records during the most recent month.
# we eliminate the bottom 30% of stocks ranked by market capitalization at the end of the previous month.

# Code 1
# ret_mon <- ret_mon %>% ddply(.(Stkcd,month),function (data){
#   data$past12freq <- ret_mon[month <= data$month & month > (data$month - 1)]$Freq %>% sum()
#   return(data)
# },.progress = 'text')

# Code 2
test <- ret_mon[.(Stkcd = Stkcd,start = month - 1, end = month),
                on = .(Stkcd, month > start, month <= end),sum(Freq,na.rm = T),by = .EACHI] %>% data.table() %>%
  .[,c(1,3,4)] %>% dplyr::rename(past12freq = V1)

ret_mon <- ret_mon[month >= '1999-12'] %>% merge(test,by = c('Stkcd','month'),all.x = T)
ret_mon[,':='(sizebreak = quantile(totalvalue,0.3,na.rm = T)), by = 'month']

Data <- ret_mon[Rank > 6 & past12freq >= 120 & Freq >= 10 & totalvalue >= sizebreak] %>%
  merge(EP_individual_mon[,.(Stkcd,month,ep,ep_recent)])

# Market Factor ------------------------------------------------------------------------------------------
MKT <- Data[!is.na(next_ret) & !is.na(totalvalue)][,.(mkt = weighted.mean(x = next_ret,w = totalvalue)), keyby = 'month'] %>%
  .[,':='(month = month + 1/12)]

nrow(MKT[month <= '2016-12'])
mean(MKT[month <= '2016-12']$mkt * 100)
sd(MKT[month <= '2016-12']$mkt * 100)
t.test(MKT[month <= '2016-12']$mkt * 100)

Data[,':='(size.notion = cut(sizet, breaks = quantile(sizet, probs = seq(0,1, length.out = 3), na.rm = TRUE),
                                                              include.lowest = TRUE) %>% as.numeric(),
           ep.notion = cut(ep, breaks = quantile(ep, probs = c(0,0.3,0.7,1), na.rm = TRUE),
                                                              include.lowest = TRUE) %>% as.numeric()),by = 'month']

Data[,':='(size.notion = ifelse(size.notion == 1, 'S', 'B'),
           ep.notion = ifelse(ep.notion == 1, 'G',ifelse(ep.notion == 2, 'M', 'V')))]

Factor_sv <- Data[size.notion == 'S' & ep.notion == 'V'][,.(SV = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']
Factor_sm <- Data[size.notion == 'S' & ep.notion == 'M'][,.(SM = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']
Factor_sg <- Data[size.notion == 'S' & ep.notion == 'G'][,.(SG = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']
Factor_bv <- Data[size.notion == 'B' & ep.notion == 'V'][,.(BV = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']
Factor_bm <- Data[size.notion == 'B' & ep.notion == 'M'][,.(BM = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']
Factor_bg <- Data[size.notion == 'B' & ep.notion == 'G'][,.(BG = weighted.mean(next_ret,totalvalue,na.rm = T)),keyby = 'month']

Factor <- Factor_sv[Factor_sm,on = 'month'] %>%
  .[Factor_sg,on = 'month'] %>%
  .[Factor_bv,on = 'month'] %>%
  .[Factor_bm,on = 'month'] %>%
  .[Factor_bg,on = 'month'] %>%
  setorder(month) %>%
  .[,':='(smb = 1/3 * (SV + SM + SG) - 1/3 * (BV + BM + BG),
          vmg = 1/2 * (BV + SV) - 1/2 * (SG + BG))] %>%
  .[,c('month','smb','vmg')] %>% .[,month := month + 1 / 12] %>%
  merge(MKT,by = 'month')

nrow(Factor[month <= '2016-12'])
mean(Factor[month <= '2016-12']$smb * 100)
sd(Factor[month <= '2016-12']$smb * 100)
t.test(Factor[month <= '2016-12']$smb * 100)

nrow(Factor[month <= '2016-12'])
mean(Factor[month <= '2016-12']$vmg * 100)
sd(Factor[month <= '2016-12']$vmg * 100)
t.test(Factor[month <= '2016-12']$vmg * 100)

cor(Factor[,c(4,2,3)])