import pandas as pd
import numpy as np
from pandas.tseries.offsets import MonthEnd, QuarterEnd
import scipy.stats as stats
import locale

# Attempt to use English locale if available
try:
    locale.setlocale(locale.LC_ALL, 'en_US.UTF-8')
except locale.Error:
    pass

# ----------------------------
# Risk free rate
# ----------------------------
# Paths here follow the original R script. Adjust to your local layout.
riskfree = pd.read_excel('Input/Market_Return/TRD_Nrrate2023.xlsx', sheet_name=0, skiprows=2)

riskfree_day = riskfree[['Clsdt', 'Nrrdaydt']].rename(columns={'Clsdt': 'Day', 'Nrrdaydt': 'rfday'})
riskfree_day['rfday'] = pd.to_numeric(riskfree_day['rfday'], errors='coerce') / 100
riskfree_day['Day'] = pd.to_datetime(riskfree_day['Day'])

riskfree_mon = riskfree[['Clsdt', 'Nrrmtdt']].rename(columns={'Clsdt': 'Day', 'Nrrmtdt': 'rf'})
riskfree_mon['month'] = pd.to_datetime(riskfree_mon['Day']).dt.to_period('M').dt.to_timestamp('M')
riskfree_mon = (
    riskfree_mon.assign(rf=pd.to_numeric(riskfree_mon['rf'], errors='coerce') / 100)
    .groupby('month')['rf'].mean()
    .reset_index()
    .rename(columns={'rf': 'rfmonth'})
)

# ----------------------------
# Individual stock monthly return
# ----------------------------
ret_mon = (
    pd.read_excel('Input/Individual Return/TRD_Mnth199012-202312.xlsx', skiprows=2)
    [[
        'Stkcd',
        'Trdmnt',
        'Ndaytrd',
        'Msmvosd',
        'Msmvttl',
        'Mretwd',
        'Markettype',
    ]]
)
ret_mon = ret_mon.rename(
    columns={
        'Trdmnt': 'month',
        'Ndaytrd': 'Freq',
        'Msmvosd': 'floatingvalue',
        'Msmvttl': 'totalvalue',
        'Mretwd': 'Return',
    }
)
ret_mon['floatingvalue'] = pd.to_numeric(ret_mon['floatingvalue'], errors='coerce') * 1000
ret_mon['totalvalue'] = pd.to_numeric(ret_mon['totalvalue'], errors='coerce') * 1000
ret_mon['Return'] = pd.to_numeric(ret_mon['Return'], errors='coerce')
ret_mon['month'] = pd.to_datetime(ret_mon['month']).dt.to_period('M').dt.to_timestamp('M')
ret_mon['Freq'] = pd.to_numeric(ret_mon['Freq'], errors='coerce')
ret_mon['Markettype'] = pd.to_numeric(ret_mon['Markettype'], errors='coerce')
ret_mon = ret_mon[ret_mon['Markettype'].isin([1, 4, 16])]
ret_mon = ret_mon.merge(riskfree_mon, on='month', how='left')
ret_mon = ret_mon.sort_values(['Stkcd', 'month']).reset_index(drop=True)

all_months = ret_mon['month'].drop_duplicates().sort_values()


def expand_stock(df: pd.DataFrame) -> pd.DataFrame:
    start = df['month'].min()
    months = all_months[all_months >= start]
    new_df = pd.DataFrame({'month': months})
    new_df['Stkcd'] = df['Stkcd'].iloc[0]
    return new_df.merge(df, on=['Stkcd', 'month'], how='left')


ret_mon = (
    ret_mon.groupby('Stkcd', group_keys=False).apply(expand_stock).reset_index(drop=True)
)
ret_mon['ipomonth'] = ret_mon.groupby('Stkcd')['month'].transform('min')
ret_mon = ret_mon[ret_mon['month'] >= ret_mon['ipomonth']]
ret_mon = ret_mon.sort_values(['Stkcd', 'month'])

ret_mon['ret'] = ret_mon['Return'] - ret_mon['rfmonth']
for i in range(1, 13):
    ret_mon[f'next_ret{i}'] = ret_mon.groupby('Stkcd')['ret'].shift(-i)
ret_mon['Rank'] = ret_mon.groupby('Stkcd').cumcount() + 1
ret_mon['sizef'] = np.log(ret_mon['floatingvalue'])
ret_mon['sizet'] = np.log(ret_mon['totalvalue'])

ret_mon = ret_mon[
    [
        'Stkcd',
        'month',
        'Rank',
        'Freq',
        'floatingvalue',
        'totalvalue',
        'sizef',
        'sizet',
        'Return',
        'rfmonth',
        'ret',
    ]
    + [f'next_ret{i}' for i in range(1, 13)]
]

# ----------------------------
# Earnings data
# ----------------------------
account_date = (
    pd.read_excel('Input/Report_Date/IAR_Rept2023.xlsx', skiprows=2)
    [[
        'Stkcd',
        'Accper',
        'Annodt',
    ]]
    .rename(columns={'Annodt': 'report_date'})
)
account_date['Accper'] = pd.to_datetime(account_date['Accper'])
account_date['report_date'] = pd.to_datetime(account_date['report_date'])
account_date['Q'] = account_date['Accper'].dt.to_period('Q').dt.to_timestamp('Q')

profit = (
    pd.read_excel('Input/Profit/FS_Comins2023.xlsx', skiprows=2)
    [[
        'Stkcd',
        'Accper',
        'Typrep',
        'B002000000',
    ]]
    .dropna()
)
profit = profit.query("Typrep == 'A'").drop('Typrep', axis=1)
profit['Accper'] = pd.to_datetime(profit['Accper'])

nonrec = (
    pd.read_excel('Input/Non_recurring_gains_and_losses/FI_T22023.xlsx', skiprows=2)
    [[
        'Stkcd',
        'Accper',
        'F020101',
    ]]
)
nonrec['Accper'] = pd.to_datetime(nonrec['Accper'])
profit = profit.merge(nonrec, on=['Stkcd', 'Accper'], how='left')
profit = profit.rename(columns={'Accper': 'Q'})
profit = profit[profit['Q'].dt.month > 1]
profit['Q'] = profit['Q'].dt.to_period('Q').dt.to_timestamp('Q')
profit = profit.sort_values(['Stkcd', 'Q'])

profit['earnings'] = np.where(
    profit['F020101'].isna(),
    pd.to_numeric(profit['B002000000'], errors='coerce'),
    pd.to_numeric(profit['B002000000'], errors='coerce') - pd.to_numeric(profit['F020101'], errors='coerce'),
)
profit = profit.merge(account_date[['Stkcd', 'Q', 'report_date']], on=['Stkcd', 'Q'], how='left')
profit['report_date'] = profit['report_date'].dt.to_period('M').dt.to_timestamp('M')

profit['Q_earnings'] = np.nan
for code, group in profit.groupby('Stkcd'):
    group = group.sort_values('Q')
    earnings = []
    for idx, row in group.iterrows():
        if row['Q'].year < 2002:
            earnings.append(np.nan)
        elif row['Q'].quarter == 1:
            earnings.append(row['earnings'])
        else:
            prev = group[group['Q'] == row['Q'] - QuarterEnd(startingMonth=12)]
            if not prev.empty:
                earnings.append(row['earnings'] - prev['earnings'].iloc[0])
            else:
                earnings.append(np.nan)
    profit.loc[group.index, 'Q_earnings'] = earnings


valid_ret = ret_mon[(ret_mon['month'] >= '1991-01-01') & (~ret_mon['Return'].isna())]


def match_profit(key):
    stk, m = key
    test = profit[(profit['report_date'] <= m) & (profit['Stkcd'] == stk)]
    if test.empty:
        return pd.Series({'matchearnings': np.nan, 'recent_earnings': np.nan})
    t0 = test['Q'].max()
    t1 = t0 - QuarterEnd(0)
    t2 = t0 - 2 * QuarterEnd(0)
    t3 = t0 - 3 * QuarterEnd(0)
    recent = test.loc[test['Q'] == t0, 'earnings'].values[0]
    if m >= pd.Timestamp('2003-05-01'):
        needed = test[test['Q'].isin([t1, t2, t3, t0])]
        if len(needed) == 4 and needed['Q_earnings'].notna().all():
            matche = (
                needed.loc[needed['Q'] == t1, 'Q_earnings'].values[0]
                + needed.loc[needed['Q'] == t2, 'Q_earnings'].values[0]
                + needed.loc[needed['Q'] == t3, 'Q_earnings'].values[0]
                + needed.loc[needed['Q'] == t0, 'Q_earnings'].values[0]
            )
        else:
            matche = np.nan
    else:
        q = t0.quarter
        if q == 4:
            matche = test.loc[test['Q'] == t0, 'earnings'].values[0]
        elif q == 3 and not test[test['Q'] == t3].empty:
            matche = test.loc[test['Q'] == t3, 'earnings'].values[0]
        elif q == 2 and not test[test['Q'] == t2].empty:
            matche = test.loc[test['Q'] == t2, 'earnings'].values[0]
        elif q == 1 and not test[test['Q'] == t1].empty:
            matche = test.loc[test['Q'] == t1, 'earnings'].values[0]
        else:
            matche = np.nan
    return pd.Series({'matchearnings': matche, 'recent_earnings': recent})


ep_df = (
    valid_ret.set_index(['Stkcd', 'month'])
    .groupby(level=[0, 1])
    .apply(lambda _: match_profit(_.name))
    .reset_index()
)
ep_df = ep_df.merge(ret_mon[['Stkcd', 'month', 'floatingvalue', 'totalvalue']], on=['Stkcd', 'month'], how='right')
# Placeholder: load daily return data with columns Stkcd, month, ratio, Clsprc, yuemo
ret_day = pd.DataFrame()
ep_df = ep_df.merge(
    ret_day[getattr(ret_day, 'yuemo', pd.Series()) == 1][['Stkcd', 'month', 'ratio', 'Clsprc']],
    on=['Stkcd', 'month'],
    how='left',
)
ep_df['ep'] = ep_df['matchearnings'] * pd.to_numeric(ep_df.get('ratio'), errors='coerce') / ep_df['floatingvalue']
ep_df['ep_recent'] = ep_df['recent_earnings'] / ep_df['totalvalue']

# ----------------------------
# Factor construction
# ----------------------------
ret_mon = ret_mon.merge(
    ret_mon.sort_values(['Stkcd', 'month'])
    .groupby('Stkcd')['Freq']
    .apply(lambda s: s.rolling(window=12, min_periods=1).sum())
    .reset_index(name='past12freq'),
    on=['Stkcd', 'month'],
    how='left',
)
ret_mon['sizebreak'] = ret_mon.groupby('month')['totalvalue'].transform(lambda x: x.quantile(0.3))

Data = ret_mon[
    (ret_mon['Rank'] > 6)
    & (ret_mon['past12freq'] >= 120)
    & (ret_mon['Freq'] >= 10)
    & (ret_mon['totalvalue'] >= ret_mon['sizebreak'])
]
Data = Data.merge(ep_df[['Stkcd', 'month', 'ep', 'ep_recent']], on=['Stkcd', 'month'])

MKT = (
    Data[Data['next_ret1'].notna() & Data['totalvalue'].notna()]
    .groupby('month')
    .apply(lambda x: np.average(x['next_ret1'], weights=x['totalvalue']))
    .reset_index(name='mkt')
)
MKT['month'] = MKT['month'] + MonthEnd(1)

Data['size.notion'] = Data.groupby('month')['sizet'].transform(
    lambda x: pd.cut(x, bins=np.quantile(x.dropna(), [0, 0.5, 1]), labels=['S', 'B'], include_lowest=True)
)
Data['ep.notion'] = Data.groupby('month')['ep'].transform(
    lambda x: pd.cut(x, bins=np.quantile(x.dropna(), [0, 0.3, 0.7, 1]), labels=['G', 'M', 'V'], include_lowest=True)
)

def weighted_mean(df: pd.DataFrame) -> float:
    return np.average(df['next_ret1'], weights=df['totalvalue'])


Factor_sv = Data[(Data['size.notion'] == 'S') & (Data['ep.notion'] == 'V')].groupby('month').apply(weighted_mean).reset_index(name='SV')
Factor_sm = Data[(Data['size.notion'] == 'S') & (Data['ep.notion'] == 'M')].groupby('month').apply(weighted_mean).reset_index(name='SM')
Factor_sg = Data[(Data['size.notion'] == 'S') & (Data['ep.notion'] == 'G')].groupby('month').apply(weighted_mean).reset_index(name='SG')
Factor_bv = Data[(Data['size.notion'] == 'B') & (Data['ep.notion'] == 'V')].groupby('month').apply(weighted_mean).reset_index(name='BV')
Factor_bm = Data[(Data['size.notion'] == 'B') & (Data['ep.notion'] == 'M')].groupby('month').apply(weighted_mean).reset_index(name='BM')
Factor_bg = Data[(Data['size.notion'] == 'B') & (Data['ep.notion'] == 'G')].groupby('month').apply(weighted_mean).reset_index(name='BG')

Factor = (
    Factor_sv.merge(Factor_sm, on='month')
    .merge(Factor_sg, on='month')
    .merge(Factor_bv, on='month')
    .merge(Factor_bm, on='month')
    .merge(Factor_bg, on='month')
)
Factor['smb'] = (Factor['SV'] + Factor['SM'] + Factor['SG']) / 3 - (
    Factor['BV'] + Factor['BM'] + Factor['BG']
) / 3
Factor['vmg'] = (Factor['BV'] + Factor['SV']) / 2 - (
    Factor['SG'] + Factor['BG']
) / 2
Factor = Factor[['month', 'smb', 'vmg']]
Factor['month'] = Factor['month'] + MonthEnd(1)
Factor = Factor.merge(MKT, on='month', how='left')

pre_2017 = Factor[Factor['month'] <= '2016-12-31']
print(len(pre_2017))
print(
    pre_2017['smb'].mean() * 100,
    pre_2017['smb'].std() * 100,
    stats.ttest_1samp(pre_2017['smb'] * 100, 0),
)
print(len(pre_2017))
print(
    pre_2017['vmg'].mean() * 100,
    pre_2017['vmg'].std() * 100,
    stats.ttest_1samp(pre_2017['vmg'] * 100, 0),
)
print(Factor[['mkt', 'smb', 'vmg']].corr())
