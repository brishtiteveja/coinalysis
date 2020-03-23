import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
%matplotlib inline
import seaborn as sns
sns.set(style='darkgrid', context='talk', palette='Dark2')

my_year_month_fmt = mdates.DateFormatter('%m/%y')

data = pd.read_csv('./btc_usd_gdax_price.csv')
data = data.set_index(data.columns[0])
data.head(10)# -*- coding: utf-8 -*-


# Calculating the short-window simple moving average
short_rolling = data.rolling(window=20).mean()
short_rolling.head(20)

# Calculating the long-window simple moving average
long_rolling = data.rolling(window=100).mean()
long_rolling.tail()


start_date = data.index[0]
start_date
n = len(data.index)
end_date = data.index[n-1]

fig, ax = plt.subplots(figsize=(16,9))

#ax.plot(data.loc[start_date:end_date, :].index, data.loc[start_date:end_date, 'close'], label='Price')
ax.plot(data.loc[start_date:end_date, 'close'], label='Price')
ax.plot(long_rolling.loc[start_date:end_date, 'close'], label = '100-period SMA')
ax.plot(short_rolling.loc[start_date:end_date, 'close'], label = '20-period SMA')
ax.legend(loc='best')
ax.set_ylabel('Price in $')

ax.set_xticks(data.index)
ax.xaxis_date.set_major_formatter(mdates.DateFormatter("%Y-%m-%d %h:%m:%s"))
ax.xaxis_date.set_minor_formatter(mdates.DateFormatter("%Y-%m-%d %h:%m:%s"))
_=plt.xticks(rotation=90)    
plt.show()

# Using Pandas to calculate a 20-days span EMA. adjust=False specifies that we are interested in the recursive calculation mode.
ema_short = data.ewm(span=20, adjust=False).mean()

fig, ax = plt.subplots(figsize=(15,9))

ax.plot(data.loc[start_date:end_date, 'close'], label='Price')
ax.plot(ema_short.loc[start_date:end_date, 'close'], label = 'Span 20-period EMA')
ax.plot(short_rolling.loc[start_date:end_date, 'close'], label = '20-days SMA')

ax.legend(loc='best')
ax.set_ylabel('Price in $')
#ax.xaxis.set_major_formatter(my_year_month_fmt)
plt.show()


ema_long = data.ewm(span=100, adjust=False).mean()

fig, ax = plt.subplots(figsize=(15,9))

ax.plot(data.loc[start_date:end_date, 'close'], label='Price')
ax.plot(ema_long.loc[start_date:end_date, 'close'], label = 'Span 100-period EMA')
ax.plot(long_rolling.loc[start_date:end_date, 'close'], label = '100-days SMA')

ax.legend(loc='best')
ax.set_ylabel('Price in $')
#ax.xaxis.set_major_formatter(my_year_month_fmt)
plt.show()


trading_positions_raw = data - ema_short
trading_positions_raw.head()

#trading_positions = trading_positions_raw.apply(np.sign) * 1/3
#trading_positions.tail()

trading_positions = trading_positions_raw

# Lagging our trading signals by one day.
trading_positions_final = trading_positions.shift(1)


fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(16,9))

ax1.plot(data.loc[start_date:end_date, :].index, data.loc[start_date:end_date, 'close'], label='Price')
ax1.plot(ema_short.loc[start_date:end_date, :].index, ema_short.loc[start_date:end_date, 'close'], label = 'Span 20-days EMA')

ax1.set_ylabel('$')
ax1.legend(loc='best')
#ax1.xaxis.set_major_formatter(my_year_month_fmt)

ax2.plot(trading_positions_final.loc[start_date:end_date, :].index, trading_positions_final.loc[start_date:end_date, 'close'], 
        label='Trading position')

ax2.set_ylabel('Trading position')
#ax2.xaxis.set_major_formatter(my_year_month_fmt)
plt.show()

# Log returns - First the logarithm of the prices is taken and the the difference of consecutive (log) observations
asset_log_returns = np.log(data).diff()
asset_log_returns.head()

