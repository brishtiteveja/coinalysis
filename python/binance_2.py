import time

from binance.client import Client as BinanceClient
from binance.exceptions import BinanceAPIException, BinanceWithdrawException

API_KEY=''
API_SECRET=''

bclient = BinanceClient(API_KEY, API_SECRET)

##binance tickers
tickers = {}
coins = []
try:
    ticker_collections = bclient.get_all_tickers()
except BinanceAPIException as e:
    print(e)
except BinanceWithdrawException as e:
    print(e)
else:
    for item in ticker_collections:
        tickers[item['symbol']] = item['price']
        
        
        try:
            symbol_info = bclient.get_symbol_info(item['symbol'])
            print(symbol_info)
            
            coin_name = symbol_info['baseAsset']   
            if coin_name not in coins:
                 coins.append(coin_name)
            coin_name = symbol_info['quoteAsset']
            if coin_name not in coins:
                 coins.append(coin_name)
        except BinanceAPIException as ee:
            print(ee)


trades = bclient.get_recent_trades(symbol='ETHBTC')
