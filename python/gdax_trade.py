import pandas as pd
import gdax

import sys
import time

public_client = gdax.PublicClient()

btc_usd_price_fn = 'btc_usd_gdax_price.csv'

granularity_array = [60, 300, 900, 3600, 21600, 86400]
granularity_type = ['1m', '5m', '15m', '1h', '24h', '1y']

granularity = 3600

if len(sys.argv) > 1:
    idx = granularity_type.index(sys.argv[1])
    granularity = granularity_array[idx]

while True:
    btc_price = public_client.get_product_historic_rates('BTC-USD', granularity=granularity)
    btc_price_df = pd.DataFrame(btc_price)

    btc_price_df.columns = ['time', 'low', 'high', 'open', 'close', 'volume']
    btc_price_df['time'] = pd.to_datetime(btc_price_df['time'], unit='s')
    btc_price_df.to_csv(btc_usd_price_fn, index=False)
    time.sleep(5)
