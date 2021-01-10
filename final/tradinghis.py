# %%
import os
from max.client import Client
from datetime import datetime
import pandas as pd

# %%
client = Client( os.getenv('MAX_API_KEY'), os.getenv('MAX_API_SECRET'))
# %%
m =  pd.json_normalize(client.get_public_all_markets())
m = m[m.quote_unit == "usdt"]
# %%
trhis = pd.DataFrame()
for i in m.id:
    df = pd.json_normalize(client.get_private_trade_history(i))
    trhis = trhis.append(df)
# %%
print(trhis.market.value_counts())
# %%
trhis = trhis.sort_values(by='created_at')
for _, i in trhis.iterrows():
    print(datetime.fromtimestamp(i.created_at).strftime("%Y-%m-%d, %H:%M:%S"), end = " ")

    if(i.side == "bid"): 
        print("買", i.market, "價格:",i.price,"量:",i.volume,"USDT:",round(float(i.funds),2))
    elif(i.side == "ask"): 
        print("賣",i.market, "價格:",i.price,"量:",i.volume,"USDT:",round(float(i.funds),2))

