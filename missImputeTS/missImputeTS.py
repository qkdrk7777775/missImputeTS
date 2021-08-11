import missImputeTS
import os
import pandas as pd
data_path = os.path.join(os.path.dirname(missImputeTS.__file__),'data')
df=pd.read_csv(rf'{data_path}/EuStockMarkets.csv')

