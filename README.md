# missImputeTS

 The missImputeTS package specializes on (multivariate) time series imputation. This algorithm is a combination of missForest and imputeTS algorithms. The missforest algorithm sometimes had a problem of presenting a bouncing value, and imputeTS had the disadvantage of being vulnerable to long-term missing and a univariate algorithm.

### Installation
 The missImputeTS package For installation execute in R:
```
library(devtools)
install_github('qkdrk7777775/missImputeTS')
```

The missImputeTS package For installation execute in python:
```
pip install missImputeTS
```
###

###python example
package load
```python
import numpy as np
import os
import pandas as pd
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
import missImputeTS
```

R package install & load
```python
# ro.r('install.packages(c("imputeTS"))')
imputeTS = importr('imputeTS')
```

Example data load
```python
data_path = os.path.join(os.path.dirname(missImputeTS.__file__),'data')
df=pd.read_csv(rf'{data_path}/EuStockMarkets.csv')
df['times']=pd.to_datetime(df.times).round('1d')
df=missImputeTS.continuousTimeForm(df=df,time_var_name='times',freq='1d')
df.iloc[:,1:]=df.drop('times',axis=1).interpolate()
```

Make Missing in Example Data
```python
for i in range(df.shape[1]):
    np.random.seed(i)
    df.iloc[np.random.choice(df.shape[0], int(df.shape[0]*.1)),i]=np.nan
xmis=df.copy()
```

Please set the index to time
```python
xmis=xmis.set_index('times')
```

Missing Data Interfolation
```python
imputer = missImputeTS.missTS()
imputer.fit_transform(xmis)
```

### Version
**0.0.0.1**

### License
GPL-3
