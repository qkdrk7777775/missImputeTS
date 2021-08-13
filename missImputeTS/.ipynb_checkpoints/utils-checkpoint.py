"""Missing interpolation using ImputeTS in R"""
# Author: Chang Je Cho
# License: GNU General Public License v3 (GPLv3)

from rpy2.robjects.packages import importr
import rpy2.robjects as ro
import pandas as pd
import warnings

def continuousTimeForm(df, time_var_name,freq):
    df_=df.copy()
    df_=df_.set_index([time_var_name])
    dt_range=pd.date_range(min(df_.index),max(df_.index),freq=freq)
    na_date=set(dt_range)-set(df_.index)
    df_=pd.concat([df_,pd.DataFrame(columns=df_.columns,index=na_date)],axis=0)
    df_=df_.sort_index()
    df_=df_.reset_index().rename(columns={"index": "times"})
    return df_

def na_kalman(ts):
    imputeTS = importr('imputeTS')
    return imputeTS.na_kalman(ro.FloatVector(ts))