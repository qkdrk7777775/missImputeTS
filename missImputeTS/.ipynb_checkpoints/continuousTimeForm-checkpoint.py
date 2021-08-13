def continuousTimeForm(df, time_var_name,freq):
    df=df.set_index([time_var_name])
    dt_range=pd.date_range(min(df.index),max(df.index),freq=freq)
    na_date=set(dt_range)-set(df.index)
    df_=pd.concat([df,pd.DataFrame(columns=df.columns,index=na_date)],axis=0)
    df_=df_.sort_index()
    df_=df_.reset_index().rename(columns={"index": "times"})
    return df_