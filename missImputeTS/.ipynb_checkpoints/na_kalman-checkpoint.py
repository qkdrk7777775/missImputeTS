def na_kalman(ts):
    imputeTS = importr('imputeTS')
    return imputeTS.na_kalman(ro.FloatVector(ts))