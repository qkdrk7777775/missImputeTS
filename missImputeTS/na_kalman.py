"""Missing interpolation using ImputeTS in R"""
# Author: Chang Je Cho
# License: GNU General Public License v3 (GPLv3)
from rpy2.robjects.packages import importr

def na_kalman(ts):
    imputeTS = importr('imputeTS')
    return imputeTS.na_kalman(ro.FloatVector(ts))