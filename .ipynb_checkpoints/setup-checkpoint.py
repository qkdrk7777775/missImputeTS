from setuptools import setup

setup(
   name='missImputeTS',
   version='0.0.0.3',
   description="The function 'missImputeTS' in this package is used to impute timeseries missing values particularly in the case of mixed-type data.It uses a random forest trained on the observed values of a data matrix to predict the missing values. It can be used to impute continuous and/or categorical data including complex interactions and non-linear relations. It can be run in parallel to save computation time.",
   url='https://github.com/sean-mcclure/datapeek_py',
   author='chang je Cho',
   author_email='qkdrk7777775@gmail.com',
   license='MIT',
   packages=['missImputeTS'],
   install_requires=['rPy2','pandas','numpy','scipy','sklearn'],
   include_package_data=True,
   keywords = ['imputation','interpolation','missforest','imputets','missImputeTS'],
   zip_safe=False)


