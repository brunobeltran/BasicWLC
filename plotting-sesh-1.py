# coding: utf-8
cd ~/developer/BasicWLC/
import numpy as np
%matpl
%matplotlib
import pandas as pd
import seaborn as sns
from wlcsim import data
coltimes = pd.read_csv('par-run-dir/small-chains-verification/old_coltimes.csv')
g = sns.factorplot(x='linear_distance', y='coltime', row='L', col='N', hue='DT', data=coltimes)
coltimes[np.logical_and(coltimes['N'] == 41, coltimes['L'] == 10.0)].head()
coltimes[coltimes['L']-1 == coltimes['N']].head()
coltimes[np.abs(coltimes['L']-1 - coltimes['N']) < 0.01].head()
coltimes['L'].unique()
coltimes['N'].unique()
coltimes[np.abs(coltimes['N']-1 - coltimes['L']) < 0.01].head()
good_coltimes = coltimes[np.abs(coltimes['N']-1 - coltimes['L']) < 0.01]
len(good_coltimes)
g = sns.factorplot(x='linear_distance', y='coltime', col='DT', hue='L', data=good_coltimes)
best_coltimes = good_coltimes[good_coltimes['DT'] == 0.01]
len(best_coltimes)
best_coltimes = good_coltimes[good_coltimes['DT'] == 0.1]
len(best_coltimes)
g = sns.factorplot(x='linear_distance', y='coltime', hue='L', data=best_coltimes)
?%save
%save plotting-sesh-1.py
%save -r plotting-sesh-1.py
%save -r plotting-sesh-1.py 1-100
