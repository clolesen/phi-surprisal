import matplotlib.pyplot as plt
import matplotlib.colors as colors
import numpy as np
import random
from ipywidgets import interact, interact_manual
import ipywidgets as widgets
from IPython import display
import time
from pathlib import Path
import pandas as pd
import os
import copy
import networkx as nx
import pickle
import sys
from scipy import stats


os.chdir("..")
path = '/Users/christoffer/Documents/CogSci/MA/DataSci/actual_agency'
print(path)
print(sys.path.append(path))

# and import packages
from pyanimats import *
from pyTPM import *
import actual_agency as agency
import pyphi
from pyphi import actual, config, Direction

# Change directory back to Example directory with mabe
os.chdir('/Users/christoffer/Documents/CogSci/MA/DataSci/comparison/Larissa_build/mabe/')




# defining some parameters to use 
generations = 60000
n_agents = int(generations/500.+1) #This assumes you read out an agent every 500 generations in the LOD
n_runs = 50 #should be at least 30 - 50 in the end
n_trials = 128



# FITNESS STUFF


with open('Experiments/task5_2runs/LOD_data.pkl','rb') as f:
    LOD_data = pickle.load(f)




for n in range(n_runs):
    LOD_data[n]['fitness'] = (LOD_data[n]['correct_AVE']
                               /(LOD_data[n]['correct_AVE']+LOD_data[n]['incorrect_AVE']))



                  
heading = 'fitness'
fit_list = []
run_list = []
agent_list = []
for n in range(n_runs):
    for x in range(len(LOD_data[n][heading][:])):
        fit_list.append(LOD_data[n][heading][x])
        agent_list.append(x)
        run_list.append(n+30)

df = pd.DataFrame({
    'run': run_list,
    'agent': agent_list,
    'fitness': fit_list
    })
df.to_csv('ResultsDataTask5/fitness_2_task5.csv', sep=',')



# ACTIVITY STUFF (uses actual agency function)

with open('Experiments/Larissa_animat/activity.pkl','rb') as f:
    activity = pickle.load(f)


# reformat the activity to a single list for each trial
brain_activity = []
for r in range(n_runs):
    brain_activity.append(agency.getBrainActivity(activity[r], n_agents, n_trials, world_height=34))



#save brain activity
with open('Experiments/Larissa_animat/brainActivity_Larissa_animat.pickle', 'wb') as f:
    pickle.dump(brain_activity, f)