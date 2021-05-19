# Import general libraries
import os
import sys

# Set the working directory to the folder containing the file
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

# Append utility folder path to import paths
sys.path.insert(1, os.getcwd() + '/python_utils')

# Import own functions
from extract_from_MABE import extract_from_MABE
from get_task_info import get_task_info
from calculate_surprisal import calculate_surprisal
from calculate_phi import calculate_phi

# settings
task = sys.argv[1]
task = 4

# Make a csv with run, agent, trial, timestep and node states
extract_from_MABE(task=task)

# Add task information to the csv
get_task_info(task=task)

# Calculate goal prior surprisal
calculate_surprisal(task=task)

# Caluclate Phi
calculate_phi(task=task)





import pickle        
import base64
import csv

your_pickle_obj = pickle.loads(open('processed_data/goal_prior_distribution_task1.pkl', 'rb').read())
with open('processed_data/goal_prior_distribution_task1.csv', 'a', encoding='utf8') as csv_file:
    wr = csv.writer(csv_file, delimiter='|')
    pickle_bytes = pickle.dumps(your_pickle_obj)            # unsafe to write
    b64_bytes = base64.b64encode(pickle_bytes)  # safe to write but still bytes
    b64_str = b64_bytes.decode('utf8')          # safe and in utf8
    wr.writerow(['col1', 'col2', b64_str])


import pickle as pkl
import pandas as pd
with open("processed_data/goal_prior_distribution_task1.pkl", "rb") as f:
    object = pkl.load(f)
    
df = pd.DataFrame(object)
df.to_csv(r'processed_data/goal_prior_distribution_task1.csv')