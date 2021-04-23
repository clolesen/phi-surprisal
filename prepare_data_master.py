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

# settings
task = sys.argv[1]
task = 4

# Make a csv with run, agent, trial, timestep and node states
extract_from_MABE(task=task)

# Add task information to the csv
get_task_info(task=task)

# Caluclate Phi

# Calculate goal prior distribution

# Calculate surprisal
