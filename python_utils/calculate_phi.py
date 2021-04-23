import actual_agency as agency
from pyanimats import *
from pyTPM import *
import pyphi
import sys
import os

# Append actual agency path to import names
sys.path.insert(1, os.getcwd() + '/actual_agency_files')

# Load libraries


# import actual agency


with open('MABE_output/genome.pkl', 'rb') as f:
    all_genomes = pickle.load(f)
