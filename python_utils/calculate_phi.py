import sys
import os
import pyphi
import csv 
import pickle
import numpy as np
import pandas as pd
from tqdm import tqdm

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4

# Append actual agency path to import names
sys.path.insert(1, os.getcwd() + '/python_utils/actual_agency')

# Load libraries
import actual_agency as agency
from pyanimats import *
from pyTPM import *

#pyphi settings
pyphi.config.VALIDATE_SUBSYSTEM_STATES = False
pyphi.config.PARTITION_TYPE = 'BI'
pyphi.config.CUT_ONE_APPROXIMATION = True 
pyphi.config.PROGRESS_BARS = False

pyphi.config.NUMBER_OF_CORES = -2
pyphi.config.PARALLEL_CUT_EVALUATION = False


## -- Function for calculating phi --##
def calculate_phi_single_row(state, genome):

    #Create a TPM
    TPM, TPM_gates, cm = genome2TPM(genome, n_nodes=8, n_sensors=2, n_motors=2, 
    gate_type='deterministic', states_convention='loli',
    remove_sensor_motor_effects=True)

    #Get network from the TPM and CM
    network = pyphi.network.Network(np.array(TPM), cm=np.array(cm), 
    node_labels=('S1','S2','M1','M2','H1','H2','H3','H4'), purview_cache=None)

    #Find major complex, i.e. the complex with the highest Phi. This complex has potential for consciousness
    major_complex = pyphi.compute.network.major_complex(network, state)

    #code for getting number of cocnepts and their phi
    #n_concepts = len(major_complex.ces) #Get value
    #concept_phis = major_complex.ces.phis #Calculate values
    #concept_phis = '-'.join([str(elem) for elem in concept_phis]) #Make values into one string

    #Return the Phi of the system
    return major_complex.phi


## -- Memoization --##
#Make memoize function
def memoize(f):
    #Make empty dictionary
    memo = {}
    #Create helper function
    def helper(x, y):
        #Which checks if the value has already been calculated
        if (x,tuple(y)) not in memo:
            #If not, it calculates it and stores it            
            memo[(x,tuple(y))] = f(x,y)
        #And then returns the stored answer
        return memo[x,tuple(y)]
    return helper

#This puts the memoize function around the phi calculations
calculate_phi_single_row = memoize(calculate_phi_single_row)





## -- Large function --##
def calculate_phi(task):

    #Get in the genomes for the animats
    with open('raw_data/genome_task{}.pkl'.format(task), 'rb') as f:
        all_genomes = pickle.load(f)

    #Read in the dataset
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task))

    #Add a column with the internal states
    timestep_data['state'] = list(zip(timestep_data.S1, timestep_data.S2, timestep_data.M1, timestep_data.M2,
                                               timestep_data.H1, timestep_data.H2, timestep_data.H3, timestep_data.H4))

    #Get the genome at each timestep
    timestep_data['genome'] = list(map(get_genomes_custom, timestep_data['run'], tqdm(timestep_data['agent'])))

    #Calculate phi at each timestep
    timestep_data['phi'] = list(map(calculate_phi_single_row, timestep_data['state'], tqdm(timestep_data['genome']))) 

    #Remove now unnecessary states
    timestep_data = timestep_data.drop(['state', 'genome'], axis=1)

    #Write the data to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}_phi.csv'.format(task), index=False)



#Make wrapper function without non-iterable input for mapping later
def get_genomes_custom(run, agent):
    return agency.get_genome(all_genomes, run, agent)










from multiprocessing import Pool

pool = Pool(2)







timestep_data_sub['genome'] = list(tqdm(pool.imap(get_genomes_custom, zip(timestep_data_sub['run'], timestep_data_sub['agent']))))







#Data subset
timestep_data_sub = timestep_data[(timestep_data['run'] == 2) & (timestep_data['agent'].isin([100,101]))]
#Get the genome at each timestep
timestep_data_sub['genome'] = list(map(get_genomes_custom, timestep_data_sub['run'], tqdm(timestep_data_sub['agent'])))
#Calculate phi at each timestep
timestep_data_sub['phi'] = list(map(calculate_phi_single_row, timestep_data_sub['state'], tqdm(timestep_data_sub['genome'])))
