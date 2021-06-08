import sys
import os
import pyphi
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

pyphi.config.PARALLEL_CUT_EVALUATION = False


#Memoization decorator for the get_genome function
def memoize_get_genome(f):
    #Make empty dictionary
    memo = {}
    #Create helper function
    def helper(run,agent):
        #Which checks if the value has already been calculated
        if (run,agent) not in memo:
            #If not, it calculates it and stores it            
            memo[(run,agent)] = f(run,agent)
        #And then returns the stored answer
        return memo[run,agent]
    return helper

#Memoization decorator for the calculate_phi function
def memoize_calculate_phi(f):
    #Make empty dictionary
    memo = {}
    #Create helper function
    def helper(state,run,agent):
        #Which checks if the value has already been calculated
        if (state,run,agent) not in memo:
            #If not, it calculates it and stores it            
            memo[(state,run,agent)] = f(state,run,agent)
        #And then returns the stored answer
        return memo[state,run,agent]
    return helper

## -- Large function --##
def calculate_phi(task):

    #Read in the timestep dataset
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task))

    #Add a column with the state of the animat's Markov Brain
    timestep_data['state'] = list(zip(timestep_data.S1, timestep_data.S2, timestep_data.M1, timestep_data.M2,
                                               timestep_data.H1, timestep_data.H2, timestep_data.H3, timestep_data.H4))


    #Get in the genomes for the animats
    with open('raw_data/genome_task{}.pkl'.format(task), 'rb') as f:
        all_genomes = pickle.load(f)
    
    #Make wrapper function without non-iterable input
    def get_genomes_custom(run, agent):
        return agency.get_genome(all_genomes, run, agent)

    #Make the function memoizing to prevent repeated calculations
    get_genomes_custom = memoize_get_genome(get_genomes_custom)

    #Function for calculating phi
    def calculate_phi_single_row(state, run, agent):

        #Get the genome for the current run and agent
        genome = get_genomes_custom(run, agent)
        
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
    
    #Wrap the function in the memoize function 
    calculate_phi_single_row = memoize_calculate_phi(calculate_phi_single_row)


    #Calculate phi at each timestep (use tqdm for a progress bar)
    timestep_data['Phi'] = list(map(calculate_phi_single_row, timestep_data['state'], timestep_data['run'],
                                tqdm(timestep_data['agent'], mininterval = 3))) 

    #Remove now unnecessary column
    timestep_data = timestep_data.drop(['state'], axis=1)

    #Write the data to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}_phi.csv'.format(task), index=False)