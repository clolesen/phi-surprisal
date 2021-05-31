import sys
import os

# Append actual agency path to import names
sys.path.insert(1, os.getcwd() + '/python_utils/actual_agency')
sys.path.insert(1, os.getcwd() + '/actual_agency')

# Load libraries
import actual_agency as agency
from pyanimats import *
from pyTPM import *
import pyphi
import csv 

pyphi.config.VALIDATE_SUBSYSTEM_STATES = False
pyphi.config.PARTITION_TYPE = 'BI'
pyphi.config.CUT_ONE_APPROXIMATION = True 
pyphi.config.PROGRESS_BARS = False

def calculate_phi(task):

    generations = 60000
    n_agents = int(generations/500.+1)
    n_runs = 50
    n_trials = 128
    n_timeSteps = 35

    with open('raw_data/genome_task{}.pkl'.format(task), 'rb') as f:
        all_genomes = pickle.load(f)

    out_file = open('processed_data/timestep_data_task{}_phi.csv'.format(task), "w", newline='')
    out_writer = csv.writer(out_file)

    last_agent_id = "nobody"
    with open("processed_data/timestep_data_task{}.csv".format(task), "r") as data:
        data_reader = csv.reader(data)

        # Write the column names
        colnames = next(data_reader) + ["Phi", "n_concepts", "concept_phis"]
        out_writer.writerow(colnames)

        for row in data_reader:
            run = int(row[0])
            agent = int(row[1])
            trial = int(row[2])
            timestep = int(row[3])
            state = tuple(np.array(row[4:12]).astype(int))

            # Check if it is the same agent as before. For new agents prepare for analysis
            agent_id = "r"+str(run)+"a"+str(agent)
            if agent_id != last_agent_id:
                state_dict = {}
                genome = agency.get_genome(all_genomes, run, agent)

                print(agent_id)

                #get TPM and CM from genome
                TPM, TPM_gates, cm = genome2TPM(genome, n_nodes=8, n_sensors=2, n_motors=2, 
                    gate_type='deterministic',states_convention='loli',
                    remove_sensor_motor_effects=True)
                
                # Get network from TPM and CM
                network = pyphi.network.Network(np.array(TPM), cm=np.array(cm), 
                    node_labels=('S1','S2','M1','M2','H1','H2','H3','H4'), purview_cache=None)
            # save this agent for next iteration     
            last_agent_id = agent_id

            if state not in state_dict.keys():

                #Find major complex, i.e. the complex with the highest Phi. This complex has potential for consciousness
                major_complex = pyphi.compute.network.major_complex(network,state)

                # Number of concepts
                n_concepts = len(major_complex.ces) #Get value
                
                # phi of concepts
                concept_phis = major_complex.ces.phis #Calculate values
                concept_phis = '-'.join([str(elem) for elem in concept_phis]) #Make values into one string
                
                # Phi of complex
                Phi = major_complex.phi #Calculate value
                
                # Create and entry in the state dictionary
                state_dict[state] = [n_concepts, concept_phis, Phi]
                
            #If the state is in the state dictionary, get values by indexing
            else:
                values = state_dict.get(state)
                n_concepts = values[0]
                concept_phis = values[1]
                Phi = values[2]
            
            #make list with info
            line = row + [Phi, n_concepts, concept_phis]
            out_writer.writerow(line)



