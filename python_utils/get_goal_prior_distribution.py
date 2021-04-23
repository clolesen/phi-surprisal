#import libraries
import pandas as pd
import pickle

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4


def get_goal_prior_distribution(task=4):

    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == 120) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}_info.csv'.format(task))

    #Subset data for only perfect agents on the last generation
    timestep_data = timestep_data.loc[(timestep_data['run'].isin(perfect_runs)) & (timestep_data['agent'] == 49)]

    #Create sensory state and context state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')
    timestep_data['context'] = timestep_data['timestep'].astype('str') + timestep_data['task_type'] + timestep_data['block_movement'].astype('str')

    #Count how many times each state appears in each context
    counts = timestep_data.groupby('context')['sensory_state'].value_counts() + 1

    #And make it into a dicitonary
    distribution_dict = counts.to_dict()

    #Save the goal prior distribution to a file
    with open('processed_data/goal_prior_distribution.pkl', 'wb') as handle:
        pickle.dump(distribution_dict, handle, protocol=pickle.HIGHEST_PROTOCOL)

    