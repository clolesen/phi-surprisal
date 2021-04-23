#import libraries
import pandas as pd
import numpy as np
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

    #Create sensory state and context state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')

    #Subset data for only perfect agents on the last generation
    timestep_data_perfect = timestep_data.loc[(timestep_data['run'].isin(perfect_runs)) & (timestep_data['agent'] == 49)]
    
    #Count how many times each state appeared in each context
    counts = timestep_data_perfect.groupby(['timestep', 'task_type', 'block_movement', 'sensory_state'])['sensory_state'].agg(surprisal='count')

    #Add 1 to all counts to include unobserved states
    counts = counts.unstack(fill_value=0).stack() + 1

    #Transform counts into probabilities
    goal_prior_surprisal = counts.groupby(['timestep', 'task_type', 'block_movement']).apply(lambda x: - np.log(x / float(x.sum())) ).reset_index()

    #Save the distribution for later plotting 
    goal_prior_surprisal.to_pickle('goal_prior_distribution.pkl')

    #Merge into the original dataset
    timestep_data = timestep_data.merge(goal_prior_surprisal, how = "left")

    #Save data to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}_surprise.csv'.format(task), index=False)