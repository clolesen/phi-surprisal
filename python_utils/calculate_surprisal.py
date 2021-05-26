#import libraries
import pandas as pd
import numpy as np
from scipy.stats import entropy as kl_divergence
from tqdm import tqdm

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4

#Function for finding the minimum kl-divergence
def find_lowest_kl(timestep_data_group, goal_prior_surprisal):

    #Count how many times each state appeared in each context
    observed_counts = timestep_data_group.groupby(['timestep', 'task_type', 'block_movement', 'sensory_state'])['sensory_state'].agg(probability='count')

    #Add 1 to all counts to include unobserved states
    observed_counts = observed_counts.unstack(fill_value=0).stack() + 1

    #Transform counts into probabilities
    observed_probabilities = observed_counts.groupby(['timestep', 'task_type', 'block_movement']).apply(lambda x: x / float(x.sum()) ).reset_index()

    #Group goal prior by run
    goal_prior_grouped = goal_prior_surprisal.groupby('run')['surprisal']
    
    #Get kl divergences to different goal prior
    kl_divergences = goal_prior_grouped.agg(lambda x: kl_divergence(observed_probabilities['probability'], np.exp(-x)))

    #Return the goal prior number with the lowest divergence
    return kl_divergences.idxmin()

#Function for calculating the surprisal at each timepoint
def calculate_surprisal(task=4):
    
    #-- get data --#
    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == data_fitness['agent'].max()) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task))
    timestep_data = timestep_data.drop('surprisal', axis=1)
    
    #Create sensory state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')

    #-- get goal priors --#
    #Subset data for only perfect agents on the last generation
    timestep_data_perfect = timestep_data.loc[(timestep_data['run'].isin(perfect_runs)) & (timestep_data['agent'] == timestep_data['agent'].max())]
    
    #Count how many times each state appeared in each context
    counts = timestep_data_perfect.groupby(['run', 'timestep', 'task_type', 'block_movement', 'sensory_state'])['sensory_state'].agg(surprisal='count')

    #Add 1 to all counts to include unobserved states
    counts = counts.unstack(fill_value=0).stack() + 1

    #Transform counts into probabilities
    goal_prior_surprisal = counts.groupby(['run', 'timestep', 'task_type', 'block_movement']).apply(lambda x: - np.log(x / float(x.sum())) ).reset_index()

    #Save the distribution for later plotting 
    goal_prior_surprisal.to_pickle('goal_priors/goal_prior_distribution_task{}.pkl'.format(task))

    #-- add surprisal column --#
    #Make empty dataframe for populating
    timestep_data_surprisal = pd.DataFrame(columns = timestep_data.columns)
    timestep_data_surprisal['surprisal'] = None

    #Group dataframe by generation
    timestep_data_grouped = timestep_data.groupby(['run', 'agent'])

    #Go through each group
    for name, group in tqdm(timestep_data_grouped):

        #Find the goal prior which has the lowest KL divergence to this generation
        min_kl_perfect_run = find_lowest_kl(group, goal_prior_surprisal)

        #Merge surprisal form that goal prior unto the dataset
        tmp_data = group.merge(goal_prior_surprisal[goal_prior_surprisal['run'] == min_kl_perfect_run].drop('run', axis = 1), how = 'left')

        #And append it to the output dataframe
        timestep_data_surprisal = timestep_data_surprisal.append(tmp_data)
    
    #Save the dataset to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}.csv'.format(task), index=False)