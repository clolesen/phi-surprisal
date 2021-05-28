#import libraries
import pandas as pd
import numpy as np
from scipy.stats import entropy as kl_divergence

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4

#Function for finding the minimum kl-divergence
def find_lowest_kl(goal_prior_surprisal,**timestep_data_group):

    timestep_data_group = pd.DataFrame.from_dict(timestep_data_group)

    #Count how many times each state appeared in each context
    observed_counts = timestep_data_group.groupby(['timestep', 'task_type', 'block_movement', 'sensory_state'])['sensory_state'].agg(probability='count')

    #Add 1 to all counts to include unobserved states
    observed_counts = observed_counts.unstack(fill_value=0).stack() + 1

    #Transform counts into probabilities
    observed_probabilities = observed_counts.groupby(['timestep', 'task_type', 'block_movement']).apply(lambda x: x / float(x.sum()) ).reset_index()

    #Group goal prior by run
    goal_prior_grouped = goal_prior_surprisal.groupby('perfect_run')['surprisal']
    
    #Get kl divergences to different goal prior
    kl_divergences = goal_prior_grouped.agg(lambda x: kl_divergence(observed_probabilities['probability'], np.exp(-x)))

    #Return the goal prior number with the lowest divergence
    return kl_divergences.idxmin()


#Function for calculating the surprisal at each timepoint
def calculate_surprisal(task=4):
    
    #-- Get data --#
    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == data_fitness['agent'].max()) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task), 
                                dtype={"concept_phis": object})
    #Remove previous versions of the data added in this script
    timestep_data = timestep_data.drop(['sensory_state', 'perfect_run', 'surprisal'], axis=1, errors = 'ignore')
    
    #Create sensory state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')

    #-- Get goal priors --#
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

    #Rename column to avoid confusion with 'run' when merging
    goal_prior_surprisal = goal_prior_surprisal.rename(columns = {'run': 'perfect_run'})

    #-- Add surprisal column --#
    #Group dataframe by generation
    timestep_data_grouped = timestep_data.groupby(['run', 'agent'])

    #Get the perfect run for each group
    lowest_kl_perfect_runs = timestep_data_grouped.agg(lambda df: find_lowest_kl(goal_prior_surprisal,**df))

    #Add the column name
    lowest_kl_perfect_runs.name = 'perfect_run'

    #Merge with original dataframe to add the perfect run column
    timestep_data = timestep_data.merge(lowest_kl_perfect_runs.to_frame(), left_on = ['run', 'agent'], right_on = ['run', 'agent'])

    #Merge with goal prior to add the surprisal depending on perfect run, timestep and task context
    timestep_data = timestep_data.merge(goal_prior_surprisal, 
                                        how = 'left',
                                        left_on = ['perfect_run', 'timestep', 'task_type', 'block_movement', 'sensory_state'],
                                        right_on = ['perfect_run', 'timestep', 'task_type', 'block_movement', 'sensory_state'])
    
    #Save the dataset to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}.csv'.format(task), index=False)