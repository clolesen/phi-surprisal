#import libraries
import pandas as pd
import numpy as np
import pickle

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4


def calculate_surprisal(task=4):

    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == data_fitness['agent'].max()) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    #timestep_data = pd.read_csv('processed_data/timestep_data_task{}_info.csv'.format(task))
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task)) #temp
    timestep_data = timestep_data.drop(['sensory_state', 'surprisal', 'Phi', 'n_concepts', 'concept_phis'], axis = 1)

    #Create sensory state and context state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')

    #Subset data for only perfect agents on the last generation
    timestep_data_perfect = timestep_data.loc[(timestep_data['run'].isin(perfect_runs)) & (timestep_data['agent'] == timestep_data['agent'].max())]
    
    #Count how many times each state appeared in each context
    counts = timestep_data_perfect.groupby(['run', 'timestep', 'task_type', 'block_movement', 'sensory_state'])['sensory_state'].agg(surprisal='count')

    #Add 1 to all counts to include unobserved states
    counts = counts.unstack(fill_value=0).stack() + 1

    #Transform counts into probabilities
    goal_prior_surprisal = counts.groupby(['run', 'timestep', 'task_type', 'block_movement']).apply(lambda x: - np.log(x / float(x.sum())) ).reset_index()

    #Save the distribution for later plotting 
    goal_prior_surprisal.to_pickle('goal_prior_distribution.pkl')

    #Find minimum surprise distribution for each generation
    test_df = pd.DataFrame(columns = timestep_data.columns)
    test_df['surprisal'] = None
    #Go through each generation (agent) in each simulation (run)
    for run in range(data_fitness['run'].max()):
        print(run)
        for agent in range(data_fitness['agent'].max()):
            min_surprise = None
           
            for perfect_run in perfect_runs:
            
                tmp_goal_prior = goal_prior_surprisal[goal_prior_surprisal['run'] == perfect_run].drop('run', axis = 1)

                tmp_data = timestep_data[(timestep_data.run == run) & (timestep_data.agent == agent)]

                tmp_data = tmp_data.merge(tmp_goal_prior, how = 'left')

                if not min_surprise:
                    min_surprise = tmp_data['surprisal'].mean()

                    saved_data = tmp_data

                elif tmp_data['surprisal'].mean() < min_surprise:
                    min_surprise = tmp_data['surprisal'].mean()

                    saved_data = tmp_data
            
            test_df = test_df.append(saved_data)

    #Save data to csv
    timestep_data.to_csv('processed_data/timestep_data_task{}_surprisal.csv'.format(task), index=False)




















def calculate_surprisal(task=4):

    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == data_fitness['agent'].max()) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    #timestep_data = pd.read_csv('processed_data/timestep_data_task{}_info.csv'.format(task))
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task)) #temp

    #Create sensory state and context state
    timestep_data['sensory_state'] = timestep_data['S1'].astype('str') + timestep_data['S2'].astype('str')

    #Subset data for only perfect agents on the last generation
    timestep_data_perfect = timestep_data.loc[(timestep_data['run'].isin(perfect_runs)) & (timestep_data['agent'] == timestep_data['agent'].max())]
    
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