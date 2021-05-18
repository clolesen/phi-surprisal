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
    
    #-- get data --#
    #Read fitness datafile
    data_fitness = pd.read_csv('raw_data/fitness_task{}.csv'.format(task))

    #Find the runs with perfect fitness on the last generation
    perfect_runs = list(data_fitness.loc[(data_fitness['agent'] == data_fitness['agent'].max()) & (data_fitness['fitness'] == 1)]['run'])

    #Read data
    #timestep_data = pd.read_csv('processed_data/timestep_data_task{}_info.csv'.format(task))
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task)) #temp
    timestep_data = timestep_data.drop(['sensory_state', 'surprisal'], axis = 1)

    #Create sensory state and context state
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
    goal_prior_surprisal.to_pickle('processed_data/goal_prior_distribution.pkl')

    #-- calculate suprisal --#
    #Make empty dataframe for populating
    timestep_data_surprisal = pd.DataFrame(columns = timestep_data.columns)
    timestep_data_surprisal['surprisal'] = None

    #Go through each generation (agent) in each simulation / LOD (run)
    for run in range(data_fitness['run'].max()+1):
        print(run)
        for agent in range(data_fitness['agent'].max()):
            
            #Reset the counter for minimum surprisal
            min_surprisal = None
           
            #Go through each of the perfect LOD's 
            for perfect_run in perfect_runs:
                
                #Get the goal prior from only the specific perfect run
                tmp_goal_prior = goal_prior_surprisal[goal_prior_surprisal['run'] == perfect_run].drop('run', axis = 1)

                #Get the data for the current generation and simulation and merge it to get the surprisal at each timestep
                tmp_data = timestep_data[(timestep_data.run == run) & (timestep_data.agent == agent)].merge(tmp_goal_prior, how = 'left')

                #If it is the first of the perfect runs
                if not min_surprisal:
                    #Save the average surprisal as the current minimum
                    min_surprisal = tmp_data['surprisal'].mean()
                    #save the current data as the one with the lowest surprisal
                    min_surprisal_data = tmp_data

                #If is one of the other perfect runs, check if the average surprisal is lower than the current minimum
                elif tmp_data['surprisal'].mean() < min_surprisal:
                    #If it is, override the current minimum surprisal
                    min_surprisal = tmp_data['surprisal'].mean()
                    #And override the data too
                    min_surprisal_data = tmp_data
            
            #Append the dataset with surprisal, using the goal prior which gives the lowest surprisal
            timestep_data_surprisal = timestep_data_surprisal.append(min_surprisal_data)

    #Save data to csv
    #timestep_data.to_csv('processed_data/timestep_data_task{}_surprisal.csv'.format(task), index=False)
    timestep_data.to_csv('processed_data/timestep_data_task{}_TEST.csv'.format(task), index=False)
