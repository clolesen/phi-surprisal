import numpy as np
import pandas as pd


def get_task_info(task):

    # Read in the dataframe
    data_timestep = pd.read_csv(
        'processed_data/timestep_data_task{}.csv'.format(task))

    # 1 set consists of 16 trials of 33 timesteps
    n_trials = 16
    n_timesteps = 33
    set_length = n_trials * n_timesteps

    # Sets alternatve between left and right
    block_movement = [-1] * set_length + [1] * set_length
    # This changes every two sets
    task_type = ['catch'] * 2*set_length + ['avoid'] * 2*set_length
    # This changes every two sets in the order 3, 4, 6, 5
    block_size = [3] * 2*set_length + [4] * 2 * \
        set_length + [6] * 2*set_length + [5] * 2*set_length

    # Each column is filled out with the patterns specified above
    data_timestep['block_movement'] = np.resize(
        block_movement, data_timestep.shape[0])
    data_timestep['task_type'] = np.resize(task_type, data_timestep.shape[0])
    data_timestep['block_size'] = np.resize(block_size, data_timestep.shape[0])

    #Make a column showing when the animat sees the block
    data_timestep['is_seeing'] = data_timestep[['S1','S2']].max(axis=1)

    # Make empty column to populate the first time the animat sees the block
    data_timestep['first_sight'] = 0

    # Go through each run and agent combination
    for run in range(data_timestep['run'].max()+1):
        print(run)
        for agent in range(data_timestep['agent'].max()):
            
            # Subset and find the index where the animat sees the block for the first time
            first_sight_idx = data_timestep[(data_timestep['run'] == run) & (data_timestep['agent'] == agent)].is_seeing.idxmax()
            # Add this moment to the dataset
            data_timestep[first_sight_idx]['first_sight'] = 1

    # Drop the is_seeing column now that is unneeded
    data_timestep.drop(['is_seeing'], axis = 1)

    # Save the csv
    data_timestep.to_csv(
        'processed_data/timestep_data_task{}.csv'.format(task), index=False)
