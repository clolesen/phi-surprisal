import numpy as np
import pandas as pd

#Only for writing code
if __name__ == "__main__":
    import os
    os.chdir('..')
    task = 4


def get_task_info(task):

    # Read in the dataframe
    timestep_data = pd.read_csv('processed_data/timestep_data_task{}.csv'.format(task), 
                            dtype={"concept_phis": object})

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
    timestep_data['block_movement'] = np.resize(block_movement, timestep_data.shape[0])
    timestep_data['task_type'] = np.resize(task_type, timestep_data.shape[0])
    timestep_data['block_size'] = np.resize(block_size, timestep_data.shape[0])

    #Make a column showing when the animat sees the block
    timestep_data['is_seeing'] = timestep_data[['S1','S2']].max(axis=1)

    # Make empty column to populate the first time the animat sees the block
    timestep_data['first_sight'] = 0
    #Get indeces for when the animat sees the block the first time
    first_sight_idx = timestep_data.groupby(['run', 'agent', 'trial'])['is_seeing'].idxmax()
    #Change the first_sight column to 1 at those places
    timestep_data.loc[first_sight_idx, 'first_sight'] = 1

    # Drop the is_seeing column now that is unneeded
    timestep_data = timestep_data.drop(['is_seeing'], axis = 1)

    # Save the csv
    timestep_data.to_csv(
        'processed_data/timestep_data_task{}.csv'.format(task), index=False)








