#### - Preparation - ####
# Load libraries
import csv
import pickle


def extract_from_MABE(task=4):
    #### - Settings - ####
    # Parameters used in the MABE simulation
    task = 4
    generations = 60000
    n_agents = int(generations/500.+1)
    n_runs = 50
    n_trials = 128
    n_timeSteps = 35

    #### - Load data - ####
    # This file is created from MABES output using the actual agency package
    with open('raw_data/brainActivity_task{}.pickle'.format(task), "rb") as f:
        brain_activity = pickle.load(f)

    # This file is where we write the data to
    out_file = open(
        'processed_data/timestep_data_task{}_init.csv'.format(task), "w", newline='')
    out_writer = csv.writer(out_file)

    # Write the column names
    colnames = ['run', 'agent', 'trial', 'timestep',
                'S1', 'S2', 'M1', 'M2', 'H1', 'H2', 'H3', 'H4']
    out_writer.writerow(colnames)

    # Go through each run
    for run in range(n_runs):

        # Go through each generation's best agent
        for agent in range(n_agents):

            # Go through each trial
            for trial in range(n_trials):

                # Go through each timestep
                # The first timestep is where all nodes are 0 is removed
                # The last timestep which signals end fo trial is removed
                for timestep in range(1, n_timeSteps-1):

                    # Extract states of the system's 8 nodes
                    state = brain_activity[run][agent][trial][timestep].astype(
                        int)

                    # Make list of information to save
                    line = [run, agent, trial, timestep, state[0], state[1],
                            state[2], state[3], state[4], state[5], state[6], state[7]]

                    # And write it to the csv
                    out_writer.writerow(line)
