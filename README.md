# Phi Fluctuates with Surprisal
This repository contains the code related to the analysis of the project _Phi Fluctuates with Surprisal_.

What follows is a breif description of the structure of the code. The code consists of two main parts. The first part is writen in python and converts the simulation output from MABE (the simulation software used to run the simulation, which is not included here) and calculates both Phi and surprisal. Note that the conversion of the MABE output utilizes scripts from a different project called actual agency, which is also not included here (contact the authors for more information). The second part is written in R and covers the main analysis and plotting. Through out these files the code will refer to task 1 and 4, which corresponds to the easy and hard task respectivly. The numbers 1 and 4 originates from the original 2014 simulation where 4 different task dificulties where simulated. Here we only use the easiest and hardest of those.

## Python part
The script prepare_data_master.py imports functions from the different scripts found in the folder python_utils, and runs them step by step. In this file the task value can be set to 1 or 4, which will determine what data will be used. 

Each script in python_utils defines functions that does a central part of the data processing and will output file with the resulting data.

## R part
There is two main R scripts, data_processing.R and plotting.R, which both utilizes function found in the folder R_utils.

data_processing.R takes the restulting data from the python part and outputs files with calculated averages, and runs the timeseries analysis of cross correlations. 

plotting.R produces the plots used in the paper and saves them in the folder plots.
