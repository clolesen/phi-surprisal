import pickle as pkl
import pandas as pd
with open("C:/Users/User/Documents/GitHub/phi-surprisal/goal_priors/goal_prior_distribution_task1.pkl", "rb") as f:
    object = pkl.load(f)
    
df = pd.DataFrame(object)
df.to_csv(r'C:/Users/User/Documents/GitHub/phi-surprisal/goal_priors/goal_prior_distribution_task1.csv')