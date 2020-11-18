###Setup###
#Load dependencies
library(dplyr)
library(tidyverse)


###Minor functions###

#Function for making the column indicating the first time the animat sees the block
make_col_first_sight <- function(see_block){
  #Find the idx of the first time the agent sees the block
  idx <- detect_index(see_block, function(x) x == 1)
  #If the animat never sees the block
  if (idx == 0) {
    #Make a list of 0's
    out = rep(0,length(see_block))
    #Otherwise
  } else {
    #Make a list of zeroes with a 1 when it sees the block first
    out <- c(rep(0,idx-1), 1, rep(0,length(see_block)-idx))}
  return(out)
}

#Function for making the column indicating whether the agent has seen the block before
make_col_seen_block <- function(see_block){
  #Find the idx of the first time the agent sees the block
  idx <- detect_index(see_block, function(x) x == 1)
  #Make a list of zeroes as long as the timesteps before it sees it and add 1's for the rest
  out <- c(rep(0,idx), rep(1,length(see_block)-idx))
  return(out)
}

#Function for making column indicating whenever vision state changes
make_col_vision_transition <- function(see_block){
  #Make empty vector
  out <- rep(0, length(see_block))
  #Set it to 1's whenever the vision state changes
  out[which(see_block != dplyr::lag(see_block))] = 1
  return(out)
}


###Full wrapper function###

#Full function for adding behavioural analysis
add_behavioural_analysis <- function(d, task){
  #Block movement direction - 16 trials * 33 timesteps left, 16 trials * 33 timesteps right
  d$block_movement <- c(rep(-1,33*16), rep(1,33*16))

  #Trial type - changes every 32 trials between catch and avoid
  d$trial_type <- c(rep('catch',33*16*2), rep('avoid',33*16*2))

  #Block size 32 trials size, in task 4: 3-4-6-5, in task 1: 1-3-1-3
  if (task == 4) {
    d$block_size <- c(rep(3,33*16*2), rep(4,33*16*2), rep(6,33*16*2), rep(5,33*16*2))
  } else if (task == 1) {
    d$block_size <- c(rep(1,33*16*2), rep(3,33*16*2), rep(1,33*16*2), rep(3,33*16*2))
  }
  
  #Movement direction
  d$animat_movement <- d$M1 - d$M2 #-1 is left, 1 is right
  
  #Does the animat see the block
  d$animat_is_seeing <- ifelse(d$S1==1 | d$S2==1, 1, 0)

  #When does the animat transition between seeing and not seeing the block?
  
  #Animat is following if it is moving the same direction as the block
  d$animat_follow <- ifelse(d$animat_movement==d$block_movement, 1, 0)
  
  #Animat following length
  d <- d %>%
    #Group into the specific trials
    group_by(trial, agent_id) %>%
    
    #Make a cumulative sum that resets at 0 to see how many timesteps the animat is following in a row
    mutate(animat_follow_length = ave(animat_follow, cumsum(animat_follow == 0), FUN = cumsum)) %>%
    
    #Make column with 0's until the animat sees the block the first time
    mutate(animat_has_seen = make_col_seen_block(animat_is_seeing)) %>%
    
    #Make column with 0's until the animat sees the block the first time
    mutate(animat_first_sight = make_col_first_sight(animat_is_seeing)) %>%
    
    #Make column with 1's whenever the agent transitions between seeing and not seeing
    mutate(animat_sight_change = make_col_vision_transition(animat_is_seeing)) %>%
    
    #Make a column for types of strategies
    mutate(strategy = ifelse(
      #if the block can see the 
      tail(animat_is_seeing, n=1) == 1 & tail(animat_follow_length, n=1) > 4, 'follow_catch', 'undefined'))
  
  return(d)
<<<<<<< HEAD
} 

=======
}

read.csv()
>>>>>>> 9717e652029b3829e50174ee31140b5797f904d7
