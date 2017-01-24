##--------------------------------------------
##
## Class: PCE Data Science Methods Class
## Monty Hall Homework Solution - Assignment 2
## January 24th 2017
## 
## Amy Werner-Allen
##
##--------------------------------------------

library(logging)


# Get the log file name that has a date-time in the name
get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW2_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}


# Redefine the sample() function properly (Straight from the help file)
resample = function(x, ...) x[sample.int(length(x), ...)]


### Main function here
# One game simulation function
one_game = function(switch_logical){
  
  # This function returns either a 0/1
  
  # First choose a winning door
  # Note that we could hypothetically make the max # doors a variable
  # Here we keep it at 3
  winning_door = sample(1:3, 1)
  
  # Now contestant picks a door
  # No constraints: contestant can pick winning door
  contestant_door = sample(1:3,1)
  
  # Two options now:
  # (1) Contestant picked winning door
  # Can open either other door
  # (2) Contestant picked non-winning door
  # Can open only one door
  
  if(winning_door == contestant_door) {
    doors = 1:3
    options = doors[!doors %in% winning_door]
    open_door = sample(options, 1)
    # pick one of two from options
    
  } else if (winning_door != contestant_door) {
    doors = 1:3
    options = doors[!doors %in% c(winning_door, contestant_door)]
    open_door = options
    # only one door in options
    
  } 
  
  # Now for the output
  # Need to factor in switch value
  
  if(switch_logical == FALSE) {
    final_pick = contestant_door
    # contestant stays with initial door
    
  } else if (switch_logical == TRUE) {
    doors = 1:3
    final_pick = doors[!doors %in% c(contestant_door, open_door)]  
    # won't pick initial or open door
    
  }
  
  if(final_pick==winning_door) {1} else {0}
  
}


# Unit test
# Test if a simulation returns TRUE or FALSE
test_simulation_return_val = function(){
  one_game_outcome = one_game(switch_logical=TRUE)
  stopifnot(one_game_outcome %in% c(TRUE, FALSE))
}

if (interactive()){
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Setup working directory
  setwd("~/Documents/UW/Semester2/Assignments")
  
  # Perform unit test
  test_simulation_return_val()
  
  # Set simulation parameters
  N_sims = 10000 # Number of games to simulate
  
  # Perform Stay & Switch Simulations
  # Here we come up with a vector of wins and losses
  # by running the function we wrote above
  
  stay_results = sapply(1:N_sims, function(x) one_game(switch_logical=FALSE))
  switch_results = sapply(1:N_sims, function(x) one_game(switch_logical=TRUE))
  
  # Look at results
  # First we compute the average
  prob_win_switch = mean(switch_results)
  prob_win_stay = mean(stay_results)
  
  # Then we compute the variance of the results
  var_switch = var(switch_results)
  var_stay = var(stay_results)
  
  # Log Results
  # Here is an example of how to log a result
  loginfo(paste('Probability of Winning with Switch:', prob_win_switch))
  loginfo(paste('Probability of Winning with Stay:', prob_win_stay))
  loginfo(paste('Variance of Winning with Switch:', var_switch))
  loginfo(paste('Variance of Winning with Stay:', var_stay))
  
  # Expected value for means is 2/3 for switch and 1/3 for stay
}