##--------------------------------------------
#
##  Class: PCE Data Science Methods Class
##  Bootstrapping
##  March 7th 2017
##  Amy Werner-Allen
#
##  Objective:  Use bootstrapping to estimate the error. (NOT on the residuals)
##  Dependent variable: Predict whether or not indivual will buy ('Buy')
#
###############################################################

library(logging)
library(ggplot2)
library(boot)
setwd("~/Documents/")

##----Log file creation-----

get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW8_log_%Y_%m_%d.log")
  return(log_file_name)
}

###############################################################
##-----Functions Here-----

# Create function for loading and manipulating data
load_data = function(){
  filename = 'AdvertisingPrediction.csv'
  ad_data = read.csv(filename)
  ad_data$Obs.No. = NULL
  loginfo(paste('Number of rows loaded in for', filename, ':', dim(ad_data)[1]))
}

# Unit test data load
# Test if all data was loaded in:
test_data_load = function(){
  stopifnot(dim(ad_data)[1] == 673)
}

# Unit test dependent variable integrity
# Check if any NA values:
test_NAs = function(){
  v = dim(ad_data[complete.cases(ad_data),])[1]
  loginfo(paste('Number of complete rows for', filename, ':', v))
  stopifnot(v==dim(ad_data)[1])
}

# Check if buy values always 0 or 1
test_buys = function(){
  stopifnot(length(unique(ad_data$Buy))==2)
  stopifnot(min(unique(ad_data$Buy))==0)
  stopifnot(max(unique(ad_data$Buy))==1)
}

###############################################################
##-----Main Portion-----

if (interactive()){
  
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Load data and run unit tests
  load_data()
  test_data_load()
  test_NAs()
  test_buys()
  
  ###############################################################
  # Bootstrapping Residuals
  
  # First we have to get the residuals from the logistic regression
  logistic_fit = glm(Buy ~ ., family=binomial, data = ad_data)
  coef(logistic_fit)
  
  # Create a function that does the same glm as above but for a subset of the data
  # Then return the coefficients of the new sampled model
  logistic_boot = function(data, indices){
    data_new = data[indices, ]
    fit_new = glm(Buy ~ ., family=binomial, data = data_new)
    return(coef(fit_new))}
  
  # Here's an example on the first 100 data points
  ind = c(1:100)
  logistic_boot(ad_data, ind)
  # We can compare these to the coef(logistic_fit) above
  
  # Now we actually do the bootstrapping using boot function
  N = 1000 # (can change this)
  boot_estimates = boot(data=ad_data, statistic=logistic_boot, R=N)
  
  # boot_estimates$t0 : actual values
  # boot_estimates$t  : simulated values (N rows by 18 columns)
  # We can plot some of the coefficients that we are trying to estimate
  # And also see how normally distributed the variables are:
  
  plot(boot_estimates, index=1)  #not too nomrmal
  plot(boot_estimates, index=18) #very normal
  
  # Calculate Confidence intervals for everything with 'boot.ci()'
  boot.ci(boot_estimates, type="norm", index=3)
  boot.ci(boot_estimates, type="norm", index=18)
  
  
  ###############################################################
  # Log results
  
  for(i in 1:dim(ad_data)[2]) {
  
    names = colnames(ad_data)
    
    if(i != 3) {
      feature = as.name(names[i])
      avg = round(mean(boot_estimates$t[,i]), 3)
      ci = boot.ci(boot_estimates, type="norm", index=i)
      ci_low = round(ci[[4]][2], 3)
      ci_high = round(ci[[4]][3], 3)
      loginfo(paste('Printing results for feature:', feature))
      loginfo(paste('Confidence Interval:', ci_low, ",", ci_high))
      loginfo(paste('Mean for feature', i, ':', avg))
    }
      
    else {
      feature = as.name(names[i])
      avg = round(mean(boot_estimates$t[,i]), 4)
      ci = boot.ci(boot_estimates, type="norm", index=i)
      ci_low = round(ci[[4]][2], 4)
      ci_high = round(ci[[4]][3], 4)
      loginfo(paste('Printing results for feature:', feature))
      loginfo(paste('Confidence Interval:', ci_low, ",", ci_high))
      loginfo(paste('Mean for feature', i, ':', avg))
    }
  
  }
  
}