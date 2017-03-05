##--------------------------------------------
##
##  Class: PCE Data Science Methods Class
##  SVD and Regression
##  February 21 2017
##  Amy Werner-Allen
## 
##--------------------------------------------

library(logging)
library(ggplot2)
# setwd('~/Documents/teaching/6_Regression_FeatureSelection/')
# setwd("~/Documents/UW/Semester2/Assignments")

##----Log file creation-----

get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW6_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

##----Statistical Modeling-----
#
# You will use SVD to do one of the following:

# 1) Perform SVD Regression (Total or Demming Regression), and report on the error.
# 2) Or perfrom Principle component regression.  
#    Here you will identify how many features we need to get a good fit 
#    and report on the adjusted R squared in the following linear regression.
#

##-----Functions Here-----

# Create function for loading and manipulating data
load_data = function(){
  
  # Load in data
  crime_data = read.table('communities.data', sep=",", header=FALSE, na.strings = c("NA","?"))
  crime_headers = read.table('crime_headers.txt')
  names(crime_data) = crime_headers$V1
  crime_data = crime_data[colSums(is.na(crime_data)) < 100]
  crime_data = crime_data[complete.cases(crime_data),]
  
  return(crime_data)
  
}

# Unit test data load
# Test if all data was loaded in:

test_data_load = function(){
  dl = load_data()
  loginfo(paste('Loaded in data with:', dim(dl)[1], 'rows and', dim(dl)[2], 'columns'))
  stopifnot(dim(dl)[1] == 1993)
}

# Unit test dependent variable integrity
# Check if any NA values for VCPP:

test_NAs = function(){
  v = sum(is.na(crime_data$ViolentCrimesPerPop))
  stopifnot(v==0)
}

# Disregard 'state' and 'communityname'.
# Consider 'ViolentCrimesPerPop' as the y-dependent variable.
# Create SVD features and perform linear regression.

##-----Main Portion-----

if (interactive()){
  
  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')
  
  # Load data
  crime_data = load_data()
  
  # Run unit tests
  test_data_load()
  test_NAs()
  
  ##-----Create matrix for SVD-----
  
  VC_model = lm(ViolentCrimesPerPop ~ . - state -communityname, data = crime_data)
  VC_matrix = model.matrix(ViolentCrimesPerPop ~ . - state -communityname, data = crime_data)
  PCVC_matrix = prcomp(VC_matrix)
  plot(PCVC_matrix$sdev, ylab='Standard Dev', xlab='PCs')
  abline(v=10, lwd=2, col='red')
  
  # Regress using top 10
  PC_model = lm(crime_data$ViolentCrimesPerPop ~ PCVC_matrix$x[,1:10])
  summary(PC_model) # eight are significant
  
  AIC(VC_model)
  AIC(PC_model)
  
  aic_by_num_pc = sapply(2:100, function(x){
    formula_rhs_temp = paste(paste0('PCVC_matrix$x[,',1:x,']'), collapse = ' + ')
    formula_temp = paste('crime_data$ViolentCrimesPerPop ~',formula_rhs_temp)
    pc_all_components_temp = lm(eval(parse(text=formula_temp)))
    return(AIC(pc_all_components_temp))
  })
  plot(aic_by_num_pc, type='l', lwd=2,
       main='AIC of P.C. Linear Reg with X components',
       xlab="# of components", ylab='AIC')
  # add a horizontal line of where the all variable AIC is at
  abline(h=AIC(VC_model), lwd=2, col='red')
  which.min(aic_by_num_pc) 

  loginfo(paste('Adjusted R Squared for Linear Regression:', round(summary(VC_model)$adj.r.squared, 3)))
  loginfo(paste('Sigma for Linear Regression:', round(summary(VC_model)$sigma, 3)))
  loginfo(paste('AIC for Full Linear Regression:', round(AIC(VC_model), 2)))
  loginfo(paste('Adjusted R Squared for top 10 PCs Regression:', round(summary(PC_model)$adj.r.squared, 3)))
  loginfo(paste('Sigma for top 10 Pcs Regression:', round(summary(PC_model)$sigma, 3)))
  loginfo(paste('AIC for top 10 PCs:', round(AIC(PC_model), 2)))
  loginfo(paste('# PCs that Minimize AIC:', which.min(aic_by_num_pc)))
  
}