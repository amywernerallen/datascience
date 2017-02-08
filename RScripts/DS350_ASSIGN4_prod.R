##--------------------------------------------
##
##  Class: PCE Data Science Methods Class
##  Statistical Models and the Central Limit Theorem
##  February 6 2017
##  Amy Werner-Allen
## 
##--------------------------------------------

library(logging)
library(microbenchmark)
library(ggplot2)

##----Log file creation-----

get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW4_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

##----Statistical Modeling-----
#
#   Model these two things
#
#   (1) Number of Hospitalizations vs. Crude Admittance Rate.
#   (2) Change in Number of Hospitalizations vs. Change in Crude Admittance Rate
#
#   Set workding directory to folder with files
#   setwd("~/Documents/UW/Semester2/Assignments")


##-----Functions Here-----

# Create function for loading and manipulating data
load_data = function(){
  
  # Load in data
  cd = read.csv("ChicagoDiabetesData.csv", sep=",", header=TRUE)
  loginfo(paste('Loaded in data with:', dim(cd)[1], 'rows and', dim(cd)[2], 'columns'))
  
  # Take the average of the numeric columns (not zipcode)
  data_means = apply(cd[-1],2,mean)
  
  # Separate hospitalizations from admit rates
  hospitalizations = data_means[grepl('Hospitalizations', names(cd[-1]))]
  admit_rate = data_means[grepl('Crude.Rate.[0-9]+$', names(cd[-1]), perl = TRUE)]
  
  loginfo(paste('Number of hospitalizations:', length(hospitalizations)[1]))
  loginfo(paste('Number of hospital admit rates:', length(admit_rate)[1]))
  
  return(dim(cd)[1])
  
}

# Unit test data load
# Test if all data was loaded in:

test_data_load = function(){
  dl = load_data()
  stopifnot(dl == 47)
}

# Unit test diff function
# Test if diff worked as expected

test_diff = function(x, x_diff){
  test = c()
  for(i in 1:length(x)-1) {
     test = c(test, x[i+1]-x[i])
  }
  stopifnot(test == x_diff)
}

##-----Main Portion-----

if (interactive()){

  # Setup Logging
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='INFO')

  # Load data
  load_data()
  
  # Run unit test
  test_data_load()

  ##-----Quick Plot of the Data-----
    
  year = seq(from=2000, to=2011)
  h = as.data.frame(cbind(year, hospitalizations, admit_rate))
  rownames(h) = NULL
  
  ggplot(h, aes(x=hospitalizations, y=admit_rate)) +
    geom_point(shape=1) +  
    geom_smooth(method=lm) +
    theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), panel.background = element_rect(fill = "white", colour = "darkblue"))+
    labs(y="Hospitalization Rate", x="Hospital Discharges")
  
  ##-----Regression 1-----  
  
  # First we regress using hosp and admit rate:
  cd.mod1 = lm(admit_rate ~ hospitalizations, data = h)
  loginfo(paste('R-Squared for Hospital Discharges vs. Admittance Rate:', round(summary(cd.mod1)$r.squared, 5)))
  loginfo(paste('Adjusted R-Squared for Hospital Discharges vs. Admittance Rate:', round(summary(cd.mod1)$adj.r.squared, 5)))

  
  ##-----Regression 2-----  
  
  # Now we look at the differences in hosp and admit rate:
  hospitalizations_diff = diff(hospitalizations)
  admit_rate_diff = diff(admit_rate)
  h2 = as.data.frame(cbind(hospitalizations_diff, admit_rate_diff))
  
  # Run unit test
  test_diff(hospitalizations, hospitalizations_diff)
  test_diff(admit_rate, admit_rate_diff)
  
  # Do a quick plot
  ggplot(h2, aes(x=hospitalizations_diff, y=admit_rate_diff)) +
    geom_point(shape=1) +  
    geom_smooth(method=lm) +
    theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), panel.background = element_rect(fill = "white", colour = "darkblue"))+
    labs(y="Change in Hospitalization Rate", x="Change in Hospitalizations")
  
  # Now combine data and regress
  h2 = as.data.frame(cbind(hospitalizations_diff, admit_rate_diff))
  rownames(h2) = NULL
  
  cd.mod2 = lm(admit_rate_diff ~ hospitalizations_diff, data = h2)
  loginfo(paste('R-Squared for Delta Hospital Discharges vs. Delta Admittance Rate:', round(summary(cd.mod2)$r.squared, 5)))
  loginfo(paste('Adjusted R-Squared for Delta Hospital Discharges vs. Delta Admittance Rate:', round(summary(cd.mod2)$adj.r.squared, 5)))
  

}