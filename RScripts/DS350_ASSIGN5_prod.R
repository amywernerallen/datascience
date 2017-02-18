##--------------------------------------------
##
##  Graph Networks - Assignment 5
##  Class: PCE Data Science Methods Class
##  Amy Werner-Allen
##  February 12th 2017
## 
##  Write Script level R-code that computes 
##  the mean degree of the graph and shows a plot 
##  of the histogram of the degrees. 
##--------------------------------------------

##----Import Libraries-----
require(logging)
require(ggplot2)
require(igraph)

##-----Declare Functions Here-----

get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW5_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

##----Setup Test Logger-----
log_file_name = get_log_filename()
basicConfig()
addHandler(writeToFile, file=log_file_name, level='DEBUG')

##-----Read in the data-----
setwd("~/Documents/UW/Semester2/Assignments")
loginfo(paste('Loading in data:', Sys.time()))
fb_data = read.csv("facebook_edge_list.csv", sep=",",
                header=TRUE, stringsAsFactors=FALSE)
# loginfo(paste('Loaded in data:', Sys.time()))
loginfo(paste('Total rows in data frame:', dim(data)[1]))

##-----Calculating mean degree length-----
fb_degree_list = table(c(fb_data$Source))
fb_mean_degree = mean(fb_degree_list)
loginfo(paste('Average degree length:', fb_mean_degree))

t = as.data.frame(fb_degree_list)
colnames(t) = c('Node', 'Freq')

##-----Plotting histogram-----
ggplot(t, aes(x=Freq, fill=Freq)) +
  geom_histogram(colour = "darkblue", fill = "lightblue", binwidth = 3) +
  theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), panel.background = element_rect(colour = "darkblue"))+
  labs(x="Degree Count", y="Total")

# Write R code that performs a K-S test for the two hypotheses:
# *Test if the distribution of degrees is Poisson. (reuse K-S code)
# *Test if the distribution of degrees is a Power Law. (Use igraph library)

##------Create k-s statistic function-----
ks_stat = function(x_min,x_max, dist_a, dist_b){
  x_seq = seq(x_min,x_max,len=1000)
  y_cdf1 = sapply(x_seq, function(x){
    sum(dist_a<x)/length(dist_a)
  })
  y_cdf2 = sapply(x_seq, function(x){
    sum(dist_b<x)/length(dist_b)
  })
  k_s_stat = max(abs(y_cdf1-y_cdf2))
  return(k_s_stat)
}

##------Generate k-s for Poisson-----
pois1 = rpois(dim(fb_data)[1], lambda=fb_mean_degree)
t2 = as.data.frame(table(pois1))
ksp = ks_stat(1, 40, pois1, t$Freq)
loginfo(paste('KS Statistic for Poisson:', round(ksp, digits=3)))

# Run simulation to get k-s statistics for two poisson distributions
start.time = Sys.time()
N = 1000
sample_null_ks = sapply(1:N, function(x) 
{ r_poiss1 = rpois(1000, fb_mean_degree) 
r_poiss2 = rpois(1000, fb_mean_degree)
ks_stat(0, 30, r_poiss1, r_poiss2) })
end.time = Sys.time()
duration = end.time - start.time
# print(duration): takes 20 sec for 1,000

# Now test our k-s statistic from the distribution of interest
# Since we know the mean, we use t-test to get p-value
pv = t.test(sample_null_ks, mu=ksp)
loginfo(paste('P-Value for Poisson:', round(pv$p.value, digits=3)))

##------Generate k-s for Power Law-----
fit = fit_power_law(t$Freq, implementation = c("plfit"))
loginfo(paste('KS Statistic for Power Law:', round(fit$KS.stat, digits=3)))
loginfo(paste('P-value for Power Law:', round(fit$KS.p, digits=3)))


