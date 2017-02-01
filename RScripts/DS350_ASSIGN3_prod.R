##--------------------------------------------
##
##  Farm-Subsidies Hypothesis Testing - Assignment 3
##  Class: PCE Data Science Methods Class
##  Amy Werner-Allen
##  January 31st 2017
## 
##  Datasets located:
##  http://www.fsa.usda.gov/FSA/webapp?area=newsroom&subject=landing&topic=foi-er-fri-pfi
##
##   Need:
##    -2011 Farm Payment File (27MB) txt file
##    -State and County Code List (374KB) xls file (probably convert this to csv)
##
##--------------------------------------------

##----Import Libraries-----
require(RSQLite)
require(logging)
require(maps)
require(ggplot2)

##----Hypotheses to test-----
#
#   Test these two things:
#   1.  Does our sample equally represent all 50 states?
#   2.  Does our sample equally represent all 50 states, weighted by number of farms/state?
#
#   Set workding directory to folder with files
#   setwd("~/Documents/UW/Semester2/Assignments")

##-----Declare Functions Here-----

get_log_filename = function(){
  log_file_name = format(Sys.time(), format="AW_HW3_log_%Y_%m_%d_%H%M%S.log")
  return(log_file_name)
}

trim = function (x) gsub("^\\s+|\\s+$", "", x)

##----Run Main Function Here----
if(interactive()){
  
  ##----Setup Test Logger-----
  log_file_name = get_log_filename()
  basicConfig()
  addHandler(writeToFile, file=log_file_name, level='DEBUG')
  
  ##-----Read in the data-----
  setwd("~/Documents/UW/Semester2/Assignments")
  loginfo(paste('Loading in data:', Sys.time()))
  data = read.csv("CAS.WDC11019.PMT11.FINAL.DT11186.TXT", sep=";",
                  header=FALSE, stringsAsFactors=FALSE)
  loginfo(paste('Loaded in data:', Sys.time()))
  loginfo(paste('Total rows in data frame:', dim(data)[1]))
  
  ##----Trim Whitespaces and Make Numbers Numeric-----
  loginfo(paste('Cleaning data:', Sys.time()))
  data = as.data.frame(apply(data,2,trim), stringsAsFactors=FALSE)
  
  names(data) = c('state_code', 'county_code', 'cust_num', 'program_code', 'program_year',
                  'commodity_code', 'amount', 'date', 'cat_code', 'farm_num',
                  'calendar_year', 'fiscal_year', 'seq_num')
  
  data$amount = as.numeric(gsub(",", "", data$amount))
  loginfo(paste('Done cleaning data:', Sys.time()))
  
  ##------Read State/County File-----
  county_state_codes = read.csv("foia_state_county_codes.csv", stringsAsFactors=FALSE)
  county_state_codes$state_code = county_state_codes$Stcd
  county_state_codes$Stcd = NULL
  county_state_codes$county_code = county_state_codes$Cntycd
  county_state_codes$Cntycd = NULL
  
  ##----Merge files together and add full state name----
  data = merge(data, county_state_codes, by=c("state_code", "county_code"), all.x=TRUE)
  data$region = tolower(state.name[match(data$ST,state.abb)])
  
  ##-----Probably do some data exploration----
  
  # First just a map of state data
  # We can see that the midwest has the most number of farms
  states = map_data("state")
  total_by_state = as.data.frame(table(data$region))
  colnames(total_by_state) = c("region", "total")
  
  gg = ggplot() + geom_map(data=states, map=states,
                      aes(x=long, y=lat, map_id=region),
                      fill="#ffffff", color="#ffffff", size=0.15)
  gg = gg + geom_map(data=total_by_state, map=states,
                      aes(fill=total, map_id=region),
                      color="#ffffff", size=0.15)
  gg = gg + scale_fill_continuous(low='thistle2', high='darkred', 
                                   guide='colorbar')
  gg = gg + labs(title="Number of Subsidized Farms by State") + labs(x=NULL, y=NULL)  + theme(panel.border = element_blank()) + theme(panel.background = element_blank()) + theme(axis.ticks = element_blank()) + theme(axis.text = element_blank())
  
  # Next we can look at average amount per state
  # We can see there are a few outliers (e.g. RI)
  t = aggregate(data$amount, by=list(Category=data$ST), FUN=mean)
  t2 = as.data.frame(table(data$ST))
  t = cbind(t, t2$Freq)
  colnames(t) = c("State", "AvgAmount", "TotalSub")
  
  ggplot(t, aes(x=State, y=AvgAmount, fill=TotalSub)) +
    geom_histogram(stat="identity") +
    theme(panel.grid.minor = element_line(colour = "red", linetype = "dotted"), panel.background = element_rect(colour = "darkblue"))+
    labs(y="Average Sub Amount")
  
  ##----Perform a test for equal representation-----
  
  # (1) check whether the number of subsidies is distributed equally across all states
  # We will use total counts by state
  
  # expected frequency is 1/50:
  prob1 = rep(1/50, 50)
  
  # actual frequency:
  freq1 = total_by_state$total
  
  # chi-squared test:
  results1 = chisq.test(freq, p=prob1) 
  
  # (2) check whether the amount of subsidies is distributed equally across all states
  # We will use total amount given out by state
  
  amts = aggregate(data$amount, by=list(Category=data$ST), FUN=sum)
  colnames(amts) = c("State", "TotalAmt")
  freq2 = amts$TotalAmt
  
  # chi-squared test:
  results2 = chisq.test(freq2, p=prob) 
  
  ##----Access the farms/state data and derive weights-----
  
  total_farms = read.csv("FarmsPerState.csv", header=TRUE)
  s = sum(total_farms$Farms)
  total_farms$Weighted = total_farms$Farms/s
  total_farms$Expected = trunc(total_farms$Weighted*sum(total_by_state$total))
  
  farm_acres = read.csv("FarmsAcresByState.csv", header=TRUE)
  farm_acres$AcresE3 = as.numeric(gsub(",", "", farm_acres$AcresE3))
  s = sum(farm_acres$AcresE3)
  farm_acres$Weighted = farm_acres$AcresE3/s
  farm_acres$Expected = trunc(farm_acres$Weighted*sum(total_by_state$total))
  
  ##----Perform a test for equal representation by farms/state-----
  
  # (1) compare by weighted-total-farms
  prob2 = total_farms$Weighted
  results3 = chisq.test(freq, p=prob2) 
  
  # (2) compare by weighted-total-acres
  prob3 = farm_acres$Weighted
  results4 = chisq.test(freq, p=prob3) 
  
  as.data.frame(cbind(as.character(total_by_state$region), total_by_state$total, total_farms$Expected, farm_acres$Expected))
  
  ##----Output Results----
  # Acceptable to print output, log output, save in DB, or write to file. Your choice.
  
  loginfo(paste('Results of ChiSq Representation Test:', 'X-Squared', results1$statistic, 'p-value', results1$p.value, sep='\n'))
  loginfo(paste('Results of ChiSq Test Weighted by Farms by State:', 'X-Squared', results3$statistic, 'p-value', results3$p.value, sep='\n'))
  loginfo(paste('Results of ChiSq Test Weighted by Acres by State:', 'X-Squared', results4$statistic, 'p-value', results4$p.value, sep='\n'))
  
}




