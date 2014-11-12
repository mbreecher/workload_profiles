#predict time

#read in model parameters
setwd("C:/R/workspace/workload_profile/data")
load("aggregate_time_for_workload_model.Rdata")

#read in simplified services from collapsed time
setwd('C:/R/workspace/collapsed_time')
source("import_functions.R")
services <- import_services()
services <- services[services$reportingPeriod %in% c("20144"),]
services$filing_week <- week(services$filing.estimate)