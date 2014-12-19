library(ggplot2)
library(RMySQL)
library(plyr)
library(reshape2)

#read in project averages to build model
# setwd("C:/R/workspace/workload_profile/data")
# load("aggregate_time_for_workload_model.Rdata")

# import tailored estimates from excel
setwd("C:/R/workspace/workload_profile/data")
agg_time_model <- read.csv("excel_params.csv", header = T, stringsAsFactors = F)

#read in simplified services from collapsed time
setwd('C:/R/workspace/shared')
source("import_functions.R")
services <- import_services()
services <- services[services$reportingPeriod %in% c("20144"),]
services$filing_week <- week(services$filing.estimate)


  #averages
  #cast model wide and merge with services to get workload based on workload profiles
  model_time_ps <- dcast(agg_time_model, service_type + form ~ relative_week, sum, value.var = "psm_time")
  model_time_srps <- dcast(agg_time_model, service_type + form ~ relative_week, sum, value.var = "srpsm_time")
  workload_ps <- merge(services, model_time_ps, by.x = c("Service.Type", "Form.Type"),
                    by.y = c("service_type", "form"), all.x = T)
  workload_srps <- merge(services, model_time_srps, by.x = c("Service.Type", "Form.Type"),
                     by.y = c("service_type", "form"), all.x = T)
  workload_ps <- melt(workload_ps, id.vars = names(services), variable.name = "relative_week", value.name = "psm_time")
  workload_srps <- melt(workload_srps, id.vars = names(services), variable.name = "relative_week", value.name = "srpsm_time")
  
  
  #join PS and Sr PS dataframes
  workload_srps <- workload_srps[,names(workload_srps) %in% c("Services.ID", "relative_week", "srpsm_time")]
  workload <- merge(workload_ps, workload_srps, by = c("Services.ID", "relative_week"))
  workload$relative_week <- as.numeric(as.character(workload$relative_week))
  workload$calendar_week <- workload$filing_week + workload$relative_week
