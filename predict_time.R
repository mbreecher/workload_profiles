#dependencies services_for_pshistory_R

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
#use filing date prediction when possible
setwd("C:/R/workspace/workload_profile")
source("predict_filing_date.R")
predicted_filing_date <- ddply(services, .var = c("Services.ID"), .fun = function(x){
  if(x$Form.Type %in% c("Q-Q", "K-Q", "10-Q")){
    loop <- predicted_offset[predicted_offset$cik %in% x$CIK & predicted_offset$form %in% "10-Q",]
  }else if (x$Form.Type %in% c("Q-K", "K-K", "10-K")){
    loop <- predicted_offset[predicted_offset$cik %in% x$CIK & predicted_offset$form %in% "10-K",]
  }
  if(is.null(dim(loop))){
    predicted_date <- x$filing.estimate
  }else{
    if (dim(loop)[1] > 1){loop <- loop[loop$filing_date == max(loop$filing_date),]}
    if (dim(loop)[1] == 0){
      predicted_date <- x$filing.estimate
    }else if(loop$sd <= 4 & !is.na(loop$sd)){
      predicted_date <- x$Quarter.End + loop$mean
    }else{
      predicted_date <- x$filing.estimate
    }  
  }
  data.frame(predicted_filing_week = week(predicted_date))
})

services <- merge(services, predicted_filing_date, by = c("Services.ID"), all.x = T)

# services$filing_week <- week(services$filing.estimate)
services$deadline_filing_week <- format(services$filing.estimate, format = "%W")
services$deadline_filing_week <- as.numeric(services$deadline_filing_week)

services$filing_date_week <- format(services$Filing.Date, format = "%W")
services$filing_date_week <- as.numeric(services$filing_date_week)


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
  workload$deadline_calendar_week <- workload$deadline_filing_week + workload$relative_week #based on filing deadline
  workload$filing_calendar_week <- workload$filing_date_week + workload$relative_week #based on filing date
  workload$predicted_calendar_week <- workload$predicted_filing_week + workload$relative_week #based on estimated filing date

setwd("C:/R/workspace/workload_profile/predict")
write.csv(workload, "workload.csv", row.names = F)
