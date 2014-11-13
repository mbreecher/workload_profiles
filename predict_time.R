library(ggplot2)
library(RMySQL)
library(plyr)
library(reshape2)
library(IDateTime)
#week function a part of 
#predict time

#read in project averages to build model
setwd("C:/R/workspace/workload_profile/data")
load("aggregate_time_for_workload_model.Rdata")

#read in simplified services from collapsed time
setwd('C:/R/workspace/collapsed_time')
source("import_functions.R")
services <- import_services()
services <- services[services$reportingPeriod %in% c("20144"),]
services$filing_week <- week(services$filing.estimate)

head(services)

#averages
  #cast model wide and merge with services to get workload - with averages
  model_time <- dcast(agg_time_model, service_type + form ~ relative_week, sum, value.var = "time")
  workload <- merge(services, model_time, by.x = c("Service.Type", "Form.Type"),
                    by.y = c("service_type", "form"), all.x = T)
  workload <- melt(workload, id.vars = names(services), variable.name = "relative_week", value.name = "time")
  workload$relative_week <- as.numeric(as.character(workload$relative_week))
  workload$calendar_week <- workload$filing_week + workload$relative_week
  
  totals <- aggregate(time ~ calendar_week, workload[workload$time >0 ,], FUN = sum)
  totals_psm <- aggregate(time ~ PSM + calendar_week, workload[workload$time >0 ,], FUN = sum)
  loaded <- unique(totals_psm[totals_psm$time >= 60 & !is.na(totals_psm$time),]$PSM)
  heavy_load <- aggregate(time ~ PSM + calendar_week, workload[workload$time >0 & workload$PSM %in% loaded ,], FUN = sum)
  
  #some plots
  totals_plot <- ggplot(totals, aes(calendar_week, time)) + geom_point()
  by_psm_plot <- ggplot(totals_psm, aes(calendar_week, time, color = PSM)) + 
    geom_point() + geom_line() +
    guides(col = guide_legend(ncol =3))
  high_psm_plot <- ggplot(heavy_load, aes(calendar_week, time, color = PSM)) + 
    geom_point() + geom_line()

#poly models
  modeled_time <- ddply(agg_time_model, c("service_type", "form"), 
                      .fun = function(x){
                        mod_x <- with(x,lm(time ~ poly(relative_week, 4, raw = T)))
                        predictor <- data.frame(relative_week = x$relative_week)
                        predicted_hours <- data.frame(relative_week = x$relative_week, predicted = predict(mod_x, predictor))
                      })
  modeled_time <- dcast(modeled_time, service_type + form ~ relative_week, value.var = "predicted")
  workload_predicted <- merge(services, modeled_time, by.x = c("Service.Type", "Form.Type"),
                  by.y = c("service_type", "form"), all.x = T)
  workload_predicted <- melt(workload_predicted, id.vars = names(services), 
                             variable.name = "relative_week", value.name = "predicted")
  workload_predicted$relative_week <- as.numeric(as.character(workload_predicted$relative_week))
  workload_predicted$calendar_week <- workload_predicted$filing_week + workload_predicted$relative_week

  totals_p <- aggregate(predicted ~ calendar_week, workload_predicted[workload_predicted$predicted >0 &
                                                    !is.na(workload_predicted$predicted),], FUN = sum)
  totals_psm <- aggregate(predicted ~ PSM + calendar_week, workload_predicted[workload_predicted$predicted >0 ,], FUN = sum)
  loaded <- unique(totals_psm[totals_psm$predicted >= 60 & !is.na(totals_psm$predicted),]$PSM)
  heavy_load <- aggregate(predicted ~ PSM + calendar_week, workload_predicted[workload_predicted$predicted >0 & workload_predicted$PSM %in% loaded ,], FUN = sum)
  
  #some plots
  total_plot <- ggplot(totals_p, aes(calendar_week, predicted)) + geom_point() +
    stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red")
  by_psm_plot <- ggplot(totals_psm, aes(calendar_week, predicted, color = PSM)) + 
    geom_point() + geom_line() +
    guides(col = guide_legend(ncol =3))
  high_psm_plot <- ggplot(heavy_load, aes(calendar_week, predicted, color = PSM)) + 
    geom_point() + geom_line()

setwd("C:/R/workspace/workload_profile/output")
write.csv(workload, file = "workload_avg.csv", row.names = F, na = "")
write.csv(workload_predicted, file = "workload_predict.csv", row.names = F, na = "")

#temp plots **
totals_all <- merge(totals, totals_p, by = c("calendar_week"))
ggplot(totals_all) + geom_point(aes(calendar_week, time)) + geom_line(aes(calendar_week, time, color = 'blue')) +
                    geom_point(aes(calendar_week, predicted)) + geom_line(aes(calendar_week, predicted, color = 'red'))
