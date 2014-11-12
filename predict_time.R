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
totals <- ggplot(totals, aes(calendar_week, time)) + geom_point()
by_psm <- ggplot(totals_psm, aes(calendar_week, time, color = PSM)) + 
  geom_point() + geom_line() +
  guides(col = guide_legend(ncol =3))
high_psm <- ggplot(heavy_load, aes(calendar_week, time, color = PSM)) + 
  geom_point() + geom_line()
