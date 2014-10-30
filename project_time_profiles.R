library(RMySQL)
library(reshape2)
library(plyr)
setwd("C:/R/workspace")
source("get_query.r")
setwd("C:/R/workspace/workload_profile")

#grab project and collapsed time data from mysql database
con <- dbConnect(dbDriver("MySQL"), user = "root", password = "", dbname = "revenue_analysis")

sql <- paste("select subcloud.service_id, subcloud.opportunity_id, timelog.is_psm, subcloud.account_name, subcloud.cik, subcloud.registrant_type, 
             subcloud.solution, subcloud.SrPSM, subcloud.PSM, subcloud.CSM, subcloud.Sr_CSM, subcloud.service_name, subcloud.cs_ps, 
             subcloud.service_type, subcloud.form, subcloud.quarter_end, subcloud.filing_date, timelog.logged_date, subcloud.filing_deadline, subcloud.filing_deadline_recalc,
             subcloud.service_status, subcloud.customer_status, subcloud.year_end, subcloud.reporting_period, subcloud.service_period, subcloud.list_price, 
             subcloud.sales_price, timelog.Billable, subcloud.filing_week_num, logged_week_num, relative_week_num, sum(timelog.hours) 
             from subcloud left join timelog 
             on subcloud.service_id collate latin1_bin = timelog.service_id collate latin1_bin
             where subcloud.service_id like 'a0%' and service_status = 'Completed' and is_psm = 1
             group by subcloud.service_id, subcloud.account_name, timelog.is_psm, relative_week_num", sep = "")                

query <- dbGetQuery(con, sql)
dbDisconnect(con)

names(query)[names(query) == "sum(timelog.hours)"] <- "time"
query$relative_week_num <- -query$relative_week_num #reverse relative week int for more intuitive presentation

#aggregate and recast in a wide format to present time by relative weeks
agg_time_long <- aggregate(time ~ account_name + service_name + service_type + form + relative_week_num, FUN = sum, data = query)
agg_time <- dcast(agg_time_long, account_name + service_name + service_type + form ~ relative_week_num, sum, value.var = "time")

#get average hours by project type by relative week
row_header <- c("account_name", "service_name", "service_type", "form")
result <- c()

#report mean, and standard deviation by service type and form by week
means <- ddply(agg_time, .(service_type,form ), numcolwise(mean))
means$type <- "mean"
stdevs <- ddply(agg_time, .(service_type,form ), numcolwise(sd))
stdevs$type <- "std. dev."
result <- rbind(means, stdevs)
result <- result[order(result$service_type, result$form),]
head <- c("service_type", "form", "type")
result <- result[, c(head, names(result)[!(names(result) %in% head)])]

#output
setwd("C:/R/workspace/workload_profile/output")
write.csv(agg_time, file = "aggregate_time.csv", row.names = F, na = "") #detailed collapsed time for each project
write.csv(result, file = "average_time_test.csv", row.names = F, na = "") #averaged time by service and form type

#view time graphically by type
for (i in 1:length(unique(agg_time_long$service_type))){
  for(j in 1:length(unique(agg_time_long$form))){
    loop <-agg_time_long[agg_time_long$service_type %in% unique(agg_time_long$service_type)[i] &
                      agg_time_long$form %in% unique(agg_time_long$form)[j], ]
    if(length(unique(loop$account_name)) > 10){
      s <- ggplot(loop, aes(relative_week_num, time))+geom_line(alpha = .2) + geom_point()   
      ggsave(paste(unique(agg_time_long$service_type)[i]," ", unique(agg_time_long$form)[j], '.png', collapse = ""), plot = s, width = 10.5, height = 7)
    }
  }
}
