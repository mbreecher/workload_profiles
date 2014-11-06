library(RMySQL)
library(reshape2)
library(plyr)
library(ggplot2)
setwd("C:/R/workspace")
source("get_query.r")
setwd("C:/R/workspace/workload_profile")
load("db_creds.Rdata")
source("helpers.r")

#grab project and collapsed time data from mysql database
con <- dbConnect(dbDriver("MySQL"), user = username, password = password, dbname = "revenue_analysis")

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
#use ddply numcolwise on wide format data
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
write.csv(result, file = "average_time.csv", row.names = F, na = "") #averaged time by service and form type

#view time averages graphically by type
# recast from query using ddply and summarize
# plot_time <- ddply(query, .(service_type,form, relative_week_num ), summarize, means = mean(time))
# plot_time <- query[, c("service_type", "form", "relative_week_num", "time")]
#need to reshape long to include all the zeroes for correct averages
week_nums <- names(result)[!(names(result) %in% c("service_type", "form", "type"))]
plot_time <- reshape(result, varying = week_nums, v.names = "mean", timevar = "relative_week_num", times = week_nums, direction = "long")
plot_time$relative_week_num <- as.numeric(plot_time$relative_week_num)
plot_time[plot_time$form %in% c("S1/S4"),]$form <- "S1_S4" #change "S1/S4" form type so it can be save in windows
plot_time <- plot_time[plot_time$mean > 1 & !is.na(plot_time$mean),]

for (i in 1:length(unique(plot_time$service_type))){
  for(j in 1:length(unique(plot_time$form))){
    #loop <-agg_time_long[agg_time_long$service_type %in% unique(agg_time_long$service_type)[i] &
    #                  agg_time_long$form %in% unique(agg_time_long$form)[j], ]
    loop <-plot_time[plot_time$service_type %in% unique(plot_time$service_type)[i] &
                       plot_time$form %in% unique(plot_time$form)[j] &
                       plot_time$relative_week_num > -20 & plot_time$relative_week_num < 3 &
                       plot_time$type %in% c("mean"), ]
    if(dim(loop)[1] > 10){
      s <- ggplot(loop, aes(relative_week_num, mean)) + geom_jitter(alpha = .4, size = 3) +
                  stat_smooth(method = "lm", se=T, formula = y ~ poly(x, 4), color = "red") +
                  annotate("text", x=min(loop$relative_week_num), y=max(loop$mean), 
                  label=lm_eqn(loop$relative_week_num, loop$mean, 4), hjust=0, size=8, parse=TRUE)
                  #family="Times", face="italic", parse=TRUE)
#                   theme(plot.title = lm_eqn(loop, loop$relative_week_num, loop$mean, 4))
      ggsave(paste(unique(plot_time$service_type)[i]," ", unique(plot_time$form)[j], '.png', collapse = ""), plot = s, width = 10.5, height = 7)
    }
  }
}

#playing with plots and lms
plot_model <- plot_time[plot_time$relative_week_num > -20 & plot_time$relative_week_num < 3, ]
plot_model <- plot_model[plot_model$service_type %in% c("Standard Import") & 
                           plot_model$form %in% c("10-Q") &
                           plot_model$type %in% c("mean"),]
ggplot(data = plot_model,aes( x = relative_week_num, y = mean)) + 
  geom_point(alpha=.5) + 
  stat_smooth(method = "lm", se=T, formula = y ~ poly(x, 4), color = "red")
