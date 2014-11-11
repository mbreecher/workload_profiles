library(RMySQL)
library(reshape2)
library(plyr)
library(ggplot2)
setwd("C:/R/workspace")
source("get_query.r")
setwd("C:/R/workspace/workload_profile")
load("db_creds.Rdata")
source("helpers.r")
source("make_plots.r")

#grab project and collapsed time data from mysql database
con <- dbConnect(dbDriver("MySQL"), user = username, password = password, dbname = "revenue_analysis")

sql <- paste("select subcloud.service_id, subcloud.opportunity_id, timelog.is_psm, subcloud.account_name, subcloud.cik, subcloud.registrant_type, 
             subcloud.solution, subcloud.SrPSM, subcloud.PSM, subcloud.CSM, subcloud.Sr_CSM, subcloud.service_name, subcloud.cs_ps, 
             subcloud.service_type, subcloud.form, subcloud.quarter_end, subcloud.filing_date, timelog.logged_date, subcloud.filing_deadline, subcloud.filing_deadline_recalc,
             subcloud.service_status, subcloud.customer_status, subcloud.year_end, subcloud.reporting_period, subcloud.service_period, subcloud.list_price, 
             subcloud.sales_price, timelog.Billable, subcloud.filing_week_num, logged_week_num, relative_week_num, sum(timelog.hours) 
             from subcloud left join timelog 
             on subcloud.service_id collate latin1_bin = timelog.service_id collate latin1_bin
             where subcloud.service_id like 'a0%' and service_status = 'Completed' and is_psm = 1 and not cs_ps = 'CS'
             group by subcloud.service_id, subcloud.account_name, timelog.is_psm, relative_week_num", sep = "")                

query <- dbGetQuery(con, sql)
dbDisconnect(con)

names(query)[names(query) == "sum(timelog.hours)"] <- "time"
query$relative_week_num <- -query$relative_week_num #reverse relative week int for more intuitive presentation

#aggregate and recast in a wide format to present time by relative weeks
agg_time_long <- aggregate(time ~ account_name + service_name + service_type + reporting_period + form + relative_week_num, FUN = sum, data = query)
agg_time <- dcast(agg_time_long, account_name + service_name + service_type + reporting_period + form ~ relative_week_num, sum, value.var = "time")
# week_nums <- names(agg_time)[!(names(agg_time) %in% c("account_name", "service_type", "reporting_period", "form", "type"))] 
agg_time_long <- melt(agg_time, id.vars = c("account_name", "service_name", "service_type", "reporting_period", "form"), 
                      variable.name="relative_week",
                      value.name="time")
#melt made the relative week number a factor, so we need to fix. and change S1/S4 and N/A to writable form
agg_time_long$relative_week <- as.numeric(as.character(agg_time_long$relative_week))
agg_time_long[agg_time_long$form %in% c("S1/S4"),]$form <- c("S1.S4")
agg_time_long[agg_time_long$form %in% c("N/A"),]$form <- c("NA")


#all-time ggplots
avg_time_by_type <- ddply(agg_time_long, c("service_type", "form", "relative_week"), summarise,
                                     sum = sum(time),
                                     mean = mean(time),
                                     sd = sd(time),
                                     se = sd / sqrt(length(time)))

#plots with weekly time by project
for (i in 1:length(unique(agg_time_long$service_type))){
  for(j in 1:length(unique(agg_time_long$form))){
    s <- plot_all_time(agg_time_long[agg_time_long$service_type %in% unique(agg_time_long$service_type)[i] &
                                          agg_time_long$form %in% unique(agg_time_long$form)[j],])
    setwd("C:/R/workspace/workload_profile/output/all_time")
    if (!is.null(s)){ggsave(paste(unique(agg_time_long$service_type)[i]," ", unique(agg_time_long$form)[j],"-all", '.png', collapse = ""), 
           plot = s, width = 10.5, height = 7)}
  }
}

setwd("C:/R/workspace/workload_profile")
source("plot_functions.r")
#plots with average weekly time by project
for (i in 1:length(unique(avg_time_by_type$service_type))){
  for(j in 1:length(unique(avg_time_by_type$form))){
    s <- plot_averages(avg_time_by_type[avg_time_by_type$service_type %in% unique(avg_time_by_type$service_type)[i] &
                                       avg_time_by_type$form %in% unique(avg_time_by_type$form)[j],])
    setwd("C:/R/workspace/workload_profile/output/weekly_time")
    if (!is.null(s)){ggsave(paste(unique(avg_time_by_type$service_type)[i]," ", unique(avg_time_by_type$form)[j], "-avg", '.png', collapse = ""), 
                            plot = s, width = 10.5, height = 7)}
  }
}


# model by quarter to see if some should be excluded, or if some should be modeled differently
avg_time_by_quarter_by_type <- ddply(agg_time_long, c("service_type", "form", "reporting_period", "relative_week"), summarise,
                                     sum = sum(time),
                                     mean = mean(time),
                                     sd = sd(time),
                                     se = sd / sqrt(length(time)))

setwd("C:/R/workspace/workload_profile")
source("plot_functions.r")
#plots with average weekly time by project by quarter
for (i in 1:length(unique(avg_time_by_quarter_by_type$service_type))){
  for(j in 1:length(unique(avg_time_by_quarter_by_type$form))){
    s <- plot_averages_by_quarter(avg_time_by_quarter_by_type[avg_time_by_quarter_by_type$service_type %in% unique(avg_time_by_quarter_by_type$service_type)[i] &
                                          avg_time_by_quarter_by_type$form %in% unique(avg_time_by_quarter_by_type$form)[j],])
    setwd("C:/R/workspace/workload_profile/output/weekly_time")
    if (!is.null(s)){ggsave(paste(unique(avg_time_by_quarter_by_type$service_type)[i]," ", unique(avg_time_by_quarter_by_type$form)[j], "-avg_by_qtr", '.png', collapse = ""), 
                            plot = s, width = 10.5, height = 7)}
  }
}

#output csv versions for review
setwd("C:/R/workspace/workload_profile/output")
write.csv(agg_time, file = "aggregate_time.csv", row.names = F, na = "") #detailed collapsed time for each project
write.csv(agg_time_long, file = "aggregate_time_long.csv", row.names = F, na = "") #detailed collapsed time for each project
write.csv(avg_time_by_type, file = "average_time.csv", row.names = F, na = "") #averaged time by service and form type
write.csv(avg_time_by_quarter_by_type, file = "average_time_by_quarter.csv", row.names = F, na = "") #averaged time by service and form type

#playing with plots and lms
service_types <- c("Standard Import", "Roll Forward", "Full Service Roll Forward", "Detail Tagging")
form_types <- c("10-Q", "10-K", "Q-Q", "Q-K", "K-Q", "K-K")
agg_time_model <- aggregate(time ~ service_type + form + relative_week, data = agg_time_long,  FUN = "mean")
agg_time_model <- agg_time_model[agg_time_model$service_type %in% service_types & agg_time_model$form %in% form_types,]
agg_time_model <- agg_time_model[agg_time_model$relative_week <= 3 & agg_time_model$relative_week >= -20, ]
agg_time_model <- agg_time_model[agg_time_model$time >= 1,]
model_params <- ddply(agg_time_model, c("service_type", "form"), summarise,
                          num = length(time),
                          sum = sum(time),
                          mean = mean(time),
                          sd = sd(time),
                          se = sd / sqrt(length(time)),
                          lm0 = lm(formula = relative_week ~ poly(time, 4, raw = T))$coef[1],
                          lm1 = lm(formula = relative_week ~ poly(time, 4, raw = T))$coef[2],
                          lm2 = lm(formula = relative_week ~ poly(time, 4, raw = T))$coef[3],
                          lm3 = lm(formula = relative_week ~ poly(time, 4, raw = T))$coef[4],
                          lm4 = lm(formula = relative_week ~ poly(time, 4, raw = T))$coef[5]
                      )
# alternate
# model_params <- c()
# case <- c()
# for (i in 1:dim(unique(agg_time_model[,c("service_type", "form")]))[1]){
#   loop <- agg_time_model[agg_time_model$service_type %in% unique(agg_time_model[,c("service_type", "form")])[i,1] &
#                            agg_time_model$form %in% unique(agg_time_model[,c("service_type", "form")])[i,2],]
#   
#   model_params <- rbind(model_params, lm(formula = loop$relative_week ~ poly(loop$time, 4))$coef)
#   case <- rbind(case, c(unique(agg_time_model[,c("service_type", "form")])[i,1], unique(agg_time_model[,c("service_type", "form")])[i,2]))
# }
# models <- cbind(case,model_params)

setwd("C:/R/workspace/workload_profile/output")
write.csv(model_params, file = "model_params.csv", row.names = F, na = "") #parameters for lm
write.csv(agg_time_model, file = "model_inputs.csv", row.names = F, na = "") #parameters for lm

plot_model <- agg_time_model[agg_time_model$service_type %in% c("Standard Import") & 
                               agg_time_model$form %in% c("10-Q"),]
ggplot(data = plot_model,aes( x = relative_week, y = time)) + 
  geom_point(alpha=.5) + 
  stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red")
