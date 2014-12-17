library(RMySQL)
library(reshape2)
library(plyr)
library(ggplot2)
library(xlsx)

setwd("C:/R/workspace/shared")
source("transformations.R")

setwd("C:/R/workspace/workload_profile")
source("helpers.r")
source("plot_functions.r")

relative_time <- weekly_time()

#aggregate and recast in a wide format to present time by relative weeks
agg_time_long <- aggregate(Hours ~ Account.Name + Service.Name + Service.Type + reportingPeriod + Form.Type + relative_week_num, FUN = sum, data = relative_time)
agg_time <- dcast(agg_time_long, Account.Name + Service.Name + Service.Type + reportingPeriod + Form.Type ~ relative_week_num, sum, value.var = "Hours")
# week_nums <- names(agg_time)[!(names(agg_time) %in% c("account_name", "service_type", "reporting_period", "form", "type"))] 
agg_time_long <- melt(agg_time, id.vars = c("Account.Name", "Service.Name", "Service.Type", "reportingPeriod", "Form.Type"), 
                      variable.name="relative_week_num",
                      value.name="Hours")
#melt made the relative week number a factor, so we need to fix. and change S1/S4 and N/A to writable form
agg_time_long$relative_week_num <- as.numeric(as.character(agg_time_long$relative_week_num))
agg_time_long[agg_time_long$Form.Type %in% c("S1/S4"),]$Form.Type <- c("S1.S4")
agg_time_long[agg_time_long$Form.Type %in% c("N/A"),]$Form.Type <- c("NA")


#avg time by week by type
avg_time_by_type <- ddply(agg_time_long, c("Service.Type", "Form.Type", "relative_week_num"), summarise,
                                    n = length(unique(Account.Name)),
                                     sum = sum(Hours),
                                     mean = mean(Hours),
                                     sd = sd(Hours),
                                     se = sd / sqrt(length(Hours)))

#plots with weekly time by project
for (i in 1:dim(unique(agg_time_long[,c("Service.Type", "Form.Type")]))[1]){
    s <- plot_all_time(agg_time_long[agg_time_long$Service.Type %in% unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,1] &
                                          agg_time_long$Form.Type %in% unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,2] &
                                       !(agg_time_long$Hours %in% 0),])
    setwd("C:/R/workspace/workload_profile/output/all_time")
    if (!is.null(s)){ggsave(paste(unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,1]," ", 
                                  unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,2],
                                  "-all", '.png', sep = ""), 
           plot = s, width = 10.5, height = 7)}
}

setwd("C:/R/workspace/workload_profile")
source("plot_functions.r")
#plots with average weekly time by project
for (i in 1:dim(unique(avg_time_by_type[,c("Service.Type", "Form.Type")]))[1]){
  s <- plot_averages(avg_time_by_type[avg_time_by_type$Service.Type %in% unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,1] &
                                     avg_time_by_type$Form.Type %in% unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,2],])
  setwd("C:/R/workspace/workload_profile/output/weekly_time")
  if (!is.null(s)){ggsave(paste(unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,1]," ",
                                unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,2], 
                                "-avg", '.png', sep = ""), 
                          plot = s, width = 10.5, height = 7)}
}


# model by quarter to see if some should be excluded, or if some should be modeled differently
avg_time_by_quarter_by_type <- ddply(agg_time_long, c("Service.Type", "Form.Type", "reportingPeriod", "relative_week_num"), summarise,
                                     sum = sum(Hours),
                                     mean = mean(Hours),
                                     sd = sd(Hours),
                                     se = sd / sqrt(length(Hours)))

setwd("C:/R/workspace/workload_profile")
source("plot_functions.r")
#plots with average weekly time by project by quarter
for (i in 1:dim(unique(avg_time_by_quarter_by_type[,c("Service.Type", "Form.Type")]))[1]){
    s <- plot_averages_by_quarter(avg_time_by_quarter_by_type[avg_time_by_quarter_by_type$Service.Type %in% unique(avg_time_by_quarter_by_type[,c("Service.Type", "Form.Type")])[i,1] &
                                          avg_time_by_quarter_by_type$Form.Type %in% unique(avg_time_by_quarter_by_type[,c("Service.Type", "Form.Type")])[i,2],])
    setwd("C:/R/workspace/workload_profile/output/weekly_time")
    if (!is.null(s)){ggsave(paste(unique(avg_time_by_quarter_by_type[,c("Service.Type", "Form.Type")])[i,1], " ", 
                                  unique(avg_time_by_quarter_by_type[,c("Service.Type", "Form.Type")])[i,2], "-avg_by_qtr", '.png', sep = ""), 
                            plot = s, width = 10.5, height = 7)}
}

#output csv versions for review
# setwd("C:/R/workspace/workload_profile/output")
# write.csv(agg_time, file = "aggregate_time.csv", row.names = F, na = "") #detailed collapsed time for each project
# write.csv(agg_time_long, file = "aggregate_time_long.csv", row.names = F, na = "") #detailed collapsed time for each project
# write.csv(avg_time_by_type, file = "average_time.csv", row.names = F, na = "") #averaged time by service and form type
# write.csv(avg_time_by_quarter_by_type, file = "average_time_by_quarter.csv", row.names = F, na = "") #averaged time by service and form type

#playing with plots and lms
service_types <- c("Standard Import", "Roll Forward", "Full Service Roll Forward", "Detail Tagging")
form_types <- c("10-Q", "10-K", "Q-Q", "Q-K", "K-Q", "K-K")
agg_time_model <- aggregate(time ~ Service.Type + Form.Type + relative_week, data = agg_time_long,  FUN = "mean")
agg_time_model <- agg_time_model[agg_time_model$Service.Type %in% service_types & agg_time_model$Form.Type %in% form_types,]
agg_time_model <- agg_time_model[agg_time_model$relative_week <= 3 & agg_time_model$relative_week >= -20, ]
agg_time_model <- agg_time_model[agg_time_model$time >= 1,]
model_params <- ddply(agg_time_model, c("Service.Type", "Form.Type"), summarise,
                          num = length(time),
                          sum = sum(time),
                          mean = mean(time),
                          sd = sd(time),
                          se = sd / sqrt(length(time)),
                          lm0 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[1],
                          lm1 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[2],
                          lm2 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[3],
                          lm3 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[4],
                          lm4 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[5]
                      )

setwd("C:/R/workspace/workload_profile/data")
save(model_params, file = "models.Rdata")
save(agg_time_model, file = "aggregate_time_for_workload_model.Rdata")

#excel output to multiple tabs
write.xlsx(x = timelog_by_week, file = "time_by_week.xlsx",sheetName = "timelog_by_week", row.names = FALSE)

#build spreadsheet with total time by project type in tabs
for (i in 14:dim(unique(agg_time_long[,c("Service.Type", "Form.Type")]))[1]){
  s <- agg_time_long[agg_time_long$Service.Type %in% unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,1] &
                                     agg_time_long$Form.Type %in% unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,2] &
                                     !(agg_time_long$Hours %in% 0),]
  setwd("C:/R/workspace/workload_profile/output") 
  if (!is.null(s)){
      write.xlsx(x = s, file = "agg_time_long.xlsx",sheetName = 
                   paste(unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,1]," ", 
                         unique(agg_time_long[,c("Service.Type", "Form.Type")])[i,2], sep = ""), 
                 row.names = FALSE, append = TRUE)
   }
}

#build spreadsheet with average time by project type by week in tabs
for (i in 1:dim(unique(avg_time_by_type[,c("Service.Type", "Form.Type")]))[1]){
  s <- avg_time_by_type[avg_time_by_type$Service.Type %in% unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,1] &
                          avg_time_by_type$Form.Type %in% unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,2] &
                       !(avg_time_by_type$sum %in% 0) & avg_time_by_type$relative_week_num <= 0,]
  setwd("C:/R/workspace/workload_profile/output") 
  if (!is.null(s)){
    write.xlsx(x = s, file = "avg_time_by_type.xlsx",sheetName = 
                 paste(unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,1]," ", 
                       unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,2], sep = ""), 
               row.names = FALSE, append = TRUE)
  }
}

# weekly time by project type export
# setwd("C:/R/workspace/workload_profile/output")
# write.csv(model_params, file = "model_params.csv", row.names = F, na = "") #parameters for lm
# write.csv(agg_time_model, file = "model_inputs.csv", row.names = F, na = "") #parameters for lm

  
# plot_model <- agg_time_model[agg_time_model$service_type %in% c("Standard Import") & 
#                                agg_time_model$form %in% c("10-Q"),]
# ggplot(data = plot_model,aes( x = relative_week, y = time)) + 
#   geom_point(alpha=.5) + 
#   stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red")


