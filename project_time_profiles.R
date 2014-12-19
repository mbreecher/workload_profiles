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

relative_time <- weekly_time_detail()

#aggregate and recast in a wide format to present time by relative weeks
agg_time_long <- aggregate(Hours ~ Account.Name + Service.Name + Service.Type + reportingPeriod + Form.Type + role + relative_week_num, FUN = sum, data = relative_time)
agg_time <- dcast(agg_time_long, Account.Name + Service.Name + Service.Type + reportingPeriod + Form.Type  + role ~ relative_week_num, sum, value.var = "Hours")
# week_nums <- names(agg_time)[!(names(agg_time) %in% c("account_name", "service_type", "reporting_period", "form", "type"))] 
agg_time_long <- melt(agg_time, id.vars = c("Account.Name", "Service.Name", "Service.Type", "reportingPeriod", "Form.Type", "role"), 
                      variable.name="relative_week_num",
                      value.name="Hours")
#melt made the relative week number a factor, so we need to fix. and change S1/S4 and N/A to writable form
agg_time_long$relative_week_num <- as.numeric(as.character(agg_time_long$relative_week_num))
agg_time_long[agg_time_long$Form.Type %in% c("S1/S4"),]$Form.Type <- c("S1.S4")
agg_time_long[agg_time_long$Form.Type %in% c("N/A"),]$Form.Type <- c("NA")


#avg time by week by type
avg_time_by_type <- ddply(agg_time_long, c("Service.Type", "Form.Type", "relative_week_num"), summarise,
                                    n = length(unique(Account.Name)),
                                    sum_psm = sum(Hours[role %in% c("PSM")]),
                                    sum_srpsm = sum(Hours[role %in% c("Sr PSM")]),
                                    mean_psm = mean(Hours[role %in% c("PSM")]),
                                    mean_srpsm = mean(Hours[role %in% c("Sr PSM")])
)


# model by quarter to see if some should be excluded, or if some should be modeled differently
avg_time_by_quarter_by_type <- ddply(agg_time_long, c("Service.Type", "Form.Type", "reportingPeriod", "role", "relative_week_num"), summarise,
                                     sum = sum(Hours),
                                     mean = mean(Hours),
                                     sd = sd(Hours),
                                     se = sd / sqrt(length(Hours)))

#********************* summary time by role type ***********************************

collapsed <- aggregate(Hours ~ Services.ID + Service.Type + Form.Type + role, data = relative_time, FUN = sum)
collapsed <- ddply(collapsed, .var = c("Service.Type", "Form.Type"),summarise, 
                   n = length(unique(Services.ID)),
                   psm_hours  = sum(Hours[role %in% c("PSM")]), 
                   psm_mean = mean(Hours[role %in% c("PSM")]),
                   srpsm_hours  = sum(Hours[role %in% c("Sr PSM")]), 
                   srpsm_mean = mean(Hours[role %in% c("Sr PSM")]))


