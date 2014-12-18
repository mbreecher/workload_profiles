setwd("C:/R/workspace/worload_profiles")
source("project_time_profiles.R")

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