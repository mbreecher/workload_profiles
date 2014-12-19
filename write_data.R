setwd("C:/R/workspace/workload_profile")
source("project_time_profiles.R")

#excel output to multiple tabs
write.xlsx(x = timelog_by_week, file = "time_by_week.xlsx",sheetName = "timelog_by_week", row.names = FALSE)

#build spreadsheet with total time by project type in tabs
for (i in 1:dim(unique(agg_time_long[,c("Service.Type", "Form.Type")]))[1]){
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
                          !(avg_time_by_type$sum_psm %in% 0 & avg_time_by_type$sum_srpsm %in% 0) & avg_time_by_type$relative_week_num <= 0,]
  setwd("C:/R/workspace/workload_profile/output") 
  if (!is.null(s)){
    write.xlsx(x = s, file = "avg_time_by_type_and_role.xlsx",sheetName = 
                 paste(unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,1]," ", 
                       unique(avg_time_by_type[,c("Service.Type", "Form.Type")])[i,2], sep = ""), 
               row.names = FALSE, append = TRUE)
  }
}

setwd("C:/R/workspace/workload_profile/output")
write.csv(collapsed, file = "summarized_collapsed_time.csv", row.names = F, na = "")

# weekly time by project type export
# setwd("C:/R/workspace/workload_profile/output")
# write.csv(model_params, file = "model_params.csv", row.names = F, na = "") #parameters for lm
# write.csv(agg_time_model, file = "model_inputs.csv", row.names = F, na = "") #parameters for lm