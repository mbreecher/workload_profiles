setwd("C:/R/workspace/worload_profiles")
source("predict_time.R")

totals_p <- aggregate(predicted ~ calendar_week, workload_predicted[workload_predicted$predicted >0 &
                                                                      !is.na(workload_predicted$predicted),], FUN = sum)
totals_psm <- aggregate(predicted ~ PSM + calendar_week, workload_predicted[workload_predicted$predicted >0 ,], FUN = sum)
loaded <- unique(totals_psm[totals_psm$predicted >= 60 & !is.na(totals_psm$predicted),]$PSM)
heavy_load <- aggregate(predicted ~ PSM + calendar_week, workload_predicted[workload_predicted$predicted >0 & workload_predicted$PSM %in% loaded ,], FUN = sum)


#some plots
total_plot <- ggplot(totals_p, aes(calendar_week, predicted)) + geom_point() + geom_line(color = "lightgreen", size = 2, alpha=.5) +
  theme(axis.text.y = element_blank()) +
  scale_x_continuous(breaks = c(0, 10,12), labels = c("Jan 1", "Mar 1", "Mar 16")) +
  ggtitle("PS Team Expected Workload by week")
# + stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red")
by_psm_plot <- ggplot(totals_psm, aes(calendar_week, predicted, color = PSM)) + 
  geom_point() + geom_line() +
  guides(col = guide_legend(ncol =3))
high_psm_plot <- ggplot(heavy_load, aes(calendar_week, predicted, color = PSM)) + 
  geom_point() + geom_line()