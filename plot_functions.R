
#plots with weekly time by project
plot_all_time <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week_num > week_min & plot_time$relative_week_num < week_max, ]
#   browser()
  if(dim(loop)[1] > 10){
    s <- ggplot(loop, aes(relative_week_num, Hours)) + geom_jitter(alpha = .4, size = 3) +
      stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red") +
      annotate("text", x=min(loop$relative_week_num), y=max(loop$Hours), 
               label=lm_eqn(loop$Hours,loop$relative_week_num), hjust=0, size=6, parse=TRUE)
    s
  }else{
    NULL
  }
}

plot_averages <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week_num > week_min & plot_time$relative_week_num < week_max & plot_time$mean >= 1, ]
  #   browser()
  if(dim(loop)[1] > 4){
    s <- ggplot(loop, aes(relative_week_num, mean)) + geom_point(alpha = .4, size = 3) +
      stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red") +
      annotate("text", x=min(loop$relative_week_num), y=max(loop$mean), 
               label=lm_eqn(loop$mean, loop$relative_week_num), hjust=0, size=6, parse=TRUE)
    s
  }else{
    NULL
  }
}

plot_averages_by_quarter <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week_num > week_min & plot_time$relative_week_num < week_max & plot_time$mean >= 1, ]
  #   browser()
  if(dim(loop)[1] > 10){
    s <- ggplot(loop, aes(relative_week_num, mean,color = factor(reportingPeriod))) + 
      geom_jitter(alpha = .4, size = 3) + 
      geom_line()
    s
  }else{
    NULL
  }
}


