
#plots with weekly time by project
plot_all_time <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week > week_min & plot_time$relative_week < week_max, ]
#   browser()
  if(dim(loop)[1] > 10){
    s <- ggplot(loop, aes(relative_week, time)) + geom_jitter(alpha = .4, size = 3) +
      stat_smooth(method = "lm", se=T, formula = y ~ poly(x, 4), color = "red") +
      annotate("text", x=min(loop$relative_week), y=max(loop$time), 
               label=lm_eqn(loop$relative_week, loop$time, 4), hjust=0, size=8, parse=TRUE)
    s
  }else{
    NULL
  }
}

plot_averages <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week > week_min & plot_time$relative_week < week_max & plot_time$mean >= 1, ]
  #   browser()
  if(dim(loop)[1] > 10){
    s <- ggplot(loop, aes(relative_week, mean)) + geom_jitter(alpha = .4, size = 3) +
      stat_smooth(method = "lm", se=T, formula = y ~ poly(x, 4), color = "red") +
      annotate("text", x=min(loop$relative_week), y=max(loop$mean), 
               label=lm_eqn(loop$relative_week, loop$mean, 4), hjust=0, size=8, parse=TRUE)
    s
  }else{
    NULL
  }
}

plot_averages_by_quarter <- function(plot_time, week_min = -20, week_max = 3){
  loop <-plot_time[plot_time$relative_week > week_min & plot_time$relative_week < week_max & plot_time$mean >= 1, ]
  #   browser()
  if(dim(loop)[1] > 10){
    s <- ggplot(loop, aes(relative_week, mean,color = factor(reporting_period))) + geom_jitter(alpha = .4, size = 3) + geom_line() +
      stat_smooth(method = "lm", se=T, formula = y ~ poly(x, 4), color = "red") +
      annotate("text", x=min(loop$relative_week), y=max(loop$mean), 
               label=lm_eqn(loop$relative_week, loop$mean, 4), hjust=0, size=8, parse=TRUE)
    s
  }else{
    NULL
  }
}


