library(ggplot2)
library(gridExtra)
library(MASS)

#code to populate query variable (if necessary)
setwd("C:/R/workspace/shared")
source("get_query.R")
query <- load_query()

data <- subset(query, time <= 105
               & time >= 1
               & service_type == "Detail Tagging"
               & form == "10-K"
               & !is.na(time)
               & !is.na(avg_q)
               & avg_q > 0
               & period != '2013Q1'
               & period != '2012Q2'
               & period != '2012Q3'
               & period != '2012Q4')

# dev.copy(png,'10-Q Standard Import 1-75 hours.png', width = 1050, height = 700)
#residuals of fact count ------------------------Q
#scatter and boxplot based on fact count
g <- ggplot(data, aes(avg_q, time, na.rm = TRUE))
g <- g + geom_point(col = "azure4", shape = 16,alpha = .5, size = 4)
g <- g + scale_color_discrete(name = "Plot Type", labels = c("Density", "Jitter"))
g <- g + geom_smooth(size = 1, col = "coral", linetype = 1, method = "lm", se = FALSE)
g <- g + xlab("average 10-Q fact count")
g <- g + ggtitle("linear model to estimate time for 10-Q Full Service RF with average 10-Q as a predictor")
g

fit_avg <-  lm(time ~ avg_q , data)
fit_dist_avg <- fitdistr(fit_avg$residuals, "normal", na.rm = TRUE)
#stat_smooth(method = "lm", se=F, formula = y ~ poly(x, 4), color = "red") +
  #lm0 = lm(formula = time ~ poly(relative_week, 4, raw = T))$coef[1]
#plots for residuals and residual density
s_plot <- ggplot(data, aes(avg_q, time-fit_avg$coef[2]* avg_q-fit_avg$coef[1], na.rm = TRUE)) 
s_plot <- s_plot + geom_point(col = "azure4", shape = 16, alpha = .5, size = 4) #+ geom_rug(col = "darkred", alpha = .3)
s_plot <- s_plot + geom_hline(aes(yintercept = 0), size = 1.33, col = "coral")
s_plot <- s_plot + 
  ylab("residuals") +
  xlab("average 10-Q fact count") +
  ggtitle("residuals of above linear model  (actual value - predicted value)")
plot_right <- ggplot(data, aes(time-fit_avg$coef[2]* avg_q-fit_avg$coef[1])) 
plot_right <- plot_right + geom_density (fill = "azure4", alpha= .7) + coord_flip() + xlab("residuals")
plot_right <- plot_right + stat_function(fun = dnorm, geom= "density", 
                                         arg = list(mean = fit_dist_avg$estimate[1], sd = fit_dist_avg$estimate[2]), 
                                         color = "darkolivegreen", fill = "darkolivegreen1", alpha= .3) + theme(legend.position = "none")
plot_right <- plot_right + ggtitle("fitted normal and residual")


empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )

fit_dist_avg_ln <- fitdistr(data$time, "normal", na.rm = TRUE)

plot_ln <- ggplot(data, aes(time)) 
plot_ln <- plot_ln + geom_density (fill = "azure4", alpha= .7) + coord_flip() + xlab("time")
plot_ln <- plot_ln + stat_function(fun = dlnorm, geom= "density", color = "darkolivegreen", fill = "darkolivegreen1", alpha= .4,
            arg = list(meanlog = fit_dist_avg_ln$estimate[1], sdlog = fit_dist_avg_ln$estimate[2])) + 
            theme(legend.position = "none")
plot_ln <- plot_ln + ggtitle("fitted log-normal and time")

grid.arrange(g, empty, s_plot, plot_right,  ncol= 2,nrow = 2, widths=c(4, 1))

# output plot
# dev.copy(png,'Q-Q Roll Forward 1-45 hours.png', width = 1050, height = 700)
# dev.off()
#qqnorm(fit_avg$residuals)
#qqline(fit_avg$residuals)