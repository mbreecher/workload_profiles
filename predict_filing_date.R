library(plyr)

setwd('C:/R/workspace/shared')
source("import_functions.R")


sec_data <- import_sec()
sec_data$reporting_offset <- as.numeric(sec_data$reporting_offset)
sec_data$actual_offset <- as.numeric(sec_data$filing_date - sec_data$period_end_date)

#data includes lots of odd cases like filing for historical periods and amendments. 
#So, we're going to limit the study to initial filings within the defined filing window.
sec_data <- sec_data[sec_data$actual_offset <= sec_data$reporting_offset & 
                       sec_data$actual_offset >= 0 & 
                       sec_data$form %in% c("10-Q", "10-K"),]

str <- proc.time()
predicted_offset <- ddply(sec_data, .var = c("reporting_offset", "cik", "form"), .fun = function(x){
  mean <- mean(x$actual_offset)
  sd <- sd(x$actual_offset)
  n <- dim(x)[1]
  
  data.frame(mean = mean, sd = sd, n = n)

})
proc.time() - str