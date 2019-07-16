## Title: First Prediction Tests
## Author: Raquel Buezo

### Load historic logs and make first predictions

# install.packages('data.table')
# install.packages('lubridate')
library(data.table)
library(lubridate)

setwd('~/Documents/Practicum/')

logs <- fread(file = './dataframes/historicLogData.csv', sep = ',', stringsAsFactors = TRUE, header = TRUE)

#setkey(logs, instance)

# Initial statistics

summary(logs)
str(logs)

# NAs in my data?
apply(logs, 2, function (x) sum(is.na(x)))

# Clean it
logs_cln <- na.omit(logs)
apply(logs_cln, 2, function (x) sum(is.na(x)))

# What are the -1?
sum(logs_cln$usage_cpu==-1)
sum(logs_cln$usage_nwin==-1)
sum(logs_cln$usage_nwout==-1)

badData <- logs_cln[logs_cln$usage_cpu==-1,]
summary(badData)

logs_cln[logs_cln$usage_cpu==-1,] <- NA
apply(logs_cln, 2, function (x) sum(is.na(x)))
logs_cln <- na.omit(logs_cln)
apply(logs_cln, 2, function (x) sum(is.na(x)))


# maybe do boxplot or histogram of this

#idk why this doesnt work
#apply(logs_cln, 2, function (x) sum(x==-1))


X <- logs_cln[, c('instance', 'timestamp','usage_cpu')]

test <- lm(usage_cpu~., X)

# Summary results: 
  # Residual standard error: 5.925 on 1153033 degrees of freedom
  # (9889 observations deleted due to missingness)
  # Multiple R-squared:  0.1901,	Adjusted R-squared:  0.1898 
  # F-statistic: 727.3 on 372 and 1153033 DF,  p-value: < 2.2e-16

X2 <- logs_cln[, c('instance', 'timestamp','usage_cpu','usage_nwin', 'usage_nwout')]

test2 <- lm(usage_cpu~., X2)

# Summary results for test2:
  # Residual standard error: 4.454 on 1151915 degrees of freedom
  # (11005 observations deleted due to missingness)
  # Multiple R-squared:  0.5422,	Adjusted R-squared:  0.542 
  # F-statistic:  3648 on 374 and 1151915 DF,  p-value: < 2.2e-16

X3 <- logs_cln[,c('timestamp','usage_cpu','usage_nwin', 'usage_nwout')]

cor(X3)
