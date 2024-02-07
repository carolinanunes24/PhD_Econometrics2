# PhD Econometrics 2
# Homework 1

# Bin Zhang (23201579)
# Carolina Nunes (23201320)
# Wanying Deng (23201604)
# Frederik Ledoux (23202214)

# Load libraries
library(tidyverse)
library(stargazer)
library(data.table)

# Set seed for replication
set.seed(11)

# Simulate the dataset
data = data.table(female = ifelse(runif(50000) >= 0.5, 1, 0))
data[,ability := runif(50000, 0, 1)]
data[,discrimination := female]
data[,occupation := rbinom(50000, 1, (data$female + data$ability + data$discrimination)/3)]
data[,earnings := 0.5 - 5*discrimination + 2*occupation + 4*ability + rnorm(50000)]

# Estimate the regressions
reg_1 = lm(earnings ~ discrimination, data)
reg_2 = lm(earnings ~ discrimination + occupation, data)
reg_3 = lm(earnings ~ discrimination + occupation + ability, data)

# Output
stargazer(reg_1,reg_2,reg_3, type = "text", 
          column.labels = c("Direct causal effect", 
                            "Controlling only for collider O",
                            "Controlling for both A and O"))
