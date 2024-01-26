library(tidyverse)
library(stargazer)
library(data.table)

data = data.table(female = ifelse(runif(50000) >= 0.5, 1, 0))
data[,ability := runif(50000, 0, 1)]
data[,discrimination := female]
data[,occupation := rbinom(50000, 1, (data$female + data$ability)/2)]
data[,earnings := 0.5 - 5*discrimination + 2*occupation + 4*ability + rnorm(50000)]

reg_1 = lm(earnings ~ discrimination, data)
reg_2 = lm(earnings ~ discrimination + occupation, data)
reg_3 = lm(earnings ~ discrimination + occupation + ability, data)

stargazer(reg_1,reg_2,reg_3, type = "text", 
          column.labels = c("Direct causal effect", 
                            "Controlling only for collider O",
                            "Controlling for both A and O"))
