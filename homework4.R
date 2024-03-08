#	Clear environment.
rm(list = ls())

#	Set the working directory.
setwd("C:/Users/frits/Documents/MLitt/ECON50580 PhD Econometrics 2/PhD_Econometrics2")

#	Load haven package.
library("haven")

#	Load data.
data <- read_dta("data_ps4.dta")


#	Question 3.
age.density <- c()
j <- 30
for (i in seq(30, 50, 1/3))
{
	age.density <- append(age.density, nrow(data[data$age > j & data$age <= i,]))
	j <- i
}
age.density <- age.density / sum(age.density)
plot(seq(30, 50, 1/3), age.density,
	xlab = "age at layof", ylab = "density", main = "density of age at layoff")

bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.lwage0 <- aggregate(cbind(data$age, data$lwage0) ~ bins, FUN = mean)
plot(plot.bins.lwage0$V1, plot.bins.lwage0$V2)