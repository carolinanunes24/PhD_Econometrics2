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
age.density[1] <- NA
age.density <- age.density / sum(age.density, na.rm = TRUE)
plot(seq(30, 50, 1/3), age.density,
	xlab = "age at layoff", ylab = "density", main = "density of age at layoff",
	ylim = c(0, 0.02))
abline(v = 40, lty = "dashed")

reg.data <- data.frame("age" = seq(30, 50, 1/3), "density" = age.density)
reg.pre <- lm(density ~ age + I(age^2), reg.data[reg.data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age" = seq(30, 40, 1/3))))
reg.post <- lm(density ~ age + I(age^2), reg.data[reg.data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age" = seq(40, 50, 1/3))))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.lwage0 <- aggregate(cbind(data$age, data$lwage0) ~ bins, FUN = mean)
plot(plot.bins.lwage0$V1, plot.bins.lwage0$V2,
	xlab = "age at layoff", ylab = "log previous wage", main = "log previous wage against the age at layoff")
abline(v = 40, lty = "dashed")

reg.pre <- lm(lwage0 ~ age + I(age^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age" = seq(30, 40, 1/3))))
reg.post <- lm(lwage0 ~ age + I(age^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age" = seq(40, 50, 1/3))))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


#	Question 4.
bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.nonemp <- aggregate(cbind(data$age, data$nonemp) ~ bins, FUN = mean)
plot.bins.jobfind <- aggregate(cbind(data$age, data$jobfind) ~ bins, FUN = mean)
plot.bins.lwage1 <- aggregate(cbind(data$age, data$lwage1) ~ bins, FUN = mean)

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "age at layoff", ylab = "non-employment duration in weeks", main = "non-employment duration in weeks against the age at layoff")
abline(v = 40, lty = "dashed")
reg.pre <- lm(nonemp ~ age + I(age^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age" = seq(30, 40, 1/3))))
reg.post <- lm(nonemp ~ age + I(age^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age" = seq(40, 50, 1/3))))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "age at layoff", ylab = "probability of finding a job within 39 weeks", main = "probability of finding a job within 39 weeks against the age at layof")
abline(v = 40, lty = "dashed")
reg.pre <- lm(jobfind ~ age + I(age^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age" = seq(30, 40, 1/3))))
reg.post <- lm(jobfind ~ age + I(age^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age" = seq(40, 50, 1/3))))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "age at layoff", ylab = "log monthly wage in new job", main = "log monthly wage in new job against the age at layoff")
abline(v = 40, lty = "dashed")
reg.pre <- lm(lwage1 ~ age + I(age^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age" = seq(30, 40, 1/3))))
reg.post <- lm(lwage1 ~ age + I(age^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age" = seq(40, 50, 1/3))))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)