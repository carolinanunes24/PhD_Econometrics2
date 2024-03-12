#	Clear environment.
rm(list = ls())

#	Set the working directory.
setwd("C:/Users/frits/Documents/MLitt/ECON50580 PhD Econometrics 2/PhD_Econometrics2")

#	Load haven package.
library("haven")

#	Load data.
data <- read_dta("data_ps4.dta")

#	Add dummy variable.
data$above40 <- 0
data[data$age >= 40,]$above40 <- 1

#	Normalise age around 40.
data$age.normalised <- data$age - 40


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
plot(seq(30, 50, 1/3)-1/6, age.density,
	xlab = "Age at layoff", ylab = "Density", main = "Density of age at layoff",
	ylim = c(0, 0.02))
abline(v = 40, lty = "dashed")

reg.data <- data.frame("age.normalised" = seq(30, 50, 1/3)-40, "density" = age.density)
reg.pre <- lm(density ~ age.normalised + I(age.normalised^2), reg.data[reg.data$age.normalised <= 0,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age.normalised" = seq(30, 40, 1/3)-40)))
reg.post <- lm(density ~ age.normalised + I(age.normalised^2), reg.data[reg.data$age.normalised >= 0,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age.normalised" = seq(40, 50, 1/3)-40)))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.lwage0 <- aggregate(cbind(data$age, data$lwage0) ~ bins, FUN = mean)
plot(plot.bins.lwage0$V1, plot.bins.lwage0$V2,
	xlab = "Age at layoff", ylab = "Log previous wage", main = "Log previous wage against the age at layoff")
abline(v = 40, lty = "dashed")

reg.pre <- lm(lwage0 ~ age.normalised + I(age.normalised^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age.normalised" = seq(30, 40, 1/3)-40)))
reg.post <- lm(lwage0 ~ age.normalised + I(age.normalised^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age.normalised" = seq(40, 50, 1/3)-40)))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


#	Question 4.
bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.nonemp <- aggregate(cbind(data$age, data$nonemp) ~ bins, FUN = mean)
plot.bins.jobfind <- aggregate(cbind(data$age, data$jobfind) ~ bins, FUN = mean)
plot.bins.lwage1 <- aggregate(cbind(data$age, data$lwage1) ~ bins, FUN = mean)

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Non-employment duration in weeks against the age at layoff")
abline(v = 40, lty = "dashed")

reg.pre <- lm(nonemp ~ age.normalised + I(age.normalised^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age.normalised" = seq(30, 40, 1/3)-40)))
reg.post <- lm(nonemp ~ age.normalised + I(age.normalised^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age.normalised" = seq(40, 50, 1/3)-40)))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability of finding a job within 39 weeks", main = "Probability of finding a job within 39 weeks against the age at layoff")
abline(v = 40, lty = "dashed")
reg.pre <- lm(jobfind ~ age.normalised + I(age.normalised^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age.normalised" = seq(30, 40, 1/3)-40)))
reg.post <- lm(jobfind ~ age.normalised + I(age.normalised^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age.normalised" = seq(40, 50, 1/3)-40)))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Log monthly wage in new job against the age at layoff")
abline(v = 40, lty = "dashed")
reg.pre <- lm(lwage1 ~ age.normalised + I(age.normalised^2), data[data$age <= 40,])
reg.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.pre, data.frame("age.normalised" = seq(30, 40, 1/3)-40)))
reg.post <- lm(lwage1 ~ age.normalised + I(age.normalised^2), data[data$age >= 40,])
reg.post <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.post, data.frame("age.normalised" = seq(40, 50, 1/3)-40)))
lines(reg.pre$age, reg.pre$predict)
lines(reg.post$age, reg.post$predict)


#	Question 5.
library("rdrobust")
reg.1.lwage1 <- lm(lwage1 ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data)
reg.2.lwage1 <- lm(lwage1 ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -5 & data$age.normalised <= 5,])
reg.3.lwage1 <- lm(lwage1 ~ age.normalised + as.factor(above40) + as.factor(above40) * age.normalised, data)
reg.4.lwage1 <- lm(lwage1 ~ age.normalised + I(age.normalised^2) + I(age.normalised^3) + I(age.normalised^4) + as.factor(above40) + as.factor(above40)*age.normalised + as.factor(above40)*I(age.normalised^2) + as.factor(above40)*I(age.normalised^3) + as.factor(above40)*I(age.normalised^4), data)
optimal.bandwidth <- rdbwselect(data$lwage1, data$age.normalised)
reg.5.lwage1 <- lm(lwage1 ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -optimal.bandwidth$bws[1] & data$age.normalised <= optimal.bandwidth$bws[2],])

reg.1.lwage1.full <- summary(reg.1.lwage1); reg.2.lwage1.full <- summary(reg.2.lwage1); reg.3.lwage1.full <- summary(reg.3.lwage1); reg.4.lwage1.full <- summary(reg.4.lwage1); reg.5.lwage1.full <- summary(reg.5.lwage1);


reg.1.nonemp <- lm(nonemp ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data)
reg.2.nonemp <- lm(nonemp ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -5 & data$age.normalised <= 5,])
reg.3.nonemp <- lm(nonemp ~ age.normalised + as.factor(above40) + as.factor(above40) * age.normalised, data)
reg.4.nonemp <- lm(nonemp ~ age.normalised + I(age.normalised^2) + I(age.normalised^3) + I(age.normalised^4) + as.factor(above40) + as.factor(above40)*age.normalised + as.factor(above40)*I(age.normalised^2) + as.factor(above40)*I(age.normalised^3) + as.factor(above40)*I(age.normalised^4), data)
optimal.bandwidth <- rdbwselect(data$nonemp, data$age.normalised)
reg.5.nonemp <- lm(nonemp ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -optimal.bandwidth$bws[1] & data$age.normalised <= optimal.bandwidth$bws[2],])

reg.1.nonemp.full <- summary(reg.1.nonemp); reg.2.nonemp.full <- summary(reg.2.nonemp); reg.3.nonemp.full <- summary(reg.3.nonemp); reg.4.nonemp.full <- summary(reg.4.nonemp); reg.5.nonemp.full <- summary(reg.5.nonemp);


reg.1.jobfind <- lm(jobfind ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data)
reg.2.jobfind <- lm(jobfind ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -5 & data$age.normalised <= 5,])
reg.3.jobfind <- lm(jobfind ~ age.normalised + as.factor(above40) + as.factor(above40) * age.normalised, data)
reg.4.jobfind <- lm(jobfind ~ age.normalised + I(age.normalised^2) + I(age.normalised^3) + I(age.normalised^4) + as.factor(above40) + as.factor(above40)*age.normalised + as.factor(above40)*I(age.normalised^2) + as.factor(above40)*I(age.normalised^3) + as.factor(above40)*I(age.normalised^4), data)
optimal.bandwidth <- rdbwselect(data$jobfind, data$age.normalised)
reg.5.jobfind <- lm(jobfind ~ age.normalised + I(age.normalised^2) + as.factor(above40) + as.factor(above40) * age.normalised + as.factor(above40) * I(age.normalised^2), data[data$age.normalised >= -optimal.bandwidth$bws[1] & data$age.normalised <= optimal.bandwidth$bws[2],])

reg.1.jobfind.full <- summary(reg.1.jobfind); reg.2.jobfind.full <- summary(reg.2.jobfind); reg.3.jobfind.full <- summary(reg.3.jobfind); reg.4.jobfind.full <- summary(reg.4.jobfind); reg.5.jobfind.full <- summary(reg.5.jobfind);


#	Additional code.
bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.lwage1 <- aggregate(cbind(data$age, data$lwage1) ~ bins, FUN = mean)

bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.nonemp<- aggregate(cbind(data$age, data$nonemp) ~ bins, FUN = mean)

bins <- cut(data$age, breaks = c(seq(30, 50, 1/3)))
plot.bins.jobfind<- aggregate(cbind(data$age, data$jobfind) ~ bins, FUN = mean)


#	lwage1.
layout(matrix(c(1:5,0), ncol = 2, byrow = TRUE), widths = c(4,4), heights = c(3,3,3))

plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Regression 1")
abline(v = 40, lty = "dashed")
reg.1.lwage1.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.1.lwage1, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.1.lwage1.pre$age, reg.1.lwage1.pre$predict.fit)
lines(reg.1.lwage1.pre$age, reg.1.lwage1.pre$predict.lwr, lty = "dashed")
lines(reg.1.lwage1.pre$age, reg.1.lwage1.pre$predict.upr, lty = "dashed")
reg.1.lwage1.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.1.lwage1, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.1.lwage1.post$age, reg.1.lwage1.post$predict.fit)
lines(reg.1.lwage1.post$age, reg.1.lwage1.post$predict.lwr, lty = "dashed")
lines(reg.1.lwage1.post$age, reg.1.lwage1.post$predict.upr, lty = "dashed")

plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Regression 2")
abline(v = 40, lty = "dashed")
reg.2.lwage1.pre <- data.frame("age" = seq(35, 40, 1/3), "predict" = predict(reg.2.lwage1, data.frame("age.normalised" = seq(35, 40, 1/3)-40, "above40" = rep(0, 16)), interval = "confidence"))
lines(reg.2.lwage1.pre$age, reg.2.lwage1.pre$predict.fit)
lines(reg.2.lwage1.pre$age, reg.2.lwage1.pre$predict.lwr, lty = "dashed")
lines(reg.2.lwage1.pre$age, reg.2.lwage1.pre$predict.upr, lty = "dashed")
reg.2.lwage1.post  <- data.frame("age" = seq(40, 45, 1/3), "predict" = predict(reg.2.lwage1, data.frame("age.normalised" = seq(40, 45, 1/3)-40, "above40" = rep(1, 16)), interval = "confidence"))
lines(reg.2.lwage1.post$age, reg.2.lwage1.post$predict.fit)
lines(reg.2.lwage1.post$age, reg.2.lwage1.post$predict.lwr, lty = "dashed")
lines(reg.2.lwage1.post$age, reg.2.lwage1.post$predict.upr, lty = "dashed")

plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Regression 3")
abline(v = 40, lty = "dashed")
reg.3.lwage1.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.3.lwage1, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.3.lwage1.pre$age, reg.3.lwage1.pre$predict.fit)
lines(reg.3.lwage1.pre$age, reg.3.lwage1.pre$predict.lwr, lty = "dashed")
lines(reg.3.lwage1.pre$age, reg.3.lwage1.pre$predict.upr, lty = "dashed")
reg.3.lwage1.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.3.lwage1, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.3.lwage1.post$age, reg.3.lwage1.post$predict.fit)
lines(reg.3.lwage1.post$age, reg.3.lwage1.post$predict.lwr, lty = "dashed")
lines(reg.3.lwage1.post$age, reg.3.lwage1.post$predict.upr, lty = "dashed")

plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Regression 4")
abline(v = 40, lty = "dashed")
reg.4.lwage1.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.4.lwage1, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.4.lwage1.pre$age, reg.4.lwage1.pre$predict.fit)
lines(reg.4.lwage1.pre$age, reg.4.lwage1.pre$predict.lwr, lty = "dashed")
lines(reg.4.lwage1.pre$age, reg.4.lwage1.pre$predict.upr, lty = "dashed")
reg.4.lwage1.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.4.lwage1, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.4.lwage1.post$age, reg.4.lwage1.post$predict.fit)
lines(reg.4.lwage1.post$age, reg.4.lwage1.post$predict.lwr, lty = "dashed")
lines(reg.4.lwage1.post$age, reg.4.lwage1.post$predict.upr, lty = "dashed")

plot(plot.bins.lwage1$V1, plot.bins.lwage1$V2,
	xlab = "Age at layoff", ylab = "Log monthly wage in new job", main = "Regression 5")
abline(v = 40, lty = "dashed")
reg.5.lwage1.pre <- data.frame("age" = seq(40-optimal.bandwidth$bws[1], 40, 1/3), "predict" = predict(reg.5.lwage1, data.frame("age.normalised" = seq(40-optimal.bandwidth$bws[1], 40, 1/3)-40, "above40" = rep(0, length(seq(40-optimal.bandwidth$bws[1], 40, 1/3)))), interval = "confidence"))
lines(reg.5.lwage1.pre$age, reg.5.lwage1.pre$predict.fit)
lines(reg.5.lwage1.pre$age, reg.5.lwage1.pre$predict.lwr, lty = "dashed")
lines(reg.5.lwage1.pre$age, reg.5.lwage1.pre$predict.upr, lty = "dashed")
reg.5.lwage1.post  <- data.frame("age" = seq(40, 40+optimal.bandwidth$bws[2], 1/3), "predict" = predict(reg.5.lwage1, data.frame("age.normalised" = seq(40, 40+optimal.bandwidth$bws[2], 1/3)-40, "above40" = rep(0, length(seq(40, 40+optimal.bandwidth$bws[2], 1/3)))), interval = "confidence"))
lines(reg.5.lwage1.post$age, reg.5.lwage1.post$predict.fit)
lines(reg.5.lwage1.post$age, reg.5.lwage1.post$predict.lwr, lty = "dashed")
lines(reg.5.lwage1.post$age, reg.5.lwage1.post$predict.upr, lty = "dashed")


#	nonemp.
layout(matrix(c(1:5,0), ncol = 2, byrow = TRUE), widths = c(4,4), heights = c(3,3,3))

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Regression 1")
abline(v = 40, lty = "dashed")
reg.1.nonemp.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.1.nonemp, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.1.nonemp.pre$age, reg.1.nonemp.pre$predict.fit)
lines(reg.1.nonemp.pre$age, reg.1.nonemp.pre$predict.lwr, lty = "dashed")
lines(reg.1.nonemp.pre$age, reg.1.nonemp.pre$predict.upr, lty = "dashed")
reg.1.nonemp.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.1.nonemp, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.1.nonemp.post$age, reg.1.nonemp.post$predict.fit)
lines(reg.1.nonemp.post$age, reg.1.nonemp.post$predict.lwr, lty = "dashed")
lines(reg.1.nonemp.post$age, reg.1.nonemp.post$predict.upr, lty = "dashed")

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Regression 2")
abline(v = 40, lty = "dashed")
reg.2.nonemp.pre <- data.frame("age" = seq(35, 40, 1/3), "predict" = predict(reg.2.nonemp, data.frame("age.normalised" = seq(35, 40, 1/3)-40, "above40" = rep(0, 16)), interval = "confidence"))
lines(reg.2.nonemp.pre$age, reg.2.nonemp.pre$predict.fit)
lines(reg.2.nonemp.pre$age, reg.2.nonemp.pre$predict.lwr, lty = "dashed")
lines(reg.2.nonemp.pre$age, reg.2.nonemp.pre$predict.upr, lty = "dashed")
reg.2.nonemp.post  <- data.frame("age" = seq(40, 45, 1/3), "predict" = predict(reg.2.nonemp, data.frame("age.normalised" = seq(40, 45, 1/3)-40, "above40" = rep(1, 16)), interval = "confidence"))
lines(reg.2.nonemp.post$age, reg.2.nonemp.post$predict.fit)
lines(reg.2.nonemp.post$age, reg.2.nonemp.post$predict.lwr, lty = "dashed")
lines(reg.2.nonemp.post$age, reg.2.nonemp.post$predict.upr, lty = "dashed")

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Regression 3")
abline(v = 40, lty = "dashed")
reg.3.nonemp.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.3.nonemp, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.3.nonemp.pre$age, reg.3.nonemp.pre$predict.fit)
lines(reg.3.nonemp.pre$age, reg.3.nonemp.pre$predict.lwr, lty = "dashed")
lines(reg.3.nonemp.pre$age, reg.3.nonemp.pre$predict.upr, lty = "dashed")
reg.3.nonemp.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.3.nonemp, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.3.nonemp.post$age, reg.3.nonemp.post$predict.fit)
lines(reg.3.nonemp.post$age, reg.3.nonemp.post$predict.lwr, lty = "dashed")
lines(reg.3.nonemp.post$age, reg.3.nonemp.post$predict.upr, lty = "dashed")

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Regression 4")
abline(v = 40, lty = "dashed")
reg.4.nonemp.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.4.nonemp, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.4.nonemp.pre$age, reg.4.nonemp.pre$predict.fit)
lines(reg.4.nonemp.pre$age, reg.4.nonemp.pre$predict.lwr, lty = "dashed")
lines(reg.4.nonemp.pre$age, reg.4.nonemp.pre$predict.upr, lty = "dashed")
reg.4.nonemp.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.4.nonemp, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.4.nonemp.post$age, reg.4.nonemp.post$predict.fit)
lines(reg.4.nonemp.post$age, reg.4.nonemp.post$predict.lwr, lty = "dashed")
lines(reg.4.nonemp.post$age, reg.4.nonemp.post$predict.upr, lty = "dashed")

plot(plot.bins.nonemp$V1, plot.bins.nonemp$V2,
	xlab = "Age at layoff", ylab = "Non-employment duration in weeks", main = "Regression 5")
abline(v = 40, lty = "dashed")
reg.5.nonemp.pre <- data.frame("age" = seq(40-optimal.bandwidth$bws[1], 40, 1/3), "predict" = predict(reg.5.nonemp, data.frame("age.normalised" = seq(40-optimal.bandwidth$bws[1], 40, 1/3)-40, "above40" = rep(0, length(seq(40-optimal.bandwidth$bws[1], 40, 1/3)))), interval = "confidence"))
lines(reg.5.nonemp.pre$age, reg.5.nonemp.pre$predict.fit)
lines(reg.5.nonemp.pre$age, reg.5.nonemp.pre$predict.lwr, lty = "dashed")
lines(reg.5.nonemp.pre$age, reg.5.nonemp.pre$predict.upr, lty = "dashed")
reg.5.nonemp.post  <- data.frame("age" = seq(40, 40+optimal.bandwidth$bws[2], 1/3), "predict" = predict(reg.5.nonemp, data.frame("age.normalised" = seq(40, 40+optimal.bandwidth$bws[2], 1/3)-40, "above40" = rep(0, length(seq(40, 40+optimal.bandwidth$bws[2], 1/3)))), interval = "confidence"))
lines(reg.5.nonemp.post$age, reg.5.nonemp.post$predict.fit)
lines(reg.5.nonemp.post$age, reg.5.nonemp.post$predict.lwr, lty = "dashed")
lines(reg.5.nonemp.post$age, reg.5.nonemp.post$predict.upr, lty = "dashed")


#	jobfind.
layout(matrix(c(1:5,0), ncol = 2, byrow = TRUE), widths = c(4,4), heights = c(3,3,3))

plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability person had a new job after 39 weeks", main = "Regression 1")
abline(v = 40, lty = "dashed")
reg.1.jobfind.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.1.jobfind, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.1.jobfind.pre$age, reg.1.jobfind.pre$predict.fit)
lines(reg.1.jobfind.pre$age, reg.1.jobfind.pre$predict.lwr, lty = "dashed")
lines(reg.1.jobfind.pre$age, reg.1.jobfind.pre$predict.upr, lty = "dashed")
reg.1.jobfind.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.1.jobfind, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.1.jobfind.post$age, reg.1.jobfind.post$predict.fit)
lines(reg.1.jobfind.post$age, reg.1.jobfind.post$predict.lwr, lty = "dashed")
lines(reg.1.jobfind.post$age, reg.1.jobfind.post$predict.upr, lty = "dashed")

plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability person had a new job after 39 weeks", main = "Regression 2")
abline(v = 40, lty = "dashed")
reg.2.jobfind.pre <- data.frame("age" = seq(35, 40, 1/3), "predict" = predict(reg.2.jobfind, data.frame("age.normalised" = seq(35, 40, 1/3)-40, "above40" = rep(0, 16)), interval = "confidence"))
lines(reg.2.jobfind.pre$age, reg.2.jobfind.pre$predict.fit)
lines(reg.2.jobfind.pre$age, reg.2.jobfind.pre$predict.lwr, lty = "dashed")
lines(reg.2.jobfind.pre$age, reg.2.jobfind.pre$predict.upr, lty = "dashed")
reg.2.jobfind.post  <- data.frame("age" = seq(40, 45, 1/3), "predict" = predict(reg.2.jobfind, data.frame("age.normalised" = seq(40, 45, 1/3)-40, "above40" = rep(1, 16)), interval = "confidence"))
lines(reg.2.jobfind.post$age, reg.2.jobfind.post$predict.fit)
lines(reg.2.jobfind.post$age, reg.2.jobfind.post$predict.lwr, lty = "dashed")
lines(reg.2.jobfind.post$age, reg.2.jobfind.post$predict.upr, lty = "dashed")

plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability person had a new job after 39 weeks", main = "Regression 3")
abline(v = 40, lty = "dashed")
reg.3.jobfind.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.3.jobfind, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.3.jobfind.pre$age, reg.3.jobfind.pre$predict.fit)
lines(reg.3.jobfind.pre$age, reg.3.jobfind.pre$predict.lwr, lty = "dashed")
lines(reg.3.jobfind.pre$age, reg.3.jobfind.pre$predict.upr, lty = "dashed")
reg.3.jobfind.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.3.jobfind, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.3.jobfind.post$age, reg.3.jobfind.post$predict.fit)
lines(reg.3.jobfind.post$age, reg.3.jobfind.post$predict.lwr, lty = "dashed")
lines(reg.3.jobfind.post$age, reg.3.jobfind.post$predict.upr, lty = "dashed")

plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability person had a new job after 39 weeks", main = "Regression 4")
abline(v = 40, lty = "dashed")
reg.4.jobfind.pre <- data.frame("age" = seq(30, 40, 1/3), "predict" = predict(reg.4.jobfind, data.frame("age.normalised" = seq(30, 40, 1/3)-40, "above40" = rep(0, 31)), interval = "confidence"))
lines(reg.4.jobfind.pre$age, reg.4.jobfind.pre$predict.fit)
lines(reg.4.jobfind.pre$age, reg.4.jobfind.pre$predict.lwr, lty = "dashed")
lines(reg.4.jobfind.pre$age, reg.4.jobfind.pre$predict.upr, lty = "dashed")
reg.4.jobfind.post  <- data.frame("age" = seq(40, 50, 1/3), "predict" = predict(reg.4.jobfind, data.frame("age.normalised" = seq(40, 50, 1/3)-40, "above40" = rep(1, 31)), interval = "confidence"))
lines(reg.4.jobfind.post$age, reg.4.jobfind.post$predict.fit)
lines(reg.4.jobfind.post$age, reg.4.jobfind.post$predict.lwr, lty = "dashed")
lines(reg.4.jobfind.post$age, reg.4.jobfind.post$predict.upr, lty = "dashed")

plot(plot.bins.jobfind$V1, plot.bins.jobfind$V2,
	xlab = "Age at layoff", ylab = "Probability person had a new job after 39 weeks", main = "Regression 5")
abline(v = 40, lty = "dashed")
reg.5.jobfind.pre <- data.frame("age" = seq(40-optimal.bandwidth$bws[1], 40, 1/3), "predict" = predict(reg.5.jobfind, data.frame("age.normalised" = seq(40-optimal.bandwidth$bws[1], 40, 1/3)-40, "above40" = rep(0, length(seq(40-optimal.bandwidth$bws[1], 40, 1/3)))), interval = "confidence"))
lines(reg.5.jobfind.pre$age, reg.5.jobfind.pre$predict.fit)
lines(reg.5.jobfind.pre$age, reg.5.jobfind.pre$predict.lwr, lty = "dashed")
lines(reg.5.jobfind.pre$age, reg.5.jobfind.pre$predict.upr, lty = "dashed")
reg.5.jobfind.post  <- data.frame("age" = seq(40, 40+optimal.bandwidth$bws[2], 1/3), "predict" = predict(reg.5.jobfind, data.frame("age.normalised" = seq(40, 40+optimal.bandwidth$bws[2], 1/3)-40, "above40" = rep(0, length(seq(40, 40+optimal.bandwidth$bws[2], 1/3)))), interval = "confidence"))
lines(reg.5.jobfind.post$age, reg.5.jobfind.post$predict.fit)
lines(reg.5.jobfind.post$age, reg.5.jobfind.post$predict.lwr, lty = "dashed")
lines(reg.5.jobfind.post$age, reg.5.jobfind.post$predict.upr, lty = "dashed")