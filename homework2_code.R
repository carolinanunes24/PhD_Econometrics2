#	Clear environment.
rm(list = ls())

#	Set seed.
set.seed(231)

#	Load required packages.
library("MASS")

#	Question 2.
#	Setting parameters.
gamma0 <- 0
gamma1 <- 0.25
alpha <- 0
beta <- 0.75
mean.vector.xez <- c(3, 0, 2)

#	Sub-question A.
varcov.matrix.xez <- rbind(c(1, 0.4, 0.5), c(0.4, 1, 0), c(0.5, 0, 1))
colour <- 1
for (n in c(50, 100, 250, 1000)) {
	beta.ols <- c()
	beta.iv <- c()
	cor.zx <- c()
	cor.xe <- c()
	cor.ze <- c()
	for (i in 1:10000){
		df <- mvrnorm(n, mean.vector.xez, varcov.matrix.xez)
		x <- df[,1]
		epsilon <- df[,2]
		z <- df[,3]
		y <- alpha + beta * x + epsilon
		u <- x - gamma0 + gamma1 * z
		cor.zx <- append(cor.zx, cor(z, x, method = "pearson"))
		cor.xe <- append(cor.xe, cor(x, epsilon, method = "pearson"))
		cor.ze <- append(cor.ze, cor(z, epsilon, method = "pearson"))
		ols <- lm(y ~ x)
		first.stage <- lm(x ~ z)
		x.fitted <- first.stage$fitted.values
		second.stage <- lm(y ~ x.fitted)
		beta.ols <- append(beta.ols, ols$coefficients[2])
		beta.iv <- append(beta.iv, second.stage$coefficients[2])
	}
	if (colour == 1){
		plot(density(beta.ols), xlim = c(0, 2), ylim = c(0, 15), 
		xlab = "", col = colour, 
		main = bquote("Distributions of" ~ beta[OLS] ~ "and" ~ beta[IV]))
		lines(density(beta.iv), col = colour + 1)
	} else {
		lines(density(beta.ols), col = colour)
		lines(density(beta.iv), col = colour + 1)
	}
	colour <- colour + 2
}
legend("topright",
	 sapply(c(bquote(beta[OLS]~","~N==50), bquote(beta[IV]~","~N==50),
		    bquote(beta[OLS]~","~N==100), bquote(beta[IV]~","~N==100),
		    bquote(beta[OLS]~","~N==250), bquote(beta[IV]~","~N==250),
		    bquote(beta[OLS]~","~N==1000), bquote(beta[IV]~","~N==1000)), 
	 as.expression),
	 col = 1:8, lty = 1)

#	Sub-question B.
varcov.matrix.xez <- rbind(c(1, 0.4, 0.15), c(0.4, 1, 0), c(0.15, 0, 1))
colour <- 1
for (n in c(50, 100, 250, 1000)) {
	beta.ols <- c()
	beta.iv <- c()
	cor.zx <- c()
	cor.xe <- c()
	cor.ze <- c()
	for (i in 1:10000){
		df <- mvrnorm(n, mean.vector.xez, varcov.matrix.xez)
		x <- df[,1]
		epsilon <- df[,2]
		z <- df[,3]
		y <- alpha + beta * x + epsilon
		u <- x - gamma0 + gamma1 * z
		cor.zx <- append(cor.zx, cor(z, x, method = "pearson"))
		cor.xe <- append(cor.xe, cor(x, epsilon, method = "pearson"))
		cor.ze <- append(cor.ze, cor(z, epsilon, method = "pearson"))
		ols <- lm(y ~ x)
		first.stage <- lm(x ~ z)
		x.fitted <- first.stage$fitted.values
		second.stage <- lm(y ~ x.fitted)
		beta.ols <- append(beta.ols, ols$coefficients[2])
		beta.iv <- append(beta.iv, second.stage$coefficients[2])
	}
	if (colour == 1){
		plot(density(beta.ols), xlim = c(-1, 2), ylim = c(0, 15), 
		xlab = "", col = colour, 
		main = bquote("Distributions of" ~ beta[OLS] ~ "and" ~ beta[IV]))
		lines(density(beta.iv, n = 1e+6), col = colour + 1)
	} else {
		lines(density(beta.ols), col = colour)
		lines(density(beta.iv, n = 1e+6), col = colour + 1)
	}
	colour <- colour + 2
}
legend("topleft",
	 sapply(c(bquote(beta[OLS]~","~N==50), bquote(beta[IV]~","~N==50),
		    bquote(beta[OLS]~","~N==100), bquote(beta[IV]~","~N==100),
		    bquote(beta[OLS]~","~N==250), bquote(beta[IV]~","~N==250),
		    bquote(beta[OLS]~","~N==1000), bquote(beta[IV]~","~N==1000)), 
	 as.expression),
	 col = 1:8, lty = 1)

########################################################################

#	Clear environment.
rm(list = ls())

#	Load required packages.
library("AER")

#	Set the working directory.
setwd("C:/Users/frits/Documents/MLitt/ECON50580 PhD Econometrics 2/PhD_Econometrics2")

#	Import assign2.dta .
data <- read_dta("assign2.dta")

#	Question 3.

#	Sub-question A.
reg.ols <- lm(logearn ~ schooling + age + I(age^2) + I(age^3) + I(age^4) + yob + I(yob^2) + I(yob^3) + I(yob^4), data)
reg.ols <- summary(reg.ols)

#	Sub-question C.
data$leave15 <- 0
data[data$schooling < 15, "leave15"] <- 1
probability.leaving.school <- c()
for (i in 21:45) {
	probability.leaving.school <- append(probability.leaving.school, 
						sum(data[data$yob == i,]$leave15) / dim(data[data$yob == i,])[1])
}
probability.leaving.school <- data.frame("yob" = 21:45, "probability" = probability.leaving.school)

plot(probability.leaving.school$yob, probability.leaving.school$probability,
	main = "Probability of leaving school before the age of 15 by year of birth",
	xlab = "Year of birth", ylab = "Probability")
abline(v = 33)

bins <- cut(data$yob, breaks = 21:45)
plot.bins.yob <- aggregate(cbind(data$yob, data$schooling) ~ bins, FUN = mean)
plot(plot.bins.yob$V1, plot.bins.yob$V2,
	main = "Average years of schooling by year of birth",
	xlab = "Year of birth", ylab = "Average years of schooling")
abline(v = 33)

bins <- cut(data$yob, breaks = 21:45)
plot.bins.yob <- aggregate(cbind(data$yob, data$logearn) ~ bins, FUN = mean)
plot(plot.bins.yob$V1, plot.bins.yob$V2,
	main = "Average log of earnings by year of birth",
	xlab = "Year of birth", ylab = "Average log of earnings")
abline(v = 33)

#	Sub-question D.
data$yob33 <- 0
data[data$yob < 33, "yob33"] <- 1
reg.2sls <- ivreg(logearn ~ schooling | yob33, data = data)
reg.2sls <- summary(reg.2sls)

conditional.mean.Y.Z1 <- mean(data[data$yob33 == 1,]$logearn)
conditional.mean.Y.Z0 <- mean(data[data$yob33 == 0,]$logearn)
conditional.mean.D.Z1 <- mean(data[data$yob33 == 1,]$schooling)
conditional.mean.D.Z0 <- mean(data[data$yob33 == 0,]$schooling)

wald.estimator <- (conditional.mean.Y.Z1 - conditional.mean.Y.Z0) / (conditional.mean.D.Z1 - conditional.mean.D.Z0)

